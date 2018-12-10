------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2018, AdaCore                      --
--                                                                          --
-- SVD2Ada is free software;  you can  redistribute it  and/or modify it    --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version.  SVD2Ada is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTA-  --
-- BILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public  --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General Public License  distributed with SVD2Ada; see file COPYING3.  If --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;

--  XML dependencies
with Input_Sources.File;          use Input_Sources.File;
with Sax.Readers;
with Schema.Dom_Readers;          use Schema.Dom_Readers;
with Schema.Schema_Readers;       use Schema.Schema_Readers;
with Schema.Validators;           use Schema.Validators;
with DOM.Core;                    use DOM.Core;
with DOM.Core.Documents;

with Ada_Gen;
with Descriptors.Device;
with SVD2Ada_Utils;

--  SVD Binding Generator: this tool is meant to handle
--  A SVD file is an xml file representing a specific hardware device, and
--  uses the following layers to describe it:
--  * <device>: the root element. Contains an optional cpu definition, some
--    default properties, and a list of peripherals
--  * <peripheral>: A peripheral descriptor. Each peripheral has a base
--    address, and a list of registers. They also can be grouped into groups
--    of peripherals of similar or interrelated functionalities.
--    When such group exist, this binding generator will group all the
--    peripherals of a given group into the same spec file.
--  * <register>: The description of a specific register of a peripheral.
--    Optionally, the different fields of a register can also be described.
--  * <field>: The field of a descriptor. Each field has a specific name,
--    a size and an offset from the register base address.
--  * <enumaratedValue>: Also optional, the SVD file may contain a description
--    of the valid values that are expected for a given field. If such
--    element is present, then the binding generator will create an enum
--    for the field, and use this enum as type of the field.
procedure SVD2Ada
is
   --  Local variables used for XML parsing
   Input           : File_Input;
   Reader          : Tree_Reader;
   Sc_Reader       : Schema_Reader;
   Grammar         : XML_Grammar;
   Doc             : Document;
   Schema          : constant String :=
                       GNAT.OS_Lib.Normalize_Pathname
                         ("schema/CMSIS-SVD_Schema_1_3_1.xsd",
                          SVD2Ada_Utils.Executable_Location);

   --  The produced Device
   Device          : Descriptors.Device.Device_T;
   SVD_File        : Unbounded_String;

   --  Command line parser
   Cmd_Line_Cfg    : GNAT.Command_Line.Command_Line_Configuration;
   Pkg             : aliased GNAT.Strings.String_Access;
   Out_Dir         : aliased GNAT.Strings.String_Access;
   Base_Types_Pkg  : aliased GNAT.Strings.String_Access;
   Gen_Booleans    : aliased Boolean := False;
   Gen_UInt_Always : aliased Boolean := False;
   No_UInt_Subtype : aliased Boolean := False;
   No_Arrays       : aliased Boolean := False;
   No_Defaults     : aliased Boolean := False;
   No_VFA_On_Types : aliased Boolean := False;
   Gen_IRQ_Support : aliased Boolean := False;

   use type GNAT.Strings.String_Access;

begin
   GNAT.Command_Line.Set_Usage
     (Cmd_Line_Cfg,
      Usage => "[options] -o DIR svd-file",
      Help  => "Generate Ada bindings from CMSIS-SVD Cortex-M hardware " &
        "description files");
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => Out_Dir'Access,
      Switch      => "-o=",
      Long_Switch => "--output=",
      Help        => "the destination directory used to " &
        "generate the binding",
      Argument    => "DIR");
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => Pkg'Access,
      Switch      => "-p=",
      Long_Switch => "--package=",
      Help        => "use pkg_name as main package name for the generated " &
        "spec hierarchy",
      Argument    => "Pkg_Name");
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => Base_Types_Pkg'Access,
      Long_Switch => "--base-types-package=",
      Help        => "the name of the package containing the low level types" &
        " definitions. If undefined, those types will be specified in the" &
        " root package.");
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => Gen_Booleans'Access,
      Long_Switch => "--boolean",
      Help        => "treat bit fields as boolean. Ignored if an enumerate " &
        "is defined for the field",
      Value       => True);
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => Gen_IRQ_Support'Access,
      Long_Switch => "--gen-interrupts",
      Help        => "Generate trap handlers and interrupt name package. " &
        "Activated by default if the generated root package is a run-time " &
        "package",
      Value       => True);
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => Gen_UInt_Always'Access,
      Long_Switch => "--gen-uint-always",
      Help        => "when generating base types, always consider UInt* and" &
        " do not use the Bit and Bytes variants for types with size 1 and 8",
      Value       => True);
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => No_Arrays'Access,
      Long_Switch => "--no-arrays",
      Help        => "in some circumstances (similar names indexed by " &
        "ascending numbers), svd2ada can generate arrays of registers or " &
        "arrays of register fields. This switch prevents this behavior",
      Value       => True);
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => No_Defaults'Access,
      Long_Switch => "--no-defaults",
      Help        => "Do not use the registers reset values to provide" &
        " default fields values",
      Value       => True);
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => No_UInt_Subtype'Access,
      Long_Switch => "--no-uint-subtypes",
      Help        => "do not generate subtypes for fields, but use the base" &
        " uint type instead",
      Value       => True);
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => No_VFA_On_Types'Access,
      Long_Switch => "--no-vfa-on-types",
      Help        => "when generating register types, do not specify the" &
        " Volatile_Full_Access aspect on them (specify it instead on the" &
        " enclosing peripheral fields",
      Value       => True);

   GNAT.Command_Line.Getopt
     (Config => Cmd_Line_Cfg);

   declare
      SVD : constant String := GNAT.Command_Line.Get_Argument;
   begin
      if SVD = "" or else Out_Dir = null or else Out_Dir.all = "" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Error: missing arguments");
         GNAT.Command_Line.Try_Help;

         Ada.Command_Line.Set_Exit_Status (2);
         return;
      end if;

      SVD_File := To_Unbounded_String (GNAT.OS_Lib.Normalize_Pathname (SVD));
   end;

   if Pkg.all /= "" then
      SVD2Ada_Utils.Set_Root_Package (Pkg.all);

      --  If Pkg is a runtime package, force the generation of Interrupts.Name
      --  and IRQ trap vector.
      if SVD2Ada_Utils.In_Runtime then
         Gen_IRQ_Support := True;
      end if;
   end if;

   SVD2Ada_Utils.Set_Use_Boolean_For_Bit (Gen_Booleans);
   SVD2Ada_Utils.Set_Gen_IRQ_Support (Gen_IRQ_Support);
   SVD2Ada_Utils.Set_Use_UInt (Gen_UInt_Always);
   SVD2Ada_Utils.Set_Gen_Arrays (not No_Arrays);
   SVD2Ada_Utils.Set_No_Defaults (No_Defaults);
   SVD2Ada_Utils.Set_No_UInt_Subtype (No_UInt_Subtype);
   SVD2Ada_Utils.Set_No_VFA_On_Reg_Types (No_VFA_On_Types);

   if Base_Types_Pkg.all /= "" then
      SVD2Ada_Utils.Set_Base_Types_Package (Base_Types_Pkg.all);
   end if;

   Ada_Gen.Set_Input_File_Name
     (GNAT.Directory_Operations.Base_Name (To_String (SVD_File)));

   --  Open the schema file
   Input_Sources.File.Open (Schema, Input);
   Sc_Reader.Parse (Input);
   Close (Input);
   Grammar := Get_Grammar (Sc_Reader);

   --  tell the reader to use the schema to validate the SVD file
   Reader.Set_Grammar (Grammar);
   Set_Feature (Reader, Sax.Readers.Schema_Validation_Feature, False);
   Use_Basename_In_Error_Messages (Reader, True);

   Input_Sources.File.Open (To_String (SVD_File), Input);
   Reader.Parse (Input);
   Close (Input);

   Doc := Get_Tree (Reader);

   --  Check if we have some helper file to give us hints as to how generate
   --  some structures. Rule: if we have both XXXX.svd and XXXX.svd2ada, then
   --  the svd2ada file is our helper file
   if GNAT.OS_Lib.Is_Regular_File (To_String (SVD_File) & "2ada") then
      Input_Sources.File.Open (To_String (SVD_File) & "2ada", Input);

      Set_Feature (Reader, Sax.Readers.Schema_Validation_Feature, False);
      Use_Basename_In_Error_Messages (Reader, True);
      Reader.Parse (Input);
      Close (Input);
   end if;

   Device := Descriptors.Device.Read_Device
     (Documents.Get_Element (Doc),
      (if Pkg = null then "" else Pkg.all));

   Descriptors.Device.Dump (Device, Out_Dir.all);

exception
   when GNAT.Command_Line.Invalid_Switch |
        GNAT.Command_Line.Invalid_Parameter |
        GNAT.Command_Line.Exit_From_Command_Line =>
      Ada.Command_Line.Set_Exit_Status (1);

   when XML_Validation_Error =>
      Close (Input);
      Ada.Text_IO.Put_Line ("Non-valid SVD file:");
      Ada.Text_IO.Put_Line (Reader.Get_Error_Message);
      Ada.Command_Line.Set_Exit_Status (2);

   when E : Sax.Readers.XML_Fatal_Error =>
      Close (Input);
      Ada.Text_IO.Put_Line ("Fatal error when parsing the svd file:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (2);

end SVD2Ada;
