------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2020, AdaCore                      --
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
with Ada.Text_IO;                 use Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
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
with SVD2Ada_Utils;               use SVD2Ada_Utils;

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

procedure SVD2Ada is

   Doc    : Document;
   Schema : constant String := GNAT.OS_Lib.Normalize_Pathname
                         ("schema/CMSIS-SVD_Schema_1_3_1.xsd",
                          Executable_Location);

   --  The generated Device
   Device          : Descriptors.Device.Device_T;
   SVD_File_Name   : Unbounded_String;

   --  Command line parser and switches/arguments
   Cmd_Line_Cfg    : GNAT.Command_Line.Command_Line_Configuration;
   Root_Pkg_Name   : aliased GNAT.Strings.String_Access;
   Output_Dir      : aliased GNAT.Strings.String_Access;
   Base_Types_Pkg  : aliased GNAT.Strings.String_Access;
   Gen_Booleans    : aliased Boolean := False;
   Gen_UInt_Always : aliased Boolean := False;
   No_UInt_Subtype : aliased Boolean := False;
   No_Arrays       : aliased Boolean := False;
   No_Defaults     : aliased Boolean := False;
   No_VFA_On_Types : aliased Boolean := False;
   Gen_IRQ_Support : aliased Boolean := False;

   use type GNAT.Strings.String_Access;

   procedure Configure_Command_Line;
   --  Define the switches used on the command line

   procedure Apply_Command_Line;
   --  Set the generation switches and options based on command line settings

   procedure Get_SVD_File_Name (Name : out Unbounded_String);
   --  Get the normalized XML input file name

   procedure Parse_XML_Files
     (File_Name : String;
      Schema    : String;
      Doc       : out Document);
   --  Parse the XML in the SVD input file and check for a helper file to
   --  give us hints as to how generate some structures

   ----------------------------
   -- Configure_Command_Line --
   ----------------------------

   procedure Configure_Command_Line is
      use GNAT.Command_Line;
   begin
      Set_Usage
        (Cmd_Line_Cfg,
         Usage => "[options] -o DIR svd-file",
         Help  => "Generate Ada bindings from CMSIS-SVD hardware description files");
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => Output_Dir'Access,
         Switch      => "-o=",
         Long_Switch => "--output=",
         Help        => "directory to contain the generated binding",
         Argument    => "DIR");
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => Root_Pkg_Name'Access,
         Switch      => "-p=",
         Long_Switch => "--package=",
         Help        => "the root package name for the generated spec hierarchy",
         Argument    => "Pkg_Name");
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => Base_Types_Pkg'Access,
         Long_Switch => "--base-types-package=",
         Help        => "the name of the package containing low level types' definitions");
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => Gen_Booleans'Access,
         Long_Switch => "--boolean",
         Help        => "represent single-bit fields as Booleans",
         Value       => True);
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => Gen_IRQ_Support'Access,
         Long_Switch => "--gen-interrupts",
         Help        => "generate trap handlers and package Interrupts.Names",
         Value       => True);
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => Gen_UInt_Always'Access,
         Long_Switch => "--gen-uint-always",
         Help        => "generate UInt* for base types; do not use the Bit and Byte variants",
         Value       => True);
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => No_Arrays'Access,
         Long_Switch => "--no-arrays",
         Help        => "don't generate arrays of registers or arrays of register fields",
         Value       => True);
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => No_Defaults'Access,
         Long_Switch => "--no-defaults",
         Help        => "don't use register reset values for fields' default values",
         Value       => True);
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => No_UInt_Subtype'Access,
         Long_Switch => "--no-uint-subtypes",
         Help        => "use the corresponding UInt type instead of subtypes for fields",
         Value       => True);
      Define_Switch
        (Cmd_Line_Cfg,
         Output      => No_VFA_On_Types'Access,
         Long_Switch => "--no-vfa-on-types",
         Help        => "apply Volatile_Full_Access to fields rather than register types. " &
                        "Forces --no-arrays",
                        --  Apply it to individual fields of some enclosing
                        --  register record type when those fields are
                        --  themselves represented as register types
         Value       => True);
   end Configure_Command_Line;

   ------------------------
   -- Apply_Command_Line --
   ------------------------

   procedure Apply_Command_Line is
   begin
      if Root_Pkg_Name.all /= "" then
         Set_Root_Package (Root_Pkg_Name.all);

         --  If Pkg is a runtime package, force the generation of Interrupts.Names
         --  and IRQ trap vector.
         if SVD2Ada_Utils.In_Runtime then
            Gen_IRQ_Support := True;
         end if;
      end if;

      Set_Use_Boolean_For_Bit (Gen_Booleans);
      Set_Gen_IRQ_Support (Gen_IRQ_Support);
      Set_Use_UInt (Gen_UInt_Always);
      Set_Gen_Arrays (not No_Arrays);
      Set_No_Defaults (No_Defaults);
      Set_No_UInt_Subtype (No_UInt_Subtype);
      Set_No_VFA_On_Reg_Types (No_VFA_On_Types);

      if Base_Types_Pkg.all /= "" then
         Set_Base_Types_Package (Base_Types_Pkg.all);
      end if;
   end Apply_Command_Line;

   -----------------------
   -- Get_SVD_File_Name --
   -----------------------

   procedure Get_SVD_File_Name (Name : out Unbounded_String) is
      SVD : constant String := GNAT.Command_Line.Get_Argument;
   begin
      if SVD /= "" then
         Name := To_Unbounded_String (GNAT.OS_Lib.Normalize_Pathname (SVD));
      end if;
   end Get_SVD_File_Name;

   ---------------------
   -- Parse_XML_Files --
   ---------------------

   procedure Parse_XML_Files
     (File_Name : String;
      Schema    : String;
      Doc       : out Document)
   is
      Input           : File_Input;
      Reader          : Tree_Reader;
      Sc_Reader       : Schema_Reader;
      Grammar         : XML_Grammar;
   begin
      --  Open the schema file
      Input_Sources.File.Open (Schema, Input);
      Sc_Reader.Parse (Input);
      Close (Input);
      Grammar := Get_Grammar (Sc_Reader);

      --  Tell the reader to use the schema to validate the SVD file
      Reader.Set_Grammar (Grammar);
      Set_Feature (Reader, Sax.Readers.Schema_Validation_Feature, False);
      Use_Basename_In_Error_Messages (Reader, True);

      Input_Sources.File.Open (File_Name, Input);
      Reader.Parse (Input);
      Close (Input);

      Doc := Get_Tree (Reader);

      --  Check if we have a helper file to give us hints as to how generate
      --  some structures. Rule: if we have both XXXX.svd and XXXX.svd2ada,
      --  then the svd2ada file is our helper file
      if GNAT.OS_Lib.Is_Regular_File (File_Name & "2ada") then
         Input_Sources.File.Open (File_Name & "2ada", Input);
         Set_Feature (Reader, Sax.Readers.Schema_Validation_Feature, False);
         Use_Basename_In_Error_Messages (Reader, True);
         Reader.Parse (Input);
         Close (Input);
      end if;
   exception
      when XML_Validation_Error =>
         Close (Input);
         Put_Line ("Invalid SVD file:");
         Put_Line (Reader.Get_Error_Message);
         raise;
      when E : Sax.Readers.XML_Fatal_Error =>
         Close (Input);
         Put_Line ("Fatal error when parsing the SVD file:");
         Put_Line (Ada.Exceptions.Exception_Message (E));
         raise;
   end Parse_XML_Files;

begin
   Configure_Command_Line;
   GNAT.Command_Line.Getopt (Cmd_Line_Cfg);
   Get_SVD_File_Name (SVD_File_Name);
   if SVD_File_Name = Null_Unbounded_String or else
      Output_Dir = null or else Output_Dir.all = ""
   then
      Put_Line (Standard_Error, "Error: missing arguments");
      GNAT.Command_Line.Try_Help;
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;
   Apply_Command_Line;

   Ada_Gen.Set_Input_File_Name (Base_Name (To_String (SVD_File_Name)));

   Parse_XML_Files (To_String (SVD_File_Name), Schema, Doc);

   Device := Descriptors.Device.Read_Device
     (Documents.Get_Element (Doc),
      (if Root_Pkg_Name = null then "" else Root_Pkg_Name.all));

   Descriptors.Device.Dump (Device, Output_Dir.all);
exception
   when GNAT.Command_Line.Invalid_Switch |
        GNAT.Command_Line.Invalid_Parameter |
        GNAT.Command_Line.Exit_From_Command_Line =>
      Ada.Command_Line.Set_Exit_Status (1);

   when XML_Validation_Error | Sax.Readers.XML_Fatal_Error =>
      Ada.Command_Line.Set_Exit_Status (2);
end SVD2Ada;
