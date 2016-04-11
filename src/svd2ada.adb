------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2016, AdaCore                      --
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

with Ada_Gen_Helpers;
with Base_Types;
with Descriptors.Device;
with SVD2Ada_Options;
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
function SVD2Ada return Integer
is
   --  Local variables used for XML parsing
   Input         : File_Input;
   Reader        : Tree_Reader;
   Sc_Reader     : Schema_Reader;
   Grammar       : XML_Grammar;
   Doc           : Document;
   Schema        : constant String :=
                     GNAT.OS_Lib.Normalize_Pathname
                       ("schema/CMSIS-SVD_Schema_1_3_1.xsd",
                        SVD2Ada_Utils.Executable_Location);

   --  The produced Device
   Device        : Descriptors.Device.Device_T;
   SVD_File      : Unbounded_String;

   --  Command line parser
   Cmd_Line_Cfg  : GNAT.Command_Line.Command_Line_Configuration;
   Pkg           : aliased GNAT.Strings.String_Access;
   Out_Dir       : aliased GNAT.Strings.String_Access;
   Use_Old_Types : aliased Boolean := False;
   Gen_Booleans  : aliased Boolean := False;

   use type GNAT.Strings.String_Access;

begin
   GNAT.Command_Line.Set_Usage
     (Cmd_Line_Cfg,
      Usage       => "[options] -o DIR svd-file",
      Help        => "Generate Ada bindings from CMSIS-SVD Cortex-M hardware " &
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
      Output      => Gen_Booleans'Access,
      Long_Switch => "--boolean",
      Help        => "treat bit fields as boolean. Ignored if an enumerate " &
        "is defined for the field",
      Value       => True);
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => Use_Old_Types'Access,
      Long_Switch => "--old",
      Help        => "maintain compatibility with GNAT GPL 2015. Take " &
        "advantage of the new features of GPL 2016 or GNAT Pro 17 " &
        "otherwise.",
      Value       => True);
   GNAT.Command_Line.Define_Switch
     (Cmd_Line_Cfg,
      Output      => SVD2Ada_Options.Use_Standard_Volatile_Aspect'Access,
      Long_Switch => "--standard-volatile",
      Help        => "Use standard Volatile aspect");
   GNAT.Command_Line.Getopt
     (Config      => Cmd_Line_Cfg);

   declare
      SVD : constant String := GNAT.Command_Line.Get_Argument;
   begin
      if SVD = "" or else Out_Dir = null or else Out_Dir.all = "" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Error: missing arguments");
         GNAT.Command_Line.Try_Help;
         return 1;
      end if;

      SVD_File := To_Unbounded_String (GNAT.OS_Lib.Normalize_Pathname (SVD));
   end;

   SVD2Ada_Utils.Set_Use_Boolean_For_Bit (Gen_Booleans);
   SVD2Ada_Utils.Set_Gen_GNAT15 (Use_Old_Types);

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

      Ada_Gen_Helpers.Read (Documents.Get_Element (Get_Tree (Reader)));
   end if;

   Device := Descriptors.Device.Read_Device
     (Documents.Get_Element (Doc),
      (if Pkg = null then "" else Pkg.all));

   Descriptors.Device.Dump (Device, Out_Dir.all);

   return 0;

exception
   when GNAT.Command_Line.Exit_From_Command_Line |
     GNAT.Command_Line.Invalid_Switch =>
      return 1; -- error information already reported
   when XML_Validation_Error =>
      Close (Input);
      Ada.Text_IO.Put_Line ("Non-valid SVD file:");
      Ada.Text_IO.Put_Line (Reader.Get_Error_Message);
      return 2;
end SVD2Ada;
