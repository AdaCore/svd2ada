------------------------------------------------------------------------------
--                              SVD Binding Generator                       --
--                                                                          --
--                         Copyright (C) 2015, AdaCore                      --
--                                                                          --
--  This tool is free software;  you can redistribute it and/or modify      --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations;

with Input_Sources.File;          use Input_Sources.File;
with Sax.Readers;
with Schema.Dom_Readers;          use Schema.Dom_Readers;
with DOM.Core;                    use DOM.Core;
with DOM.Core.Documents;

with Ada_Gen;
with Device_Descriptor;

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
   procedure Usage;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line
        ("Usage: svd2ada file.svd [-p pkg_name] -o output_dir");
      Ada.Text_IO.Put_Line
        ("   where OPTIONS can be:");
   end Usage;

   Input    : File_Input;
   Reader   : Tree_Reader;
   Doc      : Document;
   Device   : Device_Descriptor.Device_T;
   Pkg      : Unbounded_String;
   Out_Dir  : Unbounded_String;
   SVD_File : Unbounded_String;

begin
   while GNAT.Command_Line.Getopt ("* o= p=") /= ASCII.NUL loop
      if GNAT.Command_Line.Full_Switch = "p" then
         Pkg := To_Unbounded_String (GNAT.Command_Line.Parameter);
      elsif GNAT.Command_Line.Full_Switch = "o" then
         Out_Dir := To_Unbounded_String (GNAT.Command_Line.Parameter);
      else
         SVD_File := To_Unbounded_String (GNAT.Command_Line.Full_Switch);
      end if;
   end loop;


   if SVD_File = Null_Unbounded_String
     or else Out_Dir = Null_Unbounded_String
   then
      Usage;
      return 1;
   end if;

   Ada_Gen.Set_Input_File_Name
     (GNAT.Directory_Operations.Base_Name (To_String (SVD_File)));
   Input_Sources.File.Open (To_String (SVD_File), Input);

   Set_Feature (Reader, Sax.Readers.Schema_Validation_Feature, False);
   Use_Basename_In_Error_Messages (Reader, True);
   Reader.Parse (Input);
   Close (Input);

   Doc := Get_Tree (Reader);
   Device := Device_Descriptor.Read_Device (Documents.Get_Element (Doc),
                                            To_String (Pkg));

   Device_Descriptor.Dump (Device, To_String (Out_Dir));

   return 0;

exception
   when E : Sax.Readers.XML_Fatal_Error =>
      Close (Input);
      Ada.Text_IO.Put_Line ("Fatal error when parsing the svd file:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      return 2;
end SVD2Ada;
