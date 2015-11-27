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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;          use Ada.Strings;

with DOM.Core;

with Base_Types;                     use Base_Types;

with Enumerate_Descriptor;

--  Decodes the <field> elements of the SVD file.
package Field_Descriptor is

   type Field_T is record
      Name             : Unbounded.Unbounded_String;
      Description      : Unbounded.Unbounded_String;
      LSB              : Unsigned;
      Size             : Unsigned;
      Acc              : Access_Type := Undefined_Access;
      Mod_Write_Values : Modified_Write_Values_Type :=
                           Undefined_Modified_Write_Value;
      Enums            : Enumerate_Descriptor.Enumerate_Vectors.Vector;
   end record;

   function "=" (F1, F2 : Field_T) return Boolean;

   Null_Field : constant Field_T :=
                  (Unbounded.Null_Unbounded_String,
                   Unbounded.Null_Unbounded_String,
                   0,
                   0,
                   Undefined_Access,
                   Undefined_Modified_Write_Value,
                   Enumerate_Descriptor.Enumerate_Vectors.Empty_Vector);

   package Field_Vectors is new Ada.Containers.Vectors
     (Positive, Field_T);

   function Read_Field (Elt : DOM.Core.Element;
                        Vec : Field_Vectors.Vector) return Field_T;

end Field_Descriptor;
