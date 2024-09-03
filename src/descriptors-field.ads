------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2024, AdaCore                      --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;          use Ada.Strings;

with DOM.Core;

with Ada_Gen;
with Base_Types;                     use Base_Types;
with Base_Types.Register_Properties; use Base_Types.Register_Properties;

limited with Descriptors.Register;
with Descriptors.Enumerate;

--  Decodes the <field> elements of the SVD file.
package Descriptors.Field is

   --  From the SVD, these components are in an optional <dimElementGroup>:
   --  <dim> specifies the number of array elements,
   --  <dimIncrement> specifies the address (?bit) offset between consecutive
   --    array elements,
   --  (optional) <dimIndex> specifies "a comma seperated list of
   --    strings being used for identifying each element in the array",
   --    but we only see r'[0-9]+\-[0-9]+'.
   --
   --  We will produce <dim> fields, where %s in the Name translates
   --  to the first component of <dimIndex> incremented by 1 for each
   --  field, and LSB, MSB are incremented by <dimIncrement>.

   type Field_T is record
      Name             : Unbounded.Unbounded_String;
      Description      : Unbounded.Unbounded_String;
      Dimensions       : Natural;    -- <dim/>
      Increment        : Natural;    -- <dimIncrement/>
      Index            : Natural;    -- first component of <dimIndex/>
      LSB              : Natural;
      MSB              : Natural;
      Size             : Natural;
      Acc              : Access_Type;
      Mod_Write_Values : Modified_Write_Values_Type := Modify;
      Read_Action      : Read_Action_Type := Undefined_Read_Action;
      Enums            : Descriptors.Enumerate.Enumerate_Vectors.Vector;
   end record;

   overriding
   function "=" (F1, F2 : Field_T) return Boolean;

   Null_Field : constant Field_T :=
     (Name        => Unbounded.Null_Unbounded_String,
      Description => Unbounded.Null_Unbounded_String,
      Dimensions  => 0,
      Increment   => 0,
      Index       => 0,
      LSB         => 0,
      MSB         => 0,
      Size        => 0,
      Acc         => Read_Write,
      others      => <>);

   package Field_Vectors is new Ada.Containers.Vectors
     (Positive, Field_T);

   function Read_Field
     (Elt            : DOM.Core.Element;
      Vec            : Field_Vectors.Vector;
      Default_Access : Access_Type;
      Default_Read   : Read_Action_Type)
      return Field_T;

   procedure Dump
     (Spec         : in out Ada_Gen.Ada_Spec;
      Reg          : Descriptors.Register.Register_Access;
      Rec          : in out Ada_Gen.Ada_Type_Record;
      Reg_Fields   : Field_Vectors.Vector;
      Properties   : Register_Properties_T);

end Descriptors.Field;
