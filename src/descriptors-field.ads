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

   type Field_T is record
      Name             : Unbounded.Unbounded_String;
      Description      : Unbounded.Unbounded_String;
      LSB              : Natural;
      Size             : Natural;
      Acc              : Access_Type;
      Mod_Write_Values : Modified_Write_Values_Type := Modify;
      Read_Action      : Read_Action_Type := Undefined_Read_Action;
      Enums            : Descriptors.Enumerate.Enumerate_Vectors.Vector;
   end record;

   overriding
   function "=" (F1, F2 : Field_T) return Boolean;

   Null_Field : constant Field_T :=
                  (Unbounded.Null_Unbounded_String,
                   Unbounded.Null_Unbounded_String,
                   0,
                   0,
                   Read_Write,
                   others => <>);

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
