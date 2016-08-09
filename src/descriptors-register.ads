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

with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Unbounded;             use Ada.Strings;

with DOM.Core;

with Base_Types;                     use Base_Types;
with Base_Types.Register_Properties; use Base_Types.Register_Properties;

with Descriptors.Field;               use Descriptors.Field;

with Ada_Gen;

--  Decodes and then dumps the <register> elements of the SVD file.
package Descriptors.Register is

   type Register_T;

   type Register_Access is access all Register_T;

   package Type_Holders is new Ada.Containers.Indefinite_Holders
     (Ada_Gen.Ada_Type'Class, Ada_Gen."=");
   function "-" (Holder : Type_Holders.Holder) return Ada_Gen.Ada_Type'Class
     is (Type_Holders.Element (Holder));
   function "-" (Typ : Ada_Gen.Ada_Type'Class) return Type_Holders.Holder
     is (Type_Holders.To_Holder (Typ));

   type Register_T is record
      Xml_Id           : Unbounded.Unbounded_String;
      Name             : Unbounded.Unbounded_String;
      Display_Name     : Unbounded.Unbounded_String;
      Description      : Unbounded.Unbounded_String;
      Alternate_Group  : Unbounded.Unbounded_String;
      Alternate_Reg    : Unbounded.Unbounded_String;
      Address_Offset   : Natural;
      Reg_Properties   : Register_Properties_T;
      Mod_Write_Values : Modified_Write_Values_Type := Modify;
      Read_Action      : Read_Action_Type := Undefined_Read_Action;
      Fields           : Field_Vectors.Vector;

      --  By default, equal to Name. In case similar types are found, then
      --  this holds the common prefix to be used to the type definition
      Type_Name        : Unbounded.Unbounded_String;

      --  When two registers are at the same location, we specify a shared
      --  common union type name here to support this overlapping
      Is_Overlapping   : Boolean := False;
      Overlap_Suffix   : Unbounded.Unbounded_String;

      --  When two registers are identical, the second register will not
      --  generate an Ada type. We reference the first register here to
      --  keep track of the type name.
      Type_Holder      : Register_Access := null;
      --  This holds the Ada type as generated in the spec file.
      --  Only available when Type_Holder is null.
      Ada_Type         : Type_Holders.Holder;

      Dim              : Positive := 1;
      Dim_Increment    : Natural := 4;
      Dim_Index        : Unbounded.Unbounded_String;
   end record;

   function Equal (R1, R2 : Register_Access) return Boolean;

   function Similar_Type
     (R1, R2 : Register_Access) return Unbounded.Unbounded_String;
   --  If R1 and R2 share a common type, then return the base type name.
   --  else, return an empty string

   type Register_Db is interface;

   function Get_Register
     (Db     : Register_Db;
      XML_Id : String) return Register_Access is abstract;

   function Read_Register
     (Elt            : DOM.Core.Element;
      Prepend        : Unbounded.Unbounded_String;
      Append         : Unbounded.Unbounded_String;
      Reg_Properties : Register_Properties_T;
      Reg_Db         : Register_Db'Class)
      return Register_Access;

   function Get_Ada_Type (Reg : Register_Access) return Ada_Gen.Ada_Type'Class;

   procedure Dump (Spec : in out Ada_Gen.Ada_Spec; Reg : Register_Access);

end Descriptors.Register;
