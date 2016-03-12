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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;          use Ada.Strings;

with DOM.Core;

with Base_Types;                     use Base_Types;
with Base_Types.Register_Properties; use Base_Types.Register_Properties;

with Descriptors.Field;               use Descriptors.Field;

with Ada_Gen;

--  Decodes and then dumps the <register> elements of the SVD file.
package Descriptors.Register is

   type Register_T;

   type Register_Access is access all Register_T;

   type Register_T is record
      Xml_Id           : Unbounded.Unbounded_String;
      Name             : Unbounded.Unbounded_String;
      Display_Name     : Unbounded.Unbounded_String;
      Description      : Unbounded.Unbounded_String;
      Alternate_Group  : Unbounded.Unbounded_String;
      Alternate_Reg    : Unbounded.Unbounded_String;
      Address_Offset   : Unsigned;
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
      First_Overlap    : Boolean := False;
      Overlap_Name     : Unbounded.Unbounded_String;
      Overlap_Suffix   : Unbounded.Unbounded_String;


      --  When two registers are identical, the second register will not
      --  generate an Ada type. We reference the first register here to
      --  keep track of the type name.
      Type_Holder      : Register_Access := null;
      --  This holds the Ada type as generated in the spec file.
      --  Only available when Type_Holder is null.
      Ada_Type         : Unbounded.Unbounded_String;

      Dim              : Unsigned := 0;
      Dim_Increment    : Unsigned := 0;
      Dim_Index        : Unbounded.Unbounded_String;
   end record;

   function Equal (R1, R2 : Register_Access) return Boolean;

   function Similar_Type
     (R1, R2 : Register_Access) return Unbounded.Unbounded_String;
   --  If R1 and R2 share a common type, then return the base type name.
   --  else, return an empty string

   package Register_Vectors is new Ada.Containers.Vectors
     (Positive, Register_Access, Equal);

   procedure Find_Aliased (Reg_Set : Register_Vectors.Vector;
                           Resolve : Boolean);

   procedure Find_Common_Types (Reg_Set : Register_Vectors.Vector);

   function Read_Register
     (Elt            : DOM.Core.Element;
      Reg_Properties : Register_Properties_T;
      Vec            : in out Register_Vectors.Vector)
      return Register_Access;

   function Get_Ada_Type (Reg : Register_Access) return String;

   procedure Dump (Spec : in out Ada_Gen.Ada_Spec; Reg : Register_Access);

   procedure Dump_Aliased
     (Spec  : in out Ada_Gen.Ada_Spec;
      Regs  : in out Register_Vectors.Vector);

end Descriptors.Register;
