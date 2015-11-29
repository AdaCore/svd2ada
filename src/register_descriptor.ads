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
with Base_Types.Register_Properties; use Base_Types.Register_Properties;

with Field_Descriptor;               use Field_Descriptor;

with Ada_Gen;

--  Decodes and then dumps the <register> elements of the SVD file.
package Register_Descriptor is

   type Register;

   type Register_T is access all Register;

   type Register is record
      Name             : Unbounded.Unbounded_String;
      Display_Name     : Unbounded.Unbounded_String;
      Description      : Unbounded.Unbounded_String;
      Alternate_Group  : Unbounded.Unbounded_String;
      Alternate_Reg    : Unbounded.Unbounded_String;
      Address_Offset   : Unsigned;
      Reg_Properties   : Register_Properties_T;
      Mod_Write_Values : Modified_Write_Values_Type :=
                           Undefined_Modified_Write_Value;
      Read_Action      : Read_Action_Type := Undefined_Read_Action;
      Fields           : Field_Vectors.Vector;

      --  When two registers are identical, we specify a shared common type
      --  name here to generate just one Ada record
      Is_Aliased       : Boolean := False;
      First_Alias      : Boolean := False;
      Type_Name        : Unbounded.Unbounded_String;
      Alias_Name       : Unbounded.Unbounded_String;
      Alias_Suffix     : Unbounded.Unbounded_String;

      Ada_Type         : Unbounded.Unbounded_String;

      --  When two registers are identical, the second register will not
      --  generate an Ada type. We reference the first register here to
      --  keep track of the type name.
      Type_Holder      : Register_T := null;

      Dim              : Unsigned := 0;
      Dim_Increment    : Unsigned := 0;
      Dim_Index        : Unbounded.Unbounded_String;
   end record;

   function Equal (R1, R2 : Register_T) return Boolean;

   function Similar_Type
     (R1, R2 : Register) return Unbounded.Unbounded_String;
   --  If R1 and R2 share a common type, then return the base type name.
   --  else, return an empty string

   package Register_Vectors is new Ada.Containers.Vectors
     (Positive, Register_T, Equal);

   procedure Find_Aliased (Reg_Set : Register_Vectors.Vector);

   procedure Find_Common_Types (Reg_Set : Register_Vectors.Vector);

   function Read_Register
     (Elt            : DOM.Core.Element;
      Reg_Properties : Register_Properties_T;
      Vec            : in out Register_Vectors.Vector) return Register_T;

   function Get_Ada_Type (Reg : Register_T) return String;

   procedure Dump (Spec : in out Ada_Gen.Ada_Spec; Reg : Register_T);

   procedure Dump_Aliased
     (Spec  : in out Ada_Gen.Ada_Spec;
      Regs  : Register_Vectors.Vector);

end Register_Descriptor;
