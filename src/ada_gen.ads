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

with Interfaces;             use Interfaces;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Base_Types;             use Base_Types;

--  This package is responsible for dumping the Ada specs.
package Ada_Gen is

   procedure New_Spec
     (Name : String;
      Descr : String);

   procedure Set_Output_Dir (Dir : String);

   procedure Gen_NL;

   procedure Gen_With (Pkg : String; Add_Use : Boolean := False);

   procedure Gen_Comment (Comment : String);

   procedure Gen_Scalar_Type
     (Id : String; Size : Natural);

   procedure Gen_Constant (Id : String;
                           Typ : String;
                           Value : String);

   -- Enums:

   procedure Start_Enum (Id : String);
   procedure Add_Enum (Value   : String;
                       Comment : Unbounded_String);
   procedure End_Enum;

   -- Array type
   procedure Gen_Array_Type
     (Id           : String;
      Index_Type   : String;
      Low          : Unsigned;
      High         : Unsigned;
      Element_Type : String);

   -- Constant Arrays:

   procedure Start_Constant_Array (Id      : String;
                                   Index_T : String;
                                   Elt_T   : String);
   procedure Add_Array (Index   : String;
                        Value   : String);
   procedure End_Array;

   --  Records

   type Record_Type is (Register, Register_List);

   procedure Start_Record_Def (Id : String);
   procedure Add_Record_Field
     (Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Descr       : String;
      Has_Default : Boolean := False;
      Default     : Unsigned := 0);
   --  Adds a new field to the record opened via Start_Record_Def
   --  Id : The ID of the field
   --  Typ : The base type
   --  Offset : The offset in byte from the the base record address
   --  LSB : The least significant bit after offset
   --  MSB : The most significant bit after offset
   --  Desctr: Description of the field

   procedure Add_Record_Union_Field
     (Id       : String;
      Typ      : String;
      Elts     : Unsigned;
      Elts_Typ : String;
      Offset   : Unsigned;
      LSB      : Unsigned;
      MSB      : Unsigned;
      Descr    : String);
   procedure End_Record
     (Kind : Record_Type);

   procedure Gen_Addr_Block (Name : String;
                             Addr : Unsigned;
                             Size : Unsigned);

   procedure Gen_Register (Name : String;
                           Typ  : String;
                           Addr : Unsigned);

   procedure Close_All;
   --  Close all opened construction, and close the file being written

end Ada_Gen;
