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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with Ada.Text_IO;

with SVD2Ada_Utils;

package body Base_Types is

   package Unsigned_IO is new Ada.Text_IO.Modular_IO (Unsigned);

   use type DOM.Core.Node;

   procedure Read_Range_Elts
     (Tag : String;
      Elt : DOM.Core.Element;
      Val : in out Write_Constraint_Type);

   procedure Read_Write_Constraint_Elts
     (Tag : String;
      Elt : DOM.Core.Element;
      Val : in out Write_Constraint_Type);

   procedure Read_Address_Block_Elts
     (Tag : String;
      Elt : DOM.Core.Element;
      Val : in out Address_Block_Type);

   procedure Read_Interrupt_Elts
     (Tag : String;
      Elt : DOM.Core.Element;
      Val : in out Interrupt_Type);

   -----------------
   -- Target_Type --
   -----------------

   function Target_Type
     (Size            : Natural;
      Fully_Qualified : Boolean := True) return String
   is
      Pkg : constant String :=
              (if not Fully_Qualified then ""
               elsif not SVD2Ada_Utils.External_Base_Types_Package
               then SVD2Ada_Utils.Root_Package & "."
               else SVD2Ada_Utils.Base_Types_Package & ".");

   begin
      if Size = 1 then
         return Pkg & "Bit";
      elsif Size = 8 and then not SVD2Ada_Utils.Use_UInt_Always then
         return Pkg & "Byte";
      else
         return Pkg & "UInt" & To_String (Size);
      end if;
   end Target_Type;

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (I1, I2 : Interrupt_Type) return Boolean
   is
      use Unbounded;
   begin
      return I1.Name = I2.Name;
   end "=";

   ----------
   -- Less --
   ----------

   function "<" (I1, I2 : Interrupt_Type) return Boolean
   is
      use Unbounded;
   begin
      return (if I1.Value = I2.Value
              then To_String (I1.Name) < To_String (I2.Name)
              else I1.Value < I2.Value);
   end "<";

   ------------
   -- To_Hex --
   ------------

   function To_Hex (Val : Natural) return String
   is
   begin
      return To_Hex (Unsigned (Val));
   end To_Hex;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (Val : Unsigned) return String
   is
      Result : String (1 .. 12); --  16#01234567#
   begin
      Unsigned_IO.Put (Result, Val, 16);

      for J in Result'Range loop
         if Result (J) /= ' ' then
            return Result (J .. Result'Last);
         end if;
      end loop;

      return Result;
   end To_Hex;

   ---------------
   -- To_String --
   ---------------

   function To_String (Val : Integer) return String
   is
      S : constant String := Integer'Image (Val);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Val : Unsigned) return String
   is
      S : constant String := Unsigned'Image (Val);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end To_String;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return String
   is
      use DOM.Core;
      List : constant Node_List := Nodes.Child_Nodes (Elt);

   begin
      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Text_Node then
            return String (Nodes.Node_Value (Nodes.Item (List, J)));
         end if;
      end loop;

      return "";
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Boolean is
      Value : String renames Get_Value (Elt);
   begin
      return Value = "true" or Value = "1";
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Elt : DOM.Core.Element) return Unbounded.Unbounded_String
   is
      Value : String renames Get_Value (Elt);
   begin
      return Unbounded.To_Unbounded_String (Value);
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Elt : DOM.Core.Element) return Natural
   is
      U : constant Unsigned := Get_Value (Elt);
   begin
      return Natural (U);
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Unsigned is
      Value      : constant String := Get_Value (Elt);
      Multiplier : Unsigned := 1;
      Last       : Natural := Value'Last;

   begin
      --  First pass: we check the presence of a multiplier
      if Value (Value'Last) = 'k' or else Value (Value'Last) = 'K' then
         Multiplier := 1024;
         Last := Last - 1;
      elsif Value (Value'Last) = 'm' or else Value (Value'Last) = 'M' then
         Multiplier := 1024 ** 2;
         Last := Last - 1;
      elsif Value (Value'Last) = 'g' or else Value (Value'Last) = 'G' then
         Multiplier := 1024 ** 3;
         Last := Last - 1;
      elsif Value (Value'Last) = 't' or else Value (Value'Last) = 'T' then
         Multiplier := 1024 ** 4;
         Last := Last - 1;
      else
         Multiplier := 1;
      end if;

      --  we check if the value is expressed in hexa
      if Value'Length > 2
        and then (Value (1 .. 2) = "0x" or else Value (1 .. 2) = "0X")
      then
         return Unsigned'Value ("16#" & Value (3 .. Last) & "#") * Multiplier;
      elsif Value'Length > 1
        and then Value (1) = '#'
      then
         return Unsigned'Value ("2#" & Value (2 .. Last) & "#") * Multiplier;
      else
         return Unsigned'Value (Value (1 .. Last)) * Multiplier;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Access_Type is
      Value : constant String := Get_Value (Elt);

   begin
      if Value = "read-only" then
         return Read_Only;
      elsif Value = "write-only" then
         return Write_Only;
      elsif Value = "read-write" then
         return Read_Write;
      elsif Value = "writeOnce" then
         return Write_Once;
      elsif Value = "read-writeOnce" then
         return Read_Write_Once;
      else
         raise Constraint_Error
           with "Invalid access value " & Value;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Protection_Type is
      Value : String renames Get_Value (Elt);
   begin
      if Value = "s" then
         return Secure_Protection;
      elsif Value = "n" then
         return Non_Secure_Protection;
      elsif Value = "p" then
         return Privileged_Protection;
      else
         raise Constraint_Error
           with "Invalid protection value " & Value;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Endian_Type is
      Value : String renames Get_Value (Elt);
   begin
      if Value = "little" then
         return Little_Endian;
      elsif Value = "big" then
         return Big_Endian;
      elsif Value = "selectable" then
         return Selectable_Endian;
      elsif Value = "other" then
         return Other_Endian;
      else
         raise Constraint_Error
           with "Invalid endian value " & Value;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Data_Type is
      Value : String renames Get_Value (Elt);
   begin
      if Value = "uint8_t" then
         return uint8_t_Data;
      elsif Value = "uint16_t" then
         return uint16_t_Data;
      elsif Value = "uint32_t" then
         return uint32_t_Data;
      elsif Value = "uint64_t" then
         return uint64_t_Data;
      elsif Value = "int8_t" then
         return int8_t_Data;
      elsif Value = "int16_t" then
         return int16_t_Data;
      elsif Value = "int32_t" then
         return int32_t_Data;
      elsif Value = "int64_t" then
         return int64_t_Data;
      elsif Value = "uint8_t *" then
         return puint8_t_Data;
      elsif Value = "uint16_t *" then
         return puint16_t_Data;
      elsif Value = "uint32_t *" then
         return puint32_t_Data;
      elsif Value = "uint64_t *" then
         return puint64_t_Data;
      elsif Value = "int8_t *" then
         return pint8_t_Data;
      elsif Value = "int16_t *" then
         return pint16_t_Data;
      elsif Value = "int32_t *" then
         return pint32_t_Data;
      elsif Value = "int64_t *" then
         return pint64_t_Data;
      else
         raise Constraint_Error
           with "Invalid data type value " & Value;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Elt : DOM.Core.Element) return Modified_Write_Values_Type
   is
      Value : String renames Get_Value (Elt);
   begin
      if Value = "oneToClear" then
         return One_To_Clear;
      elsif Value = "oneToSet" then
         return One_To_Set;
      elsif Value = "oneToToggle" then
         return One_To_Toggle;
      elsif Value = "zeroToClear" then
         return Zero_To_Clear;
      elsif Value = "zeroToSet" then
         return Zero_To_Set;
      elsif Value = "zeroToToggle" then
         return Zero_To_Toggle;
      elsif Value = "clear" then
         return Clear;
      elsif Value = "set" then
         return Set;
      elsif Value = "modify" then
         return Modify;
      else
         raise Constraint_Error
           with "Invalid modified write value type value " & Value;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Read_Action_Type is
      Value : String renames Get_Value (Elt);
   begin
      if Value = "clear" then
         return Clear;
      elsif Value = "set" then
         return Set;
      elsif Value = "modify" then
         return Modify;
      elsif Value = "modifyExternal" then
         return Modify_Exernal;
      else
         raise Constraint_Error
           with "Invalid read action type value " & Value;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Bit_Range_Type is
      Value : String renames Get_Value (Elt);
   begin
      for J in Value'Range loop
         if Value (J) = ':' then
            return (From => Natural'Value (Value (1 .. J - 1)),
                    To   => Natural'Value (Value (J + 1 .. Value'Last)));
         end if;
      end loop;

      raise Constraint_Error
        with "Invalid bit range type value " & Value;
   end Get_Value;

   ------------------
   -- Gen_DOM_Iter --
   ------------------

   procedure Gen_DOM_Iter (Elt : DOM.Core.Element;
                           Obj : in out T)
   is
      use DOM.Core;
      List : constant Node_List := Nodes.Child_Nodes (Elt);
   begin
      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant Element := Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               Read_Elt (Tag, Child, Obj);
            end;
         end if;
      end loop;
   end Gen_DOM_Iter;

   ---------------------
   -- Read_Range_Elts --
   ---------------------

   procedure Read_Range_Elts
     (Tag : String;
      Elt : DOM.Core.Element;
      Val : in out Write_Constraint_Type) is
   begin
      if Tag = "minimum" then
         Val.Minimum := Get_Value (Elt);
      elsif Tag = "maximum" then
         Val.Maximum := Get_Value (Elt);
      else
         raise Constraint_Error with "Unexpected range tag " & Tag;
      end if;
   end Read_Range_Elts;

   procedure Read_Range is new Gen_DOM_Iter
     (T        => Write_Constraint_Type,
      Read_Elt => Read_Range_Elts);

   --------------------------------
   -- Read_Write_Constraint_Elts --
   --------------------------------

   procedure Read_Write_Constraint_Elts
     (Tag : String;
      Elt : DOM.Core.Element;
      Val : in out Write_Constraint_Type)
   is
   begin
      if Tag = "writeAsRead" then
         Val.Write_As_Read := Get_Value (Elt);
      elsif Tag = "useEnumeratedValues" then
         Val.Use_Enumerated_Values := Get_Value (Elt);
      elsif Tag = "range" then
         Read_Range (Elt, Val);
      else
         raise Constraint_Error with "Unexpected write constraint tag " & Tag;
      end if;
   end Read_Write_Constraint_Elts;

   procedure Read_Write_Constraint is new Gen_DOM_Iter
     (T        => Write_Constraint_Type,
      Read_Elt => Read_Write_Constraint_Elts);

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Write_Constraint_Type is
      Result : Write_Constraint_Type;
   begin
      Read_Write_Constraint (Elt, Result);
      return Result;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   procedure Read_Address_Block_Elts
     (Tag : String;
      Elt : DOM.Core.Element;
      Val : in out Address_Block_Type)
   is
   begin
      if Tag = "offset" then
         Val.Offset := Get_Value (Elt);
      elsif Tag = "size" then
         Val.Size := Get_Value (Elt);
      elsif Tag = "usage" then
         declare
            Str : constant String := Get_Value (Elt);
         begin
            if Str = "registers" then
               Val.Usage := Registers_Usage;
            elsif Str = "buffer" then
               Val.Usage := Buffer_Usage;
            elsif Str = "reserved" then
               Val.Usage := Reserved_Usage;
            else
               raise Constraint_Error with "invalid usage value " & Str;
            end if;
         end;
      elsif Tag = "protection" then
         Val.Protection := Get_Value (Elt);
      else
         raise Constraint_Error with "Unexpected address block tag " & Tag;
      end if;
   end Read_Address_Block_Elts;

   ------------------------
   -- Read_Address_Block --
   ------------------------

   procedure Read_Address_Block is new Gen_DOM_Iter
     (T        => Address_Block_Type,
      Read_Elt => Read_Address_Block_Elts);

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Address_Block_Type is
      Result : Address_Block_Type;
   begin
      Read_Address_Block (Elt, Result);
      return Result;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   procedure Read_Interrupt_Elts
     (Tag : String;
      Elt : DOM.Core.Element;
      Val : in out Interrupt_Type)
   is
   begin
      if Tag = "name" then
         Val.Name := Get_Value (Elt);
      elsif Tag = "description" then
         Val.Description := Get_Value (Elt);
      elsif Tag = "value" then
         --  GNAT re-numbers the interrupt to add the Sys_Tick interrupt
         --  which is a core interrupt. So we need to take this re-numbering
         --  here by adding 2 to the constants extracted from the SVD
         Val.Value := Get_Value (Elt);
      else
         raise Constraint_Error with "Unexpected interrupt tag " & Tag;
      end if;
   end Read_Interrupt_Elts;

   --------------------
   -- Read_Interrupt --
   --------------------

   procedure Read_Interrupt is new Gen_DOM_Iter
     (T        => Interrupt_Type,
      Read_Elt => Read_Interrupt_Elts);

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Interrupt_Type is
      Result : Interrupt_Type;
   begin
      Read_Interrupt (Elt, Result);
      return Result;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Elt : DOM.Core.Element) return Enum_Usage_Type
   is
      Value : String renames Get_Value (Elt);
   begin
      if Value = "read" then
         return Read;
      elsif Value = "write" then
         return Write;
      elsif Value = "read-write" then
         return Read_Write;
      else
         raise Constraint_Error
           with "Invalid 'enum-usage' type value " & Value;
      end if;
   end Get_Value;

   -------------------
   -- Common_Prefix --
   -------------------

   function Common_Prefix
     (Name1, Name2 : Unbounded.Unbounded_String)
      return Unbounded.Unbounded_String
   is
      use Unbounded;
      Prefix : Natural := Length (Name1);
   begin
      --  Try to find names of the form REGNAMEXX where XX is a number
      --  and extract the prefix
      for J in reverse 1 .. Length (Name1) loop
         Prefix := J;
         exit when Element (Name1, J) not in '0' .. '9';
      end loop;

      if Prefix > Length (Name2) then
         return Null_Unbounded_String;
      end if;

      if Element (Name1, Prefix) = '_'
        and then Element (Name2, Prefix) = '_'
      then
         --  We have names of the form PREFIX_XXXX
         --  In this case, we also strip the '_' character
         Prefix := Prefix - 1;
      end if;

      if Slice (Name1, 1, Prefix) /= Slice (Name2, 1, Prefix) then
         return Null_Unbounded_String;
      end if;

      for J in Prefix + 1 .. Length (Name2) loop
         if Element (Name2, J) not in '0' .. '9' then
            return Null_Unbounded_String;
         end if;
      end loop;

      return To_Unbounded_String (Slice (Name1, 1, Prefix));
   end Common_Prefix;

   -------------------
   -- Get_Full_Name --
   -------------------

   function Full_Name (Elt : DOM.Core.Node) return String
   is
      Parent : constant DOM.Core.Node := DOM.Core.Nodes.Parent_Node (Elt);
      Str    : Ada.Strings.Unbounded.Unbounded_String;
   begin

      if Parent /= null then
         Str := Ada.Strings.Unbounded.To_Unbounded_String (Full_Name (Parent));
      else
         Str := Ada.Strings.Unbounded.To_Unbounded_String ("");
      end if;

      --  Search for "name" tag and add it's content to Str
      declare
         Children_List : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Elt);
      begin
         for N in 0 .. DOM.Core.Nodes.Length (Children_List) - 1 loop
            declare
               Child_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item (Children_List, N);
               Text : String renames DOM.Core.Nodes.Node_Name (Child_Node);
            begin
               if Text = "name" then
                  Ada.Strings.Unbounded.Append (Str, "/" & Get_Value (Child_Node));
               end if;
            end;
         end loop;
      end;

      return Ada.Strings.Unbounded.To_String (Str);
   end Full_Name;

end Base_Types;
