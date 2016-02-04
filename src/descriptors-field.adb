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

with Ada.Text_IO;
with Interfaces;         use Interfaces;

with DOM.Core;           use DOM.Core;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;

package body Descriptors.Field is

   function Similar_Field
     (F1, F2     : Field_T;
      Prefix_Idx : in out Natural) return Boolean;

   ----------------
   -- Read_Field --
   ----------------

   function Read_Field
     (Elt : DOM.Core.Element;
      Vec : Field_Vectors.Vector)
      return Field_T
   is
      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret          : Field_T;
      Derived_From : constant String :=
                       Elements.Get_Attribute (Elt, "derivedFrom");

   begin
      if Derived_From /= "" then
         declare
            Found : Boolean := False;
         begin
            for F of Vec loop
               if Unbounded.To_String (F.Name) = Derived_From then
                  Ret := F;
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               raise Constraint_Error with
                 "field 'derivedFrom' is not known: " & Derived_From;
            end if;
         end;
      end if;

      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant Element := Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "name" then
                  Ret.Name := Get_Value (Child);

               elsif Tag = "description" then
                  Ret.Description := Get_Value (Child);

               elsif Tag = "bitOffset"
                 or else Tag = "lsb"
               then
                  Ret.LSB := Get_Value (Child);

               elsif Tag = "bitWidth" then
                  Ret.Size := Get_Value (Child);

               elsif Tag = "msb" then
                  Ret.Size := Get_Value (Child) - Ret.LSB + 1;

               elsif Tag = "bitRange" then
                  --  bitRange has the form: [XX:YY] where XX is the MSB,
                  --  and YY is the LSB
                  declare
                     Val : String renames Get_Value (Child);
                  begin
                     for K in Val'Range loop
                        if Val(K) = ':' then
                           Ret.LSB :=
                             Unsigned'Value (Val (K + 1 .. Val'Last - 1));
                           Ret.Size :=
                             Unsigned'Value (Val (2 .. K - 1)) - Ret.LSB + 1;
                        end if;
                     end loop;
                  end;

               elsif Tag = "access" then
                  Ret.Acc := Get_Value (Child);

               elsif Tag = "modifiedWriteValues" then
                  Ret.Mod_Write_Values := Get_Value (Child);

               elsif Tag = "enumeratedValues" then
                  declare
                     Enum : constant Descriptors.Enumerate.Enumerate_T :=
                              Descriptors.Enumerate.Read_Enumerate
                                (Child, Ret.Enums);
                  begin
                     Ret.Enums.Append (Enum);
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring field element " & Tag);
               end if;
            end;
         end if;
      end loop;

      return Ret;
   end Read_Field;

   ---------
   -- "=" --
   ---------

   function "=" (F1, F2 : Field_T) return Boolean
   is
      use Unbounded;
   begin
      return F1.LSB = F2.LSB
        and then F1.Size = F2.Size;
   end "=";

   ------------------------
   -- Similar_Field_Name --
   ------------------------

   function Similar_Field
     (F1, F2     : Field_T;
      Prefix_Idx : in out Natural) return Boolean
   is
      use Unbounded, Descriptors.Enumerate.Enumerate_Vectors;
      Prefix : Unbounded_String;
   begin
      if F1.Size /= F2.Size then
         return False;
      end if;

      if F1.Enums /= F2.Enums then
         return False;
      end if;

      Prefix := Common_Prefix (F1.Name, F2.Name);

      if Length (Prefix) = 0 then
         return False;
      end if;

      Prefix_Idx := Length (Prefix);

      return True;
   end Similar_Field;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Spec         : in out Ada_Gen.Ada_Spec;
      Reg_Name     : String;
      Rec          : in out Ada_Gen.Ada_Type_Record;
      Reg_Fields   : Field_Vectors.Vector;
      Properties   : Register_Properties_T)
   is
      use Unbounded, Ada_Gen;

      function Get_Default (Index : Natural; Size : Natural) return Unsigned;
      --  Retrieves the field default value from the Register's reset value

      -----------------
      -- Get_Default --
      -----------------

      function Get_Default (Index : Natural; Size : Natural) return Unsigned
      is
         Default : Unsigned;
         Mask    : Unsigned;
      begin
         if Properties.Reset_Value = 0 then
            --  Most common case
            return 0;
         else
            Default :=
              Shift_Right (Properties.Reset_Value, Index);
            Mask := 0;

            for J in 0 .. Size - 1 loop
               Mask := Mask or 2 ** J;
            end loop;

            return Default and Mask;
         end if;
      end Get_Default;

      Fields        : array (0 .. Properties.Size - 1) of Field_T :=
                        (others => Null_Field);
      Index         : Unsigned;
      Index2        : Unsigned;
      Length        : Unsigned;
      Prefix        : Natural;
      Default       : Unsigned;
      Default_Id    : Unbounded_String;
      Ada_Type      : Unbounded_String;
      Ada_Type_Size : Unsigned;
      Ada_Name      : Unbounded_String;

   begin
      for Field of Reg_Fields loop
         Fields (Field.LSB) := Field;
      end loop;

      Index        := 0;

      while Index < Properties.Size loop
         Ada_Type := Null_Unbounded_String;

         if Fields (Index) = Null_Field then
            --  First look for undefined/reserved parts of the register
            Length := 1;

            for J in Index + 1 .. Properties.Size - 1 loop
               if Fields (J) = Null_Field then
                  Length := Length + 1;
               else
                  exit;
               end if;
            end loop;

            --  Retrieve the reset value
            if Properties.Reg_Access /= Read_Only then
               Default := Get_Default (Natural (Index), Natural (Length));

               Ada_Gen.Add_Field
                 (Rec,
                  "Reserved_" & To_String (Index) &
                    "_" & To_String (Index + Length - 1),
                  Target_Type (Length),
                  Offset      => 0,
                  LSB         => Index,
                  MSB         => Index + Length - 1,
                  Default     => Default,
                  Comment     => "unspecified");
            else
               Ada_Gen.Add_Field
                 (Rec,
                  "Reserved_" & To_String (Index) &
                    "_" & To_String (Index + Length - 1),
                  Target_Type (Length),
                  Offset      => 0,
                  LSB         => Index,
                  MSB         => Index + Length - 1,
                  Comment     => "unspecified");
            end if;

            Index    := Index + Length;

         else --  Not a reserved field case:

            --  Retrieve the reset value
            if Properties.Reg_Access /= Read_Only then
               Default :=
                 Get_Default (Natural (Index), Natural (Fields (Index).Size));
            end if;

            Ada_Type_Size := Fields (Index).Size;
            Ada_Name := Fields (Index).Name;

            --  First check if some enumerate is defined for the field
            if not Fields (Index).Enums.Is_Empty then
               for Enum of Fields (Index).Enums loop
                  declare
                     Enum_Name   : constant String :=
                                     (if Unbounded.Length (Enum.Name) > 0
                                      then To_String (Enum.Name)
                                      else To_String (Fields (Index).Name) &
                                        "_Field");

                     Enum_T      : Ada_Type_Enum :=
                                     New_Type_Enum
                                       (Id      => Enum_Name,
                                        Size    => Ada_Type_Size,
                                        Comment =>
                                          To_String
                                            (Fields (Index).Description));
                     Enum_Val    : Ada_Enum_Value;
                     Has_Default : Boolean :=
                                     Properties.Reg_Access = Read_Only;
                     --  True when the enumerate contains the default field
                     --  value. Set to true by default in case of read-only
                     --  registers as in this case the notion of default value
                     --  (e.g. reset value) has no sense.
                     --  In case the enumerate does not contain the reset
                     --  value, we add it manually.

                  begin
                     Add_Size_Aspect (Enum_T, Ada_Type_Size);

                     for Val of Enum.Values loop
                        Enum_Val := Add_Enum_Id
                          (Enum_T,
                           Id      => To_String (Val.Name),
                           Repr    => Val.Value,
                           Comment => To_String (Val.Descr));

                        if Properties.Reg_Access /= Read_Only
                          and then Val.Value = Default
                        then
                           Default_Id := Id (Enum_Val);
                           Has_Default := True;
                        end if;

                     end loop;

                     if not Has_Default then
                        Enum_Val := Add_Enum_Id
                          (Enum_T,
                           Id      => Enum_Name & "_Reset",
                           Repr    => Default,
                           Comment => "Reset value for the field");
                        Default_Id := Id (Enum_Val);
                     end if;

                     Add (Spec, Enum_T);

                     Ada_Type := Id (Enum_T);
                  end;
               end loop;
            end if;

            --  Check if it's an array, in which case it's easier
            --  to handle them as such.

            Length := 1;
            Prefix := Unbounded.Length (Fields (Index).Name);

            Index2 := Index + Fields (Index).Size;
            while Index2 < Properties.Size loop
               if Similar_Field
                 (Fields (Index), Fields (Index2), Prefix)
               then
                  Length := Length + 1;
               else
                  exit;
               end if;

               Index2 := Index2 + Fields (Index).Size;
            end loop;

            if Length = 1 then
               --  Simple field
               if Ada_Type = Null_Unbounded_String then
                  --  We have a simple scalar value. Let's create a specific
                  --  subtype for it, so that programming conversion to this
                  --  field is allowed using FIELD_TYPE (Value).
                  Ada_Type :=
                    To_Unbounded_String (Target_Type (Ada_Type_Size));

                  declare
                     Sub_T : Ada_Subtype_Scalar :=
                               New_Subype_Scalar
                                 (Id  => Reg_Name &
                                         "_" &
                                         To_String (Fields (Index).Name) &
                                         "_Field",
                                  Typ => To_String (Ada_Type));
                  begin
                     Add (Spec, Sub_T);
                     Ada_Type := Id (Sub_T);
                  end;
               end if;
               --  If Ada_Type is not Null_Unbounded_String, then the Field
               --  type has already been generated

            else
               --  We have an array of values
               declare
                  F_Name  : constant String :=
                              Slice (Fields (Index).Name, 1, Prefix);
                  T_Name  : constant String :=
                              (if Reg_Name /= F_Name
                               then Reg_Name & "_" & F_Name
                               else F_Name);

                  Union_T : Ada_Type_Union :=
                              New_Type_Union
                                (Id        => T_Name & "_Field",
                                 Disc_Name => "As_Array",
                                 Disc_Type => Ada_Gen.Get_Boolean,
                                 Comment   =>
                                   "Type definition for " & T_Name);
                  Array_T  : Ada_Type_Array;

               begin
                  if Index /= 0
                    or else Fields (Index).Size * Length /= Properties.Size
                  then
                     --  Print a boxed comment only if there are more than
                     --  one fields defined in the register. Else, this
                     --  becomes a bit too verbose with one boxed comment to
                     --  start the register definition, and one boxed comment
                     --  for the unique register's field definition
                     Add (Spec,
                          New_Comment_Box (Reg_Name & "." & F_Name));
                  end if;

                  if Ada_Type = Null_Unbounded_String then
                     declare
                        Scalar_T : Ada_Subtype_Scalar :=
                                     New_Subype_Scalar
                                       (Id      => T_Name & "_Element",
                                        Typ     => Target_Type (Ada_Type_Size),
                                        Comment => T_Name & " array element");
                     begin
                        Add (Spec, Scalar_T);
                        Ada_Type := Id (Scalar_T);
                     end;
                  end if;

                  Array_T :=
                    New_Type_Array
                      (Id           => T_Name & "_Field_Array",
                       Index_Type   => "",
                       Index_First  => 0,
                       Index_Last   => Unsigned (Length - 1),
                       Element_Type => To_String (Ada_Type),
                       Comment      => T_Name & " array");

                  Add_Aspect
                    (Array_T,
                     "Component_Size => " &
                       To_String (Fields (Index).Size));
                  Add_Size_Aspect
                    (Array_T, Fields (Index).Size * Length);

                  Add (Spec, Array_T);

                  Add_Size_Aspect
                    (Union_T, Fields (Index).Size * Length);

                  Add_Field
                    (Rec      => Union_T,
                     Enum_Val => "True",
                     Id       => "Arr",
                     Typ      => Id (Array_T),
                     Offset   => 0,
                     LSB      => 0,
                     MSB      => Fields (Index).Size * Length - 1,
                     Comment  => F_Name & " as an array");
                  Add_Field
                    (Rec      => Union_T,
                     Enum_Val => "False",
                     Id       => "Val",
                     Typ      =>
                       Target_Type (Fields (Index).Size * Length),
                     Offset   => 0,
                     LSB      => 0,
                     MSB      => Fields (Index).Size * Length - 1,
                     Comment  => F_Name & " as a value");

                  Add (Spec, Union_T);

                  Ada_Type := Id (Union_T);
                  Ada_Type_Size := Fields (Index).Size * Length;
                  Ada_Name := To_Unbounded_String (F_Name);
                  Default_Id := To_Unbounded_String
                    ("(As_Array => False, Val => " & To_Hex (Default) & ")");
               end;
            end if;

            if Properties.Reg_Access = Read_Only then
               Add_Field
                 (Rec,
                  Id      => To_String (Ada_Name),
                  Typ     => To_String (Ada_Type),
                  Offset  => 0,
                  LSB     => Index,
                  MSB     => Index + Ada_Type_Size - 1,
                  Comment => To_String (Fields (Index).Description));

            elsif Default_Id = Null_Unbounded_String then
               Add_Field
                 (Rec,
                  Id      => To_String (Ada_Name),
                  Typ     => To_String (Ada_Type),
                  Offset  => 0,
                  LSB     => Index,
                  MSB     => Index + Ada_Type_Size - 1,
                  Default => Default,
                  Comment => To_String (Fields (Index).Description));

            else
               Add_Field
                 (Rec,
                  Id      => To_String (Ada_Name),
                  Typ     => To_String (Ada_Type),
                  Offset  => 0,
                  LSB     => Index,
                  MSB     => Index + Ada_Type_Size - 1,
                  Default => Default_Id,
                  Comment => To_String (Fields (Index).Description));
            end if;

            Default_Id := Null_Unbounded_String;
            Index      := Index + Ada_Type_Size;
         end if;
      end loop;
   end Dump;

end Descriptors.Field;
