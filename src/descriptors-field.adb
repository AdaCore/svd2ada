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

with Ada.Text_IO;
with Interfaces;              use Interfaces;

with DOM.Core;                use DOM.Core;
with DOM.Core.Elements;       use DOM.Core.Elements;
with DOM.Core.Nodes;

with Descriptors.Register;

with SVD2Ada_Utils;

package body Descriptors.Field is

   function Similar_Field
     (F1, F2     : Field_T;
      Prefix_Idx : in out Natural;
      First      : in out Natural) return Boolean;
   --  Checks if two fields are similar (Identical type and similar prefix,
   --  e.g. names only differing by a number suffix). Returns the lowest
   --  number as 'First'

   ----------------
   -- Read_Field --
   ----------------

   function Read_Field
     (Elt            : DOM.Core.Element;
      Vec            : Field_Vectors.Vector;
      Default_Access : Access_Type;
      Default_Read   : Read_Action_Type)
      return Field_T
   is
      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Result       : Field_T;
      Derived_From : constant String := Elements.Get_Attribute (Elt, "derivedFrom");

   begin
      Result.Acc := Default_Access;
      Result.Read_Action := Default_Read;

      if Derived_From /= "" then
         declare
            Found : Boolean := False;
         begin
            for F of Vec loop
               if Unbounded.To_String (F.Name) = Derived_From then
                  Result := F;
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
                  Result.Name := Get_Value (Child);

               elsif Tag = "description" then
                  Result.Description := Get_Value (Child);

               elsif Tag = "bitOffset"
                 or else Tag = "lsb"
               then
                  Result.LSB := Get_Value (Child);

               elsif Tag = "bitWidth" then
                  Result.Size := Get_Value (Child);

               elsif Tag = "msb" then
                  Result.Size := Get_Value (Child) - Result.LSB + 1;

               elsif Tag = "bitRange" then
                  --  bitRange has the form: [XX:YY] where XX is the MSB,
                  --  and YY is the LSB
                  declare
                     Val : String renames Get_Value (Child);
                  begin
                     for K in Val'Range loop
                        if Val (K) = ':' then
                           Result.LSB :=
                             Natural'Value (Val (K + 1 .. Val'Last - 1));
                           Result.Size :=
                             Natural'Value (Val (2 .. K - 1)) - Result.LSB + 1;
                        end if;
                     end loop;
                  end;

               elsif Tag = "access" then
                  Result.Acc := Get_Value (Child);

               elsif Tag = "modifiedWriteValues" then
                  Result.Mod_Write_Values := Get_Value (Child);

               elsif Tag = "readAction" then
                  Result.Read_Action := Get_Value (Child);

               elsif Tag = "enumeratedValues" then
                  declare
                     Enum : constant Descriptors.Enumerate.Enumerate_T :=
                              Descriptors.Enumerate.Read_Enumerate
                                (Child, Result.Enums, Result.Acc = Write_Only);
                  begin
                     Result.Enums.Append (Enum);
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring field element " & Tag & " at " & Full_Name (Child));
               end if;
            end;
         end if;
      end loop;

      return Result;
   end Read_Field;

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (F1, F2 : Field_T) return Boolean
   is
      use Unbounded;
   begin
      return F1.LSB = F2.LSB
        and then F1.Size = F2.Size
        and then F1.Name = F2.Name;
   end "=";

   ------------------------
   -- Similar_Field_Name --
   ------------------------

   function Similar_Field
     (F1, F2     : Field_T;
      Prefix_Idx : in out Natural;
      First      : in out Natural) return Boolean
   is
      use Unbounded, Descriptors.Enumerate.Enumerate_Vectors;
      Prefix : Unbounded_String;
      N      : Natural;

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

      for J in reverse 1 .. Length (F1.Name) loop
         if Unbounded.Element (F1.Name, J) not in '0' .. '9' then
            if J = Length (F1.Name) then
               N := 1;
            else
               N := Natural'Value (Slice (F1.Name, J + 1, Length (F1.Name)));
            end if;
            if N < First then
               First := N;
            end if;

            exit;
         end if;
      end loop;

      for J in reverse 1 .. Length (F2.Name) loop
         if Unbounded.Element (F2.Name, J) not in '0' .. '9' then
            if J = Length (F2.Name) then
               N := 1;
            else
               N := Natural'Value (Slice (F2.Name, J + 1, Length (F2.Name)));
            end if;

            if N < First then
               --  We can't support decreasing numbering for field numbers
               --  as we map it to an Ada array that has an increasing index
               return False;
            end if;

            exit;
         end if;
      end loop;

      return True;
   end Similar_Field;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Spec         : in out Ada_Gen.Ada_Spec;
      Reg          : Descriptors.Register.Register_Access;
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
            Default := Shift_Right (Properties.Reset_Value, Index);
            Mask := 0;

            for J in 0 .. Size - 1 loop
               Mask := Mask or 2 ** J;
            end loop;

            return Default and Mask;
         end if;
      end Get_Default;

      Fields        : array (0 .. Properties.Size - 1) of Field_T := (others => Null_Field);
      Index         : Natural;
      Index2        : Natural;
      Length        : Natural;
      First         : Natural;
      Prefix        : Natural;
      Default       : Unsigned;
      Default_Id    : Unbounded_String;
      Ada_Type      : Descriptors.Register.Type_Holders.Holder;
      Ada_Type_Size : Natural;
      Ada_Name      : Unbounded_String;
      As_Boolean    : Boolean;
      Description   : Unbounded_String;
      All_RO        : Boolean := True;
      use Descriptors.Register;

   begin
      for Field of Reg_Fields loop
         Fields (Field.LSB) := Field;
      end loop;

      Index := 0;

      while Index < Properties.Size loop
         if Fields (Index) /= Null_Field
           and then Fields (Index).Acc /= Read_Only
         then
            All_RO := False;
            exit;
         end if;

         Index := Index + 1;
      end loop;

      Index := 0;
      while Index < Properties.Size loop
         Ada_Type := Type_Holders.Empty_Holder;

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
            Default := Get_Default (Index, Length);

            if not All_RO and then SVD2Ada_Utils.Gen_Fields_Default then
               Ada_Gen.Add_Field
                 (Rec,
                  "Reserved_" & To_String (Index) & "_" & To_String (Index + Length - 1),
                  Target_Type (Length),
                  Offset      => 0,
                  LSB         => Index,
                  MSB         => Index + Length - 1,
                  Default     => Default,
                  Properties  => (Is_Aliased     => False,
                                  Is_Volatile_FA => False),
                  Comment     => "unspecified");
            else
               Ada_Gen.Add_Field
                 (Rec,
                  "Reserved_" & To_String (Index) & "_" & To_String (Index + Length - 1),
                  Target_Type (Length),
                  Offset      => 0,
                  LSB         => Index,
                  MSB         => Index + Length - 1,
                  Properties  => (Is_Aliased     => False,
                                  Is_Volatile_FA => False),
                  Comment     => "unspecified");
            end if;

            Index := Index + Length;

         else --  Not a reserved field case:

            --  Whether to use a Boolean for bit fields
            As_Boolean := SVD2Ada_Utils.Use_Boolean_For_Bit;

            if not All_RO and then SVD2Ada_Utils.Gen_Fields_Default then
               --  Retrieve the reset value
               Default := Get_Default (Index, Fields (Index).Size);
               Default_Id := Null_Unbounded_String;
            end if;

            Ada_Type_Size := Fields (Index).Size;
            Ada_Name := Fields (Index).Name;

            --  First check if some enumerate is defined for the field
            if not Fields (Index).Enums.Is_Empty then
               for Enum of Fields (Index).Enums loop
                  declare
                     Enum_Name : constant String :=
                                     To_String (Reg.Name) & "_" &
                                     (if Unbounded.Length (Enum.Name) > 0
                                      then To_String (Enum.Name)
                                      else To_String (Fields (Index).Name) &
                                        "_Field");

                     Enum_T : Ada_Type_Enum :=
                                     New_Type_Enum
                                       (Id      => Enum_Name,
                                        Size    => Ada_Type_Size,
                                        Comment => To_String (Fields (Index).Description));
                     Enum_Val      : Ada_Enum_Value;
                     Found_Default : Boolean := False;
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
                          (Spec,
                           Enum_T,
                           Id      => To_String (Val.Name),
                           Repr    => Val.Value,
                           Comment => To_String (Val.Descr));

                        if not All_RO
                          and then not Found_Default
                          and then Val.Value = Default
                        then
                           Default_Id := Id (Spec) & "." & Id (Enum_Val);
                           Found_Default := True;
                        end if;

                     end loop;

                     if not All_RO and then not Found_Default then
                        --  Reset value not found in the enumerate.
                        --  Let's create an enumerate value for it
                        Enum_Val := Add_Enum_Id
                          (Spec, Enum_T,
                           Id      => Enum_Name & "_Reset",
                           Repr    => Default,
                           Comment => "Reset value for the field");
                        Default_Id := Id (Enum_Val);
                     end if;

                     Add (Spec, Enum_T);

                     Ada_Type := -Enum_T;
                  end;
               end loop;
            end if;

            --  Check if it's an array, in which case it's easier
            --  to handle them as such.

            Length := 1;
            if SVD2Ada_Utils.Gen_Arrays then
               First  := Natural'Last;
               Prefix := Unbounded.Length (Fields (Index).Name);

               Index2 := Index + Fields (Index).Size;
               while Index2 < Properties.Size loop
                  if Similar_Field
                    (Fields (Index), Fields (Index2), Prefix, First)
                  then
                     Length := Length + 1;
                  else
                     exit;
                  end if;

                  Index2 := Index2 + Fields (Index).Size;
               end loop;
            end if;

            if Length = 1 then
               --  Simple field
               if Ada_Type_Size = 1 and then As_Boolean then
                  if Ada_Type.Is_Empty then
                     Ada_Type := -Get_Boolean;

                     if not All_RO then
                        if Default = 0 then
                           Default_Id := To_Unbounded_String ("False");
                        else
                           Default_Id := To_Unbounded_String ("True");
                        end if;
                     end if;
                  end if;

               elsif Ada_Type.Is_Empty then
                  --  We have a simple scalar value. Let's create a specific
                  --  subtype for it, so that programming conversion to this
                  --  field is allowed using FIELD_TYPE (Value).
                  Ada_Type := -Ada_Gen.Target_Type (Ada_Type_Size);

                  if SVD2Ada_Utils.Gen_UInt_Subtype then
                     declare
                        Sub_T : Ada_Subtype_Scalar :=
                                  New_Subype_Scalar
                                    (Id  => To_String (Reg.Type_Name) &
                                       "_" &
                                       To_String (Fields (Index).Name) &
                                       "_Field",
                                     Typ => -Ada_Type);
                     begin
                        Add (Spec, Sub_T);
                        Ada_Type := -Sub_T;
                     end;
                  end if;
               end if;
               --  If Ada_Type is not Null_Unbounded_String, then the Field
               --  type has already been generated

            else
               --  We have an array of values
               declare
                  F_Name  : constant String :=
                              Slice (Fields (Index).Name, 1, Prefix);
                  T_Name  : constant String :=
                              (if To_String (Reg.Type_Name) /= F_Name
                               then To_String (Reg.Type_Name) & "_" & F_Name
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
                  if Ada_Type_Size = 1 and then As_Boolean then
                     if Ada_Type.Is_Empty then
                        Ada_Type := -Get_Boolean;
                     end if;

                  elsif Ada_Type.Is_Empty then
                     Ada_Type := -Target_Type (Ada_Type_Size);

                     if SVD2Ada_Utils.Gen_UInt_Subtype then
                        declare
                           Scalar_T : Ada_Subtype_Scalar :=
                                        New_Subype_Scalar
                                          (Id      => T_Name & "_Element",
                                           Typ     =>
                                             Target_Type (Ada_Type_Size),
                                           Comment =>
                                             T_Name & " array element");
                        begin
                           Add (Spec, Scalar_T);
                           Ada_Type := -Scalar_T;
                        end;
                     end if;
                  end if;

                  Array_T :=
                    New_Type_Array
                      (Id           => T_Name & "_Field_Array",
                       Index_Type   => "",
                       Index_First  => First,
                       Index_Last   => First + Length - 1,
                       Element_Type => -Ada_Type,
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
                    (Rec        => Union_T,
                     Enum_Val   => "True",
                     Id         => "Arr",
                     Typ        => Array_T,
                     Offset     => 0,
                     LSB        => 0,
                     MSB        => Fields (Index).Size * Length - 1,
                     Properties => (Is_Aliased     => False,
                                    Is_Volatile_FA => False),
                     Comment    => F_Name & " as an array");
                  Add_Field
                    (Rec        => Union_T,
                     Enum_Val   => "False",
                     Id         => "Val",
                     Typ        =>
                       Target_Type (Fields (Index).Size * Length),
                     Offset     => 0,
                     LSB        => 0,
                     MSB        => Fields (Index).Size * Length - 1,
                     Properties => (Is_Aliased     => False,
                                    Is_Volatile_FA => False),
                     Comment    => F_Name & " as a value");

                  Add (Spec, Union_T);

                  Ada_Type := -Union_T;
                  Ada_Type_Size := Fields (Index).Size * Length;
                  Ada_Name := To_Unbounded_String (F_Name);

                  if not All_RO then
                     Default_Id := To_Unbounded_String
                       ("(As_Array => False, Val => " &
                          To_Hex (Default) & ")");
                  end if;
               end;
            end if;

            Description := Fields (Index).Description;

            case Fields (Index).Read_Action is
               when Undefined_Read_Action =>
                  null;
               when Clear =>
                  Description :=
                    To_Unbounded_String
                      ("*** This field is cleared (set to zero) following a" &
                         " read operation ***. ") & Description;
               when Set =>
                  Description :=
                    To_Unbounded_String
                      ("*** This field is set (set to one) following a" &
                         " read operation ***. ") & Description;
               when Modify =>
                  Description :=
                    To_Unbounded_String
                      ("*** This field is modified following a" &
                         " read operation ***. ") & Description;
               when Modify_Exernal =>
                  Description :=
                    To_Unbounded_String
                      ("*** Reading this field has side effects on other " &
                         "resources ***. ") & Description;
            end case;

            case Fields (Index).Acc is
               when Read_Write =>
                  null;
               when Read_Only =>
                  Description := To_Unbounded_String ("Read-only. ") & Description;
               when Write_Only =>
                  Description := To_Unbounded_String ("Write-only. ") & Description;
               when Write_Once =>
                  Description := To_Unbounded_String ("Write-once. ") & Description;
               when Read_Write_Once =>
                  Description := To_Unbounded_String ("Read-Write-once. ") & Description;
            end case;

            case Fields (Index).Mod_Write_Values is
               when One_To_Clear =>
                  Description := To_Unbounded_String
                    ("Write data bit of one shall clear (set to zero) the" &
                       " corresponding bit in the field. ") & Description;
               when One_To_Set =>
                  Description := To_Unbounded_String
                    ("Write data bit of one shall set (set to one) the" &
                       " corresponding bit in the field. ") & Description;
               when One_To_Toggle =>
                  Description := To_Unbounded_String
                    ("Write data bit of one shall toggle (invert) the" &
                       " corresponding bit in the field. ") & Description;
               when Zero_To_Clear =>
                  Description := To_Unbounded_String
                    ("Write data bit of zero shall clear (set to zero) the" &
                       " corresponding bit in the field. ") & Description;
               when Zero_To_Set =>
                  Description := To_Unbounded_String
                    ("Write data bit of zero shall set (set to one) the" &
                       " corresponding bit in the field. ") & Description;
               when Zero_To_Toggle =>
                  Description := To_Unbounded_String
                    ("Write data bit of zero shall toggle (invert) the" &
                       " corresponding bit in the field. ") & Description;
               when Clear =>
                  Description := To_Unbounded_String
                    ("After a write operation all bits in the field are" &
                       " cleared (set to zero). ") & Description;
               when Set =>
                  Description := To_Unbounded_String
                    ("After a write operation all bits in the field are" &
                       " set (set to one). ") & Description;
               when Modify =>
                  null;
            end case;

            if All_RO or else not SVD2Ada_Utils.Gen_Fields_Default then
               Add_Field
                 (Rec,
                  Id         => To_String (Ada_Name),
                  Typ        => -Ada_Type,
                  Offset     => 0,
                  LSB        => Index,
                  MSB        => Index + Ada_Type_Size - 1,
                  Properties => (Is_Aliased     => False,
                                 Is_Volatile_FA => False),
                  Comment    => To_String (Description));

            elsif Default_Id = Null_Unbounded_String then
               Add_Field
                 (Rec,
                  Id         => To_String (Ada_Name),
                  Typ        => -Ada_Type,
                  Offset     => 0,
                  LSB        => Index,
                  MSB        => Index + Ada_Type_Size - 1,
                  Default    => Default,
                  Properties => (Is_Aliased     => False,
                                 Is_Volatile_FA => False),
                  Comment    => To_String (Description));

            else
               Add_Field
                 (Rec,
                  Id         => To_String (Ada_Name),
                  Typ        => -Ada_Type,
                  Offset     => 0,
                  LSB        => Index,
                  MSB        => Index + Ada_Type_Size - 1,
                  Default    => Default_Id,
                  Properties => (Is_Aliased     => False,
                                 Is_Volatile_FA => False),
                  Comment    => To_String (Description));
            end if;

            Default_Id := Null_Unbounded_String;
            Index      := Index + Ada_Type_Size;
         end if;
      end loop;
   end Dump;

end Descriptors.Field;
