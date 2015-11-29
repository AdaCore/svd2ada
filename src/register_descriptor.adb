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

with Interfaces;         use Interfaces;
with System;
with Ada.Text_IO;

with DOM.Core;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;

with Ada_Gen;            use Ada_Gen;

package body Register_Descriptor is

   function Similar_Field
     (F1, F2     : Field_T;
      Prefix_Idx : in out Natural) return Boolean;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (Elt            : DOM.Core.Element;
      Reg_Properties : Register_Properties_T;
      Vec            : in out Register_Vectors.Vector) return Register_T
   is
      use DOM.Core;
      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret          : Register;
      Derived_From : constant String :=
                       Elements.Get_Attribute (Elt, "derivedFrom");

   begin
      Ret.Reg_Properties := Reg_Properties;

      if Derived_From /= "" then
         declare
            Found : Boolean := False;
         begin
            for Oth of Vec loop
               if Unbounded.To_String (Oth.Name) = Derived_From then
                  Ret := Oth.all;
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               raise Constraint_Error with
                 "register 'derivedFrom' is not known: " & Derived_From;
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
                  if Ret.Dim = 0 then
                     Ret.Name := Get_Value (Child);
                  else
                     declare
                        Name : String renames Get_Value (Child);
                     begin
                        if Name'Length > 4
                          and then Name (Name'Last - 3 .. Name'Last) = "[%s]"
                        then
                           Ret.Name :=
                             Unbounded.To_Unbounded_String
                               (Name (Name'First .. Name'Last - 4));
                        elsif Name'Length > 0 then
                           Ada.Text_IO.Put_Line
                             ("*** WARNING: Unsupported register " &
                                "naming schema: " & Name);
                        end if;
                     end;
                  end if;

                  Ret.Type_Name := Ret.Name;

               elsif Tag = "displayName" then
                  Ret.Display_Name := Get_Value (Child);

               elsif Tag = "description" then
                  Ret.Description := Get_Value (Child);

               elsif Tag = "alternateGroup" then
                  Ret.Alternate_Group := Get_Value (Child);

               elsif Tag = "alternateRegister" then
                  Ret.Alternate_Reg := Get_Value (Child);

               elsif Tag = "addressOffset" then
                  Ret.Address_Offset := Get_Value (Child);

               elsif Is_Register_Property (Tag) then
                  Read_Register_Property (Child, Ret.Reg_Properties);

               elsif Tag = "modifiedWriteValue" then
                  Ret.Mod_Write_Values := Get_Value (Child);

               elsif Tag = "readAction" then
                  Ret.Read_Action := Get_Value (Child);

               elsif Tag = "fields" then
                  declare
                     Child_List : constant Node_List :=
                                    Nodes.Child_Nodes (Child);
                     Field      : Field_T;
                  begin
                     for K in 0 .. Nodes.Length (Child_List) - 1 loop
                        if Nodes.Node_Type (Nodes.Item (Child_List, K)) =
                          Element_Node
                        then
                           Field :=
                             Read_Field
                               (Element (Nodes.Item (Child_List, K)),
                                Ret.Fields);
                           if not Ret.Fields.Contains (Field) then
                              Ret.Fields.Append (Field);
                           end if;
                        end if;
                     end loop;
                  end;

               elsif Tag = "dim" then
                  Ret.Dim := Get_Value (Child);
                  declare
                     Name : String renames Unbounded.To_String (Ret.Name);
                  begin
                     if Name'Length > 4
                       and then Name (Name'Last - 3 .. Name'Last) = "[%s]"
                     then
                        Ret.Name :=
                          Unbounded.To_Unbounded_String
                            (Name (Name'First .. Name'Last - 4));
                     elsif Name'Length > 0 then
                        Ada.Text_IO.Put_Line
                          ("*** WARNING: Unsupported register naming schema: "
                           & Name);
                     end if;
                  end;

               elsif Tag = "dimIncrement" then
                  Ret.Dim_Increment := Get_Value (Child);

               elsif Tag = "dimIndex" then
                  Ret.Dim_Index := Get_Value (Child);

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring register element " & Tag);
               end if;
            end;
         end if;
      end loop;

      return new Register'(Ret);
   end Read_Register;

   ------------------
   -- Find_Aliased --
   ------------------

   procedure Find_Aliased (Reg_Set : Register_Vectors.Vector)
   is
      use Unbounded;
   begin
      for J in Reg_Set.First_Index .. Reg_Set.Last_Index - 1 loop
         --  Do not perform a second pass if the register has already been
         --  detected as aliased
         if not Reg_Set (J).Is_Aliased then
            declare
               Reg    : constant Register_T := Reg_Set (J);
               Prefix : constant String := To_String (Reg.Name);
               Last   : Natural := Prefix'Last;
            begin
               for K in J + 1 .. Reg_Set.Last_Index loop
                  if Reg_Set (K).Address_Offset = Reg.Address_Offset then
                     Reg.Is_Aliased := True;

                     for J in 1 .. Last loop
                        if Prefix (J) /= Element (Reg_Set (K).Name, J) then
                           Last := J - 1;
                           if Last = 0 then
                              Ada.Text_IO.Put_Line
                                ("*** WARNING ***: cannot find a proper " &
                                 "Ada type name for the aliased registers " &
                                 Prefix & " and " &
                                 To_String (Reg_Set (K).Name));
                           end if;

                           exit;
                        end if;
                     end loop;
                  end if;
               end loop;

               if Reg.Is_Aliased then
                  if Prefix (Last) = '_'
                    or else Prefix (Last) = '-'
                  then
                     Reg.Alias_Name :=
                       To_Unbounded_String
                         (Prefix (Prefix'First .. Last - 1));
                  else
                     Reg.Alias_Name :=
                       To_Unbounded_String
                         (Prefix (Prefix'First .. Last));
                  end if;

                  if Last = Prefix'Last then
                     Reg.Alias_Suffix := To_Unbounded_String ("Default");
                  else
                     Reg.Alias_Suffix :=
                       To_Unbounded_String (Prefix (Last + 1 .. Prefix'Last));
                  end if;

                  Reg.First_Alias  := True;

                  --  Now apply the type name to all aliased registers
                  for K in J + 1 .. Reg_Set.Last_Index loop
                     if Reg_Set (K).Address_Offset = Reg.Address_Offset then
                        declare
                           Oth : Register renames Reg_Set (K).all;
                        begin
                           Oth.Alias_Name  := Reg.Alias_Name;
                           Oth.Is_Aliased  := True;
                           Oth.First_Alias := False;

                           if Last = Length (Oth.Name) then
                              Oth.Alias_Suffix :=
                                To_Unbounded_String ("Default");
                           else
                              declare
                                 Suffix : constant String :=
                                            Slice
                                              (Oth.Name, Last + 1,
                                               Length (Oth.Name));
                                 First  : Natural := Suffix'First;
                              begin
                                 while Suffix (First) not in 'a' .. 'z'
                                   and then Suffix (First) not in 'A' .. 'Z'
                                 loop
                                    First := First + 1;
                                 end loop;

                                 Oth.Alias_Suffix :=
                                   To_Unbounded_String
                                     (Suffix (First .. Suffix'Last));
                              end;
                           end if;
                        end;
                     end if;
                  end loop;
               end if;
            end;
         end if;
      end loop;
   end Find_Aliased;

   ---------
   -- "=" --
   ---------

   function Equal (R1, R2 : Register_T) return Boolean
   is
      use type Unbounded.Unbounded_String;
      use type Field_Vectors.Vector;
   begin
      if R1 = null then
         return R2 = null;
      elsif R2 = null then
         return False;
      end if;

      return R1.Name = R2.Name
        and then R1.Address_Offset = R2.Address_Offset
        and then R1.Reg_Properties.Size = R2.Reg_Properties.Size
        and then R1.Dim = R2.Dim
        and then R1.Dim_Increment = R2.Dim_Increment;
   end Equal;

   -----------------------
   -- Find_Common_Types --
   -----------------------

   procedure Find_Common_Types (Reg_Set : Register_Vectors.Vector) is
   begin
      --  Look for fields with similar types, to use a single type definition
      --  in such situation
      for J in Reg_Set.First_Index .. Reg_Set.Last_Index - 1 loop
         if Reg_Set (J).Type_Holder = null then
            for K in J + 1 .. Reg_Set.Last_Index loop
               if Equal (Reg_Set (J), Reg_Set (K)) then
                  --  Simple case: two identical registers.
                  Reg_Set (K).Type_Holder := Reg_Set (J);

               else
                  declare
                     Prefix : constant Unbounded.Unbounded_String :=
                                Similar_Type (Reg_Set (J).all,
                                              Reg_Set (K).all);
                  begin
                     if Unbounded.Length (Prefix) > 0 then
                        --  We have similar types, but with different names.
                        --  In such situation, it'd be nice to generate a
                        --  common type definition.
                        Reg_Set (J).Type_Name := Prefix;
                        Reg_Set (K).Type_Holder := Reg_Set (J);
                     end if;
                  end;
               end if;
            end loop;
         end if;
      end loop;
   end Find_Common_Types;

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

      for J in 1 .. Prefix loop
         if J > Length (Name2) then
            return Null_Unbounded_String;

         elsif Element (Name1, J) /= Element (Name2, J) then
            return Null_Unbounded_String;
         end if;
      end loop;

      for J in Prefix + 1 .. Length (Name2) loop
         if Element (Name2, J) not in '0' .. '9' then
            return Null_Unbounded_String;
         end if;
      end loop;

      return To_Unbounded_String (Slice (Name1, 1, Prefix));
   end Common_Prefix;

   -----------------
   -- Common_Type --
   -----------------

   function Similar_Type
     (R1, R2 : Register) return Unbounded.Unbounded_String
   is
      use Field_Vectors;
      use type Ada.Containers.Count_Type;

   begin
      if Length (R1.Fields) /= Length (R2.Fields) then
         return Unbounded.Null_Unbounded_String;
      end if;

      if R1.Dim /= R2.Dim then
         return Unbounded.Null_Unbounded_String;
      end if;

      for J in R1.Fields.First_Index .. R1.Fields.Last_Index loop
         if Field_T'(R1.Fields (J)) /= R2.Fields (J) then
            return Unbounded.Null_Unbounded_String;
         end if;
      end loop;

      return Common_Prefix (R1.Name, R2.Name);
   end Similar_Type;

   ------------------
   -- Get_Ada_Type --
   ------------------

   function Get_Ada_Type (Reg : Register_T) return String
   is
      use type Ada.Containers.Count_Type;
      use Unbounded;
   begin
      if Reg.Type_Holder /= null then
         return Get_Ada_Type (Reg.Type_Holder);

      elsif Length (Reg.Ada_Type) > 0 then
         return To_String (Reg.Ada_Type);

      else
         raise Constraint_Error with "No ada type defined yet for " &
           To_String (Reg.Name);
      end if;
   end Get_Ada_Type;

   ------------------------
   -- Similar_Field_Name --
   ------------------------

   function Similar_Field
     (F1, F2     : Field_T;
      Prefix_Idx : in out Natural) return Boolean
   is
      use Unbounded;
      Prefix : Unbounded_String;
   begin
      if F1.Size /= F2.Size then
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

   procedure Dump (Spec : in out Ada_Gen.Ada_Spec; Reg : Register_T)
   is
      use Ada.Strings.Unbounded;
      use type Ada.Containers.Count_Type;
   begin
      if Reg.Type_Holder /= null then
         --  This register is not responsible for emitting the Ada type.
         return;
      end if;

      if Field_Vectors.Length (Reg.Fields) = 1
        and then Reg.Fields.First_Element.Size = Reg.Reg_Properties.Size
      then
         --  Don't generate anything here: we use a base type
         Reg.Ada_type :=
           To_Unbounded_String
             (Target_Type (Integer (Reg.Reg_Properties.Size)));

         if Reg.Dim > 0 then
            --  Just generate a comment to document the array that's going
            --  to be generated
            Add (Spec, New_Comment (To_String (Reg.Description)));
         end if;

      elsif Reg.Fields.Is_Empty then
         Reg.Ada_type :=
           To_Unbounded_String
             (Target_Type (Integer (Reg.Reg_Properties.Size)));

      else
         declare
            Fields   : array (0 .. Reg.Reg_Properties.Size - 1) of
                         Field_T := (others => Null_Field);
            Index    : Unsigned := 0;
            Index2   : Unsigned;
            Length   : Unsigned;
            Prefix   : Natural;
            Default  : Unsigned;
            Mask     : Unsigned;
            Rec      : Ada_Type_Record;

         begin
            for Field of Reg.Fields loop
               Fields (Field.LSB) := Field;
            end loop;

            Rec := New_Type_Record
              (To_String (Reg.Type_Name) & "_Register",
               To_String (Reg.Description));

            Add_Aspect (Rec, "Volatile_Full_Access");
            Add_Size_Aspect (Rec, Reg.Reg_Properties.Size);
            Add_Bit_Order_Aspect (Rec, System.Low_Order_First);

            while Index < Reg.Reg_Properties.Size loop
               if Fields (Index) = Null_Field then
                  --  First look for undefined/reserved parts of the register
                  Length := 1;

                  for J in Index + 1 .. Reg.Reg_Properties.Size - 1 loop
                     if Fields (J) = Null_Field then
                        Length := Length + 1;
                     else
                        exit;
                     end if;
                  end loop;

                  --  Retrieve the reset value
                  if Reg.Reg_Properties.Reset_Value = 0 then
                     --  Most common case
                     Default := 0;
                  else
                     Default :=
                       Shift_Right (Reg.Reg_Properties.Reset_Value,
                                    Natural (Index));
                     Mask := 0;
                     for J in 0 .. Length - 1 loop
                        Mask := Mask or 2 ** Natural (J);
                     end loop;
                     Default := Default and Mask;
                  end if;

                  Add_Field
                    (Rec,
                     "Reserved_" & To_String (Index) &
                       "_" & To_String (Index + Length - 1),
                     Target_Type (Length),
                     Offset      => 0,
                     LSB         => Index,
                     MSB         => Index + Length - 1,
                     Default     => Default,
                     Comment     => "unspecified");

                  Index    := Index + Length;

               else
                  --  Check if it's an array, in which case it's easier
                  --  to handle them as such.

                  Length := 1;
                  Prefix := Unbounded.Length (Fields (Index).Name);

                  Index2 := Index + Fields (Index).Size;
                  while Index2 < Reg.Reg_Properties.Size loop
                     if Similar_Field
                       (Fields (Index), Fields (Index2), Prefix)
                     then
                        Length := Length + 1;
                     else
                        exit;
                     end if;

                     Index2 := Index2 + Fields (Index).Size;
                  end loop;

                  if Length > 1 then
                     declare
                        T_Name  : constant String :=
                                    Slice (Fields (Index).Name,
                                           1, Prefix);
                        Union_T : Ada_Type_Union :=
                                    New_Type_Union
                                      (Id        => T_Name & "_Field",
                                       Disc_Name => "As_Array",
                                       Disc_Type => Get_Boolean,
                                       Comment   =>
                                         "Type definition for " & T_Name);
                        Array_T : Ada_Type_Array :=
                                    New_Type_Array
                                      (Id           => T_Name & "_Field_Array",
                                       Index_Type   => "",
                                       Index_First  => 0,
                                       Index_Last   => Unsigned (Length - 1),
                                       Element_Type =>
                                         Target_Type (Fields (Index).Size),
                                       Comment      => "");
                     begin
                        Add (Spec, New_Comment (To_String (Reg.Description)));

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
                           Comment  =>
                             "Array vision of " &
                             To_String (Fields (Index).Name));
                        Add_Field
                          (Rec      => Union_T,
                           Enum_Val => "False",
                           Id       => "Val",
                           Typ      =>
                             Target_Type (Fields (Index).Size * Length),
                           Offset   => 0,
                           LSB      => 0,
                           MSB      => Fields (Index).Size * Length - 1,
                           Comment  =>
                             "Value vision of " &
                             To_String (Fields (Index).Name));

                        Add (Spec, Union_T);

                        Add_Field
                          (Rec,
                           Id      => T_Name,
                           Typ     => Id (Union_T),
                           Offset  => 0,
                           LSB     => Index,
                           MSB     => Index + Fields (Index).Size * Length - 1,
                           Comment => To_String (Fields (Index).Description));
                     end;

                     Index   := Index + Length * Fields (Index).Size;
                  else
                     Add_Field
                       (Rec,
                        Id      => To_String (Fields (Index).Name),
                        Typ     => Target_Type (Fields (Index).Size),
                        Offset  => 0,
                        LSB     => Index,
                        MSB     => Index + Fields (Index).Size - 1,
                        Comment => To_String (Fields (Index).Description));
                     Index := Index + Fields (Index).Size;
                  end if;
               end if;
            end loop;

            Add (Spec, Rec);
            Reg.Ada_Type := Id (Rec);
         end;
      end if;

      if Reg.Dim > 0 then
         declare
            Array_T : Ada_Type_Array :=
                        New_Type_Array
                          (Id           =>
                                    To_String (Reg.Type_Name) & "_Registers",
                           Index_Type   => "",
                           Index_First  => 0,
                           Index_Last   => Reg.Dim - 1,
                           Element_Type => Get_Ada_Type (reg),
                           Comment      => To_String (Reg.Description));
         begin
            Add (Spec, Array_T);
            Reg.Ada_Type := Id (Array_T);
         end;
      end if;
   end Dump;

   ------------------
   -- Dump_Aliased --
   ------------------

   procedure Dump_Aliased
     (Spec  : in out Ada_Gen.Ada_Spec;
      Regs  : Register_Vectors.Vector)
   is
      use Ada.Strings.Unbounded;
      List : Register_Vectors.Vector := Regs;

   begin
      for J in Regs.First_Index .. Regs.Last_Index loop
         if Regs (J).Is_Aliased
           and then Regs (J).First_Alias
           and then Regs (J).Type_Holder = null
         then
            declare
               Reg   : constant Register_T := Regs (J);
               Enum  : Ada_Type_Enum :=
                         New_Type_Enum
                           (To_String (Reg.Alias_Name) & "_Discriminent");
               Union : Ada_Type_Union;
            begin
               Add_Enum_Id (Enum, To_String (Reg.Alias_Suffix));
               List.Delete_First;

               for Reg2 of Regs loop
                  if Reg2 /= Reg
                    and then Reg2.Is_Aliased
                    and then Reg2.Alias_Name = Reg.Alias_Name
                  then
                     Add_Enum_Id (Enum, To_String (Reg2.Alias_Suffix));
                  end if;
               end loop;

               Add (Spec, Enum);
               Union := New_Type_Union
                 (Id        =>
                    To_String (Reg.Alias_Name) & "_Aliased_Register",
                  Disc_Name => "Disc",
                  Disc_Type => Enum);

               Add_Field
                 (Rec      => Union,
                  Enum_Val => To_String (Reg.Alias_Suffix),
                  Id       => To_String (Reg.Alias_Suffix),
                  Typ      => Get_Ada_Type (Reg),
                  Offset   => 0,
                  LSB      => 0,
                  MSB      => Reg.Reg_Properties.Size - 1);

               for Reg2 of Regs loop
                  if Reg2 /= Reg
                    and then Reg2.Is_Aliased
                    and then Reg2.Alias_Name = Reg.Alias_Name
                  then
                     Add_Field
                       (Rec      => Union,
                        Enum_Val => To_String (Reg2.Alias_Suffix),
                        Id       => To_String (Reg2.Alias_Suffix),
                        Typ      => Get_Ada_Type (Reg2),
                        Offset   => 0,
                        LSB      => 0,
                        MSB      => Reg2.Reg_Properties.Size - 1);
                  end if;
               end loop;

               Add (Spec, Union);
               Reg.Ada_Type := Id (Union);
            end;
         end if;
      end loop;
   end Dump_Aliased;

end Register_Descriptor;
