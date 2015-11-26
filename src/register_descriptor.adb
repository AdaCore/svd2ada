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
      Ret          : Register_T;
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
                  Ret := Oth;
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

      --  Look for fields with similar types, to use a single type definition
      --  in such situation
      for J in Vec.First_Index .. Vec.Last_Index loop
         declare
            Oth    : Register_T := Vec (J);
            Prefix : constant Unbounded.Unbounded_String :=
                       Similar_Type (Oth, Ret);
         begin
            if Unbounded.Length (Prefix) > 0 then
               Ret.Type_Name := Prefix;
               Ret.Gen_Type := False;

               if Unbounded.Length (Oth.Type_Name) = 0 then
                  Oth.Type_Name := Prefix;
                  Vec.Replace_Element (J, Oth);
               end if;
            end if;
         end;
      end loop;

      return Ret;
   end Read_Register;

   ---------
   -- "=" --
   ---------

   function "=" (R1, R2 : Register_T) return Boolean
   is
      use type Unbounded.Unbounded_String;
      use type Field_Vectors.Vector;
   begin
      return R1.Name = R2.Name
        and then R1.Address_Offset = R2.Address_Offset
        and then R1.Reg_Properties.Size = R2.Reg_Properties.Size
        and then R1.Dim = R2.Dim
        and then R1.Dim_Increment = R2.Dim_Increment;
   end "=";

   --------------
   -- Add_Regs --
   --------------

   procedure Add_Regs
     (Vec1, Vec2 : in out Register_Vectors.Vector)
   is
      Idx        : Natural;
      Reg1, Reg2 : Register_T;
      Prefix     : Unbounded.Unbounded_String;
      use Unbounded;
   begin
      --  Merge two register vectors, to handle type declaration of registers
      --  that use the same type.
      for J in Vec2.First_index .. Vec2.Last_Index loop
         Reg2 := Vec2 (J);

         --  See if Reg2 already exists in Vec1
         Idx := Vec1.Find_Index (Reg2);
         if Idx >= Vec1.First_Index then
            --  Found it, try to merge the types
            Reg1 := Vec1.Element (Idx);
            if Length (Reg1.Type_Name) > 0 then
               --  Make sure that similar registers have the same type name
               Reg2.Type_Name := Reg1.Type_Name;
               Reg2.Gen_Type  := False;
               Vec2.Replace_Element (J, Reg2);

            elsif Length (Reg2.Type_Name) > 0 then
               Vec1.Append (Reg2);
               --  ??? We should replace Reg1 here with a proper type_name
            end if;
         else
            Vec1.Append (Reg2);
         end if;
      end loop;
   end Add_Regs;

   -------------------
   -- Common_Prefix --
   -------------------

   function Common_Prefix
     (Name1, Name2 : Unbounded.Unbounded_String)
      return Unbounded.Unbounded_String
   is
      use Unbounded;
      Prefix : Natural;
   begin
      for J in 1 .. Length (Name1) loop
         if J > Length (Name2) then
            return Null_Unbounded_String;

         elsif Element (Name1, J) in '0' .. '9' then
            --  If Name2(J) is also a number, we have a proper prefix
            exit when Element (Name2, J) in '0' .. '9';
            --  Else, R1 and R2 don't have a shared prefix
            return Null_Unbounded_String;

         elsif Element (Name1, J) /= Element (Name2, J) then
            return Null_Unbounded_String;
         end if;

         Prefix := J;
      end loop;

      for J in Prefix + 1 .. Length (Name1) loop
         if Element (Name1, J) not in '0' .. '9' then
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
     (R1, R2 : Register_T) return Unbounded.Unbounded_String
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

   function Get_Ada_Type (Reg      : Register_T;
                          Elt_Type : Boolean := False) return String
   is
      use type Ada.Containers.Count_Type;
   begin
      if not Elt_Type and then Reg.Dim > 0 then
         if Unbounded.Length (Reg.Type_Name) > 0 then
            return Unbounded.To_String (Reg.Type_Name) & "_Array";
         else
            return Unbounded.To_String (Reg.Name) & "_Array";
         end if;

      elsif not Field_Vectors.Is_Empty (Reg.Fields)
        and then
          (Field_Vectors.Length (Reg.Fields) > 1
           or else Reg.Fields.First_Element.Size /= Reg.Reg_Properties.Size)
      then
         if Unbounded.Length (Reg.Type_Name) > 0 then
            return Unbounded.To_String (Reg.Type_Name) & "_T";
         else
            return Unbounded.To_String (Reg.Name) & "_T";
         end if;
      else
         return Target_Type (Integer (Reg.Reg_Properties.Size));
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

   procedure Dump (Reg : Register_T)
   is
      use Ada.Strings.Unbounded;
      use type Ada.Containers.Count_Type;
   begin
      if Field_Vectors.Length (Reg.Fields) = 1
        and then Reg.Fields.First_Element.Size = Reg.Reg_Properties.Size
      then
         --  Don't generate anything here: we use a base type definition here
         if Reg.Dim > 0 then
            --  Just generate a comment to document the array that's going
            --  to be generated
            Gen_Comment (To_String (Reg.Description));
         end if;

      elsif not Field_Vectors.Is_Empty (Reg.Fields) and then Reg.Gen_Type then
         declare
            Fields   : array (0 .. Reg.Reg_Properties.Size - 1) of
                         Field_T := (others => Null_Field);
            Index    : Unsigned := 0;
            Index2   : Unsigned;
            Length   : Unsigned;
            Prefix   : Natural;
            Default  : Unsigned_32;
            Mask     : Unsigned_32;

         begin
            for Field of Reg.Fields loop
               Fields (Field.LSB) := Field;
            end loop;

            Gen_Comment (To_String (Reg.Description));

            Ada_Gen.Start_Record_Def (Get_Ada_Type (Reg, Elt_Type => True));

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

                  Ada_Gen.Add_Record_Field
                    ("Reserved_" & To_String (Index) &
                       "_" & To_String (Index + Unsigned (Length) - 1),
                     Target_Type (Length),
                     Offset      => 0,
                     LSB         => Index,
                     MSB         => Index + Unsigned (Length) - 1,
                     Descr       => "",
                     Has_Default => True,
                     Default     => Default);

                  Index    := Index + Unsigned (Length);

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
                     Ada_Gen.Add_Record_Union_Field
                       (Slice (Fields (Index).Name, 1, Prefix),
                        Target_Type (Length * Fields (Index).Size),
                        Elts     => Unsigned (Length),
                        Elts_Typ => Target_Type (Fields (Index).Size),
                        Offset   => 0,
                        LSB      => Index,
                        MSB      => Index + Fields (Index).Size * Length - 1,
                        Descr    => To_String (Fields (Index).Description));
                     Index   := Index + Length * Fields (Index).Size;
                  else
                     Ada_Gen.Add_Record_Field
                       (To_String (Fields (Index).Name),
                        Target_Type (Fields (Index).Size),
                        Offset => 0,
                        LSB    => Index,
                        MSB    => Index + Fields (Index).Size - 1,
                        Descr  => To_String (Fields (Index).Description));
                     Index := Index + Fields (Index).Size;
                  end if;
               end if;
            end loop;
            Ada_Gen.End_Record (Register);
         end;
      end if;

      if Reg.Dim > 0 then
         Gen_Array_Type
           (Get_Ada_Type (Reg, Elt_Type => False),
            Index_Type   => "Natural",
            Low          => 0,
            High         => Reg.Dim - 1,
            Element_Type => Get_Ada_Type (Reg, Elt_Type => True));
      end if;
   end Dump;

end Register_Descriptor;
