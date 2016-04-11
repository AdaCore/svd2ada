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

with System;
with Ada.Text_IO;

with DOM.Core;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;

with Ada_Gen;            use Ada_Gen;
with SVD2Ada_Options;

package body Descriptors.Register is

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (Elt            : DOM.Core.Element;
      Prepend        : Unbounded.Unbounded_String;
      Append         : Unbounded.Unbounded_String;
      Reg_Properties : Register_Properties_T;
      Vec            : in out Register_Vectors.Vector) return Register_Access
   is
      use DOM.Core;
      use type Unbounded.Unbounded_String;
      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret          : Register_T;
      Derived_From : constant String :=
                       Elements.Get_Attribute (Elt, "derivedFrom");

      ------------------
      -- Compute_Name --
      ------------------

      function Compute_Name return Unbounded.Unbounded_String
      is
      begin
         if Ret.Dim = 0 then
            return Ret.Xml_Id;
         else
            declare
               Name : constant String := Unbounded.To_String (Ret.Xml_Id);
               Ret  : String (Name'Range);
               Idx  : Natural;
               Skip : Boolean := False;
            begin
               Idx := Ret'First - 1;

               for J in Name'Range loop
                  if Skip then
                     Skip := False;

                  elsif Name (J) = '['
                    or else Name (J) = ']'
                  then
                     null;

                  elsif J < Name'Last and then Name (J .. J + 1) = "%s" then
                     --  Skip the next character (e.g. 's')
                     Skip := True;

                  else
                     Idx := Idx + 1;
                     Ret (Idx) := Name (J);
                  end if;
               end loop;

               if Idx in Ret'Range and then Ret (Idx) = '_' then
                  Idx := Idx - 1;
               end if;

               return Unbounded.To_Unbounded_String (Ret (Ret'First .. Idx));
            end;
         end if;
      end Compute_Name;

   begin
      Ret.Reg_Properties := Reg_Properties;

      if Derived_From /= "" then
         declare
            Found : Boolean := False;
         begin
            for Oth of Vec loop
               if Unbounded.To_String (Oth.Xml_Id) = Derived_From then
                  Ret := Oth.all;
                  Ret.Name := Unbounded.Null_Unbounded_String;
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
                  Ret.Xml_Id := Get_Value (Child);
                  Ret.Name := Compute_Name;
                  Ret.Type_Name := Prepend & Ret.Name & Append;

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
                                Ret.Fields,
                                Ret.Reg_Properties.Reg_Access,
                                Ret.Read_Action);
                           if not Ret.Fields.Contains (Field) then
                              Ret.Fields.Append (Field);
                           end if;
                        end if;
                     end loop;
                  end;

               elsif Tag = "dim" then
                  Ret.Dim := Get_Value (Child);

                  if Unbounded.Length (Ret.Xml_Id) > 0 then
                     Ret.Name := Compute_Name;
                  end if;

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

      return new Register_T'(Ret);
   end Read_Register;

   ------------------
   -- Find_Aliased --
   ------------------

   procedure Find_Aliased
     (Reg_Set : Register_Vectors.Vector;
      Resolve : Boolean)
   is
      use Unbounded;
   begin
      if not Resolve then
         --  In this case, the actual handling of the aliased registers is
         --  performed by the peripheral helper. All we need here is to flag
         --  the aliased registers.
         For J in Reg_Set.First_Index .. Reg_Set.Last_Index - 1 loop
            declare
               Reg1 : Register_Access renames Reg_Set (J);
            begin
               for K in J + 1 .. Reg_Set.Last_Index loop
                  declare
                     Reg2 : Register_Access renames Reg_Set (K);
                  begin
                     if Reg1.Address_Offset = Reg2.Address_Offset then
                        Reg_Set (J).Is_Overlapping := True;
                        Reg_Set (K).Is_Overlapping := True;
                     end if;
                  end;
               end loop;
            end;
         end loop;

         return;
      end if;

      for J in Reg_Set.First_Index .. Reg_Set.Last_Index - 1 loop
         --  Do not perform a second pass if the register has already been
         --  detected as aliased
         if not Reg_Set (J).Is_Overlapping then
            declare
               Reg    : constant Register_Access := Reg_Set (J);
               Prefix : constant String := To_String (Reg.Name);
               Last   : Natural := Prefix'Last;
            begin
               --  First loop: look of another register at the same offset
               --  If found, mark the current register as overlapping, and find
               --  a prefix common to all overlapping registers.
               for K in J + 1 .. Reg_Set.Last_Index loop
                  if Reg_Set (K).Address_Offset = Reg.Address_Offset then
                     Reg.Is_Overlapping := True;

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

               --  Second loop: if we found overlapping registers, then mark
               --  all with the Is_Overlapping flag, and assign them an enum
               --  value
               if Reg.Is_Overlapping then
                  if Prefix (Last) = '_'
                    or else Prefix (Last) = '-'
                  then
                     Reg.Overlap_Name :=
                       To_Unbounded_String
                         (Prefix (Prefix'First .. Last - 1));
                  else
                     Reg.Overlap_Name :=
                       To_Unbounded_String
                         (Prefix (Prefix'First .. Last));
                  end if;

                  if Last = Prefix'Last then
                     Reg.Overlap_Suffix := To_Unbounded_String ("Default");
                  else
                     Reg.Overlap_Suffix :=
                       To_Unbounded_String (Prefix (Last + 1 .. Prefix'Last));
                  end if;

                  --  Current register is the first found
                  Reg.First_Overlap := True;

                  --  Now apply the type name to all aliased registers
                  for K in J + 1 .. Reg_Set.Last_Index loop
                     if Reg_Set (K).Address_Offset = Reg.Address_Offset then
                        declare
                           Oth : Register_T renames Reg_Set (K).all;
                        begin
                           Oth.Overlap_Name   := Reg.Overlap_Name;
                           Oth.Is_Overlapping := True;
                           Oth.First_Overlap  := False;

                           if Last = Length (Oth.Name) then
                              Oth.Overlap_Suffix :=
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

                                 Oth.Overlap_Suffix :=
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

   function Equal (R1, R2 : Register_Access) return Boolean
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
        and then R1.Dim_Increment = R2.Dim_Increment
        and then R1.Fields = R2.Fields;
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
                                Similar_Type (Reg_Set (J), Reg_Set (K));
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

   -----------------
   -- Common_Type --
   -----------------

   function Similar_Type
     (R1, R2 : Register_Access) return Unbounded.Unbounded_String
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
         if R1.Fields.Element (J) /= R2.Fields.Element (J) then
            return Unbounded.Null_Unbounded_String;
         end if;
      end loop;

      return Common_Prefix (R1.Name, R2.Name);
   end Similar_Type;

   ------------------
   -- Get_Ada_Type --
   ------------------

   function Get_Ada_Type (Reg : Register_Access) return String
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

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Spec : in out Ada_Gen.Ada_Spec;
      Reg  : Register_Access)
   is
      use Ada.Strings.Unbounded;
      use type Ada.Containers.Count_Type;
   begin
      if Reg.Type_Holder /= null then
         --  This register is not responsible for emitting the Ada type.
         return;
      end if;

      if (Reg.Fields.Length = 1
          and then Reg.Fields.First_Element.Size = Reg.Reg_Properties.Size)
        or else Reg.Fields.Is_Empty
      then
         --  Don't generate anything here: we use a base type
         Reg.Ada_type :=
           To_Unbounded_String
             (Target_Type (Reg.Reg_Properties.Size));

         if Reg.Dim > 0 then
            --  Just generate a comment to document the array that's going
            --  to be generated
            Add (Spec, New_Comment (To_String (Reg.Description)));
         end if;

      else
         declare
            Rec             : Ada_Type_Record;

         begin
            Add (Spec,
                 New_Comment_Box (To_String (Reg.Type_Name) & "_Register"));
            Rec := Ada_Type_Record
              (New_Type_Record
                 (To_String (Reg.Type_Name) & "_Register",
                  To_String (Reg.Description)));

            Descriptors.Field.Dump
              (Spec,
               Reg,
               Rec,
               Reg.Fields,
               Reg.Reg_Properties);

            if SVD2Ada_Options.Use_Standard_Volatile_Aspect then
               Add_Aspect (Rec, "Volatile");
            else
               Add_Aspect (Rec, "Volatile_Full_Access");
            end if;
            Add_Size_Aspect (Rec, Reg.Reg_Properties.Size);
            Add_Bit_Order_Aspect (Rec, System.Low_Order_First);

            declare
               Res : Ada_Type'Class := Simplify (Rec, Spec);
            begin
               Add (Spec, Res);
               Reg.Ada_Type := Id (Res);
            end;
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
      Regs  : in out Register_Vectors.Vector)
   is
      use Ada.Strings.Unbounded;

   begin
      for Reg of Regs loop
         if Reg.Is_Overlapping
           and then Reg.First_Overlap
           and then Reg.Type_Holder = null
         then
            declare
               Enum  : Ada_Type_Enum :=
                         New_Type_Enum
                           (To_String (Reg.Overlap_Name) & "_Discriminent");
               Val   : Ada_Enum_Value;
               Union : Ada_Type_Union;

            begin
               for Reg2 of Regs loop
                  if Reg2.Is_Overlapping
                    and then Reg2.Overlap_Name = Reg.Overlap_Name
                  then
                     Val :=
                       Add_Enum_Id (Enum, To_String (Reg2.Overlap_Suffix));
                     Reg2.Overlap_Suffix := Id (Val);
                  end if;
               end loop;

               Add (Spec, Enum);
               Union := New_Type_Union
                 (Id        =>
                    To_String (Reg.Overlap_Name) & "_Aliased_Register",
                  Disc_Name => "Disc",
                  Disc_Type => Enum);

               for Reg2 of Regs loop
                  if Reg2.Is_Overlapping
                    and then Reg2.Overlap_Name = Reg.Overlap_Name
                  then
                     Add_Field
                       (Rec      => Union,
                        Enum_Val => To_String (Reg2.Overlap_Suffix),
                        Id       => To_String (Reg2.Overlap_Suffix),
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

end Descriptors.Register;
