------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2019, AdaCore                      --
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

with DOM.Core;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;

with Ada_Gen;            use Ada_Gen;
with SVD2Ada_Utils;

package body Descriptors.Peripheral is

   procedure Insert_Element
     (Periph : in out Peripheral_T;
      Elt    : Peripheral_Element);

   function Less (P1, P2 : Peripheral_Access) return Boolean;

   package Peripheral_Sort is new Peripheral_Vectors.Generic_Sorting
     (Less);

   procedure Dump_Periph_Type
     (Spec       : in out Ada_Gen.Ada_Spec;
      Peripheral : Peripheral_T;
      Type_Name  : String);

   ----------
   -- Less --
   ----------

   function Less (P1, P2 : Peripheral_Access) return Boolean
   is
      Name1  : constant String := Unbounded.To_String (P1.Name);
      Name2  : constant String := Unbounded.To_String (P2.Name);
      I1, I2 : Natural := 0;
      N1, N2 : Natural;
   begin
      for J in reverse Name1'Range loop
         if Name1 (J) not in '0' .. '9' then
            I1 := J;
            exit;
         end if;
      end loop;

      for J in reverse Name2'Range loop
         if Name2 (J) not in '0' .. '9' then
            I2 := J;
            exit;
         end if;
      end loop;

      if I1 = I2
        and then I1 /= Name1'Last
        and then I2 /= Name2'Last
        and then Name1 (1 .. I1) = Name2 (1 .. I2)
      then
         N1 := Natural'Value (Name1 (I1 + 1 .. Name1'Last));
         N2 := Natural'Value (Name2 (I2 + 1 .. Name2'Last));

         return N1 < N2;
      end if;

      return Name1 < Name2;
   end Less;

   ---------------------
   -- Read_Peripheral --
   ---------------------

   function Read_Peripheral
     (Elt            : DOM.Core.Element;
      Reg_Properties : Register_Properties_T;
      Periph_Db      : Peripheral_Db'Class) return Peripheral_T
   is
      use DOM.Core;
      use type Ada.Strings.Unbounded.Unbounded_String;

      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Result       : Peripheral_T;
      Derived_From : constant String :=
                       Elements.Get_Attribute (Elt, "derivedFrom");

   begin
      Result.Reg_Properties := Reg_Properties;

      if Derived_From /= "" then
         declare
            Oth : constant Peripheral_Access :=
                    Periph_Db.Get_Peripheral (Derived_From);
         begin
            if Oth /= null then
--              for P of Vector loop
--                 if Unbounded.To_String (P.Name) = Derived_From then
               Result   := Oth.all;
               --  Deep copy of the registers list
               Result.Content.Clear;

               for Elt of Oth.Content loop
                  Result.Content.Append (Deep_Copy (Elt));
               end loop;

               --  Do not inherit interrupts
               Result.Interrupts.Clear;

            else
               raise Constraint_Error with
                 "peripheral 'derivedFrom' is not known: " & Derived_From;
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
                  Result.Type_Name := Result.Name;

               elsif Tag = "headerStructName" then
                  Result.Type_Name := Get_Value (Child);

               elsif Tag = "version" then
                  Result.Version := Get_Value (Child);

               elsif Tag = "description" then
                  Result.Description := Get_Value (Child);

               elsif Tag = "groupName" then
                  Result.Group_Name := Get_Value (Child);

               elsif Tag = "prependToName" then
                  Result.Prepend_To_Name := Get_Value (Child);

               elsif Tag = "appendToName" then
                  Result.Append_To_Name := Get_Value (Child);

               elsif Tag = "baseAddress" then
                  Result.Base_Address := Get_Value (Child);

               elsif Register_Properties.Is_Register_Property (Tag) then
                  Register_Properties.Read_Register_Property
                    (Child, Result.Reg_Properties);

               elsif Tag = "addressBlock" then
                  Result.Address_Blocks.Append (Get_Value (Child));

               elsif Tag = "interrupt" then
                  declare
                     Int : Interrupt_Type renames Get_Value (Child);
                  begin
                     --  Check against invalid svd file that define several
                     --  times the same item
                     if not Result.Interrupts.Contains (Int) then
                        Result.Interrupts.Append (Int);
                     end if;
                  end;

               elsif Tag = "registers" then
                  declare
                     Child_List : constant Node_List :=
                                    Nodes.Child_Nodes (Child);
                     Register   : Register_Access;
                     Cluster    : Cluster_Access;
                     Reg2       : Register_Access;
                     use Ada.Strings.Unbounded;
                  begin
                     for K in 0 .. Nodes.Length (Child_List) - 1 loop
                        if Nodes.Node_Type (Nodes.Item (Child_List, K)) =
                          Element_Node
                        then
                           declare
                              C_Child : constant DOM.Core.Element :=
                                          DOM.Core.Element
                                            (Nodes.Item (Child_List, K));
                              C_Tag : String renames
                                        Elements.Get_Tag_Name (C_Child);
                           begin
                              if C_Tag = "register" then
                                 Register := Read_Register
                                   (C_Child,
                                    Result.Prepend_To_Name,
                                    Result.Append_To_Name,
                                    Result.Reg_Properties,
                                    Result);

                                 if not SVD2Ada_Utils.Gen_Arrays
                                   or else (Register.Dim > 1
                                            and then Register.Dim_Increment /=
                                              Register.Reg_Properties.Size / 8)
                                 then
                                    --  in such case, this certainly indicates
                                    --  two intertwined arrays of registers, We
                                    --  need in this case to expand the erray
                                    --  into individual values
                                    for J in 0 .. Register.Dim - 1 loop
                                       Reg2 := new Register_T'(Register.all);
                                       Reg2.Dim := 1;
                                       Reg2.Address_Offset :=
                                         Register.Address_Offset +
                                           J * Register.Dim_Increment;
                                       Reg2.Name :=
                                         Register.Name & To_String (J);
                                       Insert_Element (Result, +Reg2);
                                    end loop;
                                 else
                                    Insert_Element (Result, +Register);
                                 end if;

                              elsif C_Tag = "cluster" then
                                 Cluster := new Cluster_T'
                                   (Read_Cluster
                                      (C_Child,
                                       Result.Prepend_To_Name,
                                       Result.Append_To_Name,
                                       Result.Reg_Properties,
                                       Result));
                                 Insert_Element (Result, +Cluster);
                              end if;
                           end;
                        end if;
                     end loop;
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring peripheral element " & Tag & " at " & Full_Name (Child));
               end if;
            end;
         end if;
      end loop;

      return Result;
   end Read_Peripheral;

   ------------------
   -- Get_Register --
   ------------------

   overriding function Get_Register
     (Db     : Peripheral_T;
      XML_Id : String) return Register_Access
   is
   begin
      for Elt of Db.Content loop
         if Elt.Kind = Register_Element
           and then Ada.Strings.Unbounded.To_String (Elt.Reg.Xml_Id) = XML_Id
         then
            return Elt.Reg;
         end if;
      end loop;

      return null;
   end Get_Register;

   -----------------
   -- Get_Cluster --
   -----------------

   overriding function Get_Cluster
     (Db     : Peripheral_T;
      XML_Id : String) return Cluster_Access
   is
   begin
      for Elt of Db.Content loop
         if Elt.Kind = Cluster_Element
           and then Unbounded.To_String (Elt.Cluster.Xml_Id) = XML_Id
         then
            return Elt.Cluster;
         end if;
      end loop;

      return null;
   end Get_Cluster;

   --------------------
   -- Insert_Element --
   --------------------

   procedure Insert_Element
     (Periph : in out Peripheral_T;
      Elt    : Peripheral_Element)
   is
      Added  : Boolean;
      Offset : Natural;
   begin
      if Periph.Content.Contains (Elt) then
         return;
      end if;

      case Elt.Kind is
         when Register_Element =>
            Offset := Elt.Reg.Address_Offset;
         when Cluster_Element =>
            Offset := Elt.Cluster.Address_Offset;
      end case;

      Added := False;

      for J in 1 ..
        Integer (Peripheral_Element_Vectors.Length (Periph.Content))
      loop
         case Periph.Content (J).Kind is
            when Register_Element =>
               if Periph.Content (J).Reg.Address_Offset > Offset then
                  Periph.Content.Insert (J, Elt);
                  Added := True;
                  exit;
               end if;
            when Cluster_Element =>
               if Periph.Content (J).Cluster.Address_Offset > Offset then
                  Periph.Content.Insert (J, Elt);
                  Added := True;
                  exit;
               end if;
         end case;
      end loop;

      if not Added then
         Periph.Content.Append (Elt);
      end if;
   end Insert_Element;

   ----------------------
   -- Dump_Periph_Type --
   ----------------------

   procedure Dump_Periph_Type
     (Spec       : in out Ada_Gen.Ada_Spec;
      Peripheral : Peripheral_T;
      Type_Name  : String)
   is
      use Ada.Strings.Unbounded;

      function Create_Record return Ada_Type_Record'Class;

      -------------------
      -- Create_Record --
      -------------------

      function Create_Record return Ada_Type_Record'Class
      is
         Found : Boolean;
      begin
         Process_Overlapping_Registers (Peripheral.Content, Found);
         if Found then
            declare
               Enum : Ada_Type_Enum :=
                        Get_Discriminent_Type
                          (Peripheral.Content,
                           Spec,
                           To_String (Peripheral.Type_Name));
            begin
               Add (Spec, Enum);
               return New_Type_Union
                 (Type_Name,
                  "Discriminent",
                  Enum,
                  To_String (Peripheral.Description));
            end;
         else
            --  No overlapping register: generate a simple record
            return New_Type_Record
              (Type_Name,
               To_String (Peripheral.Description));
         end if;
      end Create_Record;

      Rec : Ada_Type_Record'Class := Create_Record;

   begin
      Add_Aspect (Rec, "Volatile");

      Dump_Peripheral_Elements
        (Rec,
         Peripheral.Content);

      Add (Spec, Rec);
   end Dump_Periph_Type;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Peripheral : in out Peripheral_T;
      Dev_Name   : String;
      Output_Dir : String)
   is
      use Ada.Strings.Unbounded;
      Spec : Ada_Spec;

   begin
      Spec := New_Child_Spec
        (To_String (Peripheral.Type_Name),
         Parent        => Dev_Name,
         Descr         => To_String (Peripheral.Description),
         Preelaborated => True);

      Ada.Text_IO.Put_Line ("Generating package " & To_String (Id (Spec)));

      if not Peripheral.Content.Is_Empty then
         Add (Spec, New_Comment_Box ("Registers"));
      end if;

      Find_Common_Types (Peripheral.Content);

      for Elt of Peripheral.Content loop
         case Elt.Kind is
            when Register_Element =>
               Dump (Spec, Elt.Reg);
            when Cluster_Element =>
               Dump (Spec, Elt.Cluster);
         end case;
      end loop;

      Add (Spec, New_Comment_Box ("Peripherals"));

      Dump_Periph_Type
        (Spec, Peripheral, To_String (Peripheral.Type_Name) & "_Peripheral");

      declare
         Object : Ada_Variable :=
                  New_Variable
                    (To_String (Peripheral.Name) & "_Periph",
                     To_String (Peripheral.Type_Name) & "_Peripheral",
                     True,
                     To_String (Peripheral.Description));
      begin
         Add_Aspect (Object, "Import");
         Add_Address_Aspect
           (Object, To_String (Peripheral.Name) & "_Base");
         Add (Spec, Object);
      end;

      Write_Spec (Spec, Output_Dir);
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Group      : Peripheral_Vectors.Vector;
      Dev_Name   : String;
      Output_Dir : String)
   is
      use Ada.Strings.Unbounded;
      use type Peripheral_Element_Vectors.Vector;
      use type Ada.Containers.Count_Type;

      Regs               : Peripheral_Element_Vectors.Vector;
      Sorted             : Peripheral_Vectors.Vector := Group;
      Partial_Similarity : Boolean := False;
      Spec               : Ada_Spec;

   begin
      Spec := New_Child_Spec
        (To_String (Sorted.First_Element.Group_Name),
         Dev_Name,
         "",
         True);

      Ada.Text_IO.Put_Line ("Generating package " & To_String (Id (Spec)));

      Peripheral_Sort.Sort (Sorted);

      --  Registers

      for Periph of Sorted loop
         Regs.Append (Periph.Content);
      end loop;

      if not Regs.Is_Empty then
         Add (Spec, New_Comment_Box ("Registers"));
      end if;

      Find_Common_Types (Regs);

      for Elt of Regs loop
         case Elt.Kind is
            when Register_Element =>
               Dump (Spec, Elt.Reg);
            when Cluster_Element =>
               Dump (Spec, Elt.Cluster);
         end case;
      end loop;

      Add (Spec, New_Comment_Box ("Peripherals"));
      Add (Spec, New_With_Clause ("System"));

      --  Determine if all peripherals of the group have the same layout

      while not Sorted.Is_Empty loop
         declare
            First : constant Peripheral_Access := Sorted.First_Element;
            List  : Peripheral_Vectors.Vector;
            Idx   : Natural;
         begin
            Sorted.Delete_First;
            List.Append (First);

            Idx := Sorted.First_Index;
            while Idx <= Sorted.Last_Index loop
               if Sorted (Idx).Content = First.Content then
                  List.Append (Sorted (Idx));
                  Sorted.Delete (Idx);
               else
                  Idx := Idx + 1;
               end if;
            end loop;

            if List.Length = 1 then
               Dump_Periph_Type
                 (Spec, First.all,
                  To_String (First.Type_Name) & "_Peripheral");

               declare
                  Inst : Ada_Variable :=
                           New_Variable
                             (To_String (First.Name) & "_Periph",
                              To_String (First.Type_Name) & "_Peripheral",
                              True,
                              To_String (First.Description));
               begin
                  Add_Aspect (Inst, "Import");
                  Add_Address_Aspect
                    (Inst, To_String (First.Name) & "_Base");
                  Add (Spec, Inst);
               end;

            elsif Sorted.Is_Empty and then not Partial_Similarity then
               Dump_Periph_Type
                 (Spec, First.all,
                  To_String (First.Group_Name) & "_Peripheral");

               for Periph of List loop
                  declare
                     Object : Ada_Variable :=
                              New_Variable
                                (To_String (Periph.Name) & "_Periph",
                                 To_String (First.Group_Name) & "_Peripheral",
                                 True,
                                 To_String (Periph.Description));
                  begin
                     Add_Aspect (Object, "Import");
                     Add_Address_Aspect
                       (Object, To_String (Periph.Name) & "_Base");
                     Add (Spec, Object);
                  end;
               end loop;

            else
               Partial_Similarity := True;
               Dump_Periph_Type
                 (Spec, First.all,
                  To_String (First.Type_Name) & "_Peripheral");

               for Periph of List loop
                  declare
                     Object : Ada_Variable :=
                              New_Variable
                                (To_String (Periph.Name) & "_Periph",
                                 To_String (First.Type_Name) & "_Peripheral",
                                 True,
                                 To_String (Periph.Description));
                  begin
                     Add_Aspect (Object, "Import");
                     Add_Address_Aspect
                       (Object, To_String (Periph.Name) & "_Base");
                     Add (Spec, Object);
                  end;
               end loop;
            end if;
         end;
      end loop;

      Write_Spec (Spec, Output_Dir);
   end Dump;

end Descriptors.Peripheral;
