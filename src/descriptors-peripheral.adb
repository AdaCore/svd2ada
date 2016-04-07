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

with Ada.Text_IO;
with Interfaces;         use Interfaces;
with DOM.Core;           use DOM.Core;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;

with Ada_Gen;            use Ada_Gen;

package body Descriptors.Peripheral is

   procedure Insert_Register (Periph : in out Peripheral_T;
                              Reg    : Register_Access);

   function Less (P1, P2 : Peripheral_T) return Boolean;

   package Peripheral_Sort is new Peripheral_Vectors.Generic_Sorting
     (Less);

   ----------
   -- Less --
   ----------

   function Less (P1, P2 : Peripheral_T) return Boolean
   is
   begin
      return P1.Base_Address < P2.Base_Address;
   end Less;

   ---------------------
   -- Read_Peripheral --
   ---------------------

   function Read_Peripheral
     (Elt            : DOM.Core.Element;
      Reg_Properties : Register_Properties_T;
      Vector         : Peripheral_Vectors.Vector) return Peripheral_T
   is
      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret          : Peripheral_T;
      Derived_From : constant String :=
                       Elements.Get_Attribute (Elt, "derivedFrom");

   begin
      Ret.Reg_Properties := Reg_Properties;

      if Derived_From /= "" then
         declare
            Found : Boolean := False;
         begin
            for P of Vector loop
               if Unbounded.To_String (P.Name) = Derived_From then
                  Found := True;
                  Ret   := P;
                  --  Deep copy of the registers list
                  Ret.Registers.Clear;
                  for Reg of P.Registers loop
                     Ret.Registers.Append (new Register_T'(Reg.all));
                  end loop;
                  --  Do not inherit interrupts
                  Ret.Interrupts.Clear;
                  exit;
               end if;
            end loop;

            if not Found then
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
                  Ret.Name := Get_Value (Child);
                  Ret.Gen_Helper :=
                    Get_Peripheral_Helper
                      (Ada.Strings.Unbounded.To_String (Ret.Name));

               elsif Tag = "version" then
                  Ret.Version := Get_Value (Child);

               elsif Tag = "description" then
                  Ret.Description := Get_Value (Child);

               elsif Tag = "groupName" then
                  Ret.Group_Name := Get_Value (Child);

               elsif Tag = "prependToName" then
                  Ret.Prepend_To_Name := Get_Value (Child);

               elsif Tag = "appendToName" then
                  Ret.Append_To_Name := Get_Value (Child);

               elsif Tag = "baseAddress" then
                  Ret.Base_Address := Get_Value (Child);

               elsif Register_Properties.Is_Register_Property (Tag) then
                  Register_Properties.Read_Register_Property
                    (Child, Ret.Reg_Properties);

               elsif Tag = "addressBlock" then
                  Ret.Address_Blocks.Append (Get_Value (Child));

               elsif Tag = "interrupt" then
                  declare
                     Int : Interrupt_Type renames Get_Value (Child);
                  begin
                     --  Check against invalid svd file that define several
                     --  times the same item
                     if not Ret.Interrupts.Contains (Int) then
                        Ret.Interrupts.Append (Int);
                     end if;
                  end;

               elsif Tag = "registers" then
                  declare
                     Child_List : constant Node_List :=
                                    Nodes.Child_Nodes (Child);
                     Register   : Register_Access;
                  begin
                     for K in 0 .. Nodes.Length (Child_List) - 1 loop
                        if Nodes.Node_Type (Nodes.Item (Child_List, K)) =
                          Element_Node
                        then
                           Register :=
                             Read_Register
                               (Element (Nodes.Item (Child_List, K)),
                                Ret.Prepend_To_Name,
                                Ret.Append_To_Name,
                                Ret.Reg_Properties,
                                Ret.Registers);
                           Insert_Register (Ret, Register);
                        end if;
                     end loop;
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring peripheral element " & Tag);
               end if;
            end;
         end if;
      end loop;

      Find_Aliased (Ret.Registers,
                    Resolve => Ret.Gen_Helper = Null_Helper);

      return Ret;
   end Read_Peripheral;

   ---------------------
   -- Insert_Register --
   ---------------------

   procedure Insert_Register (Periph : in out Peripheral_T;
                              Reg    : Register_Access)
   is
      Added : Boolean;
   begin
      if Periph.Registers.Contains (Reg) then
         return;
      end if;

      Added := False;
      for J in 1 .. Integer (Register_Vectors.Length (Periph.Registers)) loop
         if Periph.Registers (J).Address_Offset > Reg.Address_Offset then
            Periph.Registers.Insert (J, Reg);
            Added := True;
            exit;
         end if;
      end loop;

      if not Added then
         Periph.Registers.Append (Reg);
      end if;
   end Insert_Register;

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

      function Create_Record return Ada_Type_Record'Class
      is
      begin
         if Peripheral.Gen_Helper = Null_Helper then
            return New_Type_Record
              (Type_Name,
               To_String (Peripheral.Description));
         else
            declare
               Enum : Ada_Type_Enum :=
                        Get_Discriminent_Type (Peripheral.Gen_Helper);
            begin
               Add (Spec, Enum);
               return New_Type_Union
                 (Type_Name,
                  Get_Discriminent_Name (Peripheral.Gen_Helper),
                  Enum,
                  To_String (Peripheral.Description));
            end;
         end if;
      end Create_Record;

      Rec              : Ada_Type_Record'Class := Create_Record;
      Overriding_Found : Boolean := False;

   begin

      Add_Aspect (Rec, "Volatile");

      for Reg of Peripheral.Registers loop
         if Reg.Is_Overlapping then
            if Peripheral.Gen_Helper /= Null_Helper then
               Add_Field
                 (Ada_Type_Union (Rec),
                  Enum_Val =>
                    Get_Discriminent_Value
                      (Peripheral.Gen_Helper,
                       To_String (Reg.Name)),
                  Id       => To_String (Reg.Name),
                  Typ      => Get_Ada_Type (Reg),
                  Offset   => Reg.Address_Offset,
                  LSB      => 0,
                  MSB      => (if Reg.Dim = 0
                           then Reg.Reg_Properties.Size - 1
                           else Reg.Dim * Reg.Dim_Increment * 8 - 1),
                  Comment  => To_String (Reg.Description));

            elsif Reg.First_Overlap then
               Ada.Text_IO.Put_Line
                 ("... Found overlapping registers: " &
                    To_String (Reg.Name));
               Overriding_Found := True;

               Add_Field
                 (Rec,
                  Id      => To_String (Reg.Overlap_Name),
                  Typ     => Get_Ada_Type (Reg),
                  Offset  => Reg.Address_Offset,
                  LSB     => 0,
                  MSB     => (if Reg.Dim = 0
                              then Reg.Reg_Properties.Size - 1
                              else Reg.Dim * Reg.Dim_Increment * 8 - 1),
                  Comment => To_String (Reg.Description));
            else
               Ada.Text_IO.Put_Line
                 ("... Found overriding registers: " &
                    To_String (Reg.Name));
            end if;
         else
            Add_Field
              (Rec,
               Id      => To_String (Reg.Name),
               Typ     => Get_Ada_Type (Reg),
               Offset  => Reg.Address_Offset,
               LSB     => 0,
               MSB     => (if Reg.Dim = 0
                           then Reg.Reg_Properties.Size - 1
                           else Reg.Dim * Reg.Dim_Increment * 8 - 1),
               Comment => To_String (Reg.Description));
         end if;
      end loop;

      if Overriding_Found then
         Ada.Text_IO.Put_Line
           ("... you may check the automatic generation for " &
              "those registers");
      end if;

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
      Ada.Text_IO.Put_Line
        ("Generate " & To_String (Peripheral.Name));

      Spec := New_Child_Spec
        (To_String (Peripheral.Name),
         Parent        => Dev_Name,
         Descr         => To_String (Peripheral.Description),
         Preelaborated => True);

      if not Register_Vectors.Is_Empty (Peripheral.Registers) then
         Add (Spec, New_Comment_Box ("Registers"));
      end if;

      Find_Common_Types (Peripheral.Registers);

      for Reg of Peripheral.Registers loop
         Dump (Spec, Reg);
      end loop;

      if Peripheral.Gen_Helper = Null_Helper then
         Dump_Aliased (Spec, Peripheral.Registers);
      end if;

      Add (Spec, New_Comment_Box ("Peripherals"));

      Dump_Periph_Type
        (Spec, Peripheral, To_String (Peripheral.Name) & "_Peripheral");

      declare
         Inst : Ada_Instance :=
                  New_Instance
                    (To_String (Peripheral.Name) & "_Periph",
                     To_String (Peripheral.Name) & "_Peripheral",
                     True,
                     To_String (Peripheral.Description));
      begin
         Add_Aspect (Inst, "Import");
         Add_Address_Aspect (Inst, To_String (Peripheral.Name) & "_Base");
         Add (Spec, Inst);
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
      use type Register_Vectors.Vector;
      use type Ada.Containers.Count_Type;
      Regs               : Register_Vectors.Vector;
      Sorted             : Peripheral_Vectors.Vector := Group;
      Partial_Similarity : Boolean := False;
      Spec               : Ada_Spec;

   begin
      Ada.Text_IO.Put_Line
        ("Generate " & To_String (Group.First_Element.Group_Name));

      Spec := New_Child_Spec
        (To_String (Sorted.First_Element.Group_Name),
         Dev_Name,
         "",
         True);

      --  Registers

      for Periph of Sorted loop
         Regs.Append (Periph.Registers);
      end loop;

      if not Regs.Is_Empty then
         Add (Spec, New_Comment_Box ("Registers"));
      end if;

      Find_Common_Types (Regs);

      for Reg of Regs loop
         Dump (Spec, Reg);
      end loop;

      for Periph of Sorted loop
         if Periph.Gen_Helper = Null_Helper then
            Dump_Aliased (Spec, Periph.Registers);
         end if;
      end loop;

      Add (Spec, New_Comment_Box ("Peripherals"));

      --  Determine if all peripherals of the group have the same layout
      Peripheral_Sort.Sort (Sorted);

      while not Sorted.Is_Empty loop
         declare
            First : constant Peripheral_T := Sorted.First_Element;
            List  : Peripheral_Vectors.Vector;
            Idx   : Natural;
         begin
            Sorted.Delete_First;
            List.Append (First);

            Idx := Sorted.First_Index;
            while Idx <= Sorted.Last_Index loop
               if Sorted (Idx).Registers = First.Registers then
                  List.Append (Sorted (Idx));
                  Sorted.Delete (Idx);
               else
                  Idx := Idx + 1;
               end if;
            end loop;

            if List.Length = 1 then
               Dump_Periph_Type
                 (Spec, First, To_String (First.Name) & "_Peripheral");

               declare
                  Inst : Ada_Instance :=
                           New_Instance
                             (To_String (First.Name) & "_Periph",
                              To_String (First.Name) & "_Peripheral",
                              True,
                              To_String (First.Description));
               begin
                  Add_Aspect (Inst, "Import");
                  Add_Address_Aspect (Inst, To_String (First.Name) & "_Base");
                  Add (Spec, Inst);
               end;

            elsif Sorted.Is_Empty and then not Partial_Similarity then
               Dump_Periph_Type
                 (Spec, First, To_String (First.Group_Name) & "_Peripheral");

               for Periph of List loop
                  declare
                     Inst : Ada_Instance :=
                              New_Instance
                                (To_String (Periph.Name) & "_Periph",
                                 To_String (First.Group_Name) & "_Peripheral",
                                 True,
                                 To_String (Periph.Description));
                  begin
                     Add_Aspect (Inst, "Import");
                     Add_Address_Aspect
                       (Inst, To_String (Periph.Name) & "_Base");
                     Add (Spec, Inst);
                  end;
               end loop;

            else
               Partial_Similarity := True;
               Dump_Periph_Type
                 (Spec, First, To_String (First.Name) & "_Peripheral");

               for Periph of List loop
                  declare
                     Inst : Ada_Instance :=
                              New_Instance
                                (To_String (Periph.Name) & "_Periph",
                                 To_String (First.Name) & "_Peripheral",
                                 True,
                                 To_String (Periph.Description));
                  begin
                     Add_Aspect (Inst, "Import");
                     Add_Address_Aspect
                       (Inst, To_String (Periph.Name) & "_Base");
                     Add (Spec, Inst);
                  end;
               end loop;
            end if;
         end;
      end loop;

      Write_Spec (Spec, Output_Dir);
   end Dump;

end Descriptors.Peripheral;
