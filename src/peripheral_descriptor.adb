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

with Ada_Gen;            use Ada_Gen;

package body Peripheral_Descriptor is

   procedure Insert_Register (Periph : in out Peripheral_T;
                              Reg    : Register_T);

   package Interrupt_Sort is new Interrupt_Vectors.Generic_Sorting
     (Base_Types."<");

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
                     Register   : Register_T;
                  begin
                     for K in 0 .. Nodes.Length (Child_List) - 1 loop
                        if Nodes.Node_Type (Nodes.Item (Child_List, K)) =
                          Element_Node
                        then
                           Register :=
                             Read_Register
                               (Element (Nodes.Item (Child_List, K)),
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

      return Ret;
   end Read_Peripheral;

   ---------------------
   -- Insert_Register --
   ---------------------

   procedure Insert_Register (Periph : in out Peripheral_T;
                              Reg    : Register_T)
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

   procedure Dump_Periph_Type (Peripheral : Peripheral_T; Type_Name : String)
   is
      use Ada.Strings.Unbounded;
      Off          : Natural := 0;
      Reserved_Num : Natural := 0;
   begin
      Ada_Gen.Start_Record_Def (Type_Name);

      for Reg of Peripheral.Registers loop
         while Natural (Reg.Address_Offset) > Off loop
            Ada_Gen.Add_Record_Field
              ("Reserved_" & To_String (Reserved_Num),
               Target_Type (32),
               Off, 0, 31,
               "");
            Reserved_Num := Reserved_Num + 1;
            Off := Off + 4;
         end loop;
         Ada_Gen.Add_Record_Field
           (To_String (Reg.Name),
            Get_Ada_Type (Reg),
            Off, 0, Natural (Reg.Reg_Properties.Size) - 1,
            To_String (Reg.Description));
         Off := Off + Natural (Reg.Reg_Properties.Size / 8);
      end loop;

      Ada_Gen.End_Record (Register_List);
   end Dump_Periph_Type;

   ----------
   -- Dump --
   ----------

   procedure Dump (Peripheral : Peripheral_T;
                   Dev_Name   : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Ada_Gen.New_Spec (Dev_Name & "." & To_String (Peripheral.Name),
                        Unbounded.To_String (Peripheral.Description));

      if Length (Peripheral.Version) > 0 then
         Ada_Gen.Gen_Constant
           ("Version", "String", """" & To_String (Peripheral.Version) & """");
      end if;

      if not Interrupt_Vectors.Is_Empty (Peripheral.Interrupts) then
         Ada_Gen.Gen_Comment ("**************");
         Ada_Gen.Gen_Comment ("* Interrupts *");
         Ada_Gen.Gen_Comment ("**************");
         Ada_Gen.Gen_NL;
      end if;

      for Int of Peripheral.Interrupts loop
         Ada_Gen.Gen_Comment (To_String (Int.Description));
         Ada_Gen.Gen_Constant
           (To_String (Int.Name) & "_Int", "Natural",
            To_String (Integer (Int.Value)));
      end loop;

      if not Register_Vectors.Is_Empty (Peripheral.Registers) then
         Ada_Gen.Gen_Comment ("*************");
         Ada_Gen.Gen_Comment ("* Registers *");
         Ada_Gen.Gen_Comment ("*************");
         Ada_Gen.Gen_NL;
      end if;

      for Reg of Peripheral.Registers loop
         Dump (Reg);
      end loop;

      Dump_Periph_Type
        (Peripheral, To_String (Peripheral.Name) & "_Periph_T");

      Ada_Gen.Gen_Register
        (To_String (Peripheral.Name) & "_Periph",
         To_String (Peripheral.Name) & "_Periph_T",
         Peripheral.Base_Address);

      Ada_Gen.Close_All;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Group    : in out Peripheral_Vectors.Vector;
      Dev_Name : String)
   is
      use Ada.Strings.Unbounded;
      use type Register_Vectors.Vector;
      use type Ada.Containers.Count_Type;
      Interrupts : Interrupt_Vectors.Vector;
      Regs       : Register_Vectors.Vector;
      Sorted     : Peripheral_Vectors.Vector := Group;
      Partial_Similarity : Boolean := False;

   begin
      Ada_Gen.New_Spec
        (Dev_Name & "." & To_String (Sorted.First_Element.Group_Name),
         Unbounded.To_String (Sorted.First_Element.Description));

      for Periph of Sorted loop
         for Int of Periph.Interrupts loop
            if not Interrupts.Contains (Int) then
               Interrupts.Append (Int);
            end if;
         end loop;
      end loop;

      for J in Sorted.First_Index .. Sorted.Last_Index loop
         declare
            Periph : Peripheral_T := Sorted (J);
         begin
            Add_Regs (Regs, Periph.Registers);
            --  Registers may have been modified due to type merge between
            --  similar registers of different peripheral. We thus re-inject
            --  the potentially modified peripheral into the group
            Sorted.Replace_Element (J, Periph);
         end;
      end loop;

      if not Interrupts.Is_Empty then
         Ada_Gen.Gen_Comment ("**************");
         Ada_Gen.Gen_Comment ("* Interrupts *");
         Ada_Gen.Gen_Comment ("**************");
         Ada_Gen.Gen_NL;
      end if;

      Interrupt_Sort.Sort (Interrupts);

      for Int of Interrupts loop
         Ada_Gen.Gen_Comment (To_String (Int.Description));
         Ada_Gen.Gen_Constant
           (To_String (Int.Name) & "_Int", "Natural",
            To_String (Integer (Int.Value)));
      end loop;

      if not Regs.Is_Empty then
         Ada_Gen.Gen_Comment ("*************");
         Ada_Gen.Gen_Comment ("* Registers *");
         Ada_Gen.Gen_Comment ("*************");
         Ada_Gen.Gen_NL;
      end if;

      for Reg of Regs loop
         Dump (Reg);
      end loop;

      Ada_Gen.Gen_Comment ("***************");
      Ada_Gen.Gen_Comment ("* Peripherals *");
      Ada_Gen.Gen_Comment ("***************");
      Ada_Gen.Gen_NL;

      --  Determine if all peripherals of the group have the same layout
      Peripheral_Sort.Sort (Sorted);

      while not Sorted.Is_Empty loop
         declare
            First : Peripheral_T := Sorted.First_Element;
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
                 (First, To_String (First.Name) & "_Periph_T");

               Ada_Gen.Gen_Register
                 (To_String (First.Name) & "_Periph",
                  To_String (First.Name) & "_Periph_T",
                  First.Base_Address);

            elsif Sorted.Is_Empty and then not Partial_Similarity then
               Dump_Periph_Type
                 (First, To_String (First.Group_Name) & "_Periph_T");

               for Periph of List loop
                  Ada_Gen.Gen_Register
                    (To_String (Periph.Name) & "_Periph",
                     To_String (First.Group_Name) & "_Periph_T",
                     Periph.Base_Address);
               end loop;

            else
               Partial_Similarity := True;
               Dump_Periph_Type
                 (First, To_String (First.Name) & "_Periph_T");

               for Periph of List loop
                  Ada_Gen.Gen_Register
                    (To_String (Periph.Name) & "_Periph",
                     To_String (First.Name) & "_Periph_T",
                     Periph.Base_Address);
               end loop;
            end if;
         end;
      end loop;

      Ada_Gen.Close_All;
   end Dump;

end Peripheral_Descriptor;
