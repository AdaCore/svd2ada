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

with Interfaces;            use Interfaces;
with Ada.Text_IO;

with DOM.Core;              use DOM.Core;
with DOM.Core.Elements;     use DOM.Core.Elements;
with DOM.Core.Nodes;

with Ada_Gen;               use Ada_Gen;

package body Device_Descriptor is

   package Interrupt_Sort is new Interrupt_Vectors.Generic_Sorting
     (Base_Types."<");

   -----------------
   -- Read_Device --
   -----------------

   function Read_Device
     (Elt      : DOM.Core.Element;
      Pkg_Name : String) return Device_T
   is
      List : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret  : Device_T;

   begin
      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant Element := Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "name" then
                  if Pkg_Name'Length > 0 then
                     Ret.Name :=
                       Ada.Strings.Unbounded.To_Unbounded_String (Pkg_Name);
                  else
                     Ret.Name := Get_Value (Child);
                  end if;
                  Base_Types.Base_Package := Ret.Name;

               elsif Tag = "version" then
                  Ret.Version := Get_Value (Child);

               elsif Tag = "description" then
                  Ret.Description := Get_Value (Child);

               elsif Tag = "addressUnitBits" then
                  Ret.Address_Unit_Bits := Get_Value (Child);

               elsif Tag = "width" then
                  Ret.Width := Get_Value (Child);

               elsif Register_Properties.Is_Register_Property (Tag) then
                  Register_Properties.Read_Register_Property
                    (Child, Ret.Reg_Properties);

               elsif Tag = "peripherals" then
                  declare
                     Child_List : constant Node_List :=
                                    Nodes.Child_Nodes (Child);
                     Peripheral : Peripheral_T;
                  begin
                     for K in 0 .. Nodes.Length (Child_List) - 1 loop
                        if Nodes.Node_Type (Nodes.Item (Child_List, K)) =
                          Element_Node
                        then
                           Peripheral :=
                             Read_Peripheral
                               (Element (Nodes.Item (Child_List, K)),
                                Ret.Reg_Properties,
                                Ret.Peripherals);
                           Ret.Peripherals.Append (Peripheral);
                        end if;
                     end loop;
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring device element " & Tag);
               end if;
            end;
         end if;
      end loop;

      return Ret;
   end Read_Device;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Device     : Device_T;
      Output_Dir : String)
   is
      use Ada.Strings.Unbounded;
      Peripherals : Peripheral_Vectors.Vector;
      Interrupts  : Interrupt_Vectors.Vector;
      Spec        : Ada_Gen.Ada_Spec :=
                      New_Spec (To_String (Device.Name),
                                To_String (Device.Description),
                                True);
   begin
      if Length (Device.Version) > 0 then
         Add (Spec,
              New_Constant_Value
                (Id    => "Version",
                 Typ   => "String",
                 Value => '"' & To_String (Device.Version) & '"'));
      end if;

      Add (Spec, New_Comment_Box ("Base type"));
      Add_No_Check
        (Spec, New_Type_Scalar (Target_Type (Natural'(32), False), 32));
      Add_No_Check
        (Spec, New_Type_Scalar (Target_Type (Natural'(16), False), 16));
      Add_No_Check
        (Spec, New_Type_Scalar (Target_Type (Natural'(8), False), 8));
      Add_No_Check
        (Spec, New_Type_Scalar (Target_Type (Natural'(1), False), 1));

      for J in 2 .. Device.Width loop
         if J /= 8 and then J /= 16 and then J /= 32 then
            Add_No_Check
              (Spec, New_Type_Scalar (Target_Type (J, False), J));
         end if;
      end loop;

      Add (Spec, New_Comment_Box ("Base addresses"));
      Add (Spec, New_With_Clause ("System", True));

      for Periph of Device.Peripherals loop
         Add (Spec,
              New_Constant_Value
                (Id    => To_String (Periph.Name) & "_Base",
                 Typ   => "System.Address",
                 Value => "System'To_Address (" &
                   To_Hex (Periph.Base_Address) & ")"));
      end loop;

      Ada_Gen.Write_Spec (Spec, Output_Dir);

      Spec := New_Child_Spec ("Interrupts",
                              To_String (Device.Name),
                              "Definition of the device's interrupts",
                              False);

      Add (Spec, New_Comment_Box ("Interrupts"));
      Add (Spec, New_With_Clause ("Ada.Interrupts", True));

      for Periph of Device.Peripherals loop
         for Int of Periph.Interrupts loop
            if not Interrupts.Contains (Int) then
               Interrupts.Append (Int);
            end if;
         end loop;
      end loop;

      Interrupt_Sort.Sort (Interrupts);

      for Int of Interrupts loop
         --  GNAT re-numbers the interrupt to add the Sys_Tick interrupt
         --  which is a core interrupt. So we need to take this re-numbering
         --  here by adding 2 to the constants extracted from the SVD
         Add (Spec,
              New_Constant_Value
                (Id    => To_String (Int.Name) & "_Interrupt",
                 Typ   => "Interrupt_ID",
                 Value => To_String (Integer (Int.Value) + 2)));
      end loop;

      Write_Spec (Spec, Output_Dir);

      Peripherals := Device.Peripherals;

      while not Peripherals.Is_Empty loop
         declare
            P     : constant Peripheral_T := Peripherals.First_Element;
            Vec   : Peripheral_Vectors.Vector;
            Index : Natural;
         begin
            Peripherals.Delete_First;

            if Unbounded.Length (P.Group_Name) = 0 then
               Dump (P, Unbounded.To_String (Device.Name), Output_Dir);
            else
               Vec.Append (P);
               Index := Peripherals.First_Index;

               while Index <= Peripherals.Last_Index loop
                  if Peripherals (Index).Group_Name = P.Group_Name then
                     Vec.Append (Peripherals (Index));
                     Peripherals.Delete (Index);
                  else
                     Index := Index + 1;
                  end if;
               end loop;

               Dump (Vec, Unbounded.To_String (Device.Name), Output_Dir);
            end if;
         end;
      end loop;
   end Dump;

end Device_Descriptor;
