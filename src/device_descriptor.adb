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

with DOM.Core;           use DOM.Core;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;

with Ada_Gen;            use Ada_Gen;

package body Device_Descriptor is

   -----------------
   -- Read_Device --
   -----------------

   function Read_Device (Elt : DOM.Core.Element) return Device_T is
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
                  Ret.Name := Get_Value (Child);

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

   procedure Dump (Device : Device_T)
   is
      use Ada.Strings.Unbounded;
      Peripherals : Peripheral_Vectors.Vector;
   begin
      Ada_Gen.New_Spec (Unbounded.To_String (Device.Name),
                        Unbounded.To_String (Device.Description));

      if Length (Device.Version) > 0 then
         Ada_Gen.Gen_Constant
           ("Version", "String", '"' & To_String (Device.Version) & '"');
      end if;

      Ada_Gen.Gen_Comment ("Base type:");
      Ada_Gen.Gen_Scalar_Type (Target_Type (Natural'(32)), 32);
      Ada_Gen.Gen_Scalar_Type (Target_Type (Natural'(16)), 16);
      Ada_Gen.Gen_Scalar_Type (Target_Type (Natural'(8)), 8);
      Ada_Gen.Gen_Scalar_Type (Target_Type (Natural'(1)), 1);
      Ada_Gen.Gen_NL;

      for J in 2 .. Natural (Device.Width) loop
         if J /= 8 and then J /= 16 and then J /= 32 then
            Ada_Gen.Gen_Scalar_Type (Target_Type (J), J);
         end if;
      end loop;

      Ada_Gen.Gen_Comment ("Base addresses:");

      for Periph of Device.Peripherals loop
         Ada_Gen.Gen_Constant
           (To_String (Periph.Name) & "_Base", "",
            To_Hex (Periph.Base_Address));
      end loop;

      Ada_Gen.Close_All;

      Peripherals := Device.Peripherals;

      while not Peripherals.Is_Empty loop
         declare
            P     : constant Peripheral_T := Peripherals.First_Element;
            Vec   : Peripheral_Vectors.Vector;
            Index : Natural;
         begin
            Peripherals.Delete_First;

            if Unbounded.Length (P.Group_Name) = 0 then
               Dump (P, Unbounded.To_String (Device.Name));
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

               Dump (Vec, Unbounded.To_String (Device.Name));
            end if;
         end;
      end loop;
   end Dump;

end Device_Descriptor;
