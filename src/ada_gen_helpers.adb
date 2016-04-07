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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

with DOM.Core;              use DOM.Core;
with DOM.Core.Elements;     use DOM.Core.Elements;
with DOM.Core.Nodes;

with Base_Types;            use Base_Types;

package body Ada_Gen_Helpers is

   package String_Vectors is new Ada.Containers.Indefinite_vectors
     (Index_Type   => Positive,
      Element_Type => String,
      "="          => "=");

   type Discriminent is record
      Id     : Ada.Strings.Unbounded.Unbounded_String;
      Typ    : Ada.Strings.Unbounded.Unbounded_String;
      Values : String_Vectors.Vector;
   end record;

   package When_Statements is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String_Vectors.Vector,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => String_Vectors."=");

   type Peripheral is record
      Disc : Discriminent;
      Regs : When_Statements.Map;
   end record;

   package Peripheral_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Peripheral_Helper,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   G_Peripherals : Peripheral_Maps.Map;

   function Read_Discriminent (Elt : DOM.Core.Element) return Discriminent;
   procedure Read_Peripheral (Elt : DOM.Core.Element);

   -----------------------
   -- Read_Discriminent --
   -----------------------

   function Read_Discriminent (Elt : DOM.Core.Element) return Discriminent
   is
      List : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret  : Discriminent;
   begin
      Ret.Id := Ada.Strings.Unbounded.To_Unbounded_String
        (Elements.Get_Attribute (Elt, "id"));
      Ret.Typ := Ada.Strings.Unbounded.To_Unbounded_String
        (Elements.Get_Attribute (Elt, "type"));

      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant Element := Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "value" then
                  Ret.Values.Append (String'(Get_Value (Child)));
               else
                  raise Constraint_Error with
                    "Unexpected element " & Tag &
                    " while parsing svd2ada file";
               end if;
            end;
         end if;
      end loop;

      return Ret;
   end Read_Discriminent;

   ---------------------
   -- Read_Peripheral --
   ---------------------

   procedure Read_Peripheral (Elt : DOM.Core.Element)
   is
      List   : constant Node_List := Nodes.Child_Nodes (Elt);
      Name   : constant String := Elements.Get_Attribute (Elt, "name");
      Periph : Peripheral;

   begin
      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant Element := Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "discriminent" then
                  Periph.Disc := Read_Discriminent (Child);
                  for Val of Periph.Disc.Values loop
                     Periph.Regs.Insert (Val, String_Vectors.Empty_Vector);
                  end loop;

               elsif Tag = "register" then
                  declare
                     Disc_Value : constant String :=
                                    Elements.Get_Attribute (Child, "when");
                     Reg_Name   : constant String :=
                                    Elements.Get_Attribute (Child, "name");
                  begin
                     Periph.Regs (Disc_Value).Append (Reg_Name);
                  end;
               else
                  raise Constraint_Error with
                    "Unexpected element " & Tag &
                    " while parsing svd2ada file";
               end if;
            end;
         end if;
      end loop;

      G_Peripherals.Insert (Name, new Peripheral'(Periph));
   end Read_Peripheral;

   ----------
   -- Read --
   ----------

   procedure Read (Elt : DOM.Core.Element)
   is
      List : constant Node_List := Nodes.Child_Nodes (Elt);
   begin
      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant Element := Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "peripheral" then
                  Read_Peripheral (Child);
               else
                  raise Constraint_Error with
                    "Unexpected element " & Tag &
                    " while parsing svd2ada file";
               end if;
            end;
         end if;
      end loop;
   end Read;

   ---------------------------
   -- Get_Peripheral_Helper --
   ---------------------------

   function Get_Peripheral_Helper (Name : String) return Peripheral_Helper
   is
   begin
      if G_Peripherals.Contains (Name) then
         return G_Peripherals (Name);
      else
         return null;
      end if;
   end Get_Peripheral_Helper;

   ---------------------------
   -- Get_Discriminent_Type --
   ---------------------------

   function Get_Discriminent_Type
     (Helper : Peripheral_Helper) return Ada_Type_Enum
   is
      Ret : Ada_Type_Enum;
      Dead : Ada_Enum_Value;
      use Ada.Strings.Unbounded;
   begin
      Ret := New_Type_Enum
        (To_String (Helper.Disc.Typ),
         Size => 0);

      for Val of Helper.Disc.Values loop
         Dead := Add_Enum_Id (Ret, Val);
      end loop;

      return Ret;
   end Get_Discriminent_Type;

   ---------------------------
   -- Get_Discriminent_Name --
   ---------------------------

   function Get_Discriminent_Name
     (Helper : Peripheral_Helper) return String
   is
      use Ada.Strings.Unbounded;
   begin
      return To_String (Helper.Disc.Id);
   end Get_Discriminent_Name;

   ----------------------------
   -- Get_Discriminent_Value --
   ----------------------------

   function Get_Discriminent_Value
     (Helper   : Peripheral_Helper;
      Reg_Name : String) return String
   is
   begin
      for Val of Helper.Disc.Values loop
         for Reg of Helper.Regs (Val) loop
            if Reg = Reg_Name then
               return Val;
            end if;
         end loop;
      end loop;

      raise Constraint_Error with
        "Missing aliased register in SVD2ADA helper: " & Reg_Name;
   end Get_Discriminent_Value;

end Ada_Gen_Helpers;
