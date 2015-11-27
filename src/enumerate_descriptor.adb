------------------------------------------------------------------------------
--                              SVD Binding Generator                       --
--                                                                          --
--                         Copyright (C) 2015, AdaCore                      --
--                                                                          --
--  SVD2Ada is free software;  you can redistribute it and/or modify        --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This tool is distributed in the hope that it will be     --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  You should have received a copy of the GNU General Public License with  --
--  this program; see the files COPYING.  If not, see                       --
--  <http://www.gnu.org/licenses/>.                                         --
------------------------------------------------------------------------------

with Ada.Text_IO;

with DOM.Core;           use DOM.Core;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;

package body Enumerate_Descriptor is

   function Read_Value (Elt : DOM.Core.Element) return Enumerate_Value;
   --  Reads the enum value from the DOM element

   ----------------
   -- Read_Value --
   ----------------

   function Read_Value (Elt : DOM.Core.Element) return Enumerate_Value
   is
      List : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret  : Enumerate_Value;
   begin
      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant Element := Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "name" then
                  Ret.Name := Get_Value (Child);

               elsif Tag = "description" then
                  Ret.Descr := Get_Value (Child);

               elsif Tag = "value" then
                  Ret.Value := Get_Value (Child);
                  Ret.IsDefault := False;

               elsif Tag = "isDefault" then
                  Ret.Value := 0;
                  Ret.IsDefault := True;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring enumerate element " & Tag);
               end if;
            end;
         end if;
      end loop;

      return Ret;
   end Read_Value;


   --------------------
   -- Read_Enumerate --
   --------------------

   function Read_Enumerate
     (Elt    : DOM.Core.Element;
      Vector : Enumerate_Vectors.Vector)
      return Enumerate_T
   is
      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret          : Enumerate_T;
      Derived_From : constant String :=
                       Elements.Get_Attribute (Elt, "derivedFrom");
   begin
      if Derived_From /= "" then
         declare
            Found : Boolean := False;
         begin
            for Oth of Vector loop
               if Unbounded.To_String (Oth.Name) = Derived_From then
                  --  Copy the derived from enumerate type into the new value
                  Ret := Oth;
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               raise Constraint_Error with
                 "enumerate 'derivedFrom' is not known: " & Derived_From;
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

               elsif Tag = "usage" then
                  Ret.Usage := Get_Value (Child);

               elsif Tag = "enumeratedValue" then
                  Ret.Values.Append (Read_Value (Child));

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring enumerate element " & Tag);
               end if;
            end;
         end if;
      end loop;


      return Ret;
   end Read_Enumerate;

end Enumerate_Descriptor;
