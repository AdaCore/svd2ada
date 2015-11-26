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

package body Field_Descriptor is

   ----------------
   -- Read_Field --
   ----------------

   function Read_Field
     (Elt : DOM.Core.Element;
      Vec : Field_Vectors.Vector)
      return Field_T
   is
      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret          : Field_T;
      Derived_From : constant String :=
                       Elements.Get_Attribute (Elt, "derivedFrom");

   begin
      if Derived_From /= "" then
         declare
            Found : Boolean := False;
         begin
            for F of Vec loop
               if Unbounded.To_String (F.Name) = Derived_From then
                  Ret := F;
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               raise Constraint_Error with
                 "field 'derivedFrom' is not known: " & Derived_From;
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

               elsif Tag = "description" then
                  Ret.Description := Get_Value (Child);

               elsif Tag = "bitOffset"
                 or else Tag = "lsb"
               then
                  Ret.LSB := Get_Value (Child);

               elsif Tag = "bitWidth" then
                  Ret.Size := Get_Value (Child);

               elsif Tag = "msb" then
                  Ret.Size := Get_Value (Child) - Ret.LSB + 1;

               elsif Tag = "bitRange" then
                  --  bitRange has the form: [XX:YY] where XX is the MSB,
                  --  and YY is the LSB
                  declare
                     Val : String renames Get_Value (Child);
                  begin
                     for K in Val'Range loop
                        if Val(K) = ':' then
                           Ret.LSB :=
                             Unsigned'Value (Val (K + 1 .. Val'Last - 1));
                           Ret.Size :=
                             Unsigned'Value (Val (2 .. K - 1)) - Ret.LSB + 1;
                        end if;
                     end loop;
                  end;

               elsif Tag = "access" then
                  Ret.Acc := Get_Value (Child);

               elsif Tag = "modifiedWriteValues" then
                  Ret.Mod_Write_Values := Get_Value (Child);

               elsif Tag = "enumeratedValues" then
                  declare
                     Enum : constant Enumerate_Descriptor.Enumerate_T :=
                              Enumerate_Descriptor.Read_Enumerate
                                (Child, Ret.Enums);
                  begin
                     Ret.Enums.Append (Enum);
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring field element " & Tag);
               end if;
            end;
         end if;
      end loop;

      return Ret;
   end Read_Field;

   ---------
   -- "=" --
   ---------

   function "=" (F1, F2 : Field_T) return Boolean
   is
      use Unbounded;
   begin
      return F1.LSB = F2.LSB
        and then F1.Size = F2.Size;
   end "=";

end Field_Descriptor;
