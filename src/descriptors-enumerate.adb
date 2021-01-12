------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2020, AdaCore                      --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces;

with DOM.Core;           use DOM.Core;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;

package body Descriptors.Enumerate is

   procedure Read_Value
     (Elt        : DOM.Core.Element;
      Write_Only : Boolean;
      Values     : in out Enumerate_Values_Vectors.Vector);
   --  Reads the enum value from the DOM element

   ----------------
   -- Read_Value --
   ----------------

   procedure Read_Value
     (Elt        : DOM.Core.Element;
      Write_Only : Boolean;
      Values     : in out Enumerate_Values_Vectors.Vector)
   is
      procedure Read_Value
        (Val  : String;
         Name : String);

      List   : constant Node_List := Nodes.Child_Nodes (Elt);
      Result : Enumerate_Value;
      Has_X  : Boolean := False;

      procedure Read_Value
        (Val  : String;
         Name : String)
      is
      begin
         for J in Val'Range loop
            if Val (J) = 'x' then
               Read_Value
                 (Val (Val'First .. J - 1) & "0" & Val (J + 1 .. Val'Last),
                  Name & "0");
               Read_Value
                 (Val (Val'First .. J - 1) & "1" & Val (J + 1 .. Val'Last),
                  Name & "1");
               return;
            end if;
         end loop;

         Result.Name := To_Unbounded_String (Name);
         Result.Value := Interfaces.Unsigned_64'Value ("2" & Val & "#");
         Values.Append (Result);
      end Read_Value;

   begin
      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant DOM.Core.Element := DOM.Core.Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "name" then
                  Result.Name := Get_Value (Child);

               elsif Tag = "description" then
                  Result.Descr := Get_Value (Child);

               elsif Tag = "value" then
                  declare
                     S : String := Get_Value (Child);
                  begin
                     if S'Length >= 2
                       and then S (S'First) = '#'
                     then
                        Has_X := False;

                        for J in S'Range loop
                           if S (J) = 'x' or else S (J) = 'X' then
                              if Write_Only then
                                 --  Easy case: we just fill with '0'
                                 S (J) := '0';
                              else
                                 Has_X := True;
                              end if;
                           end if;
                        end loop;

                        if not Has_X then
                           Result.Value := Interfaces.Unsigned_64'Value ("2#" & S (S'First + 1 .. S'Last) & "#");
                        else
                           Read_Value (S, To_String (Result.Name));
                        end if;
                     else

                        Result.Value := Get_Value (Child);
                     end if;
                  end;

                  Result.IsDefault := False;

               elsif Tag = "isDefault" then
                  Result.Value := 0;
                  Result.IsDefault := True;

               else
                  Ada.Text_IO.Put_Line ("*** WARNING: ignoring enumerate element " & Tag & " at " & Full_Name (Child));
               end if;
            end;
         end if;
      end loop;

      if not Has_X then
         Values.Append (Result);
      end if;
   end Read_Value;

   --------------------
   -- Read_Enumerate --
   --------------------

   function Read_Enumerate
     (Elt        : DOM.Core.Element;
      Vector     : Enumerate_Vectors.Vector;
      Write_Only : Boolean)
      return Enumerate_T
   is
      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Result       : Enumerate_T;
      Derived_From : constant String := Elements.Get_Attribute (Elt, "derivedFrom");
   begin
      if Derived_From /= "" then
         declare
            Found : Boolean := False;
         begin
            for Oth of Vector loop
               if Unbounded.To_String (Oth.Name) = Derived_From then
                  --  Copy the derived from enumerate type into the new value
                  Result := Oth;
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
               Child : constant DOM.Core.Element := DOM.Core.Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "name" then
                  Result.Name := Get_Value (Child);

               elsif Tag = "usage" then
                  Result.Usage := Get_Value (Child);

               elsif Tag = "enumeratedValue" then
                  Read_Value (Child, Write_Only, Result.Values);

               else
                  Ada.Text_IO.Put_Line ("*** WARNING: ignoring enumerate element " & Tag & " at " & Full_Name (Child));
               end if;
            end;
         end if;
      end loop;

      return Result;
   end Read_Enumerate;

end Descriptors.Enumerate;
