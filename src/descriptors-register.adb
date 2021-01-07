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
with SVD2Ada_Utils;

package body Descriptors.Register is

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (Elt            : DOM.Core.Element;
      Prepend        : Unbounded.Unbounded_String;
      Append         : Unbounded.Unbounded_String;
      Reg_Properties : Register_Properties_T;
      Reg_Db         : Register_Db'Class)
      return Register_Access
   is
      use DOM.Core;
      use type Unbounded.Unbounded_String;

      List         : constant Node_List := Nodes.Child_Nodes (Elt);
      Result       : Register_T;
      Derived_From : constant String :=
                       Elements.Get_Attribute (Elt, "derivedFrom");

      function Compute_Name return Unbounded.Unbounded_String;

      ------------------
      -- Compute_Name --
      ------------------

      function Compute_Name return Unbounded.Unbounded_String
      is
         Name   : constant String := Unbounded.To_String (Result.Xml_Id);
         Result : String (Name'Range);
         Idx    : Natural;
         Skip   : Boolean := False;
      begin
         Idx := Result'First - 1;

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
               Result (Idx) := Name (J);
            end if;
         end loop;

         if Idx in Result'Range and then Result (Idx) = '_' then
            Idx := Idx - 1;
         end if;

         return Unbounded.To_Unbounded_String (Result (Result'First .. Idx));
      end Compute_Name;

   begin
      Result.Reg_Properties := Reg_Properties;

      if Derived_From /= "" then
         declare
            Oth : constant Register_Access :=
                    Reg_Db.Get_Register (Derived_From);
         begin
            if Oth /= null then
               Result := Oth.all;
               Result.Name := Unbounded.Null_Unbounded_String;
            else
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
                  Result.Xml_Id := Get_Value (Child);
                  Result.Name := Compute_Name;
                  Result.Type_Name := Prepend & Result.Name & Append;

               elsif Tag = "displayName" then
                  Result.Display_Name := Get_Value (Child);

               elsif Tag = "description" then
                  Result.Description := Get_Value (Child);

               elsif Tag = "alternateGroup" then
                  Result.Alternate_Group := Get_Value (Child);

               elsif Tag = "alternateRegister" then
                  Result.Alternate_Reg := Get_Value (Child);

               elsif Tag = "addressOffset" then
                  Result.Address_Offset := Get_Value (Child);

               elsif Is_Register_Property (Tag) then
                  Read_Register_Property (Child, Result.Reg_Properties);

               elsif Tag = "modifiedWriteValue" then
                  Result.Mod_Write_Values := Get_Value (Child);

               elsif Tag = "readAction" then
                  Result.Read_Action := Get_Value (Child);

               elsif Tag = "fields" then
                  declare
                     Child_List : constant Node_List :=
                                    Nodes.Child_Nodes (Child);
                     Field      : Field_T;
                  begin
                     for K in 0 .. Nodes.Length (Child_List) - 1 loop
                        if Nodes.Node_Type (Nodes.Item (Child_List, K)) = Element_Node
                        then
                           Field :=
                             Read_Field
                               (Element (Nodes.Item (Child_List, K)),
                                Result.Fields,
                                Result.Reg_Properties.Reg_Access,
                                Result.Read_Action);
                           if not Result.Fields.Contains (Field) then
                              Result.Fields.Append (Field);
                           end if;
                        end if;
                     end loop;
                  end;

               elsif Tag = "dim" then
                  Result.Dim := Get_Value (Child);

                  if Unbounded.Length (Result.Xml_Id) > 0 then
                     Result.Name := Compute_Name;
                  end if;

               elsif Tag = "dimIncrement" then
                  Result.Dim_Increment := Get_Value (Child);

               elsif Tag = "dimIndex" then
                  Result.Dim_Index := Get_Value (Child);

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring register element " & Tag & " at " & Full_Name (Child));
               end if;
            end;
         end if;
      end loop;

      return new Register_T'(Result);
   end Read_Register;

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

   -----------------
   -- Common_Type --
   -----------------

   function Similar_Type
     (R1, R2 : Register_Access)
      return Unbounded.Unbounded_String
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

   function Get_Ada_Type (Reg : Register_Access) return Ada_Gen.Ada_Type'Class
   is
      use Unbounded;
   begin
      if Reg.Type_Holder /= null then
         return Get_Ada_Type (Reg.Type_Holder);

      elsif not Reg.Ada_Type.Is_Empty then
         return -Reg.Ada_Type;

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
         Reg.Ada_Type := -Ada_Gen.Target_Type (Reg.Reg_Properties.Size);

         if Reg.Dim > 1 then
            --  Just generate a comment to document the array that's going
            --  to be generated
            Add (Spec,
                 New_Comment (To_String (Reg.Description), Strip => True));
         end if;

      else
         declare
            Rec : Ada_Type_Record;

         begin
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

            if not SVD2Ada_Utils.No_VFA_On_Reg_Types then
               Add_Aspect (Rec, "Volatile_Full_Access");
            end if;
            Add_Object_Size_Aspect (Rec, Reg.Reg_Properties.Size);
            Add_Bit_Order_Aspect (Rec, System.Low_Order_First);

            declare
               Res : Ada_Type'Class := Simplify (Rec, Spec);
            begin
               Add (Spec, Res);
               Reg.Ada_Type := -Res;
            end;
         end;
      end if;

      --  Create an array of registers if Dim > 1 and the registers are
      --  contiguous.
      --  Do not do this when we apply the Virtual_Full_Access to clusters or
      --  peripheral fields instead of the base register type, as we can't
      --  specify that array components are Virtual_Full_Access.
      if Reg.Dim > 1
        and then Reg.Dim_Increment = Reg.Reg_Properties.Size / 8
        and then not SVD2Ada_Utils.No_VFA_On_Reg_Types
      then
         declare
            Array_T : Ada_Type_Array :=
                        New_Type_Array
                          (Id           => To_String (Reg.Type_Name) & "_Registers",
                           Index_Type   => "",
                           Index_First  => 0,
                           Index_Last   => Reg.Dim - 1,
                           Element_Type => Get_Ada_Type (Reg),
                           Comment      => To_String (Reg.Description));
         begin
            Add (Spec, Array_T);
            Reg.Ada_Type := -Array_T;
         end;
      end if;
   end Dump;

end Descriptors.Register;
