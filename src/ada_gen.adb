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

with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

package body Ada_Gen is

   Max_Width    : Natural := 79;

   type Block_Kind is
     (Package_Spec, Enum, Rec);

   type Block_Type is record
      Id   : Unbounded_String;
      Kind : Block_Kind;
   end record;

   package Opened_Blocks_Vectors is new Ada.Containers.Vectors
     (Positive, Block_Type);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   package Bool_Vectors is new Ada.Containers.Vectors
     (Positive, Boolean);


   type Unit_Type is record
      Id            : Unbounded_String;
      Descr         : Unbounded_String;
      With_List     : String_Vectors.Vector;
      Use_List      : Bool_Vectors.Vector;
      Public_Part   : String_Vectors.Vector;
      Private_Part  : String_Vectors.Vector;
      Opened_Blocks : Opened_Blocks_Vectors.Vector;
   end record;

   G_Unit : Unit_Type;
   G_Indent : Natural := 0;

   procedure Put (S : String; Public : Boolean := True);
   procedure Put_Indent (Public : Boolean := True);
   procedure New_Line (Public : Boolean := True);

   --------------------
   -- Set_Output_Dir --
   --------------------

   procedure Set_Output_Dir (Dir : String)
   is
      Full : constant String := GNAT.OS_Lib.Normalize_Pathname (Dir);
   begin
      if not GNAT.OS_Lib.Is_Directory (Full) then
         GNAT.Directory_Operations.Make_Dir (Full);
      end if;

      GNAT.Directory_Operations.Change_Dir (Full);
   end Set_Output_Dir;

   ---------
   -- Put --
   ---------

   procedure Put (S : String; Public : Boolean := True)
   is
   begin
      if Public and G_Unit.Public_Part.Is_Empty then
         New_Line (True);
      elsif not Public and G_Unit.Private_Part.Is_Empty then
         New_Line (False);
      end if;

      declare
         Current : constant String :=
                     (if Public then G_Unit.Public_Part.Last_Element
                      else G_Unit.Private_Part.Last_Element) & S;
      begin
         if Public then
            G_Unit.Public_Part.Replace_Element
              (G_Unit.Public_Part.Last_Index,
               Current);
         else
            G_Unit.Private_Part.Replace_Element
              (G_Unit.Private_Part.Last_Index,
               Current);
         end if;
      end;
   end Put;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Public : Boolean := True) is
   begin
      if Public then
         G_Unit.Public_Part.Append ("");
      else
         G_Unit.Private_Part.Append ("");
      end if;
   end New_Line;

   ----------------
   -- Put_Indent --
   ----------------

   procedure Put_Indent (Public : Boolean := True)
   is
      Str : String (1 .. (G_Indent * 3)) := (others => ' ');
   begin
      Put (Str, Public);
   end Put_Indent;

   --------------
   -- New_Unit --
   --------------

   procedure New_Spec
     (Name : String;
      Descr : String)
   is
   begin
      G_Unit := (others => <>);
      G_Unit.Id := To_Unbounded_String (Name);
      G_Unit.Descr := To_Unbounded_String (Name);
      G_Indent := 1;
   end New_Spec;

   ------------
   -- Gen_NL --
   ------------

   procedure Gen_NL is
   begin
      New_Line;
   end Gen_NL;

   --------------
   -- Gen_With --
   --------------

   procedure Gen_With (Pkg : String; Add_Use : Boolean := False)
   is
   begin
      if not G_Unit.With_List.Contains (Pkg) then
         G_Unit.With_List.Append (Pkg);
         G_Unit.Use_List.Append (Add_Use);
      end if;
   end Gen_With;

   --------------------
   -- Comment_String --
   --------------------

   function Strip_String (Comment : String) return String is
      Stripped   : Unbounded_String;
      Prev       : character := ' ';
      Current    : Character := ' ';
   begin
      for J in Comment'Range loop
         if Comment (J) = ASCII.CR then
            null;
         elsif Comment (J) = ASCII.LF then
            Current := ' ';
         else
            Current := Comment (J);
         end if;

         if Current /= ' ' or else Prev /= ' ' then
            Append (Stripped, Current);
         end if;

         Prev := Current;
      end loop;

      return To_String (Stripped);
   end Strip_String;

   -----------------
   -- Gen_Comment --
   -----------------

   function Gen_Comment (Comment : String;
                         Inline  : Boolean := False) return Boolean
   is
      First      : Natural := Comment'First;
      Last_Space : Integer := -1;
      Stripped   : String renames Strip_String (Comment);

   begin
      if Stripped'Length = 0 then
         return False;
      end if;

      for J in Stripped'Range loop
         if Stripped (J) = ' ' then
            Last_Space := J;
         end if;
         if not Inline
           and then J - First + G_Indent + 4 > Max_Width
           and then Last_Space > 0
         then
            Put_Indent;
            Put ("--  ");
            Put (Stripped (First .. Last_Space - 1));
            New_Line;
            First := Last_Space + 1;
         end if;
      end loop;

      if not Inline then
         Put_Indent;
      end if;

      Put ("--  ");
      Put (Stripped (First .. Stripped'Last));

      if not Inline then
         New_Line;
      end if;

      return True;
   end Gen_Comment;

   -----------------
   -- Gen_Comment --
   -----------------

   procedure Gen_Comment (Comment : String)
   is
      Dead : Boolean;
      pragma Unreferenced (Dead);
   begin
      Dead := Gen_Comment (Comment, False);
   end Gen_Comment;

   ---------------------
   -- Gen_Scalar_Type --
   ---------------------

   procedure Gen_Scalar_Type
     (Id : String; Size : Natural)
   is
   begin
      Put_Indent;
      if Size = 64 then
         Gen_With ("Interfaces", True);
         Put ("subtype " & Id & " is Interfaces.Unsigned_64;");
      elsif Size = 32 then
         Gen_With ("Interfaces", True);
         Put ("subtype " & Id & " is Interfaces.Unsigned_32;");
      elsif Size = 16 then
         Gen_With ("Interfaces", True);
         Put ("subtype " & Id & " is Interfaces.Unsigned_16;");
      elsif Size = 8 then
         Gen_With ("Interfaces", True);
         Put ("subtype " & Id & " is Interfaces.Unsigned_8;");
      else
         Put ("type " & Id & " is mod 2**" & To_String (Size) & ";");
         New_Line;
         Put_Indent;
         Put ("for " & Id & "'Size use " & To_String (Size) & ";");
      end if;

      New_Line;
   end Gen_Scalar_Type;

   ------------------
   -- Gen_Constant --
   ------------------

   procedure Gen_Constant (Id : String;
                           Typ : String;
                           Value : String)
   is
   begin
      Put_Indent;
      if Typ'Length > 0 then
         Put (Id & ": constant " & Typ & " := " & Value & ";");
      else
         Put (Id & ": constant := " & Value & ";");
      end if;
      New_Line;
      New_Line;
   end Gen_Constant;

   --------------------
   -- Gen_Addr_Block --
   --------------------

   procedure Gen_Addr_Block (Name : String;
                             Addr : Unsigned;
                             Size : Unsigned)
   is
   begin
      Put_Indent;
      Put (Name & ": array (Natural range 1 .." & Unsigned'Image (Size) &
             ") of Char;");
      New_Line;
      Put_Indent;
      Put
        ("for " & Name & "'Address use System.Storage_Elements.To_Address ("
         & To_Hex (Addr) & ");");
      New_Line;
      New_Line;
   end Gen_Addr_Block;

   ------------------
   -- Gen_Register --
   ------------------

   procedure Gen_Register (Name : String;
                           Typ  : String;
                           Addr : Unsigned)
   is
   begin
      Gen_With ("System");
      Put_Indent;
      Put (Name & ": aliased " & Typ & " with");
      New_Line;
      Put_Indent;
      Put ("  Volatile, Import,");
      New_Line;
      Put_Indent;
      Put ("  Address => System'To_Address (" & To_Hex (Addr) & ");");
      New_Line;
      New_Line;
   end Gen_Register;

   ----------------
   -- Start_Enum --
   ----------------

   G_Enum_First_Item : Boolean;

   procedure Start_Enum (Id : String)
   is
   begin
      Put_Indent;
      Put ("type " & Id & " is");
      New_Line;
      Put_Indent;
      Put ("  (");
      G_Indent := G_Indent + 1;
      G_Enum_First_Item := True;
   end Start_Enum;

   --------------
   -- Add_Enum --
   --------------

   procedure Add_Enum (Value   : String;
                       Comment : Unbounded_String)
   is
   begin
      if not G_Enum_First_Item then
         Put (",");
         New_Line;
         Put_Indent;
      else
         G_Enum_First_Item := False;
      end if;

      if (Gen_Comment (To_String (Comment), True)) then
         New_Line;
         Put_Indent;
      end if;

      Put (Value);
   end Add_Enum;

   --------------
   -- End_Enum --
   --------------

   procedure End_Enum is
   begin
      Put (");");
      New_Line;
      New_Line;
      G_Indent := G_Indent - 1;
   end End_Enum;

   --------------------------
   -- Start_Constant_Array --
   --------------------------

   procedure Start_Constant_Array (Id      : String;
                                   Index_T : String;
                                   Elt_T   : String)
   is
   begin
      Put_Indent;
      Put (Id & ": constant Array (" & Index_T & ") of " & Elt_T & " :=");
      New_Line;
      Put_Indent;
      Put ("  (");
      G_Indent := G_Indent + 1;
      G_Enum_First_Item := True;
   end Start_Constant_Array;

   ---------------
   -- Add_Array --
   ---------------

   procedure Add_Array (Index   : String;
                        Value   : String)
   is
   begin
      if not G_Enum_First_Item then
         Put (",");
         New_Line;
         Put_Indent;
      else
         G_Enum_First_Item := False;
      end if;

      Put (Index & " => " & Value);
   end Add_Array;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array
   is
   begin
      End_Enum;
   end End_Array;

   ----------------------
   -- Start_Record_Def --
   ----------------------

   G_Rec_Field_Id      : String_Vectors.Vector;
   G_Rec_Field_Type    : String_Vectors.Vector;
   G_Rec_Field_Desc    : String_Vectors.Vector;
   G_Rec_Repr_Clause   : String_Vectors.Vector;
   G_Union_Types       : String_Vectors.Vector;

   procedure Start_Record_Def (Id : String)
   is
   begin
      G_Unit.Opened_Blocks.Append
        ((Id => To_Unbounded_String (Id),
          Kind => Rec));
      G_Rec_Field_Id.Clear;
      G_Rec_Field_Type.Clear;
      G_Rec_Field_Desc.Clear;
   end Start_Record_Def;

   ----------------------
   -- Add_Record_Field --
   ----------------------

   procedure Add_Record_Field
     (Id          : String;
      Typ         : String;
      Offset      : Natural;
      LSB         : Natural;
      MSB         : Natural;
      Descr       : String;
      Has_Default : Boolean := False;
      Default     : Unsigned_32 := 0)
   is
      Low : constant String := Ada.Characters.Handling.To_Lower (Id);
   begin
      --  ??? TODO: Check for all keywords
      if Low = "or"
        or else Low = "abort"
        or else Low = "delay"
      then
         Add_Record_Field (Id & "_r", Typ, Offset, LSB, MSB, Descr);
         return;
      end if;

      G_Rec_Field_Id.Append (Id);
      if not Has_Default then
         G_Rec_Field_Type.Append (Typ & ";");
      else
         G_Rec_Field_Type.Append (Typ & " :=" & Default'Img & ";");
      end if;
      G_Rec_Field_Desc.Append (Descr);
      G_Rec_Repr_Clause.Append
        ("at " & To_String (Offset) & " range " &
           To_String (LSB) & " .. " & To_String (MSB));
   end Add_Record_Field;

   ----------------------------
   -- Add_Record_Union_Field --
   ----------------------------

   procedure Add_Record_Union_Field
     (Id       : String;
      Typ      : String;
      Elts     : Natural;
      Elts_Typ : String;
      Offset   : Natural;
      LSB      : Natural;
      MSB      : Natural;
      Descr    : String)
   is
      Union_T : constant String := Id & "_Union_T";
   begin
      if G_Union_Types.Contains (Union_T) then
         --  ??? We cross fingers here that having the same name, they have
         --  the same type, but an already defined union type for field
         --  representation is there.
         Add_Record_Field (Id, Union_T, Offset, LSB, MSB, Descr);
         return;
      end if;

      G_Union_Types.Append (Union_T);
      Put_Indent;
      Put ("type " & Id & "_Arr is array (0 .. " &
             To_String (Elts - 1) & ") of " & Elts_Typ);
      New_Line; Put_Indent;
      Put ("  with Component_Size => " &
             To_String ((MSB - LSB + 1) / Elts) & ", Size => " &
             To_String ((MSB - LSB + 1)) & ";");
      New_Line;
      New_Line;
      Put_Indent;
      Put ("type " & Union_T & " (As_Array : Boolean := False) is record");
      New_Line;
      G_Indent := G_Indent + 1;
      Put_Indent; Put ("case As_Array is"); New_Line;
      Put_Indent; Put ("when True =>"); New_Line;
      G_Indent := G_Indent + 1;
      Put_Indent;
      Put ("Arr : " & Id & "_Arr;");
      New_Line;
      G_Indent := G_Indent - 1;
      Put_Indent; Put ("when False =>"); New_Line;
      G_Indent := G_Indent + 1;
      Put_Indent;
      Put ("Val : " & Typ & ";");
      New_Line;
      G_Indent := G_Indent - 1;
      Put_Indent; Put ("end case;"); New_Line;
      G_Indent := G_Indent - 1;
      Put_Indent; Put ("end record with Unchecked_Union, Import, Size => " &
                         To_String ((MSB - LSB + 1)) & ";");
      New_Line;
      Put_Indent;
      Put ("for " & Union_T & " use record");
      New_Line; G_Indent := G_Indent + 1; Put_Indent;
      Put ("Arr at 0 range 0 .. " & To_String ((MSB - LSB)) & "; ");
      New_Line; Put_Indent;
      Put ("Val at 0 range 0 .. " & To_String ((MSB - LSB)) & ";");
      New_Line; G_Indent := G_Indent - 1; Put_Indent;
      Put ("end record;");
      New_Line;
      New_Line;
      Add_Record_Field (Id, Union_T, Offset, LSB, MSB, Descr);
   end Add_Record_Union_Field;

   ----------------
   -- End_Record --
   ----------------

   procedure End_Record (Kind : Record_Type)
   is
      Id : constant Unbounded_String :=
             G_Unit.Opened_Blocks.Last_Element.Id;
      Max : Natural := 0;
      Dead : Boolean;
      pragma Unreferenced (Dead);

      procedure Put_Item (Idx : Natural);
      procedure Put_Clause (Idx : Natural);

      --------------
      -- Put_Item --
      --------------

      procedure Put_Item (Idx : Natural) is
         Id    : String renames G_Rec_Field_Id (Idx);
         Typ   : String renames G_Rec_Field_Type (Idx);
         Descr : String renames G_Rec_Field_Desc (Idx);
      begin

         Put_Indent;
         Put (Id);
         if Max > Id'Length then
            Put ((1 .. (Max - Id'Length) => ' '));
         end if;
         Put (" : " & Typ);
         if Descr /= "" then
            Put (" ");
            Dead := Gen_Comment (Descr, True);
         end if;
         New_Line;
      end Put_Item;

      ----------------
      -- Put_Clause --
      ----------------

      procedure Put_Clause (Idx : Natural)
      is
         Id    : String renames G_Rec_Field_Id (Idx);
         Typ   : String renames G_Rec_Field_Type (Idx);
         Descr : String renames G_Rec_Field_Desc (Idx);
         Repr  : String renames G_Rec_Repr_Clause (Idx);
      begin

         Put_Indent;
         Put (Id & " ");

         if Max > Id'Length then
            Put ((1 .. (Max - Id'Length) => ' '));
         end if;

         Put (Repr & ";");
         New_Line;
      end Put_Clause;

   begin
      for Name of G_Rec_Field_Id loop
         if Name'Length > Max then
            Max := Name'Length;
         end if;
      end loop;

      G_Unit.Opened_Blocks.Delete_Last;

      Put_Indent;
      Put ("type " & To_String (Id) & " is record");
      New_Line;
      G_Indent := G_Indent + 1;

      case Kind is
         when Register_List =>
            for J in String_Vectors.First_Index (G_Rec_Field_Id) ..
              String_Vectors.Last_Index (G_Rec_Field_Id)
            loop
               Put_Item (J);
            end loop;
         when Register =>
            for J in String_Vectors.First_Index (G_Rec_Field_Id) ..
              String_Vectors.Last_Index (G_Rec_Field_Id)
            loop
               Put_Item (J);
            end loop;
      end case;

      G_Indent := G_Indent - 1;
      Put_Indent;
      Put ("end record");
      New_Line;
      Put_Indent;

      case Kind is
         when Register =>
            Gen_With ("System");
            Put ("  with Volatile_Full_Access, Size => 32,");
            Put (" Bit_Order => System.Low_Order_First");
         when Register_List =>
            Put ("  with Volatile");
      end case;

      Put (";");
      New_Line;
      New_Line;

      Put_Indent;
      Put ("for " & To_String (Id) & " use record");
      New_Line;
      G_Indent := G_Indent + 1;

      case Kind is
         when Register_List =>
            for J in String_Vectors.First_Index (G_Rec_Field_Id) ..
              String_Vectors.Last_Index (G_Rec_Field_Id)
            loop
               Put_Clause (J);
            end loop;
         when Register =>
            for J in String_Vectors.First_Index (G_Rec_Field_Id) ..
              String_Vectors.Last_Index (G_Rec_Field_Id)
            loop
               Put_Clause (J);
            end loop;
      end case;

      G_Indent := G_Indent - 1;
      Put_Indent;
      Put ("end record;");
      New_Line;
      New_Line;

      G_Rec_Field_Id.Clear;
      G_Rec_Field_Type.Clear;
      G_Rec_Field_Desc.Clear;
      G_Rec_Repr_Clause.Clear;
   end End_Record;

   ---------------
   -- Close_All --
   ---------------

   procedure Close_All is
      Ada_Name : String := To_String (G_Unit.Id);
      F        : Ada.Text_IO.File_Type;
   begin
      for J in Ada_Name'Range loop
         Ada_Name (J) :=
           Ada.Characters.Handling.To_Lower (Ada_Name (J));

         if Ada_Name (J) = '.' then
            Ada_Name (J) := '-';
         end if;
      end loop;

      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Ada_Name & ".ads");

      Ada.Text_IO.Put_Line
        (F, "pragma Restrictions (No_Elaboration_Code);");
      Ada.Text_IO.New_Line (F);
      for J in G_Unit.With_List.First_Index .. G_Unit.With_List.Last_Index loop
         declare
            W : constant String := G_Unit.With_List (J);
            U : constant Boolean := G_Unit.Use_List (J);
         begin
            if U then
               Ada.Text_IO.Put_Line (F, "with " & W & "; use " & W & ";");
            else
               Ada.Text_IO.Put_Line (F, "with " & W & ";");
            end if;
         end;
      end loop;
      Ada.Text_IO.New_Line (F);

      Ada.Text_IO.Put_Line
        (F, "--  " & Strip_String (To_String (G_Unit.Descr)));
      Ada.Text_IO.Put_Line
        (F, "package " & To_String (G_Unit.Id) & " is");
      Ada.Text_IO.Put_Line
        (F, "   pragma Preelaborate;");
      Ada.Text_IO.New_Line (F);

      for L of G_Unit.Public_Part loop
         Ada.Text_IO.Put_Line (F, L);
      end loop;

      if not G_Unit.Private_Part.Is_Empty then
         Ada.Text_IO.Put_Line (F, "private");
         Ada.Text_IO.New_Line (F);

         for L of G_Unit.Private_Part loop
            Ada.Text_IO.Put_Line (F, L);
         end loop;
      end if;

      Ada.Text_IO.Put_Line (F, "end " & To_String (G_Unit.Id) & ";");

      G_Union_Types.Clear;

      Ada.Text_IO.Close (F);
   end Close_All;

end Ada_Gen;
