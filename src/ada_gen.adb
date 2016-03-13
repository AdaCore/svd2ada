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

with Interfaces; use Interfaces;

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Tags;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with SVD2Ada_Utils;

package body Ada_Gen is

   Max_Width    : constant Natural := 79;
   G_Input_File : Unbounded_String;
   G_Empty_Line : Boolean := False;
   G_Withed_All : Unbounded_String := Null_Unbounded_String;

   function Is_Parent
     (Spec        : Ada_Spec;
      With_Clause : Ada_With_Clause) return Boolean;

   function Protect_Keywords
     (S : String) return Unbounded_String;

   ----------------------
   -- Protect_Keywords --
   ----------------------

   function Protect_Keywords
     (S : String) return Unbounded_String
   is
      Low_Id : constant String := Ada.Characters.Handling.To_Lower (S);

   begin
      if Low_Id = "abort"
        or else Low_Id = "abs"
        or else Low_Id = "abstract"
        or else Low_Id = "accept"
        or else Low_Id = "access"
        or else Low_Id = "aliased"
        or else Low_Id = "all"
        or else Low_Id = "and"
        or else Low_Id = "array"
        or else Low_Id = "at"
        or else Low_Id = "begin"
        or else Low_Id = "body"
        or else Low_Id = "case"
        or else Low_Id = "constant"
        or else Low_Id = "declare"
        or else Low_Id = "delay"
        or else Low_Id = "delta"
        or else Low_Id = "digits"
        or else Low_Id = "do"
        or else Low_Id = "else"
        or else Low_Id = "elsif"
        or else Low_Id = "end"
        or else Low_Id = "entry"
        or else Low_Id = "exception"
        or else Low_Id = "exit"
        or else Low_Id = "for"
        or else Low_Id = "function"
        or else Low_Id = "generic"
        or else Low_Id = "goto"
        or else Low_Id = "if"
        or else Low_Id = "in"
        or else Low_Id = "interface"
        or else Low_Id = "is"
        or else Low_Id = "limited"
        or else Low_Id = "loop"
        or else Low_Id = "mod"
        or else Low_Id = "new"
        or else Low_Id = "not"
        or else Low_Id = "null"
        or else Low_Id = "of"
        or else Low_Id = "or"
        or else Low_Id = "others"
        or else Low_Id = "out"
        or else Low_Id = "overriding"
        or else Low_Id = "package"
        or else Low_Id = "pragma"
        or else Low_Id = "private"
        or else Low_Id = "procedure"
        or else Low_Id = "protected"
        or else Low_Id = "raise"
        or else Low_Id = "range"
        or else Low_Id = "record"
        or else Low_Id = "rem"
        or else Low_Id = "renames"
        or else Low_Id = "requeue"
        or else Low_Id = "return"
        or else Low_Id = "reverse"
        or else Low_Id = "select"
        or else Low_Id = "separate"
        or else Low_Id = "some"
        or else Low_Id = "subtype"
        or else Low_Id = "synchronized"
        or else Low_Id = "tagged"
        or else Low_Id = "task"
        or else Low_Id = "terminate"
        or else Low_Id = "then"
        or else Low_Id = "type"
        or else Low_Id = "until"
        or else Low_Id = "use"
        or else Low_Id = "when"
        or else Low_Id = "while"
        or else Low_Id = "with"
        or else Low_Id = "xor"
      then
         return To_Unbounded_String (S & "_k");
      else
         return To_Unbounded_String (S);
      end if;
   end Protect_Keywords;

   -------------------------
   -- Set_Input_File_Name --
   -------------------------

   procedure Set_Input_File_Name (FName : String)
   is
   begin
      G_Input_File := To_Unbounded_String (FName);
   end Set_Input_File_Name;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Comment : Ada_Comment) return Boolean is
   begin
      return Length (Comment.Comment) = 0;
   end Is_Empty;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Comment : Ada_Comment;
      F       : Ada.Text_IO.File_Type;
      Indent  : Natural;
      Inline  : Boolean)
   is
      First      : Natural := 1;
      Last_Space : Integer := -1;
      Pre        : constant String (1 .. 3 * Indent) := (others => ' ');

   begin
      if Comment.Is_Empty then
         return;
      end if;

      if not Inline then
         for J in 1 .. Length (Comment.Comment) loop
            if Element (Comment.Comment, J) = ' ' then
               Last_Space := J;
            end if;

            if J - First + Pre'Length + 4 >= Max_Width
              and then Last_Space > 0
            then
               Ada.Text_IO.Put_Line
                 (F,
                  Pre & "--  " & Slice (Comment.Comment, First, Last_Space - 1));
               First := Last_Space + 1;
            end if;
         end loop;
      end if;

      Ada.Text_IO.Put_Line
        (F,
         Pre & "--  " &
           Slice (Comment.Comment, First, Length (Comment.Comment)));
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Comment;
      File    : Ada.Text_IO.File_Type)
   is
   begin
      Ada.Text_IO.Put_Line (File, "   --  " & To_String (Element.Comment));
      G_Empty_Line := False;
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Comment_Box;
      File    : Ada.Text_IO.File_Type)
   is
      Center : constant String := "-- " & To_String (Element.Comment) & " --";
      Top    : constant String (Center'Range) := (others => '-');
   begin
      if not G_Empty_Line then
         Ada.Text_IO.New_Line (File);
      end if;
      Ada.Text_IO.Put_Line (File, "   " & Top);
      Ada.Text_IO.Put_Line (File, "   " & Center);
      Ada.Text_IO.Put_Line (File, "   " & Top);
      Ada.Text_IO.New_Line (File);
      G_Empty_Line := True;
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Pragma;
      File    : Ada.Text_IO.File_Type)
   is
   begin
      if not G_Empty_Line then
         Ada.Text_IO.New_Line (File);
      end if;

      if not Element.Comment.Is_Empty then
         Dump (Element.Comment, File);
      end if;

      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "   pragma " & To_String (Element.Id) & ";");
      Ada.Text_IO.New_Line (File);
      G_Empty_Line := True;
   end Dump;

   ------------------
   -- Dump_Aspects --
   ------------------

   procedure Dump_Aspects (Aspects : String_Vectors.Vector;
                           File    : Ada.Text_IO.File_Type;
                           Inline  : Boolean := False)
   is
      First : Boolean := True;
      Col   : Natural;
   begin
      if Inline then
         Ada.Text_IO.Put (File, " with ");
      else
         Ada.Text_IO.Put (File, "     with ");
      end if;

      Col := 11;

      for A of Aspects loop
         if First then
            Ada.Text_IO.Put (File, A);
            Col := Col + A'Length;
            First := False;

         elsif Col + A'Length + 2 > Max_Width and then not Inline then
            Ada.Text_IO.Put_Line (File, ",");
            Ada.Text_IO.Put (File, (1 .. 10 => ' ') & A);
            Col := A'Length + 11;

         else
            Ada.Text_IO.Put (File, ", " & A);
            Col := Col + A'Length + 2;
         end if;
      end loop;

      Ada.Text_IO.Put_Line (File, ";");
      G_Empty_Line := False;
   end Dump_Aspects;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Type_Scalar;
      File    : Ada.Text_IO.File_Type)
   is
   begin
      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      if Element.Size = 8
        or else Element.Size = 16
        or else Element.Size = 32
        or else Element.Size = 64
      then
         Ada.Text_IO.Put_Line
           (File, "   subtype " & To_String (Element.Id) &
              " is Interfaces.Unsigned_" & To_String (Element.Size) & ";");
      else
         Ada.Text_IO.Put
           (File, "   type " & To_String (Element.Id) &
              " is mod 2**" & To_String (Element.Size));
         if Element.Aspects.Is_Empty then
            Ada.Text_IO.Put_Line (File, ";");
         else
            Ada.Text_IO.New_Line (File);
            Dump_Aspects (Element.Aspects, File);
         end if;
      end if;

      G_Empty_Line := False;
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Subtype_Scalar;
      File    : Ada.Text_IO.File_Type)
   is
   begin
      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      declare
         Subt_String : constant String :=
                         "   subtype " & To_String (Element.Id) & " is";
         Val         : constant String := To_String (Element.Typ);
      begin
         Ada.Text_IO.Put (File, Subt_String);

         if Subt_String'Length + 2 + Val'Length > Max_Width then
            Ada.Text_IO.New_Line (File);
            Ada.Text_IO.Put_Line (File, "     " & Val & ";");
         else
            Ada.Text_IO.Put_Line  (File, " " & Val & ";");
         end if;
      end;

      G_Empty_Line := False;
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Type_Array;
      File    : Ada.Text_IO.File_Type)
   is
      Line  : Unbounded_String;
      Line2 : Unbounded_String;

   begin
      if not G_Empty_Line then
         Ada.Text_IO.New_Line (File);
      end if;

      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Line := To_Unbounded_String (String'(1 .. 3 => ' ')) &
        "type " & Element.Id & " is array (";

      if Length (Element.Index_Type) > 0 then
         Append (Line, Element.Index_Type & " range ");
      end if;

      Append (Line,
              To_String (Element.Index_First) & " .. " &
                  To_String (Element.Index_Last) & ")");

      Line2 := To_Unbounded_String (" of ") & Element.Element_Type;

      if Length (Line) + Length (Line2) + 1 < Max_Width then
         Append (Line, Line2);
         Ada.Text_IO.Put (File, To_String (Line));
      else
         Ada.Text_IO.Put_Line (File, To_String (Line));
         Ada.Text_IO.Put (File, (1 .. 4 => ' ') & To_String (Line2));
      end if;

      if Element.Aspects.Is_Empty then
         Ada.Text_IO.Put_Line (File, ";");
      else
         Ada.Text_IO.New_Line (File);
         Dump_Aspects (Element.Aspects, File);
      end if;

      Ada.Text_IO.New_Line (File);
      G_Empty_Line := True;
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Type_Enum;
      File    : Ada.Text_IO.File_Type)
   is
      Has_Repr : Boolean := False;
      Value    : Ada_Enum_Value;

   begin
      if not G_Empty_Line then
         Ada.Text_IO.New_Line (File);
      end if;

      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Ada.Text_IO.Put_Line
        (File, "   type " & To_String (Element.Id) & " is");

      for J in Element.Values.First_Index .. Element.Values.Last_Index loop
         Value := Element.Values (J);

         if J = Element.Values.First_Index then
            Ada.Text_IO.Put_Line (File, (1 .. 5 => ' ') & '(');
         end if;

         if not Is_Empty (Value.Comment) then
            Dump
              (Value.Comment,
               F      => File,
               Indent => 2,
               Inline => False);
         end if;

         Ada.Text_IO.Put (File, (1 .. 6 => ' ') & To_String (Value.Id));

         if J < Element.Values.Last_Index then
            Ada.Text_IO.Put_Line (File, ",");
         else
            Ada.Text_IO.Put (File, ")");
         end if;

         if Value.Has_Repr then
            Has_Repr := True;
         end if;
      end loop;

      if Element.Aspects.Is_Empty then
         Ada.Text_IO.Put_Line (File, ";");
      else
         Ada.Text_IO.New_Line (File);
         Dump_Aspects (Element.Aspects, File, False);
      end if;

      if Has_Repr then
         Ada.Text_IO.Put_Line
           (File, "   for " & To_String (Element.Id) & " use");

         for J in Element.Values.First_Index .. Element.Values.Last_Index loop
            Value := Element.Values (J);
            if J = Element.Values.First_Index then
               Ada.Text_IO.Put (File, (1 .. 5 => ' ') & '(');
            else
               Ada.Text_IO.Put_Line (File, ",");
               Ada.Text_IO.Put (File, (1 .. 6 => ' '));
            end if;

            Ada.Text_IO.Put
              (File, To_String (Value.Id) & " => " & To_String (Value.Repr));
         end loop;

         Ada.Text_IO.Put_Line (File, ");");
      end if;

      Ada.Text_IO.New_Line (File);
      G_Empty_Line := True;
   end Dump;

   ------------------------
   -- Dump_Record_Fields --
   ------------------------

   procedure Dump_Record_Fields
     (Element : Ada_Type_Record'Class;
      Max_Id  : Natural;
      File    : Ada.Text_IO.File_Type)
   is
      ------------
      -- Get_Id --
      ------------

      function Get_Id (F : Record_Field) return String is
         Id : String (1 .. Max_Id) := (others => ' ');
      begin
         Id (1 .. Length (F.Id)) := To_String (F.Id);

         return Id;
      end Get_Id;

   begin
      for F of Element.Fields loop
         if not F.Comment.Is_Empty then
            Dump (Comment => F.Comment,
                  F       => File,
                  Indent  => 2,
                  Inline  => False);
         end if;

         declare
            Id   : constant String := Get_Id (F);
            Line : constant String :=
                     (1 .. 6 => ' ') & Id & " : " & To_String (F.Typ);
         begin
            Ada.Text_IO.Put (File, Line);

            if F.Has_Default then
               declare
                  Val : constant String := To_String (F.Default);
               begin
                  if Line'Length + 5 + Val'Length >= Max_Width then
                     Ada.Text_IO.Put_Line (File, " :=");
                     Ada.Text_IO.Put_Line (File, (1 .. Id'Length + 10 => ' ') &
                                             Val & ";");
                  else
                     Ada.Text_IO.Put_Line
                       (File, " := " & To_String (F.Default) & ";");
                  end if;
               end;

            else
               Ada.Text_IO.Put_Line (File, ";");
            end if;
         end;
      end loop;
   end Dump_Record_Fields;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Type_Record;
      File    : Ada.Text_IO.File_Type)
   is
      Max_Id  : Natural := 0;

      function Get_Id (F : Record_Field) return String is
         Id : String (1 .. Max_Id) := (others => ' ');
      begin
         Id (1 .. Length (F.Id)) := To_String (F.Id);

         return Id;
      end Get_Id;
   begin
      if not G_Empty_Line then
         Ada.Text_IO.New_Line (File);
      end if;

      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Ada.Text_IO.Put_Line
        (File, "   type " & To_String (Element.Id) & " is record");

      for F of Element.Fields loop
         Max_Id  := Natural'Max (Length (F.Id), Max_Id);
      end loop;

      Dump_Record_Fields (Element, Max_Id, File);

      Ada.Text_IO.Put_Line (File, "   end record");
      Dump_Aspects (Element.Aspects, File);
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line
        (File, "   for " & To_String (Element.Id) & " use record");

      for F of Element.Fields loop
         Ada.Text_IO.Put (File, (1 .. 6 => ' '));
         Ada.Text_IO.Put_Line
           (File,
            Get_Id (F) &
              " at " & To_String (F.Offset) &
              " range " & To_String (F.LSB) & " .. " & To_String (F.MSB) &
              ";");
      end loop;

      Ada.Text_IO.Put_Line (File, "   end record;");
      Ada.Text_IO.New_Line (File);
      G_Empty_Line := True;
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Type_Union;
      File    : Ada.Text_IO.File_Type)
   is
      Max_Id : Natural := 0;

      function Get_Id (F : Record_Field) return String is
         Id : String (1 .. Max_Id) := (others => ' ');
      begin
         Id (1 .. Length (F.Id)) := To_String (F.Id);

         return Id;
      end Get_Id;

   begin
      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Ada.Text_IO.Put_Line
        (File,
         "   type " & To_String (Element.Id));
      Ada.Text_IO.Put_Line
        (File, "     (" & To_String (Element.Disc_Name) &
           " : " & To_String (Element.Discriminent.Id) &
           " := " & To_String (Element.Discriminent.Values.First_Element.Id) &
           ")");
      Ada.Text_IO.Put_Line (File, "   is record");

      --  First dump the values that are common to all discriminent values
      for F of Element.Fields loop
         Max_Id  := Natural'Max (Length (F.Id), Max_Id);
      end loop;

      for Vect of Element.Disc_Fields loop
         for F of Vect loop
            Max_Id := Natural'Max (Length (F.Id), Max_Id);
         end loop;
      end loop;

      Dump_Record_Fields (Element, Max_Id, File);

      Ada.Text_IO.Put (File, (1 .. 2 * 3 => ' '));
      Ada.Text_IO.Put_Line
        (File, "case " & To_String (Element.Disc_Name) & " is");

      for Val of Element.Discriminent.Values loop
         Ada.Text_IO.Put (File, (1 .. 3 * 3 => ' '));
         Ada.Text_IO.Put_Line (File, "when " & To_String (Val.Id) & " =>");

         for F of Element.Disc_Fields (To_String (Val.Id)) loop
            if not F.Comment.Is_Empty then
               Dump (Comment => F.Comment,
                     F       => File,
                     Indent  => 4,
                     Inline  => False);
            end if;
            Ada.Text_IO.Put (File, (1 .. 4 * 3 => ' '));
            Ada.Text_IO.Put_Line
              (File, To_String (F.Id) & " : " & To_String (F.Typ) & ";");
         end loop;
      end loop;

      Ada.Text_IO.Put (File, (1 .. 2 * 3 => ' '));
      Ada.Text_IO.Put_Line
        (File, "end case;");

      Ada.Text_IO.Put_Line (File, "   end record");
      Dump_Aspects (Element.Aspects, File);
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line
        (File, "   for " & To_String (Element.Id) & " use record");

      for F of Element.Fields loop
         Ada.Text_IO.Put (File, (1 .. 6 => ' '));
         Ada.Text_IO.Put_Line
           (File,
            Get_Id (F) &
              " at " & To_String (F.Offset) &
              " range " & To_String (F.LSB) & " .. " & To_String (F.MSB) &
              ";");
      end loop;

      for Val of Element.Discriminent.Values loop
         for F of Element.Disc_Fields (To_String (Val.Id)) loop
            Ada.Text_IO.Put (File, (1 .. 6 => ' '));
            Ada.Text_IO.Put_Line
              (File,
               Get_Id (F) &
                 " at " & To_String (F.Offset) &
                 " range " & To_String (F.LSB) & " .. " & To_String (F.MSB) &
                 ";");
         end loop;
      end loop;

      Ada.Text_IO.Put_Line (File, "   end record;");
      Ada.Text_IO.New_Line (File);
      G_Empty_Line := True;
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Constant_Value;
      File    : Ada.Text_IO.File_Type)
   is
   begin
      if not Element.Comment.Is_Empty then
         if not G_Empty_Line then
            Ada.Text_IO.New_Line (File);
            G_Empty_Line := True;
         end if;

         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Ada.Text_IO.Put (File, "   ");

      if Element.Id_Size > Length (Element.Id) then
         declare
            Str : String (1 .. Element.Id_Size) := (others => ' ');
            S   : constant String := To_String (Element.Id);
         begin
            Str (1 .. S'Length) := S;
            Ada.Text_IO.Put (File, Str);
         end;
      else
         Ada.Text_IO.Put (File, To_String (Element.Id));
      end if;

      Ada.Text_IO.Put (File, " : constant ");

      if Length (Element.Typ) > 0 then
         Ada.Text_IO.Put (File, To_String (Element.Typ) & " ");
      end if;

      declare
         Value : constant String := To_String (Element.Value);
      begin
         if Value'Length <= 4 then --  arbitrary
            Ada.Text_IO.Put_Line (File, ":= " & Value & ";");
         else
            Ada.Text_IO.Put_Line (File, ":=");
            Ada.Text_IO.Put_Line (File, "     " & Value & ";");
         end if;
      end;
      G_Empty_Line := False;
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Instance;
      File    : Ada.Text_IO.File_Type)
   is
   begin
      if not G_Empty_Line then
         Ada.Text_IO.New_Line (File);
      end if;

      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Ada.Text_IO.Put (File, "   " & To_String (Element.Id) & " : ");

      if Element.Aliasd then
         Ada.Text_IO.Put (File, "aliased ");
      end if;

      Ada.Text_IO.Put (File, To_String (Element.Typ));

      if Element.Aspects.Is_Empty then
         Ada.Text_IO.Put_Line (File, ";");
      else
         Ada.Text_IO.New_Line (File);
         Dump_Aspects (Element.Aspects, File);
      end if;

      Ada.Text_IO.New_Line (File);
      G_Empty_Line := True;
   end Dump;

   --------------
   -- New_Spec --
   --------------

   function New_Spec
     (Name          : String;
      Descr         : String;
      Preelaborated : Boolean) return Ada_Spec
   is
      Spec : Ada_Spec;
   begin
      Spec.Id := Protect_Keywords (Name);
      Spec.Comment := New_Comment (Descr);
      Spec.Preelaborated := Preelaborated;

      if Length (G_Withed_All) /= 0 then
         Spec.With_Clauses.Insert
           (To_STring (G_Withed_All), True);
      end if;

      return Spec;
   end New_Spec;

   --------------------
   -- New_Child_Spec --
   --------------------

   function New_Child_Spec
     (Name          : String;
      Parent        : String;
      Descr         : String;
      Preelaborated : Boolean) return Ada_Spec
   is
   begin
      return New_Spec
        (Parent & "." & To_String (Protect_Keywords (Name)),
         Descr, Preelaborated);
   end New_Child_Spec;

   --------
   -- Id --
   --------

   function Id (Spec : Ada_Spec) return Unbounded_String
   is
   begin
      return Spec.Id;
   end Id;

   -----------------------------
   -- Is_Interfaces_Hierarchy --
   -----------------------------

   function Is_Interfaces_Hierarchy
     (Spec : Ada_Spec) return Boolean
   is
   begin
      return Length (Spec.Id) > 10
        and then Ada.Characters.Handling.To_Lower
          (Slice (Spec.Id, 1, 11)) = "interfaces.";
   end Is_Interfaces_Hierarchy;

   ---------------------
   -- Add_Global_With --
   ---------------------

   procedure Add_Global_With (Spec : String)
   is
   begin
      G_Withed_All := To_Unbounded_String (Spec);
   end Add_Global_With;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Spec : Ada_Spec) return String
   is
      Ada_Name : String := To_String (Spec.Id);
   begin
      for J in Ada_Name'Range loop
         Ada_Name (J) :=
           Ada.Characters.Handling.To_Lower (Ada_Name (J));

         if Ada_Name (J) = '.' then
            Ada_Name (J) := '-';
         end if;
      end loop;

      if Ada_Name = "ada-interrupts-names" then
         return "a-intnam.ads";
      end if;

      if Ada_Name'Length >= 10
        and then Ada_Name (1 .. 10) = "interfaces"
        and then
          (Ada_Name'Last = 10
           or else Ada_Name (11) = '-')
      then
         return "i" & Ada_Name (11 .. Ada_Name'Last) & ".ads";
      end if;

      return Ada_Name & ".ads";
   end File_Name;

   ----------------
   -- Write_Spec --
   ----------------

   procedure Write_Spec
     (Spec       : Ada_Spec;
      Output_Dir : String)
   is
      function Spec_Id_Starts_With (Ptrn : String) return Boolean;

      -----------------
      -- Starts_With --
      -----------------

      function Spec_Id_Starts_With (Ptrn : String) return Boolean
      is
      begin
         return Length (Spec.Id) > Ptrn'Length
           and then Slice (Spec.Id, 1, Ptrn'Length) = Ptrn;
      end Spec_Id_Starts_With;

      Full      : constant String :=
                    GNAT.OS_Lib.Normalize_Pathname (Output_Dir);
      F_Name    : constant String :=
                    GNAT.OS_Lib.Normalize_Pathname
                      (File_Name (Spec),
                       Full);
      F         : Ada.Text_IO.File_Type;
      Max_Width : Natural := 0;
      Curs      : With_Maps.Cursor;

   begin
      if not GNAT.OS_Lib.Is_Directory (Full) then
         Ada.Text_IO.Put_Line ("Creating Dir " & Full);
         GNAT.Directory_Operations.Make_Dir (Full);
      end if;

      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File,
                          F_Name);

      if Spec_Id_Starts_With ("Interfaces.")
        or else Spec_Id_Starts_With ("Ada.")
      then
         --  Add the AdaCore copyright notice to the spec.
         Ada.Text_IO.Put_Line
           (F, "--");
         Ada.Text_IO.Put_Line
           (F, "--  Copyright (C)" &
              Ada.Calendar.Year (Ada.Calendar.Clock)'Img &
              ", AdaCore");
         Ada.Text_IO.Put_Line
           (F, "--");
         Ada.Text_IO.New_Line (F);
      end if;

      Ada.Text_IO.Put_Line
        (F,
         "--  This spec has been automatically generated from " &
           To_String (G_Input_File));
      Ada.Text_IO.New_Line (F);
      G_Empty_Line := True;

      if Spec.Preelaborated then
         if SVD2Ada_Utils.Gen_GNAT15 then
            --  In GNAT GPL 2015 or GNAT Pro 7.4, using the pragma
            --  No_Elaboration_Code_All will not work because of dependencies
            --  over System.Unsigned_Types. So we stick with the restriction
            --  No_Elaboration_Code here.
            Ada.Text_IO.Put_Line
              (F, "pragma Restrictions (No_Elaboration_Code);");
         end if;

         Ada.Text_IO.Put_Line
           (F, "pragma Ada_2012;");
         Ada.Text_IO.New_Line (F);
         G_Empty_Line := True;
      end if;

      Curs := Spec.With_Clauses.First;

      while With_Maps.Has_Element (Curs) loop
         Max_Width :=
           Natural'Max (Max_Width, With_Maps.Key (Curs)'Length);
         With_Maps.Next (Curs);
      end loop;

      Curs := Spec.With_Clauses.First;

      while With_Maps.Has_Element (Curs) loop
         declare
            W     : constant String := With_Maps.Key (Curs);
            Use_C : constant Boolean := With_Maps.Element (Curs);
         begin
            Ada.Text_IO.Put (F, "with " & W & ";");

            if Use_C then
               declare
                  Space : constant String (W'Length .. Max_Width + 1) :=
                            (others => ' ');
               begin
                  Ada.Text_IO.Put (F, Space);
               end;
               Ada.Text_IO.Put (F, "use " & W & ";");
            end if;

            Ada.Text_IO.New_Line (F);
            G_Empty_Line := False;
            With_maps.Next (Curs);
         end;
      end loop;

      if not G_Empty_Line then
         Ada.Text_IO.New_Line (F);
      end if;

      if not Spec.Comment.Is_Empty then
         Spec.Comment.Dump (F, Indent => 0, Inline => False);
      end if;

      Ada.Text_IO.Put_Line (F, "package " & To_String (Spec.Id) & " is");
      if Spec.Preelaborated then
         Ada.Text_IO.Put_Line
           (F, "   pragma Preelaborate;");
         if not SVD2Ada_Utils.Gen_GNAT15 then
            --  See above: from GNAT GPL 2016 or GNAT Pro 17, we can now use
            --  pragma No_Elaboration_Code_All to ensure that we don't rely
            --  on runtime intiialization when using the generated packages.
            Ada.Text_IO.Put_Line
              (F, "   pragma No_Elaboration_Code_All;");
         end if;
      end if;
      G_Empty_Line := False;

      for Elt of Spec.Elements loop
         Dump (Elt, F);
      end loop;

      if not G_Empty_Line then
         Ada.Text_IO.New_Line (F);
      end if;

      Ada.Text_IO.Put_Line (F, "end " & To_String (Spec.Id) & ";");

      Ada.Text_IO.Close (F);
   end Write_Spec;

   ---------
   -- Add --
   ---------

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_With_Clause)
   is
      With_Pkg : String renames To_String (Elt.Pkg);
   begin
      if Is_Parent (Spec, Elt) then
         return;
      end if;

      if Spec.With_Clauses.Contains (With_Pkg) then
         if Elt.Add_Use_Clause then
            -- Make sure we have use visibility for this package
            Spec.With_Clauses.Replace (With_Pkg, True);
         end if;

         return;
      end if;

      Spec.With_Clauses.Insert (With_Pkg, Elt.Add_Use_Clause);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Comment'Class)
   is
   begin
      Spec.Elements.Append (Elt);
      Elt.Added_In_Spec (Spec);
   end Add;

   ------------------
   -- Add_No_Check --
   ------------------

   procedure Add_No_Check
     (Spec : in out Ada_Spec;
      Elt  : Ada_Type'Class)
   is
      use type Ada.Tags.Tag;
   begin
      for Prev of Spec.Elements loop
         if Prev'Tag = Elt'Tag then
            declare
               Prev_T : constant Ada_Type'Class := Ada_Type'Class (Prev);
            begin
               if Prev_T.Id = Elt.Id then
                  if Is_Similar (Elt, Prev_T) then
                     return;
                  else
                     raise Constraint_Error with
                       "Two different types with the same Id: " &
                       To_String (Elt.Id);
                  end if;
               end if;
            end;
         end if;
      end loop;

      Spec.Elements.Append (Elt);
      Elt.Added_In_Spec (Spec);
   end Add_No_Check;

   ---------
   -- Add --
   ---------

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : in out Ada_Type'Class)
   is
      use type Ada.Tags.Tag;
      Retry     : Boolean := True;
      Idx       : Natural := 0;
      Name_Orig : Unbounded_String;

   begin
      while Retry loop
         Retry := False;

         for Prev of Spec.Elements loop
            if Prev'Tag = Elt'Tag then
               declare
                  Prev_T : constant Ada_Type'Class := Ada_Type'Class (Prev);
               begin
                  if Prev_T.Id = Elt.Id then
                     if Is_Similar (Elt, Prev_T) then
                        return;
                     else
                        if Name_Orig = Null_Unbounded_String then
                           Name_Orig := Elt.Id;
                        end if;

                        Idx := Idx + 1;
                        Elt.Id := Name_Orig & "_" & To_String (Idx);
                        Retry := True;

                        exit;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end loop;

      Spec.Elements.Append (Elt);
      Elt.Added_In_Spec (Spec);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Constant_Value)
   is
   begin
      Spec.Elements.Append (Elt);
      Elt.Added_In_Spec (Spec);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Instance)
   is
   begin
      Spec.Elements.Append (Elt);
      Elt.Added_In_Spec (Spec);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Pragma)
   is
   begin
      Spec.Elements.Append (Elt);
      Elt.Added_In_Spec (Spec);
   end Add;

   ---------------------
   -- New_With_Clause --
   ---------------------

   function New_With_Clause
     (Pkg         : String;
      Use_Visible : Boolean := False) return Ada_With_Clause
   is
   begin
      return (Pkg            => To_Unbounded_String (Pkg),
              Add_Use_Clause => Use_Visible);
   end New_With_Clause;

   ---------------
   -- Is_Parent --
   ---------------

   function Is_Parent
     (Spec        : Ada_Spec;
      With_Clause : Ada_With_Clause) return Boolean
   is
      Spec_Lower : constant String :=
                     Ada.Characters.Handling.To_Lower (To_String (Spec.Id));
      With_Lower : constant String :=
                     Ada.Characters.Handling.To_Lower
                       (To_String (With_Clause.Pkg));
   begin
      return With_Lower'Length < Spec_Lower'Length
        and then Spec_Lower (1 .. With_Lower'Length + 1) = (With_Lower & '.');
   end Is_Parent;

   -----------------
   -- New_Comment --
   -----------------

   function New_Comment
     (Comment : String) return Ada_Comment
   is
      function Strip_String (Str : String) return String
      is
         Ret     : String (Str'Range);
         Idx     : Natural := Ret'First;
         Prev    : character := ' ';
         Current : Character := ' ';
      begin
         for J in Str'Range loop
            if Str (J) = ASCII.CR
              or else Str (J) = ASCII.LF
            then
               Current := ' ';
            else
               Current := Str (J);
            end if;

            if Current /= ' ' or else Prev /= ' ' then
               Ret (Idx) := Current;
               Idx := Idx + 1;
            end if;

            Prev := Current;
         end loop;

         return Ret (Ret'First .. Idx - 1);
      end Strip_String;
   begin
      return (Comment => To_Unbounded_String
              (Strip_String
                 (Ada.Strings.Fixed.Trim (Comment, Ada.Strings.Both))));
   end New_Comment;

   ---------------------
   -- New_Comment_Box --
   ---------------------

   function New_Comment_Box
     (Comment : String) return Ada_Comment_Box
   is
      C : constant Ada_Comment := New_Comment (Comment);
   begin
      return (Comment => C.Comment);
   end New_Comment_Box;

   ----------------
   -- New_Pragma --
   ----------------

   function New_Pragma
     (Identifier : String;
      Comment    : String := "") return Ada_Pragma
   is
      C : constant Ada_Pragma :=
            (Id      => To_Unbounded_String (Identifier),
             Comment => New_Comment (Comment));
   begin
      return C;
   end New_Pragma;

   --------
   -- Id --
   --------

   function Id (Elt : Ada_Type'Class) return Unbounded_String
   is
   begin
      return Elt.Id;
   end Id;

   function Id (Elt : Ada_Type'Class) return String
   is
   begin
      return To_String (Elt.Id);
   end Id;

   ---------------------
   -- Add_Size_Aspect --
   ---------------------

   procedure Add_Size_Aspect
     (Elt  : in out Ada_Type'Class;
      Size : Unsigned)
   is
      Size_Str : String renames To_String (Size);
   begin
      for Aspect of Elt.Aspects loop
         if Aspect'Length > 8
           and then Aspect (Aspect'First .. Aspect'First + 7) = "Size => "
         then
            if Aspect (Aspect'First + 8 .. Aspect'Last) /= Size_Str then
               raise Constraint_Error with
                 "Size aspect already defined for " & To_String (Elt.Id);
            end if;

            return;
         end if;
      end loop;

      Elt.Aspects.Append ("Size => " & Size_Str);
   end Add_Size_Aspect;

   ---------------------
   -- Get_Size_Aspect --
   ---------------------

   function Get_Size_Aspect (Elt  : Ada_Type'Class) return Unsigned
   is
   begin
      for Aspect of Elt.Aspects loop
         if Aspect'Length > 8
           and then Aspect (Aspect'First .. Aspect'First + 7) = "Size => "
         then
            return Unsigned'Value (Aspect (Aspect'First + 8 .. Aspect'Last));
         end if;
      end loop;

      return 0;
   end Get_Size_Aspect;

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Aspect
     (Elt    : in out Ada_Type'Class;
      Aspect : String)
   is
   begin
      for A of Elt.Aspects loop
         if Aspect = A then
            --  Aspect already defined
            return;
         end if;
      end loop;

      Elt.Aspects.Append (Aspect);
   end Add_Aspect;

   ---------------------
   -- New_Type_Scalar --
   ---------------------

   function New_Type_Scalar
     (Id      : String;
      Size    : Unsigned;
      Comment : String := "") return Ada_Type_Scalar
   is
      Ret : Ada_Type_Scalar;
   begin
      Ret.Id      := Protect_Keywords (Id);
      Ret.Comment := New_Comment (Comment);
      Ret.Size    := Size;
      Add_Size_Aspect (Ret, Size);
      return Ret;
   end New_Type_Scalar;

   ---------------------
   -- New_Type_Scalar --
   ---------------------

   function New_Subype_Scalar
     (Id      : String;
      Typ     : String;
      Comment : String := "") return Ada_Subtype_Scalar
   is
   begin
      return (Id      => Protect_Keywords (Id),
              Comment => New_Comment (Comment),
              Typ     => To_Unbounded_String (Typ),
              others  => <>);
   end New_Subype_Scalar;

   -------------------
   -- Added_In_Spec --
   -------------------

   overriding procedure Added_In_Spec
     (Element : Ada_Type_Scalar;
      Spec    : in out Ada_Spec)
   is
   begin
      if Element.Size = 8
        or else Element.Size = 16
        or else Element.Size = 32
        or else Element.Size = 64
      then
         Add (Spec, New_With_Clause ("Interfaces", True));
      end if;
   end Added_In_Spec;

   ----------------
   -- Is_Similar --
   ----------------

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Scalar) return Boolean
   is
   begin
      return T1.Id = T2.Id
        and then T1.Size = T2.Size;
   end Is_Similar;

   ----------------
   -- Is_Similar --
   ----------------

   overriding function Is_Similar
     (T1, T2 : Ada_Subtype_Scalar) return Boolean
   is
   begin
      return T1.Id = T2.Id
        and then T1.Typ = T2.Typ;
   end Is_Similar;

   --------------------
   -- New_Type_Array --
   --------------------

   function New_Type_Array
     (Id           : String;
      Index_Type   : String;
      Index_First  : Unsigned;
      Index_Last   : Unsigned;
      Element_Type : String;
      Comment      : String := "") return Ada_Type_Array
   is
   begin
      return (Id           => Protect_Keywords (Id),
              Comment      => New_Comment (Comment),
              Aspects      => <>,
              Index_Type   => To_Unbounded_String (Index_Type),
              Index_First  => Index_First,
              Index_Last   => Index_Last,
              Element_Type => To_Unbounded_String (Element_Type));
   end New_Type_Array;

   ----------------
   -- Is_Similar --
   ----------------

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Array) return Boolean
   is
   begin
      return T1.Id = T2.Id
        and then T1.Index_First = T2.Index_First
        and then T1.Index_Last = T2.Index_Last
        and then T1.Element_Type = T2.Element_Type;
   end Is_Similar;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean return Ada_Type_Enum
   is
      Ret  : Ada_Type_Enum;
      Dead : Ada_Enum_Value with Unreferenced;
   begin
      Ret := (Id      => To_Unbounded_String ("Boolean"),
              Comment => New_Comment (""),
              Aspects => <>,
              Values  => <>);
      Dead := Add_Enum_Id (Ret, "False", 0);
      Dead := Add_Enum_Id (Ret, "True", 0);
      return Ret;
   end Get_Boolean;

    -------------------
   -- New_Type_Enum --
   -------------------

   function New_Type_Enum
     (Id      : String;
      Size    : Unsigned := 0;
      Comment : String := "") return Ada_Type_Enum
   is
      Ret : Ada_Type_Enum;
   begin
      Ret := (Id      => Protect_Keywords (Id),
              Comment => New_Comment (Comment),
              Aspects => <>,
              Values  => <>);
      if Size > 0 then
         Add_Size_Aspect (Ret, Size);
      end if;

      return Ret;
   end New_Type_Enum;

   --------------------------
   -- Add_Enum_Id_Internal --
   --------------------------

   function Add_Enum_Id_Internal
     (Enum     : in out Ada_Type_Enum;
      Id       : String;
      Has_Repr : Boolean;
      Repr     : Unsigned;
      Comment  : String := "")  return Ada_Enum_Value
   is
      Enum_Value : Ada_Enum_Value;
      Camel_C    : String := Id;
      First      : Boolean := True;

   begin
      for J in Camel_C'Range loop
         if First then
           if Camel_C (J) in 'a' .. 'z' then
               Camel_C (J) := Ada.Characters.Handling.To_Upper (Camel_C (J));
            elsif Camel_C (J) in 'A' .. 'Z' then
               First := False;
            end if;

         else
            if Camel_C (J) in 'A' .. 'Z' then
               Camel_C (J) := Ada.Characters.Handling.To_Lower (Camel_C (J));
            elsif Camel_C (J) not in 'a' .. 'z' then
               First := True;
            end if;
         end if;
      end loop;

      if Id (Id'First) in '0' .. '9' then
         Enum_Value :=
           (Id       => Enum.Id & "_" & Camel_C,
            Has_Repr => Has_Repr,
            Repr     => Repr,
            Comment  => New_Comment (Comment));
      else
         Enum_Value :=
           (Id       => Protect_Keywords (Camel_C),
            Has_Repr => Has_Repr,
            Repr     => Repr,
            Comment  => New_Comment (Comment));
      end if;

      --  Insert the value as an ordered list, as mandated by Ada
      if Has_Repr then
         for J in Enum.Values.First_Index .. Enum.Values.Last_Index loop
            --  simple search here, we don't really care about the bad perfs
            if Enum.Values (J).Repr > Repr then
               Enum.Values.Insert (J, Enum_Value);
               return Enum_Value;
            end if;
         end loop;
      end if;

      Enum.Values.Append (Enum_Value);

      return Enum_Value;
   end Add_Enum_Id_Internal;

   -----------------
   -- Add_Enum_Id --
   -----------------

   function Add_Enum_Id
     (Enum    : in out Ada_Type_Enum;
      Id      : String;
      Comment : String := "") return Ada_Enum_Value
   is
   begin
      return Add_Enum_Id_Internal (Enum, Id, False, 0, Comment);
   end Add_Enum_Id;

   -----------------
   -- Add_Enum_Id --
   -----------------

   function Add_Enum_Id
     (Enum    : in out Ada_Type_Enum;
      Id      : String;
      Repr    : Unsigned;
      Comment : String := "") return Ada_Enum_Value
   is
   begin
      return Add_Enum_Id_Internal (Enum, Id, True, Repr, Comment);
   end Add_Enum_Id;

   --------
   -- Id --
   --------

   function Id (Elt : Ada_Enum_Value) return Unbounded_String
   is
   begin
      return Elt.Id;
   end id;

   ---------
   -- "=" --
   ---------

   function "=" (V1, V2 : Ada_Enum_Value) return Boolean
   is
   begin
      return V1.Id = V2.Id
        and then V1.Has_Repr = V2.Has_Repr
        and then (if V1.Has_Repr then V1.Repr = V2.Repr else True);
   end "=";

   ----------------
   -- Is_Similar --
   ----------------

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Enum) return Boolean
   is
      use type Enum_Value_Vectors.Vector;
   begin
      return T1.Id = T2.Id
        and then T1.Values = T2.Values;
   end Is_Similar;

   ---------------------
   -- New_Type_Record --
   ---------------------

   function New_Type_Record
     (Id      : String;
      Comment : String := "") return Ada_Type_Record'Class
   is
   begin
      return Ada_Type_Record'
        (Id          => Protect_Keywords (Id),
         Comment     => New_Comment (Comment),
         Aspects     => <>,
         Fields      => <>,
         Need_System => False);
   end New_Type_Record;

   --------------------------
   -- Add_Bit_Order_Aspect --
   --------------------------

   procedure Add_Bit_Order_Aspect
     (Elt   : in out Ada_Type_Record'Class;
      Order : System.Bit_Order)
   is
   begin
      case Order is
         when System.High_Order_First =>
            Add_Aspect (Elt, "Bit_Order => System.High_Order_First");
         when System.Low_Order_First =>
            Add_Aspect (Elt, "Bit_Order => System.Low_Order_First");
      end case;

      Elt.Need_System := True;
   end Add_Bit_Order_Aspect;

   -------------------
   -- Added_In_Spec --
   -------------------

   overriding procedure Added_In_Spec
     (Element : Ada_Type_Record;
      Spec    : in out Ada_Spec)
   is
   begin
      if Element.Need_System then
         Add (Spec, New_With_Clause ("System"));
      end if;
   end Added_In_Spec;

   --------------
   -- Simplify --
   --------------

   function Simplify
     (Elt  : Ada_Type_Record;
      Spec : in out Ada_Spec)
      return Ada_Type'Class
   is
      function Find (Typ : Unbounded_String) return Ada_Type'Class;

      ----------
      -- Find --
      ----------

      function Find (Typ : Unbounded_String) return Ada_Type'Class
      is
         Curs : Element_Vectors.Cursor := Spec.Elements.First;
         use Element_Vectors;
      begin
         while Has_Element (Curs) loop
            if Element (Curs) in Ada_Type'Class
              and then Ada_Type'Class (Element (Curs)).Id = Typ
            then
               declare
                  Ret : constant Ada_Type'Class :=
                          Ada_Type'Class (Element (Curs));
               begin
                  Spec.Elements.Delete (Curs);
                  return Ret;
               end;
            end if;

            Next (Curs);
         end loop;

         raise Constraint_Error with "Cannot find type " & To_String (Typ);
      end Find;

      use type Ada.Containers.Count_Type;

   begin
      if Elt.Fields.Length /= 1 then
         return Elt;
      end if;

      declare
         Typ : Ada_Type'Class := Find (Elt.Fields.First_Element.Typ);
      begin
         Typ.Id      := Elt.Id;
         for Aspect of Elt.Aspects loop
            if not Typ.Aspects.Contains (Aspect) then
               Typ.Aspects.Append (Aspect);
            end if;
         end loop;
         Typ.Comment := Elt.Comment;

         return Typ;
      end;
   end Simplify;

   ------------------------
   -- Add_Field_Internal --
   ------------------------

   procedure Add_Field_Internal
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Has_Default : Boolean;
      Default     : Unbounded_String;
      Comment     : String := "")
   is
   begin
      Rec.Fields.Append
        ((Id          => Protect_Keywords (Id),
          Typ         => To_Unbounded_String (Typ),
          Offset      => Offset,
          LSB         => LSB,
          MSB         => MSB,
          Has_Default => Has_Default,
          Default     => Default,
          Comment     => New_Comment (Comment)));
   end Add_Field_Internal;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Comment     : String := "")
   is
   begin
      Add_Field_Internal
        (Rec,
         Id          => Id,
         Typ         => Typ,
         Offset      => Offset,
         LSB         => LSB,
         MSB         => MSB,
         Has_Default => False,
         Default     => Null_Unbounded_String,
         Comment     => Comment);
   end Add_Field;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Default     : Unsigned;
      Comment     : String := "")
   is
   begin
      Add_Field_Internal
        (Rec,
         Id          => Id,
         Typ         => Typ,
         Offset      => Offset,
         LSB         => LSB,
         MSB         => MSB,
         Has_Default => True,
         Default     => To_Unbounded_String (To_Hex (Default)),
         Comment     => Comment);
   end Add_Field;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Default     : Unbounded_String;
      Comment     : String := "")
   is
   begin
      Add_Field_Internal
        (Rec,
         Id          => Id,
         Typ         => Typ,
         Offset      => Offset,
         LSB         => LSB,
         MSB         => MSB,
         Has_Default => True,
         Default     => Default,
         Comment     => Comment);
   end Add_Field;

   ---------
   -- "=" --
   ---------

   function "=" (R1, R2 : Record_Field) return Boolean
   is
   begin
      return R1.Id = R2.Id
        and then R1.Typ = R2.Typ
        and then R1.Offset = R2.Offset
        and then R1.LSB = R2.LSB
        and then R1.Has_Default = R2.Has_Default
        and then (if R1.Has_Default then R1.Default = R2.Default else True);
   end "=";

   ----------------
   -- Is_Similar --
   ----------------

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Record) return Boolean
   is
      use type Record_Field_Vectors.Vector;
   begin
      return T1.Id = T2.Id
        and then T1.Fields = T2.Fields;
   end Is_Similar;

   --------------------
   -- New_Type_Union --
   --------------------

   function New_Type_Union
     (Id        : String;
      Disc_Name : String;
      Disc_Type : Ada_Type_Enum'Class;
      Comment   : String := "") return Ada_Type_Union
   is
      Ret : Ada_Type_Union;
   begin
      Ret :=
        (Id           => Protect_Keywords (Id),
         Comment      => New_Comment (Comment),
         Aspects      => <>,
         Disc_name    => To_Unbounded_String (Disc_Name),
         Discriminent => Ada_Type_Enum (Disc_Type),
         Fields       => <>,
         Disc_Fields  => <>,
         Need_System  => False);
      Add_Aspect (Ret, "Unchecked_Union");

      for Val of Disc_Type.Values loop
         Ret.Disc_Fields.Insert (To_String (Val.Id),
                                 Record_Field_Vectors.Empty_Vector);
      end loop;

      return Ret;
   end New_Type_Union;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Rec         : in out Ada_Type_Union'Class;
      Enum_Val    : String;
      Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Comment     : String := "")
   is
   begin
      Rec.Disc_Fields (Enum_Val).Append
        ((Id          => To_Unbounded_String (Id),
          Typ         => To_Unbounded_String (Typ),
          Offset      => Offset,
          LSB         => LSB,
          MSB         => MSB,
          Has_Default => False,
          Default     => Null_Unbounded_String,
          Comment     => New_Comment (Comment)));
   end Add_Field;

   ----------------
   -- Is_Similar --
   ----------------

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Union) return Boolean
   is
      use type Record_Field_Vectors.Vector;
   begin
      if T1.Id /= T2.Id
        or else T1.Fields /= T2.Fields
        or else T1.Disc_Name /= T2.Disc_Name
        or else not Is_Similar (T1.Discriminent, T2.Discriminent)
      then
         return False;
      end if;

      for Key of T1.Discriminent.Values loop
         declare
            Key_Str : String renames To_String (Key.Id);
         begin
            if T1.Disc_Fields (Key_Str) /= T2.Disc_Fields (Key_Str) then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Is_Similar;

   ------------------------
   -- New_Constant_Value --
   ------------------------

   function New_Constant_Value
     (Id       : String;
      Align_Id : Natural;
      Typ      : String;
      Value    : String;
      Comment  : String := "") return Ada_Constant_Value
   is
   begin
      return (Id      => Protect_Keywords (Id),
              Id_Size => Align_Id,
              Typ     => To_Unbounded_String (Typ),
              Value   => To_Unbounded_String (Value),
              Comment => New_Comment (Comment));
   end New_Constant_Value;

   ------------------
   -- New_Instance --
   ------------------

   function New_Instance
     (Id           : String;
      Typ          : String;
      Aliased_Inst : Boolean;
      Comment      : String := "") return Ada_Instance
   is
   begin
      return (Id      => Protect_Keywords (Id),
              Typ     => To_Unbounded_String (Typ),
              Aliasd  => Aliased_Inst,
              Comment => New_Comment (Comment),
              Aspects => <>);
   end New_Instance;

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Address_Aspect
     (Elt     : in out Ada_Instance;
      Address : Unsigned)
   is
   begin
      Elt.Aspects.Append ("Address => System'To_Address (" &
                            To_Hex (Address) & ")");
   end Add_Address_Aspect;

   ------------------------
   -- Add_Address_Aspect --
   ------------------------

   procedure Add_Address_Aspect
     (Elt : in out Ada_Instance;
      Val : String)
   is
   begin
      Elt.Aspects.Append ("Address => " & Val);
   end Add_Address_Aspect;

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Aspect
     (Elt    : in out Ada_Instance;
      Aspect : String)
   is
   begin
      Elt.Aspects.Append (Aspect);
   end Add_Aspect;

   -------------------
   -- Added_In_Spec --
   -------------------

   overriding procedure Added_In_Spec
     (Element : Ada_Instance;
      Spec    : in out Ada_Spec)
   is
   begin
      if not Element.Aspects.Is_Empty then
         --  Address aspect requires the System package
         Add (Spec, New_With_Clause ("System"));
      end if;
   end Added_In_Spec;

end Ada_Gen;
