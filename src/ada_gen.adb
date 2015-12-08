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

with Interfaces; use Interfaces;

with Ada.Characters.Handling;
with Ada.Tags;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

package body Ada_Gen is

   Max_Width    : constant Natural := 79;

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

            if J - First + Pre'Length + 4 > Max_Width
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
      Ada.Text_IO.New_Line (File);
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
      Ada.Text_IO.Put_Line (File, "   " & Top);
      Ada.Text_IO.Put_Line (File, "   " & Center);
      Ada.Text_IO.Put_Line (File, "   " & Top);
      Ada.Text_IO.New_Line (File);
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

      Ada.Text_IO.New_Line (File);
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Type_Array;
      File    : Ada.Text_IO.File_Type)
   is
   begin
      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Ada.Text_IO.Put (File, (1 .. 3 => ' '));
      Ada.Text_IO.Put
        (File, "type " & To_String (Element.Id) & " is array (");

      if Length (Element.Index_Type) > 0 then
         Ada.Text_IO.Put (File, To_String (Element.Index_Type) & " range ");
      end if;

      Ada.Text_IO.Put
        (File,
         To_String (Element.Index_First) & " .. " &
           To_String (Element.Index_Last) & ") of " &
           To_String (Element.Element_Type));

      if Element.Aspects.Is_Empty then
         Ada.Text_IO.Put_Line (File, ";");
      else
         Ada.Text_IO.New_Line (File);
         Dump_Aspects (Element.Aspects, File);
      end if;

      Ada.Text_IO.New_Line (File);
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
      Inline_Aspect : Boolean := False;

   begin
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
            Ada.Text_IO.Put (File, (1 .. 5 => ' ') & '(');
         else
            Ada.Text_IO.Put (File, (1 .. 6 => ' '));
         end if;

         Ada.Text_IO.Put (File, To_String (Value.Id));

         if J < Element.Values.Last_Index then
            if Is_Empty (Value.Comment) then
               Ada.Text_IO.Put_Line (File, ",");
            else
               Ada.Text_IO.Put (File, ", ");
               Dump (Value.Comment,
                     F      => File,
                     Indent => 0,
                     Inline => True);
            end if;
         else
            if Is_Empty (Value.Comment) then
               Ada.Text_IO.Put (File, ")");
            else
               Ada.Text_IO.Put (File, " ");
               Dump (Value.Comment,
                     F      => File,
                     Indent => 0,
                     Inline => True);
               Ada.Text_IO.Put (File, (1 .. 5 => ' ') & ")");
               Inline_Aspect := True;
            end if;
         end if;

         if Value.Has_Repr then
            Has_Repr := True;
         end if;
      end loop;

      if Element.Aspects.Is_Empty then
         Ada.Text_IO.Put_Line (File, ";");
      else
         if not Inline_Aspect then
            Ada.Text_IO.New_Line (File);
         end if;

         Dump_Aspects (Element.Aspects, File, Inline_Aspect);
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
   end Dump;

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

      for F of Element.Fields loop
         if not F.Comment.Is_Empty then
            Dump (Comment => F.Comment,
                  F       => File,
                  Indent  => 2,
                  Inline  => False);
         end if;

         Ada.Text_IO.Put (File, (1 .. 6 => ' '));
         Ada.Text_IO.Put (File, Get_Id (F) & " : " & To_String (F.Typ));

         if F.Has_Default then
            Ada.Text_IO.Put_Line (File, " := " & To_String (F.Default) & ";");
         else
            Ada.Text_IO.Put_Line (File, ";");
         end if;
      end loop;

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
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Type_Union;
      File    : Ada.Text_IO.File_Type)
   is
   begin
      if not Element.Comment.Is_Empty then
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Ada.Text_IO.Put_Line
        (File,
         "   type " & To_String (Element.Id) &
           " (" & To_String (Element.Disc_Name) &
           " : " & To_String (Element.Discriminent.Id) &
           " := " & To_String (Element.Discriminent.Values.First_Element.Id) &
           ") is record");

      Ada.Text_IO.Put (File, (1 .. 2 * 3 => ' '));
      Ada.Text_IO.Put_Line
        (File, "case " & To_String (Element.Disc_Name) & " is");

      for Val of Element.Discriminent.Values loop
         Ada.Text_IO.Put (File, (1 .. 3 * 3 => ' '));
         Ada.Text_IO.Put_Line (File, "when " & To_String (Val.Id) & " =>");

         for F of Element.Fields (To_String (Val.Id)) loop
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

      for Val of Element.Discriminent.Values loop
         for F of Element.Fields (To_String (Val.Id)) loop
            Ada.Text_IO.Put (File, (1 .. 6 => ' '));
            Ada.Text_IO.Put_Line
              (File,
               To_String (F.Id) &
                 " at " & To_String (F.Offset) &
                 " range " & To_String (F.LSB) & " .. " & To_String (F.MSB) &
                 ";");
         end loop;
      end loop;

      Ada.Text_IO.Put_Line (File, "   end record;");
      Ada.Text_IO.New_Line (File);
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
         Dump (Comment => Element.Comment,
               F       => File,
               Indent  => 1,
               Inline  => False);
      end if;

      Ada.Text_IO.Put (File, "   " & To_String (Element.Id) & ": constant ");

      if Length (Element.Typ) > 0 then
         Ada.Text_IO.Put (File, To_String (Element.Typ) & " ");
      end if;

      Ada.Text_IO.Put_Line (File, ":= " & To_String (Element.Value) & ";");
      Ada.Text_IO.New_Line (File);
   end Dump;

   ----------
   -- Dump --
   ----------

   overriding procedure Dump
     (Element : Ada_Instance;
      File    : Ada.Text_IO.File_Type)
   is
   begin
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
      Spec.Id := To_Unbounded_String (Name);
      Spec.Comment := New_Comment (Descr);
      Spec.Preelaborated := Preelaborated;
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
      return New_Spec (Parent & "." & Name, Descr, Preelaborated);
   end New_Child_Spec;

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
      return Ada_Name & ".ads";
   end File_Name;

   ----------------
   -- Write_Spec --
   ----------------

   procedure Write_Spec
     (Spec       : Ada_Spec;
      Output_Dir : String)
   is
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
            With_maps.Next (Curs);
         end;
      end loop;

      Ada.Text_IO.New_Line (F);
      if not Spec.Comment.Is_Empty then
         Spec.Comment.Dump (F, Indent => 0, Inline => False);
      end if;

      Ada.Text_IO.Put_Line (F, "package " & To_String (Spec.Id) & " is");
      if Spec.Preelaborated then
         Ada.Text_IO.Put_Line (F, "   pragma Preelaborate;");
      end if;
      Ada.Text_IO.New_Line (F);

      for Elt of Spec.Elements loop
         Dump (Elt, F);
      end loop;

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

   -----------------
   -- New_Comment --
   -----------------

   function New_Comment
     (Comment : String) return Ada_Comment
   is
      function Strip_String (Str : String) return String
      is
         Ret     : String := Str;
         Idx     : Natural := Str'First;
         Prev    : character := ' ';
         Current : Character := ' ';
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
               Ret (Idx) := Current;
               Idx := Idx + 1;
            end if;

            Prev := Current;
         end loop;

         return Ret (Ret'First .. Idx - 1);
      end Strip_String;
   begin
      return (Comment => To_Unbounded_String (Strip_String (Comment)));
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
      Ret.Id      := To_Unbounded_String (Id);
      Ret.Comment := New_Comment (Comment);
      Ret.Size    := Size;
      Add_Size_Aspect (Ret, Size);
      return Ret;
   end New_Type_Scalar;

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
      return (Id           => To_Unbounded_String (Id),
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
      Ret : Ada_Type_Enum;
   begin
      Ret := (Id      => To_Unbounded_String ("Boolean"),
              Comment => New_Comment (""),
              Aspects => <>,
              Values  => <>);
      Add_Enum_Id (Ret, "False", 0);
      Add_Enum_Id (Ret, "True", 0);
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
      Ret := (Id      => To_Unbounded_String (Id),
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

   procedure Add_Enum_Id_Internal
     (Enum     : in out Ada_Type_Enum;
      Id       : String;
      Has_Repr : Boolean;
      Repr     : Unsigned;
      Comment  : String := "")
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
           (Id       => To_Unbounded_String (Camel_C),
            Has_Repr => Has_Repr,
            Repr     => Repr,
            Comment  => New_Comment (Comment));
      end if;
      Enum.Values.Append (Enum_Value);
   end Add_Enum_Id_Internal;

   -----------------
   -- Add_Enum_Id --
   -----------------

   procedure Add_Enum_Id
     (Enum    : in out Ada_Type_Enum;
      Id      : String;
      Comment : String := "")
   is
   begin
      Add_Enum_Id_Internal (Enum, Id, False, 0, Comment);
   end Add_Enum_Id;

   -----------------
   -- Add_Enum_Id --
   -----------------

   procedure Add_Enum_Id
     (Enum    : in out Ada_Type_Enum;
      Id      : String;
      Repr    : Unsigned;
      Comment : String := "")
   is
   begin
      Add_Enum_Id_Internal (Enum, Id, True, Repr, Comment);
   end Add_Enum_Id;

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
      Comment : String := "") return Ada_Type_Record
   is
   begin
      return (Id          => To_Unbounded_String (Id),
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
      Low_Id : constant String := Ada.Characters.Handling.To_Lower (Id);
      Unb_Id : Unbounded_String;
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
         Unb_Id := To_Unbounded_String (Id & "_k");
      else
         Unb_Id := To_Unbounded_String (Id);
      end if;

      Rec.Fields.Append
        ((Id          => Unb_Id,
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
        (Id           => To_Unbounded_String (Id),
         Comment      => New_Comment (Comment),
         Aspects      => <>,
         Disc_name    => To_Unbounded_String (Disc_Name),
         Discriminent => Ada_Type_Enum (Disc_Type),
         Fields       => <>);
      Add_Aspect (Ret, "Unchecked_Union");

      for Val of Disc_Type.Values loop
         Ret.Fields.Insert (To_String (Val.Id),
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
      Fields : Record_Field_Vectors.Vector :=
                 Rec.Fields.Element (Enum_Val);
   begin
      Fields.Append
        ((Id          => To_Unbounded_String (Id),
          Typ         => To_Unbounded_String (Typ),
          Offset      => Offset,
          LSB         => LSB,
          MSB         => MSB,
          Has_Default => False,
          Default     => Null_Unbounded_String,
          Comment     => New_Comment (Comment)));
      Rec.Fields.Replace (Enum_Val, Fields);
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
        or else T1.Disc_Name /= T2.Disc_Name
        or else not Is_Similar (T1.Discriminent, T2.Discriminent)
      then
         return False;
      end if;

      for Key of T1.Discriminent.Values loop
         declare
            Key_Str : String renames To_String (Key.Id);
         begin
            if T1.Fields (Key_Str) /= T2.Fields (Key_Str) then
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
     (Id      : String;
      Typ     : String;
      Value   : String;
      Comment : String := "") return Ada_Constant_Value
   is
   begin
      return (Id      => To_Unbounded_String (Id),
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
      return (Id      => To_Unbounded_String (Id),
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
      Elt.Aspects.Append ("Address => System'To_Address(" &
                            To_Hex (Address) & ")");
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
