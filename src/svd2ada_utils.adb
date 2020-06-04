------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-200, AdaCore                      --
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

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Case_Util;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

package body SVD2Ada_Utils is

   G_Use_Boolean         : Boolean := False;
   G_Types_Pkg           : Unbounded_String := Null_Unbounded_String;
   G_Root_Pkg            : Unbounded_String := Null_Unbounded_String;
   G_Use_UInt            : Boolean := False;
   G_Gen_Arrays          : Boolean := True;
   G_No_VFA_On_Reg_Types : Boolean := False;
   G_Gen_IRQ_Support     : Boolean := False;
   G_Gen_UInt_Subtype    : Boolean := True;
   G_Gen_Fields_Default  : Boolean := True;

   function Installation_Dir (Exec_with_Path : String) return String;
   --  Exec_with_Path is the executable name preceeded by the absolute or
   --  relative path, e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc". Returns
   --  the absolute or relative directory where "bin" lies (in the example
   --  "C:\usr" or ".."). If the executable is not a "bin" directory, return
   --  "".

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean is
     (C = Directory_Separator or else C = '/');

   -------------------------
   -- Executable_Location --
   -------------------------

   --  Executable_Location is extracted from the gnatcoll library
   function Executable_Location return String is
      Exec_Name : constant String := Ada.Command_Line.Command_Name;
   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Is_Directory_Separator (Exec_Name (J)) then
            return Installation_Dir (Exec_Name);
         end if;
      end loop;

      --  If we are here, the user has typed the executable name with no
      --  directory prefix.
      --  There is a potential issue here (see K112-046) where GNAT.OS_Lib
      --  will in fact return any non-executable file found in the PATH,
      --  whereas shells only consider executable files. As a result, the
      --  user might end up with a wrong directory, not matching the one
      --  found by the shell.

      declare
         Ex  : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
         Dir : constant String := Installation_Dir (Ex.all);
      begin
         Free (Ex);
         return Dir;
      end;
   end Executable_Location;

   -----------------------------
   -- Set_Use_Boolean_For_Bit --
   -----------------------------

   procedure Set_Use_Boolean_For_Bit (Value : Boolean) is
   begin
      G_Use_Boolean := Value;
   end Set_Use_Boolean_For_Bit;

   -------------------------
   -- Use_Boolean_For_Bit --
   -------------------------

   function Use_Boolean_For_Bit return Boolean is
   begin
      return G_Use_Boolean;
   end Use_Boolean_For_Bit;

   ------------------
   -- Set_Use_UInt --
   ------------------

   procedure Set_Use_UInt (Value : Boolean) is
   begin
      G_Use_UInt := Value;
   end Set_Use_UInt;

   ---------------------
   -- Use_UInt_Always --
   ---------------------

   function Use_UInt_Always return Boolean is
   begin
      return G_Use_UInt;
   end Use_UInt_Always;

   -------------------------
   -- Set_No_UInt_Subtype --
   -------------------------

   procedure Set_No_UInt_Subtype (Value : Boolean) is
   begin
      G_Gen_UInt_Subtype := not Value;
   end Set_No_UInt_Subtype;

   ----------------------
   -- Gen_UInt_Subtype --
   ----------------------

   function Gen_UInt_Subtype return Boolean is
   begin
      return G_Gen_UInt_Subtype;
   end Gen_UInt_Subtype;

   ---------------------
   -- Set_No_Defaults --
   ---------------------

   procedure Set_No_Defaults (Value : Boolean) is
   begin
      G_Gen_Fields_Default := not Value;
   end Set_No_Defaults;

   ------------------------
   -- Gen_Fields_Default --
   ------------------------

   function Gen_Fields_Default return Boolean is
   begin
      return G_Gen_Fields_Default;
   end Gen_Fields_Default;

   ----------------------------
   -- Set_Base_Types_Package --
   ----------------------------

   procedure Set_Base_Types_Package (Value : String) is
   begin
      G_Types_Pkg := To_Unbounded_String (Value);
   end Set_Base_Types_Package;

   ------------------------
   -- Base_Types_Package --
   ------------------------

   function Base_Types_Package return String is
   begin
      return To_String (G_Types_Pkg);
   end Base_Types_Package;

   ---------------------------------
   -- External_Base_Types_Package --
   ---------------------------------

   function External_Base_Types_Package return Boolean is
   begin
      return G_Types_Pkg /= Null_Unbounded_String;
   end External_Base_Types_Package;

   ----------------------
   -- Set_Root_Package --
   ----------------------

   procedure Set_Root_Package (Value : String) is
   begin
      G_Root_Pkg := To_Unbounded_String (Value);
   end Set_Root_Package;

   ------------------
   -- Root_Package --
   ------------------

   function Root_Package return String is
   begin
      return To_String (G_Root_Pkg);
   end Root_Package;

   ----------------
   -- In_Runtime --
   ----------------

   function In_Runtime return Boolean is
      Intf : constant String := "Interfaces.";
      Root : constant String := Root_Package;
   begin
      if Root'Length <= Intf'Length then
         return False;
      elsif Root (Root'First .. Root'First + Intf'Length - 1) /= Intf then
         return False;
      else
         return True;
      end if;
   end In_Runtime;

   -----------------------------
   -- Set_No_VFA_On_Reg_Types --
   -----------------------------

   procedure Set_No_VFA_On_Reg_Types (Value : Boolean) is
   begin
      G_No_VFA_On_Reg_Types := Value;
   end Set_No_VFA_On_Reg_Types;

   -------------------------
   -- No_VFA_On_Reg_Types --
   -------------------------

   function No_VFA_On_Reg_Types return Boolean is
   begin
      return G_No_VFA_On_Reg_Types;
   end No_VFA_On_Reg_Types;

   --------------------
   -- Set_Gen_Arrays --
   --------------------

   procedure Set_Gen_Arrays (Value : Boolean) is
   begin
      G_Gen_Arrays := Value;
   end Set_Gen_Arrays;

   ----------------
   -- Gen_Arrays --
   ----------------

   function Gen_Arrays return Boolean is
   begin
      return G_Gen_Arrays;
   end Gen_Arrays;

   -------------------------
   -- Set_Gen_IRQ_Support --
   -------------------------

   procedure Set_Gen_IRQ_Support (Value : Boolean) is
   begin
      G_Gen_IRQ_Support := Value;
   end Set_Gen_IRQ_Support;

   ---------------------
   -- Gen_IRQ_Support --
   ---------------------

   function Gen_IRQ_Support return Boolean is
   begin
      return G_Gen_IRQ_Support or else In_Runtime;
   end Gen_IRQ_Support;

   ----------------------
   -- Installation_Dir --
   ----------------------

   function Installation_Dir (Exec_with_Path : String) return String is
      Exec      : String := GNAT.OS_Lib.Normalize_Pathname (Exec_with_Path, Resolve_Links => True);
      Path_Last : Integer := 0;
   begin
      for J in reverse Exec'Range loop
         if Is_Directory_Separator (Exec (J)) then
            Path_Last := J - 1;
            exit;
         end if;
      end loop;

      if Path_Last >= Exec'First + 2 then
         GNAT.Case_Util.To_Lower (Exec (Path_Last - 2 .. Path_Last));
      end if;

      --  If we are not in a bin/ directory

      if Path_Last < Exec'First + 2
        or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
        or else (Path_Last - 3 >= Exec'First
                 and then not Is_Directory_Separator (Exec (Path_Last - 3)))
      then
         return Exec (Exec'First .. Path_Last) & GNAT.OS_Lib.Directory_Separator;
      else
         --  Skip bin/, but keep the last directory separator
         return Exec (Exec'First .. Path_Last - 3);
      end if;
   end Installation_Dir;

   ----------------------
   -- Is_Reserved_Word --
   ----------------------

   function Is_Reserved_Word (Word : String) return Boolean is
    (Word = "abort"
     or else Word = "abs"
     or else Word = "abstract"
     or else Word = "accept"
     or else Word = "access"
     or else Word = "aliased"
     or else Word = "all"
     or else Word = "and"
     or else Word = "array"
     or else Word = "at"
     or else Word = "begin"
     or else Word = "body"
     or else Word = "case"
     or else Word = "constant"
     or else Word = "declare"
     or else Word = "delay"
     or else Word = "delta"
     or else Word = "digits"
     or else Word = "do"
     or else Word = "else"
     or else Word = "elsif"
     or else Word = "end"
     or else Word = "entry"
     or else Word = "exception"
     or else Word = "exit"
     or else Word = "for"
     or else Word = "function"
     or else Word = "generic"
     or else Word = "goto"
     or else Word = "if"
     or else Word = "in"
     or else Word = "interface"
     or else Word = "is"
     or else Word = "limited"
     or else Word = "loop"
     or else Word = "mod"
     or else Word = "new"
     or else Word = "not"
     or else Word = "null"
     or else Word = "of"
     or else Word = "or"
     or else Word = "others"
     or else Word = "out"
     or else Word = "overriding"
     or else Word = "package"
     or else Word = "pragma"
     or else Word = "private"
     or else Word = "procedure"
     or else Word = "protected"
     or else Word = "raise"
     or else Word = "range"
     or else Word = "record"
     or else Word = "rem"
     or else Word = "renames"
     or else Word = "requeue"
     or else Word = "return"
     or else Word = "reverse"
     or else Word = "select"
     or else Word = "separate"
     or else Word = "some"
     or else Word = "subtype"
     or else Word = "synchronized"
     or else Word = "tagged"
     or else Word = "task"
     or else Word = "terminate"
     or else Word = "then"
     or else Word = "type"
     or else Word = "until"
     or else Word = "use"
     or else Word = "when"
     or else Word = "while"
     or else Word = "with"
     or else Word = "xor");

end SVD2Ada_Utils;
