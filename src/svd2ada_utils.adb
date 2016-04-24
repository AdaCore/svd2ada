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

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Case_Util;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

package body SVD2Ada_Utils is

   G_Use_Boolean : Boolean := False;
   G_Types_Pkg   : Unbounded_String := Null_Unbounded_String;

   -------------------------
   -- Executable_Location --
   -------------------------

   --  Executable_Location is extracted from the gnatcoll library
   function Executable_Location return String
   is
      Exec_Name : constant String := Ada.Command_Line.Command_Name;

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceeded by the absolute or relative
      --  path, e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc". Returns the absolute
      --  or relative directory where "bin" lies (in the example "C:\usr"
      --  or ".."). If the executable is not a "bin" directory, return "".

      function Is_Directory_Separator (C : Character) return Boolean is
        (C = Directory_Separator or else C = '/');

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  := GNAT.OS_Lib.Normalize_Pathname
            (S, Resolve_Links => True);
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
            return Exec (Exec'First .. Path_Last)
               & GNAT.OS_Lib.Directory_Separator;

         else
            --  Skip bin/, but keep the last directory separator
            return Exec (Exec'First .. Path_Last - 3);
         end if;
      end Get_Install_Dir;

   --  Beginning of Executable_Location

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Is_Directory_Separator (Exec_Name (J)) then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If you are here, the user has typed the executable name with no
      --  directory prefix.
      --  There is a potential issue here (see K112-046) where GNAT.OS_Lib
      --  will in fact return any non-executable file found in the PATH,
      --  whereas shells only consider executable files. As a result, the
      --  user might end up with a wrong directory, not matching the one
      --  found by the shell.

      declare
         Ex  : GNAT.OS_Lib.String_Access :=
                 GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
         Dir : constant String := Get_Install_Dir (Ex.all);
      begin
         Free (Ex);
         return Dir;
      end;
   end Executable_Location;

   -----------------------------
   -- Set_Use_Boolean_For_Bit --
   -----------------------------

   procedure Set_Use_Boolean_For_Bit (Value : Boolean)
   is
   begin
      G_Use_Boolean := Value;
   end Set_Use_Boolean_For_Bit;

   -------------------------
   -- Use_Boolean_For_Bit --
   -------------------------

   function Use_Boolean_For_Bit return Boolean
   is
   begin
      return G_Use_Boolean;
   end Use_Boolean_For_Bit;

   ----------------------------
   -- Set_Base_Types_Package --
   ----------------------------

   procedure Set_Base_Types_Package (Value : String)
   is
   begin
      G_Types_Pkg := To_Unbounded_String (Value);
   end Set_Base_Types_Package;

   ------------------------
   -- Base_Types_Package --
   ------------------------

   function Base_Types_Package return String
   is
   begin
      return To_String (G_Types_Pkg);
   end Base_Types_Package;

   ---------------------------------
   -- External_Base_Types_Package --
   ---------------------------------

   function External_Base_Types_Package return Boolean
   is
   begin
      return G_Types_Pkg /= Null_Unbounded_String;
   end External_Base_Types_Package;

end SVD2Ada_Utils;
