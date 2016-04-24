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

package SVD2Ada_Utils is

   function Executable_Location return String;

   procedure Set_Use_Boolean_For_Bit (Value : Boolean);
   function Use_Boolean_For_Bit return Boolean;

   procedure Set_Base_Types_Package (Value : String);
   function Base_Types_Package return String;
   function External_Base_Types_Package return Boolean;

   procedure Set_Root_Package (Value : String);
   function Root_Package return String;
   function In_Runtime return Boolean;

end SVD2Ada_Utils;
