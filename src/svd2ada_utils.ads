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

package SVD2Ada_Utils is

   function Executable_Location return String;

   procedure Set_Use_Boolean_For_Bit (Value : Boolean);

   function Use_Boolean_For_Bit return Boolean;

   procedure Set_Use_UInt (Value : Boolean);

   function Use_UInt_Always return Boolean;

   procedure Set_No_UInt_Subtype (Value : Boolean);

   function Gen_UInt_Subtype return Boolean;

   procedure Set_No_Defaults (Value : Boolean);

   function Gen_Fields_Default return Boolean;

   procedure Set_Base_Types_Package (Value : String);

   function Base_Types_Package return String;

   function External_Base_Types_Package return Boolean;

   procedure Set_Root_Package (Value : String);

   function Root_Package return String;

   function In_Runtime return Boolean;

   procedure Set_No_VFA_On_Reg_Types (Value : Boolean);

   function No_VFA_On_Reg_Types return Boolean;
   --  Whether the Volatile_Full_Access pragma is applied on the base
   --  register type or on the fields of peripherals/clusters.
   --  This has impacts on the way arrays of registers are generated:
   --  until we have something like Volatile_Full_Access_Components pragma
   --  for arrays, there's no way to indicate to the compiler that the
   --  components of an array should be VFA, unless we apply the pragma to the
   --  base register type.

   procedure Set_Gen_Arrays (Value : Boolean);

   function Gen_Arrays return Boolean;
   --  If Gen_Arrays is set, the register fields with similar names and types
   --  only differing by a numbered suffix are generated as an array.

   procedure Set_Gen_IRQ_Support (Value : Boolean);

   function Gen_IRQ_Support return Boolean;
   --  Whether to generate trap handler assembly file and Interrupts.Names
   --  package

   function Is_Reserved_Word (Word : String) return Boolean;
   --  Check if Word one of the Ada languuage reserved words.
   --  Word is required to be in lower case.

end SVD2Ada_Utils;
