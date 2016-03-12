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

with DOM.Core;

with Ada_Gen;   use Ada_Gen;

package Ada_Gen_Helpers is

   procedure Read (Elt : DOM.Core.Element);

   type Peripheral_Helper is private;
   Null_Helper : constant Peripheral_Helper;

   function Get_Peripheral_Helper (Name : String) return Peripheral_Helper;

   function Has_Peripheral_Helper (Name : String) return Boolean
   is (Get_Peripheral_Helper (Name) /= Null_Helper);

   function Get_Discriminent_Type
     (Helper : Peripheral_Helper) return Ada_Type_Enum;

   function Get_Discriminent_Name
     (Helper : Peripheral_Helper) return String;

   function Get_Discriminent_Value
     (Helper   : Peripheral_Helper;
      Reg_Name : String) return String;

private

   type Peripheral;

   type Peripheral_Helper is access all Peripheral;

   Null_Helper : constant Peripheral_Helper := null;

end Ada_Gen_Helpers;
