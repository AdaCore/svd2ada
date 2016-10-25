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

with DOM.Core; use DOM.Core;

--  Decodes the register properties group from an SVD element.
--  This group is used in Device, Peripherals and Registers descriptors.
package Base_Types.Register_Properties is

   type Register_Properties_T is record
      Size        : Natural := 0;
      Reg_Access  : Access_Type := Read_Write;
      Protection  : Protection_Type := Undefined_Protection;
      Reset_Value : Unsigned := 0;
      Reset_Mask  : Unsigned := 0;
   end record;

   Null_Register_Property : constant Register_Properties_T := (others => <>);

   function Is_Register_Property (Tag : String) return Boolean;

   procedure Read_Register_Property
     (Child : Element;
      Props : in out Register_Properties_T);

end Base_Types.Register_Properties;
