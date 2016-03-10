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

with DOM.Core.Elements;

package body Base_Types.Register_Properties is

   TAG_SIZE        : constant String := "size";
   TAG_ACCESS      : constant String := "access";
   TAG_PROTECTION  : constant String := "protection";
   TAG_RESET_VALUE : constant String := "resetValue";
   TAG_RESET_MASK  : constant String := "resetMask";

   --------------------------
   -- Is_Register_Property --
   --------------------------

   function Is_Register_Property (Tag : String) return Boolean is
   begin
      return Tag = TAG_SIZE
        or else Tag = TAG_ACCESS
        or else Tag = TAG_PROTECTION
        or else Tag = TAG_RESET_VALUE
        or else Tag = TAG_RESET_MASK;
   end Is_Register_Property;

   ----------------------------
   -- Read_Register_Property --
   ----------------------------

   procedure Read_Register_Property
     (Child : Element;
      Props : in out Register_Properties_T)
   is
      Tag : String renames Elements.Get_Tag_Name (Child);
   begin
      if Tag = TAG_SIZE then
         Props.Size := Get_Value (Child);

      elsif Tag = TAG_ACCESS then
         Props.Reg_Access := Get_Value (Child);

      elsif Tag = TAG_PROTECTION then
         Props.Protection := Get_Value (Child);

      elsif Tag = TAG_RESET_VALUE then
         Props.Reset_Value := Get_Value (Child);

      elsif Tag = TAG_RESET_MASK then
         Props.Reset_Mask := Get_Value (Child);

      end if;
   end Read_Register_Property;

end Base_Types.Register_Properties;
