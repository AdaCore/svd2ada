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

with DOM.Core; use DOM.Core;

--  Decodes the register properties group from an SVD element.
--  This group is used in Device, Peripherals and Registers descriptors.
package Base_Types.Register_Properties is

   type Register_Properties_T is record
      Size        : Unsigned;
      Reg_Access  : Access_Type;
      Protection  : Protection_Type;
      Reset_Value : Unsigned;
      Reset_Mask  : Unsigned;
   end record;

   Null_Register_Property : constant Register_Properties_T :=
                              (Size        => 0,
                               Reg_Access  => Undefined_Access,
                               Protection  => Undefined_Protection,
                               Reset_Value => 0,
                               Reset_Mask  => 0);

   function Is_Register_Property (Tag : String) return Boolean;

   procedure Read_Register_Property
     (Child : Element;
      Props : in out Register_Properties_T);

end Base_Types.Register_Properties;
