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

with Ada.Strings.Unbounded;          use Ada.Strings;
with Ada.Strings.Hash;

with DOM.Core;

with Base_Types;                     use Base_Types;
with Base_Types.Register_Properties;

with Peripheral_Descriptor;          use Peripheral_Descriptor;

--  Decodes and dumps the <device> elements of the SVD file. This is the
--  main entry point for decoding this file.
package Device_Descriptor is

   type Device_T is record
      Name              : Unbounded.Unbounded_String;
      Version           : Unbounded.Unbounded_String;
      Description       : Unbounded.Unbounded_String;

      --  BUS interface properties:
      --  adressable unit
      Address_Unit_Bits : Unsigned := 0;
      --  maximum data bit width accessbile within a single transfer
      Width             : Unsigned := 0;

      --  REGISTERS properties
      Reg_Properties    : Register_Properties.Register_Properties_T;

      Peripherals       : Peripheral_Vectors.Vector;
   end record;

   function Read_Device (Elt : DOM.Core.Element) return Device_T;

   procedure Dump (Device : Device_T);

end Device_Descriptor;
