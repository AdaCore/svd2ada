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

with Ada.Strings.Unbounded;          use Ada.Strings;

with DOM.Core;

with Base_Types;                     use Base_Types;
with Base_Types.Register_Properties;

with Descriptors.Peripheral;         use Descriptors.Peripheral;

--  Decodes and dumps the <device> elements of the SVD file. This is the
--  main entry point for decoding this file.
package Descriptors.Device is

   type Device_T is new Peripheral_Db with record
      Name              : Unbounded.Unbounded_String;
      Version           : Unbounded.Unbounded_String;
      Description       : Unbounded.Unbounded_String;
      Short_Desc        : Unbounded.Unbounded_String;

      --  BUS interface properties:
      --  adressable unit
      Address_Unit_Bits : Unsigned := 0;
      --  maximum data bit width accessbile within a single transfer
      Width             : Natural := 0;

      Has_FPU           : Boolean := True;

      --  REGISTERS properties
      Reg_Properties    : Register_Properties.Register_Properties_T;

      Peripherals       : Peripheral_Vectors.Vector;
   end record;

   function Read_Device (Elt      : DOM.Core.Element;
                         Pkg_Name : String) return Device_T;

   overriding function Get_Peripheral
     (Db     : Device_T;
      XML_Id : String) return Peripheral_Access;

   procedure Dump
     (Device     : Device_T;
      Output_Dir : String);

end Descriptors.Device;
