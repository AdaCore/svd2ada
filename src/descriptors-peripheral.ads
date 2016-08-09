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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;          use Ada.Strings;

with DOM.Core;

with Base_Types;                     use Base_Types;
with Base_Types.Register_Properties; use Base_Types.Register_Properties;

with Descriptors.Cluster;            use Descriptors.Cluster;
with Descriptors.Register;           use Descriptors.Register;

--  Decodes and then dumps the <peripheral> elements of the SVD file.
package Descriptors.Peripheral is

   type Peripheral_T;
   type Peripheral_Access is access all Peripheral_T;

   type Peripheral_Db is interface;

   function Get_Peripheral
     (Db     : Peripheral_Db;
      XML_Id : String) return Peripheral_Access is abstract;

   type Peripheral_T is new Register_Db and Cluster_Db with record
      Name            : Unbounded.Unbounded_String;
      Type_Name       : Unbounded.Unbounded_String;
      Version         : Unbounded.Unbounded_String;
      Description     : Unbounded.Unbounded_String;
      Group_Name      : Unbounded.Unbounded_String;
      Prepend_To_Name : Unbounded.Unbounded_String;
      Append_To_Name  : Unbounded.Unbounded_String;
      Base_Address    : Unsigned := 0;
      Reg_Properties  : Register_Properties_T := Null_Register_Property;
      Address_Blocks  : Address_Block_Vectors.Vector;
      Interrupts      : Interrupt_Vectors.Vector;
      Content         : Peripheral_Element_Vectors.Vector;
   end record;

   overriding function Get_Register
     (Db     : Peripheral_T;
      XML_Id : String) return Register_Access;

   overriding function Get_Cluster
     (Db     : Peripheral_T;
      XML_Id : String) return Cluster_Access;

   function Read_Peripheral
     (Elt            : DOM.Core.Element;
      Reg_Properties : Register_Properties_T;
      Periph_Db      : Peripheral_Db'Class) return Peripheral_T;

   procedure Dump
     (Peripheral : in out Peripheral_T;
      Dev_Name   : String;
      Output_Dir : String);

   package Peripheral_Vectors is new Ada.Containers.Vectors
     (Positive, Peripheral_Access);

   procedure Dump
     (Group      : Peripheral_Vectors.Vector;
      Dev_Name   : String;
      Output_Dir : String);

end Descriptors.Peripheral;
