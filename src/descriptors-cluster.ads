------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2019, AdaCore                      --
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
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;

with DOM.Core;

with Ada_Gen;                        use Ada_Gen;
with Base_Types;                     use Base_Types;
with Base_Types.Register_Properties; use Base_Types.Register_Properties;

with Descriptors.Register;           use Descriptors.Register;

--  Decodes and then dumps the <cluster> elements of the SVD file.
package Descriptors.Cluster is

   type Cluster_T;
   type Cluster_Access is access all Cluster_T;

   type Cluster_Db is interface;

   function Get_Cluster
     (Db     : Cluster_Db;
      XML_Id : String) return Cluster_Access is abstract;

   type Peripheral_Element_Type is (Register_Element, Cluster_Element);

   type Peripheral_Element (Kind : Peripheral_Element_Type := Register_Element)
   is record
      case Kind is
         when Register_Element =>
            Reg     : Register_Access;
         when Cluster_Element =>
            Cluster : Cluster_Access;
      end case;
   end record;

   function "+" (Reg : Register_Access) return Peripheral_Element;
   function "+" (Cluster : Cluster_Access) return Peripheral_Element;
   function Equal (E1, E2 : Peripheral_Element) return Boolean;
   function Deep_Copy (E : Peripheral_Element) return Peripheral_Element;
   function Address_Offset (Elt : Peripheral_Element) return Natural;
   function Is_Overlapping (Elt : Peripheral_Element) return Boolean;

   package Peripheral_Element_Vectors is new Ada.Containers.Vectors
     (Positive, Peripheral_Element, Equal);

   procedure Process_Overlapping_Registers
     (Reg_Set : Peripheral_Element_Vectors.Vector;
      Found   : out Boolean);
   --  Detect any overlapping registers and, for those that overlap together,
   --  mark them as such and determine a common name. Found indicates whether
   --  any overlapping registers were identified.

   procedure Find_Common_Types (Elts : Peripheral_Element_Vectors.Vector);

   function Get_Discriminent_Type
     (Reg_Set   : Peripheral_Element_Vectors.Vector;
      Spec      : in out Ada_Gen.Ada_Spec;
      Type_Name : String) return Ada_Type_Enum;

   procedure Dump_Peripheral_Elements
     (Parent : in out Ada_Gen.Ada_Type_Record'Class;
      Elts   : Peripheral_Element_Vectors.Vector);

   type Cluster_T is new Register_Db and Cluster_Db with record
      Name            : Unbounded_String;
      Xml_Id          : Unbounded_String;
      Type_Name       : Unbounded_String;

      --  When two clusters are identical, the second register will not
      --  generate an Ada type. We reference the first register here to
      --  keep track of the type name.
      Type_Holder     : Cluster_Access := null;

      Ada_Type        : Type_Holders.Holder;
      Description     : Unbounded_String;

      --  When two registers are at the same location, we specify a shared
      --  common union type name here to support this overlapping
      Is_Overlapping   : Boolean := False;
      Overlap_Suffix   : Unbounded_String;

      Address_Offset  : Natural;
      Reg_Properties  : Register_Properties_T := Null_Register_Property;
      Dim             : Positive := 1;
      Dim_Increment   : Natural := 4;
      Dim_Index       : Unbounded_String;
      Content         : Peripheral_Element_Vectors.Vector;
   end record;

   function Equal (C1, C2 : Cluster_Access) return Boolean;

   function Read_Cluster
     (Elt            : DOM.Core.Element;
      Prepend        : Unbounded_String;
      Append         : Unbounded_String;
      Reg_Properties : Register_Properties_T;
      Db             : Cluster_Db'Class) return Cluster_T;

   function Deep_Copy (Cluster : Cluster_Access) return Cluster_Access;

   overriding function Get_Register
     (Db     : Cluster_T;
      XML_Id : String) return Register_Access;

   overriding function Get_Cluster
     (Db     : Cluster_T;
      XML_Id : String) return Cluster_Access;

   function Get_Size (Cluster : Cluster_T) return Positive;

   procedure Dump
     (Spec    : in out Ada_Gen.Ada_Spec;
      Cluster : Cluster_Access);

private

   function Name (Elt : Peripheral_Element) return Unbounded_String
   is (case Elt.Kind is
          when Register_Element => Elt.Reg.Name,
          when Cluster_Element => Elt.Cluster.Name);

   function Address_Offset (Elt : Peripheral_Element) return Natural
   is (case Elt.Kind is
          when Register_Element => Elt.Reg.Address_Offset,
          when Cluster_Element => Elt.Cluster.Address_Offset);

   function Is_Overlapping (Elt : Peripheral_Element) return Boolean
   is (case Elt.Kind is
          when Register_Element => Elt.Reg.Is_Overlapping,
          when Cluster_Element => Elt.Cluster.Is_Overlapping);

   function Overlap_Suffix
     (Elt : Peripheral_Element) return Unbounded_String
   is (case Elt.Kind is
          when Register_Element => Elt.Reg.Overlap_Suffix,
          when Cluster_Element => Elt.Cluster.Overlap_Suffix);

   function "+" (Reg : Register_Access) return Peripheral_Element
   is ((Register_Element, Reg));

   function "+" (Cluster : Cluster_Access) return Peripheral_Element
   is ((Cluster_Element, Cluster));

   function Equal (E1, E2 : Peripheral_Element) return Boolean is
     (E1.Kind = E2.Kind and then
        (case E1.Kind is
            when Register_Element => Equal (E1.Reg, E2.Reg),
            when Cluster_Element  => Equal (E1.Cluster, E2.Cluster)));

   function Deep_Copy (E : Peripheral_Element) return Peripheral_Element is
     (case E.Kind is
         when Register_Element =>
           (Register_Element, new Register_T'(E.Reg.all)),
         when Cluster_Element =>
           (Cluster_Element, Deep_Copy (E.Cluster)));

end Descriptors.Cluster;
