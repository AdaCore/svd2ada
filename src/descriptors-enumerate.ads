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

package Descriptors.Enumerate is

   type Enumerate_Value is record
      Name      : Unbounded.Unbounded_String;
      Descr     : Unbounded.Unbounded_String;
      Value     : Unsigned;
      IsDefault : Boolean := False;
   end record;

   package Enumerate_Values_Vectors is new Ada.Containers.Vectors
     (Positive, Enumerate_Value);

   type Enumerate_T is record
      Name   : Unbounded.Unbounded_String;
      Values : Enumerate_Values_Vectors.Vector;
      Usage  : Enum_Usage_Type := Read_Write;
   end record;

   package Enumerate_Vectors is new Ada.Containers.Vectors
     (Positive, Enumerate_T);

   function Read_Enumerate
     (Elt        : DOM.Core.Element;
      Vector     : Enumerate_Vectors.Vector;
      Write_Only : Boolean)
      return Enumerate_T;

end Descriptors.Enumerate;
