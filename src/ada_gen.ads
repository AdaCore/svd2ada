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

with System;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Base_Types;             use Base_Types;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Less_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;

--  This package is responsible for dumping the Ada specs.
package Ada_Gen is

   type Ada_Spec is private;

   function New_Spec
     (Name : String;
      Descr : String) return Ada_Spec;

   function New_Child_Spec
     (Name   : String;
      Parent : String;
      Descr  : String) return Ada_Spec;

   procedure Write_Spec
     (Spec       : Ada_Spec;
      Output_Dir : String);

   type Ada_With_Clause is private;
   --  with/use clauses

   type Ada_Comment is tagged private;
   --  A comment

   type Ada_Comment_Box is new Ada_Comment with private;
   --  A comment within a box
   --  such as:
   -- -------------
   -- -- comment --
   -- -------------

   type Ada_Type is abstract tagged private;
   --  Base type for type definition

   function Is_Similar
     (T1, T2 : Ada_Type) return Boolean is abstract;
   --  This should have been private, but visibility rule RM 3.9.3(10) do
   --  not let me do that.

   type Ada_Type_Scalar is new Ada_Type with private;
   --  A scalar type definition

   type Ada_Type_Array is new Ada_Type with private;
   --  An array type definition

   type Ada_Type_Enum is new Ada_Type with private;
   --  An enum type definition

   type Ada_Type_Record is new Ada_Type with private;
   --  A record type definition

   type Ada_Type_Union is new Ada_Type with private;
   --  A union record definition

   type Ada_Constant_Value is private;
   --  Some constant value

   type Ada_Instance is private;
   --  A global variable definition

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_With_Clause);
   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Comment'Class);

   procedure Add_No_Check
     (Spec : in out Ada_Spec;
      Elt  : Ada_Type'Class);
   --  Adds a new type definition.
   --  If a previous type has the same Id and different definition, then
   --  a constraint_error is raised

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : in out Ada_Type'Class);
   --  Adds a new type definition.
   --  If a previous type has the same Id and different definition, then
   --  Elt's id is modified

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Constant_Value);
   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Instance);

   function New_With_Clause
     (Pkg         : String;
      Use_Visible : Boolean := False) return Ada_With_Clause;

   function New_Comment
     (Comment : String) return Ada_Comment;

   function New_Comment_Box
     (Comment : String) return Ada_Comment_Box;

   -----------------------
   -- Type declarations --
   -----------------------

   function Id (Elt : Ada_Type'Class) return Unbounded_String;
   function Id (Elt : Ada_Type'Class) return String;

   procedure Add_Size_Aspect
     (Elt  : in out Ada_Type'Class;
      Size : Unsigned);
   --  Generates a 'with Sixe => value' aspect

   function Get_Size_Aspect (Elt  : Ada_Type'Class) return Unsigned;
   --  Retrieve the size aspect of the type, or 0 if undefined

   procedure Add_Aspect
     (Elt    : in out Ada_Type'Class;
      Aspect : String);
   --  Generic method to add aspects to type definition

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Scalar) return Boolean;
   overriding function Is_Similar
     (T1, T2 : Ada_Type_Array) return Boolean;
   overriding function Is_Similar
     (T1, T2 : Ada_Type_Enum) return Boolean;
   overriding function Is_Similar
     (T1, T2 : Ada_Type_Record) return Boolean;
   overriding function Is_Similar
     (T1, T2 : Ada_Type_Union) return Boolean;

   ------------
   -- Scalar --
   ------------

   function New_Type_Scalar
     (Id      : String;
      Size    : Unsigned;
      Comment : String := "") return Ada_Type_Scalar;

   ------------
   -- Arrays --
   ------------

   function New_Type_Array
     (Id           : String;
      Index_Type   : String;
      Index_First  : Unsigned;
      Index_Last   : Unsigned;
      Element_Type : String;
      Comment      : String := "") return Ada_Type_Array;

   -----------
   -- Enums --
   -----------

   function Get_Boolean return Ada_Type_Enum;

   function New_Type_Enum
     (Id      : String;
      Size    : Unsigned := 0;
      Comment : String := "") return Ada_Type_Enum;

   procedure Add_Enum_Id
     (Enum    : in out Ada_Type_Enum;
      Id      : String;
      Comment : String := "");

   procedure Add_Enum_Id
     (Enum    : in out Ada_Type_Enum;
      Id      : String;
      Repr    : Unsigned;
      Comment : String := "");

   ------------
   -- Record --
   ------------

   function New_Type_Record
     (Id      : String;
      Comment : String := "") return Ada_Type_Record;

   procedure Add_Bit_Order_Aspect
     (Elt   : in out Ada_Type_Record'Class;
      Order : System.Bit_Order);

   procedure Add_Field
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Comment     : String := "");

   procedure Add_Field
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Default     : Unsigned;
      Comment     : String := "");
   --  Adds a new field to the record 'rec'
   --  Id : The ID of the field
   --  Typ : The base type
   --  Offset : The offset in byte from the the base record address
   --  LSB : The least significant bit after offset
   --  MSB : The most significant bit after offset
   --  Desctr: Description of the field

   ----------------
   -- Union Type --
   ----------------

   function New_Type_Union
     (Id        : String;
      Disc_Name : String;
      Disc_Type : Ada_Type_Enum'Class;
      Comment   : String := "") return Ada_Type_Union;

   procedure Add_Field
     (Rec         : in out Ada_Type_Union'Class;
      Enum_Val    : String;
      Id          : String;
      Typ         : String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Comment     : String := "");

   ---------------
   -- Constants --
   ---------------

   function New_Constant_Value
     (Id      : String;
      Typ     : String;
      Value   : String;
      Comment : String := "") return Ada_Constant_Value;

   ---------------
   -- Instances --
   ---------------

   function New_Instance
     (Id           : String;
      Typ          : String;
      Aliased_Inst : Boolean;
      Comment      : String := "") return Ada_Instance;

   procedure Add_Address_Aspect
     (Elt     : in out Ada_Instance;
      Address : Unsigned);

   procedure Add_Aspect
     (Elt    : in out Ada_Instance;
      Aspect : String);

private

   type Ada_With_Clause is record
      Pkg            : Unbounded_String;
      Add_Use_Clause : Boolean;
   end record;

   package With_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Boolean,
      "<"          => Ada.Strings.Less_Case_Insensitive);

   type Spec_Element is abstract tagged null record;

   procedure Added_In_Spec
     (Element : Spec_Element;
      Spec    : in out Ada_Spec) is null;
   --  Called when the element is added to the spec.
   --  It is the element's opportunity to add the required with and use
   --  clauses to the spec

   procedure Dump
     (Element : Spec_Element;
      File    : Ada.Text_IO.File_Type) is abstract;

   package Element_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Spec_Element'Class);

   type Ada_Comment is new Spec_Element with record
      Comment : Unbounded_String;
   end record;

   function Is_Empty (Comment : Ada_Comment) return Boolean;

   procedure Dump
     (Comment : Ada_Comment;
      F       : Ada.Text_IO.File_Type;
      Indent  : Natural;
      Inline  : Boolean);

   overriding procedure Dump
     (Element : Ada_Comment;
      File    : Ada.Text_IO.File_Type);

   type Ada_Comment_Box is new Ada_Comment with null record;

   overriding procedure Dump
     (Element : Ada_Comment_Box;
      File    : Ada.Text_IO.File_Type);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   type Ada_Type is abstract new Spec_Element with record
      Id      : Unbounded_String;
      Comment : Ada_Comment;
      Aspects : String_Vectors.Vector;
   end record;

   overriding procedure Dump
     (Element : Ada_Type;
      File    : Ada.Text_IO.File_Type) is null;

   type Ada_Type_Scalar is new Ada_Type with record
      Size : Unsigned;
   end record;

   overriding procedure Added_In_Spec
     (Element : Ada_Type_Scalar;
      Spec    : in out Ada_Spec);

   overriding procedure Dump
     (Element : Ada_Type_Scalar;
      File    : Ada.Text_IO.File_Type);

   type Ada_Type_Array is new Ada_Type with record
      Index_Type   : Unbounded_String;
      Index_First  : Unsigned;
      Index_Last   : Unsigned;
      Element_Type : Unbounded_String;
   end record;

   overriding procedure Dump
     (Element : Ada_Type_Array;
      File    : Ada.Text_IO.File_Type);

   type Ada_Enum_Value is record
      Id       : Unbounded_String;
      Has_Repr : Boolean;
      Repr     : Unsigned;
      Comment  : Ada_Comment;
   end record;

   function "=" (V1, V2 : Ada_Enum_Value) return Boolean;

   package Enum_Value_Vectors is new Ada.Containers.Vectors
     (Positive, Ada_Enum_Value);

   type Ada_Type_Enum is new Ada_Type with record
      Values : Enum_Value_Vectors.Vector;
   end record;

   overriding procedure Dump
     (Element : Ada_Type_Enum;
      File    : Ada.Text_IO.File_Type);

   type Record_Field is record
      Id          : Unbounded_String;
      Typ         : Unbounded_String;
      Offset      : Unsigned;
      LSB         : Unsigned;
      MSB         : Unsigned;
      Has_Default : Boolean;
      Default     : Unsigned;
      Comment     : Ada_Comment;
   end record;

   function "=" (R1, R2 : Record_Field) return Boolean;

   package Record_Field_Vectors is new Ada.Containers.Vectors
     (Positive, Record_Field);

   type Ada_Type_Record is new Ada_Type with record
      Fields      : Record_Field_Vectors.Vector;
      Need_System : Boolean := False;
   end record;

   overriding procedure Added_In_Spec
     (Element : Ada_Type_Record;
      Spec    : in out Ada_Spec);

   overriding procedure Dump
     (Element : Ada_Type_Record;
      File    : Ada.Text_IO.File_Type);

   package Record_Field_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String,
      Record_Field_Vectors.Vector,
      Ada.Strings.Hash_Case_Insensitive,
      Ada.Strings.Equal_Case_Insensitive,
      Record_Field_Vectors."=");

   type Ada_Type_Union is new Ada_Type with record
      Disc_Name    : Unbounded_String;
      Discriminent : Ada_Type_Enum;
      Fields       : Record_Field_Maps.Map;
   end record;

   overriding procedure Dump
     (Element : Ada_Type_Union;
      File    : Ada.Text_IO.File_Type);

   type Ada_Constant_Value is new Spec_Element with record
      Id      : Unbounded_String;
      Typ     : Unbounded_String;
      Value   : Unbounded_String;
      Comment : Ada_Comment;
   end record;

   overriding procedure Dump
     (Element : Ada_Constant_Value;
      File    : Ada.Text_IO.File_Type);

   type Ada_Instance is new Spec_Element with record
      Id      : Unbounded_String;
      Typ     : Unbounded_String;
      Aliasd  : Boolean;
      Comment : Ada_Comment;
      Aspects : String_Vectors.Vector;
   end record;

   overriding procedure Dump
     (Element : Ada_Instance;
      File    : Ada.Text_IO.File_Type);

   overriding procedure Added_In_Spec
     (Element : Ada_Instance;
      Spec    : in out Ada_Spec);

   type Ada_Spec is record
      With_Clauses : With_Maps.Map;
      Id           : Unbounded_String;
      Comment      : Ada_Comment;
      Elements     : Element_Vectors.Vector;
   end record;

   function File_Name (Spec : Ada_Spec) return String;

end Ada_Gen;
