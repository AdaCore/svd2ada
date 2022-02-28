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

   procedure Set_Input_File_Name (FName : String);

   procedure Set_License_Text (Text : Unbounded_String);

   type Ada_Spec is private;

   -----------------
   -- With clause --
   -----------------

   type Ada_With_Clause is private;
   --  with/use clauses

   function New_With_Clause
     (Pkg         : String;
      Use_Visible : Boolean := False) return Ada_With_Clause;

   procedure Add_Global_With (Spec : String);
   --  Adds a with/use clause to this Spec on all generated spec from now on.

   -------------
   -- Comment --
   -------------

   type Ada_Comment is private;
   --  A comment

   function New_Comment
     (Comment : String;
      Strip   : Boolean) return Ada_Comment;

   -----------------
   -- Comment box --
   -----------------

   type Ada_Comment_Box is private;
   --  A comment within a box
   --  such as:
   -- -------------
   -- -- comment --
   -- -------------

   function New_Comment_Box
     (Comment : String) return Ada_Comment_Box;

   ------------
   -- Pragma --
   ------------

   type Ada_Pragma is private;

   function New_Pragma
     (Identifier : String;
      Comment    : String := "") return Ada_Pragma;

   ----------------------
   -- Type declaration --
   ----------------------

   type Ada_Type is abstract tagged private;
   --  Base type for type definition

   function Is_Similar
     (T1, T2 : Ada_Type) return Boolean is abstract;
   --  This should have been private, but visibility rule RM 3.9.3(10) do
   --  not let me do that.

   function Id (Elt : Ada_Type'Class) return Unbounded_String;
   function Id (Elt : Ada_Type'Class) return String;

   procedure Add_Aspect
     (Elt         : in out Ada_Type'Class;
      Aspect_Mark : String);
   --  General routine to add an aspect (without a value specified) to a type definition

   procedure Add_Aspect
     (Elt         : in out Ada_Type'Class;
      Aspect_Mark : String;
      Value       : Integer);
   --  General routine to add an aspect with a numeric value to a type
   --  definition. It is OK to add the same aspect with the same numeric
   --  value more than once to a given type.

   function Aspect_Value
     (Elt         : Ada_Type'Class;
      Aspect_Mark : String)
      return Integer;
   --  General routine to get the numeric value specifed for the aspect named
   --  Aspect_Mark. Returns 0 if the aspect is not currently specified for Elt.

   procedure Add_Size_Aspect
     (Elt  : in out Ada_Type'Class;
      Size : Natural);
   --  Generates a 'with Size => value' aspect.

   function Get_Size_Aspect (Elt  : Ada_Type'Class) return Unsigned;
   --  Retrieve the Size aspect of the type, or 0 if undefined.

   procedure Add_Object_Size_Aspect
     (Elt  : in out Ada_Type'Class;
      Size : Natural);
   --  Generates a 'with Object_Size => value' aspect.

   function Get_Object_Size_Aspect (Elt  : Ada_Type'Class) return Unsigned;
   --  Retrieve the Object_Size aspect of the type, or 0 if undefined.

   -------------------
   -- Type: scalars --
   -------------------

   type Ada_Type_Scalar is new Ada_Type with private;
   --  A scalar type definition

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Scalar) return Boolean;

   function New_Type_Scalar
     (Id      : String;
      Size    : Natural;
      Comment : String := "") return Ada_Type_Scalar;

   function Target_Type
     (Size            : Natural;
      Fully_Qualified : Boolean := True) return Ada_Type'Class;

   type Ada_Subtype_Scalar is new Ada_Type with private;
   --  A scalar subtype definition

   function New_Subype_Scalar
     (Id      : String;
      Typ     : Ada_Type'Class;
      Comment : String := "") return Ada_Subtype_Scalar;

   overriding function Is_Similar
     (T1, T2 : Ada_Subtype_Scalar) return Boolean;

   ------------------
   -- Type: arrays --
   ------------------

   type Ada_Type_Array is new Ada_Type with private;
   --  An array type definition

   function New_Type_Array
     (Id           : String;
      Index_Type   : String;
      Index_First  : Natural;
      Index_Last   : Natural;
      Element_Type : Ada_Type'Class;
      Comment      : String := "") return Ada_Type_Array;

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Array) return Boolean;

   -----------------
   -- Type: enums --
   -----------------

   type Ada_Type_Enum is new Ada_Type with private;
   --  An enum type definition

   type Ada_Enum_Value is private;
   --  A particular value of an enum type

   function Get_Boolean return Ada_Type_Enum;

   function New_Type_Enum
     (Id      : String;
      Size    : Natural := 0;
      Comment : String := "") return Ada_Type_Enum;

   function Id (Elt : Ada_Enum_Value) return Unbounded_String;

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Enum) return Boolean;

   -------------------
   -- Type: records --
   -------------------

   type Ada_Type_Record is new Ada_Type with private;
   --  A record type definition

   function New_Type_Record
     (Id      : String;
      Comment : String := "") return Ada_Type_Record'Class;

   procedure Add_Bit_Order_Aspect
     (Elt   : in out Ada_Type_Record'Class;
      Order : System.Bit_Order);

   type Field_Properties is record
      Is_Aliased     : Boolean := True;
      Is_Volatile_FA : Boolean;
   end record;

   procedure Add_Field
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : Ada_Type'Class;
      Offset      : Natural;
      LSB         : Natural;
      MSB         : Natural;
      Properties  : Field_Properties;
      Comment     : String := "");

   procedure Add_Field
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : Ada_Type'Class;
      Offset      : Natural;
      LSB         : Natural;
      MSB         : Natural;
      Default     : Unsigned;
      Properties  : Field_Properties;
      Comment     : String := "");

   procedure Add_Field
     (Rec         : in out Ada_Type_Record'Class;
      Id          : String;
      Typ         : Ada_Type'Class;
      Offset      : Natural;
      LSB         : Natural;
      MSB         : Natural;
      Default     : Unbounded_String;
      Properties  : Field_Properties;
      Comment     : String := "");
   --  Adds a new field to the record 'rec'
   --  Id : The ID of the field
   --  Typ : The base type
   --  Offset : The offset in byte from the the base record address
   --  LSB : The least significant bit after offset
   --  MSB : The most significant bit after offset
   --  Desctr: Description of the field

   function Simplify
     (Elt  : Ada_Type_Record;
      Spec : in out Ada_Spec) return Ada_Type'Class;
   --  If the record has just one field, then this returns the field with
   --  the type name substituted with Elt's type name

   ------------------
   -- Type: unions --
   ------------------

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Record) return Boolean;

   type Ada_Type_Union is new Ada_Type_Record with private;
   --  A union record definition

   function New_Type_Union
     (Id        : String;
      Disc_Name : String;
      Disc_Type : Ada_Type_Enum'Class;
      Comment   : String := "") return Ada_Type_Union;

   procedure Add_Field
     (Rec         : in out Ada_Type_Union'Class;
      Enum_Val    : String;
      Id          : String;
      Typ         : Ada_Type'Class;
      Offset      : Natural;
      LSB         : Natural;
      MSB         : Natural;
      Properties  : Field_Properties;
      Comment     : String := "");

   overriding function Is_Similar
     (T1, T2 : Ada_Type_Union) return Boolean;

   --------------
   -- Constant --
   --------------

   type Ada_Constant_Value is private;
   --  Some constant value

   function New_Constant_Value
     (Id        : String;
      Align_Id  : Natural;
      Type_Name : String;
      Value     : String;
      Comment   : String := "") return Ada_Constant_Value;
   --  Generates "Id : constant Type_Name := Value";
   --  If Align_Id is 0 or less than Id's length, then the semicolon is just
   --  next to the constant identifier, else some space is left to align
   --  declarations

   --------------
   -- Variable --
   --------------

   type Ada_Variable is private;
   --  A global variable definition

   function New_Variable
     (Id         : String;
      Type_Name  : String;
      Is_Aliased : Boolean;
      Comment    : String := "") return Ada_Variable;

   procedure Add_Address_Aspect
     (Elt     : in out Ada_Variable;
      Address : Unsigned);

   procedure Add_Address_Aspect
     (Elt : in out Ada_Variable;
      Val : String);

   procedure Add_Aspect
     (Elt    : in out Ada_Variable;
      Aspect_Mark : String);

   --------------
   -- Ada Spec --
   --------------

   function New_Spec
     (Name          : String;
      Descr         : String;
      Preelaborated : Boolean) return Ada_Spec;

   function New_Child_Spec
     (Name          : String;
      Parent        : String;
      Descr         : String;
      Preelaborated : Boolean) return Ada_Spec;

   function Id (Spec : Ada_Spec) return Unbounded_String;

   procedure Write_Spec
     (Spec       : Ada_Spec;
      Output_Dir : String);

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_With_Clause);

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Comment);

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Comment_Box);

   procedure Add
     (Spec : in out Ada_Spec;
      Elt  : Ada_Pragma);

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
      Elt  : Ada_Variable);

   function Add_Enum_Id
     (Spec    : Ada_Spec;
      Enum    : in out Ada_Type_Enum;
      Id      : String;
      Comment : String := "") return Ada_Enum_Value;

   function Add_Enum_Id
     (Spec    : Ada_Spec;
      Enum    : in out Ada_Type_Enum;
      Id      : String;
      Repr    : Unsigned;
      Comment : String := "") return Ada_Enum_Value;

   -----------------------------
   -- String comparison utils --
   -----------------------------

   function Starts_With (S1, S2 : String) return Boolean;
   --  Whether S1 starts with the substring S2 (or is equal to S2)

   function Ends_With (S1, S2 : String) return Boolean;
   --  Whether S1 ends with the substring S2 (or is equal to S2)

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

   type Ada_Pragma is new Spec_Element with record
      Id      : Unbounded_String;
      Comment : Ada_Comment;
   end record;

   overriding procedure Dump
     (Element : Ada_Pragma;
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
      Size : Natural;
   end record;

   overriding procedure Added_In_Spec
     (Element : Ada_Type_Scalar;
      Spec    : in out Ada_Spec);

   overriding procedure Dump
     (Element : Ada_Type_Scalar;
      File    : Ada.Text_IO.File_Type);

   type Ada_Subtype_Scalar is new Ada_Type with record
      Typ : Unbounded_String;
   end record;

   overriding procedure Dump
     (Element : Ada_Subtype_Scalar;
      File    : Ada.Text_IO.File_Type);

   type Ada_Type_Array is new Ada_Type with record
      Index_Type   : Unbounded_String;
      Index_First  : Natural;
      Index_Last   : Natural;
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
      Offset      : Natural;
      LSB         : Natural;
      MSB         : Natural;
      Has_Default : Boolean;
      Default     : Unbounded_String;
      Properties  : Field_Properties;
      Comment     : Ada_Comment;
   end record;

   overriding
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

   type Ada_Type_Union is new Ada_Type_Record with record
      Disc_Name    : Unbounded_String;
      Discriminent : Ada_Type_Enum;
      Disc_Fields  : Record_Field_Maps.Map;
   end record;

   overriding procedure Dump
     (Element : Ada_Type_Union;
      File    : Ada.Text_IO.File_Type);

   type Ada_Constant_Value is new Spec_Element with record
      Id        : Unbounded_String;
      Id_Size   : Natural;
      Type_Name : Unbounded_String;
      Value     : Unbounded_String;
      Comment   : Ada_Comment;
   end record;

   overriding procedure Dump
     (Element : Ada_Constant_Value;
      File    : Ada.Text_IO.File_Type);

   type Ada_Variable is new Spec_Element with record
      Id        : Unbounded_String;
      Type_Name : Unbounded_String;
      Aliasd    : Boolean;
      Comment   : Ada_Comment;
      Aspects   : String_Vectors.Vector;
   end record;

   overriding procedure Dump
     (Element : Ada_Variable;
      File    : Ada.Text_IO.File_Type);

   overriding procedure Added_In_Spec
     (Element : Ada_Variable;
      Spec    : in out Ada_Spec);

   type Ada_Spec is record
      With_Clauses  : With_Maps.Map;
      Id            : Unbounded_String;
      Comment       : Ada_Comment;
      Elements      : Element_Vectors.Vector;
      Preelaborated : Boolean;
   end record;

   function File_Name (Spec : Ada_Spec) return String;

end Ada_Gen;
