package SVD2Ada_Utils is

   function Executable_Location return String;

   procedure Set_Use_Boolean_For_Bit (Value : Boolean);
   function Use_Boolean_For_Bit return Boolean;

   procedure Set_Gen_GNAT15 (Value : Boolean);
   --  Code generation for GNAT GPL 2015

   function Gen_GNAT15 return Boolean;

end SVD2Ada_Utils;
