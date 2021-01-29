------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2020, AdaCore                      --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.OS_Lib;

with DOM.Core;
with DOM.Core.Elements;     use DOM.Core.Elements;
with DOM.Core.Nodes;        use DOM.Core.Nodes;

with Ada_Gen;               use Ada_Gen;
with SVD2Ada_Utils;
with Ada.Containers;

package body Descriptors.Device is

   use type Ada.Containers.Count_Type;

   package Interrupt_Sort is new Interrupt_Vectors.Generic_Sorting
     (Base_Types."<");

   procedure Dump_Handler_ASM
     (Device     : Device_T;
      Ints       : Interrupt_Vectors.Vector;
      Output_Dir : String);

   procedure Dump_IRQ_Support
     (Device : Device_T;
      Output_Dir : String);
   --  Dump the IRQ names and trap handlers

   procedure Gather_Peripheral_Group
     (Group_Name  : Unbounded_String;
      Peripherals : in out Peripheral_Vectors.Vector;
      Group       : in out Peripheral_Vectors.Vector)
     --  Gather into Group each peripheral in Peripherals that is defined in
     --  the SVD file to be in the same group, as specified by having the same
     --  Group_Name. When finished, all peripherals now in Group are no longer
     --  in Peripherals.
     with
       Pre  => Group_Name /= "" and
               Group.Length = 1,  -- the first member has been inserted
       Post => Group.Length >= 1  -- perhaps only the initial member is in group
               and
               --  all members of Peripherals in the same group are in Group
               (for all P of Peripherals'Old =>
                  (if P.Group_Name = Group_Name then Group.Contains (P)))
               and
               --  all members of Group are in the same group and are no longer
               --  in Peripherals
               (for all P of Group =>
                  P.Group_Name = Group_Name and not Peripherals.Contains (P));

   function Is_Group_Member (P : Peripheral_Access) return Boolean is
     (Length (P.Group_Name) /= 0);
   --  a convenience function for readability

   -----------------
   -- Read_Device --
   -----------------

   function Read_Device
     (Elt      : DOM.Core.Element;
      Pkg_Name : String)
      return Device_T
   is
      use type DOM.Core.Node_Types;
      List   : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Elt);
      Result : Device_T;

   begin
      if Pkg_Name'Length > 0 then
         Result.Name := Ada.Strings.Unbounded.To_Unbounded_String (Pkg_Name);
      end if;

      for J in 0 .. DOM.Core.Nodes.Length (List) - 1 loop
         if Node_Type (Item (List, J)) = DOM.Core.Element_Node then
            declare
               Child : constant DOM.Core.Element :=
                         DOM.Core.Element (Item (List, J));
               Tag   : String renames DOM.Core.Elements.Get_Tag_Name (Child);
            begin
               if Tag = "name" then
                  if Ada.Strings.Unbounded.Length (Result.Name) = 0 then
                     Result.Name := Get_Value (Child);
                  end if;

                  SVD2Ada_Utils.Set_Root_Package
                    (Ada.Strings.Unbounded.To_String (Result.Name));

               elsif Tag = "version" then
                  Result.Version := Get_Value (Child);

               elsif Tag = "description" then
                  Result.Description := Get_Value (Child);
                  Result.Short_Desc  := Result.Description;

                  for J in 1 .. Length (Result.Description) loop
                     if Element (Result.Description, J) = ASCII.CR
                       or else Element (Result.Description, J) = ASCII.LF
                     then
                        Result.Short_Desc := To_Unbounded_String
                          (Slice (Result.Description, 1, J - 1));
                        exit;
                     end if;
                  end loop;

               elsif Tag = "licenseText" then
                  Ada_Gen.Set_License_Text (Get_Value (Child));

               elsif Tag = "addressUnitBits" then
                  Result.Address_Unit_Bits := Get_Value (Child);

               elsif Tag = "width" then
                  Result.Width := Get_Value (Child);

               elsif Register_Properties.Is_Register_Property (Tag) then
                  Register_Properties.Read_Register_Property
                    (Child, Result.Reg_Properties);

               elsif Tag = "vendor" then
                  null; --  No need to decode, at least for now

               elsif Tag = "series" then
                  null; --  No need to decode, at least for now

               elsif Tag = "cpu" then
                  null; --  No need to decode, at least for now

               elsif Tag = "peripherals" then
                  declare
                     Child_List : constant DOM.Core.Node_List := Child_Nodes (Child);
                     Peripheral : Peripheral_T;
                  begin
                     for K in 0 .. Length (Child_List) - 1 loop
                        if Node_Type (Item (Child_List, K)) = DOM.Core.Element_Node then
                           Peripheral :=
                             Read_Peripheral
                               (DOM.Core.Element (Item (Child_List, K)),
                                Result.Reg_Properties,
                                Result);
                           Result.Peripherals.Append
                             (new Peripheral_T'(Peripheral));
                        end if;
                     end loop;
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring device element " & Tag & " at " & Full_Name (Child));
               end if;
            end;
         end if;
      end loop;

      return Result;
   end Read_Device;

   --------------------
   -- Get_Peripheral --
   --------------------

   overriding function Get_Peripheral
     (Db     : Device_T;
      XML_Id : String) return Peripheral_Access
   is
   begin
      for P of Db.Peripherals loop
         if Ada.Strings.Unbounded.To_String (P.Name) = XML_Id then
            return P;
         end if;
      end loop;

      return null;
   end Get_Peripheral;

   ----------------------
   -- Dump_Handler_ASM --
   ----------------------

   procedure Dump_Handler_ASM
     (Device     : Device_T;
      Ints       : Interrupt_Vectors.Vector;
      Output_Dir : String)
   is
      ASM      : File_Type;
      F_Name   : constant String :=
                   GNAT.OS_Lib.Normalize_Pathname ("handler.S", Output_Dir);
      Tab_Size : Natural;
      Curs     : Interrupt_Vectors.Cursor;

      procedure Dump_Stub (Name : String);
      --  Dumps a weak empty function. To be overwritten by the run-time

      ---------------
      -- Dump_Stub --
      ---------------

      procedure Dump_Stub (Name : String) is
      begin
         Put_Line (ASM, ASCII.HT & ".thumb_func");
         Put_Line (ASM, ".weak " & Name);
         Put_Line (ASM, ".type " & Name & ", %function");
         Put_Line (ASM, Name & ":");
         Put_Line (ASM, "0:" & ASCII.HT & "b 0b");
         Put_Line (ASM, ASCII.HT & ".size " & Name & ", . - " & Name);
         New_Line (ASM);
      end Dump_Stub;

   begin
      Create (ASM, Out_File, F_Name);
      Put_Line (ASM, "## -*- asm -*- #############################");
      Put_Line (ASM, "# Automatically generated by SVD2Ada");
      Put_Line (ASM, "# For the " &
                  Ada.Strings.Unbounded.To_String (Device.Short_Desc) &
                  " target");
      Put_Line (ASM, "############################################");
      New_Line (ASM);
      Put_Line (ASM, ASCII.HT & ".syntax unified");
      --  ??? target is m4/m7, but what about previous versions?
      Put_Line (ASM, ASCII.HT & ".cpu cortex-m4");
      Put_Line (ASM, ASCII.HT & ".thumb");
      New_Line (ASM);

      --  Calculate the vector size to properly align it as requested.
      --  The vector consists on 16 reserved values for RESET, Hard float and
      --  so on, + the IRQs
      --  Removing 2 from the GNAT IRQ IDs to obtain the actual number of
      --  IRQs (the GNAT ID includes Systick, 0 being reserved).
      Tab_Size := (16 + Natural (Ints.Last_Element.Value) - 2) * 4;
      --  Now essie the proper alignment directive: the table needs to be
      --  aligned on a power of 2.
      Put_Line (ASM, ASCII.HT & ".text");
      Put_Line (ASM, ASCII.HT & ".globl __vectors");

      for J in 5 .. 16 loop
         if 2 ** J >= Tab_Size then
            Put_Line (ASM, ASCII.HT & ".p2align" & J'Img);
            exit;
         end if;
      end loop;

      Put_Line (ASM, "__vectors:");
      --  Cortex ARMv7-m defined 16 first values
      Put_Line
        (ASM,
         ASCII.HT & "/* Cortex-M core interrupts */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   0                    /* stack top address */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 1 Reset.  */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 2 NMI. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 3 Hard fault. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 4 Mem manage. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 5 Bus fault. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 6 Usage fault. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 7 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 8 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 9 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 10 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   __gnat_sv_call_trap  /* 11 SVCall. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   __gnat_bkpt_trap     /* 12 Breakpoint. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 13 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   __gnat_pend_sv_trap  /* 14 PendSV. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   __gnat_sys_tick_trap /* 15 Systick. */");

      Put_Line
        (ASM,
         ASCII.HT & "/* MCU interrupts */");

      Curs := Ints.First;

      for J in 0 .. Ints.Last_Element.Value loop
         while Interrupt_Vectors.Element (Curs).Value < J loop
            Interrupt_Vectors.Next (Curs);
         end loop;

         Put
           (ASM, ASCII.HT & ".word __gnat_irq_trap        /*" &
              Integer'Image (J + 16) & " ");
         if Interrupt_Vectors.Element (Curs).Value = J then
            Put_Line
              (ASM,
               Ada.Strings.Unbounded.To_String
                 (Interrupt_Vectors.Element (Curs).Name) &
                 " */");
         else
            Put_Line
              (ASM, "IRQ" & Integer'Image (J) & ". */");
         end if;
      end loop;

      New_Line (ASM);
      Put_Line (ASM, ASCII.HT & ".text");
      New_Line (ASM);
      --  Add weak symbols for the functions denoted by the Vector
      Dump_Stub ("__gnat_irq_trap");
      Dump_Stub ("__gnat_sv_call_trap");
      Dump_Stub ("__gnat_pend_sv_trap");
      Dump_Stub ("__gnat_sys_tick_trap");

      --  Add the never-ending 'fault' function
      Put_Line (ASM, ASCII.HT & ".thumb_func");
      Put_Line (ASM, "fault:" & ASCII.HT & "b fault");
      Close (ASM);
   end Dump_Handler_ASM;

   ----------------------
   -- Dump_IRQ_Support --
   ----------------------

   procedure Dump_IRQ_Support
     (Device : Device_T;
      Output_Dir : String)
   is
      Spec       : Ada_Gen.Ada_Spec;
      Interrupts : Interrupt_Vectors.Vector;
      Max_Len    : Natural := 0;

   begin
      if SVD2Ada_Utils.In_Runtime then
         --  When generating stubs for the Interfaces run-time hierarchy, also
         --  generate the Ada.Exceptions.Name file from the interrupts list
         Spec := New_Spec ("Ada.Interrupts.Names",
                           "This is a version for the " &
                             To_String (Device.Description) & " MCU",
                           False);

         Add (Spec,
              New_Pragma
                ("Implementation_Defined",
                 "All identifiers in this unit are implementation defined"));

         --  Add core interrupts
         Interrupts.Append
           ((Name        => To_Unbounded_String ("Sys_Tick"),
             Description => To_Unbounded_String
               ("System tick"),
             Value       => -1));

         if Length (Device.Description) >= 5 and then Slice (Device.Description, 1, 5) = "STM32"
           and then Slice (Device.Description, 6, 7) /= "F0"
         then
            --  ??? Workaround for the STM32F* svd files that do not define the
            --  FPU Interrupt. The STM32F0 series do not have an FPU.
            Interrupts.Append
              ((Name        => To_Unbounded_String ("FPU"),
                Description => To_Unbounded_String
                  ("FPU global interrupt"),
                Value       => 81));
         end if;

      else
         Spec := New_Child_Spec ("Interrupts",
                                 To_String (Device.Name),
                                 "Definition of the device's interrupts",
                                 False);
      end if;

      Add (Spec, New_Comment_Box ("Interrupts"));

      for Periph of Device.Peripherals loop
         for Int of Periph.Interrupts loop
            if not Interrupts.Contains (Int) then
               Interrupts.Append (Int);
               Max_Len := Natural'Max (Length (Int.Name), Max_Len);
            end if;
         end loop;
      end loop;

      Interrupt_Sort.Sort (Interrupts);

      declare
         Typ : constant String :=
                 (if SVD2Ada_Utils.In_Runtime then "Interrupt_ID" else "");
         --  When generating code for the run-time, we use the
         --  Ada.Interrupts.Interrupt_ID type. Otherwise, the interrupts are
         --  declared as named number to avoid dependency on Ada.Interrupts
         --  that may not be available, for instance when using ZFP
         --  run-time.
      begin
         for Int of Interrupts loop
            declare
               Id : constant String :=
                      (if Ends_With (To_String (Int.Name), "_IRQ")
                       then Slice (Int.Name, 1, Length (Int.Name) - 4)
                       else To_String (Int.Name));
               --  Remove the trailing _IRQ of the interrupt name, if any
            begin
               Add (Spec,
                    New_Constant_Value
                      (Id        => Id & "_Interrupt",
                       Align_Id  => Max_Len + 11,
                       Type_Name => Typ,
                       Value     => To_String (Int.Value),
                       Comment   => To_String (Int.Description)));
            end;
         end loop;
      end;

      Ada_Gen.Write_Spec (Spec, Output_Dir);

      Dump_Handler_ASM (Device, Interrupts, Output_Dir);
   end Dump_IRQ_Support;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Device     : Device_T;
      Output_Dir : String)
   is
      Peripherals : Peripheral_Vectors.Vector;
      Spec        : Ada_Gen.Ada_Spec :=
                      New_Spec (To_String (Device.Name),
                                To_String (Device.Description),
                                True);

   begin
      ----------------
      -- Interrupts --
      ----------------

      if SVD2Ada_Utils.Gen_IRQ_Support then
         Dump_IRQ_Support (Device, Output_Dir);
      end if;

      ----------------------------
      --  Base types definition --
      ----------------------------

      if SVD2Ada_Utils.External_Base_Types_Package then
         --  From GNAT GPL 2016 and GNAT Pro 17, Interfaces.Bit_Types is
         --  defined
         Ada_Gen.Add_Global_With (SVD2Ada_Utils.Base_Types_Package);

      else
         Add (Spec, New_Comment_Box ("Base type"));
         Add_No_Check (Spec, New_Type_Scalar (Target_Type (32, False), 32));
         Add_No_Check (Spec, New_Type_Scalar (Target_Type (16, False), 16));
         Add_No_Check (Spec, New_Type_Scalar (Target_Type (8, False), 8));
         Add_No_Check (Spec, New_Type_Scalar (Target_Type (1, False), 1));

         for J in 2 .. Device.Width loop
            if J not in 8 | 16 | 32 then
               Add_No_Check (Spec, New_Type_Scalar (Target_Type (J, False), J));
            end if;
         end loop;
      end if;

      -----------------------------------------
      --  Base addresses for the peripherals --
      -----------------------------------------

      Add (Spec, New_Comment_Box ("Base addresses"));
      Add (Spec, New_With_Clause ("System", False));

      for Periph of Device.Peripherals loop
         Add (Spec,
              New_Constant_Value
                (Id        => To_String (Periph.Name) & "_Base",
                 Align_Id  => 0,
                 Type_Name => "System.Address",
                 Value     => "System'To_Address (" & To_Hex (Periph.Base_Address) & ")"));
      end loop;

      ----------------------------------
      --  Root package for the device --
      ----------------------------------

      Ada.Text_IO.Put_Line ("Generating package " & To_String (Id (Spec)));

      Write_Spec (Spec, Output_Dir);

      ---------------------------------------------------
      --  Child packages for the device's peripherals  --
      ---------------------------------------------------

      Peripherals := Device.Peripherals;

      while not Peripherals.Is_Empty loop
         declare
            P     : constant Peripheral_Access := Peripherals.First_Element;
            Group : Peripheral_Vectors.Vector;
         begin
            Peripherals.Delete_First;

            if not Is_Group_Member (P) then
               Dump (P.all,
                     To_String (Device.Name),
                     Output_Dir);
            else
               Group.Append (P);
               Gather_Peripheral_Group (P.Group_Name, Peripherals, Group);
               Dump (Group,
                     To_String (Device.Name),
                     Output_Dir);
            end if;
         end;
      end loop;
   end Dump;

   -----------------------------
   -- Gather_Peripheral_Group --
   -----------------------------

   procedure Gather_Peripheral_Group
     (Group_Name  : Unbounded_String;
      Peripherals : in out Peripheral_Vectors.Vector;
      Group       : in out Peripheral_Vectors.Vector)
   is
      Index : Natural;
   begin
      Index := Peripherals.First_Index;

      while Index <= Peripherals.Last_Index loop
         if Peripherals (Index).Group_Name = Group_Name then
            Group.Append (Peripherals (Index));
            Peripherals.Delete (Index);
         else
            Index := Index + 1;
         end if;
      end loop;
   end Gather_Peripheral_Group;

end Descriptors.Device;
