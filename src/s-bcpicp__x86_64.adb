------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2021, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

separate (System.BB.CPU_Primitives)
procedure Initialize_CPU is
   Spurious_Interrupt_Vector : constant := 32;
   --  Vector for the spurious interrupts. Keep in sync with vector_table.S
   Min_Required_CPUID_Index : constant := 16#0D#;
   --  The minimum Intel processor architecture we support is Sandy Bridge
   --  which has a max CPUID Index of DH. This leaf corresponds to the
   --  TSC Leaf which we use

begin
   --  Verify CPU by checking the processors' vendor string. At this point
   --  we only support Intel. AMD support will come later once we verify it
   --  supports what we need.

   declare
      EAX : constant := 0;
      --  CPUID Leaf 0H, Vendor ID string

      subtype CPU_ID_String is String (1 .. 4);
      EBX, EDX, ECX : CPU_ID_String;
   begin
      Asm ("cpuid",
           Inputs  => Unsigned_32'Asm_Input ("a", EAX),
           Outputs =>
             (Unsigned_32'Asm_Output ("=a", Max_CPUID_Index),
              CPU_ID_String'Asm_Output ("=b", EBX),
              CPU_ID_String'Asm_Output ("=c", ECX),
              CPU_ID_String'Asm_Output ("=d", EDX)),
           Volatile => True);

      --  Validate that we are on a supported processor

      if EBX = "Genu" and then EDX = "ineI" and then ECX = "ntel" then
         if Max_CPUID_Index < Min_Required_CPUID_Index then
            raise Program_Error with "Unsupported Intel processor";
         end if;
      else
         raise Program_Error with "Runtime only supports Intel processors";
      end if;
   end;

   --  Check processor supported features

   declare
      --  CPUID Leaf 01H, Feature Information

      Feature_Info_Leaf : constant := 16#01#;
      Features_ECX      : Feature_Information_ECX;
      Features_EDX      : Feature_Information_EDX;

      --  CPUID Leaf 0DH, Processor Extended State Enumeration

      Extended_State_Leaf  : constant := 16#0D#;
      Extended_State_1_EAX : Extended_State_Subleaf_1_EAX;

      --  CPUID Subleaf

      Sub_Leaf_1 : constant := 1;

   begin
      --  Check for XSAVE processor feature, builtin APIC and TSC

      Asm ("cpuid",
           Inputs   => Unsigned_32'Asm_Input ("a", Feature_Info_Leaf),
           Outputs  =>
             (Feature_Information_ECX'Asm_Output ("=c", Features_ECX),
              Feature_Information_EDX'Asm_Output ("=d", Features_EDX)),
           Clobber  => "rbx",
           Volatile => True);

      if not Features_ECX.XSAVE then
         raise Program_Error
           with "Runtime requires XSAVE processor feature";
      end if;

      if not Features_EDX.APIC then
         raise Program_Error with "Processor is missing builtin APIC!";
      end if;

      if not Features_EDX.TSC then
         raise Program_Error
           with "Runtime requires Time Stamp Counter";
      end if;

      --  Check for XSAVEOPT instruction. This enables us to not store the
      --  floating-pointer and vector state on each interrupt handler if
      --  if the state has not changed.

      Asm ("cpuid",
           Inputs   =>
             (Unsigned_32'Asm_Input ("a", Extended_State_Leaf),
              Unsigned_32'Asm_Input ("c", Sub_Leaf_1)),
           Outputs  =>
             Extended_State_Subleaf_1_EAX'Asm_Output
                ("=a", Extended_State_1_EAX),
           Clobber  => "rbx, rdx",
           Volatile => True);

      if not Extended_State_1_EAX.XSAVEOPT then
         raise Program_Error
           with "Runtime requires XSAVEOPT instruction";
      end if;
   end;

   --  Setup Control Registers to enable SSE and XSAVE (the latter enabling
   --  AVX support).

   declare
      CR4 : Control_Register_4 := Get_CR4;
   begin
      CR4.FXSAVE_FXRSTOR_And_SSE_Enable := True;
      CR4.XSAVE_and_Processor_Extended_States_Enable := True;
      Set_CR4 (CR4);
   end;

   --  Set Extended Control Register XCR0 so that XSAVE can store x87, SSE
   --  AVX and AVX-512 registers. See Section 13.3, Vol 1. Note that
   --  XCR0 is set via EDX:EAX rather than a RAX, so we need to split our
   --  state into a high and low register. %rcx selects XCR0. To ensure we
   --  only set only valid bits we need to AND BB_X86_Context_State with the
   --  support state given in CPUID leaf 0DH, sub-leaf 0.

   declare
      --  CPUID Leaf 0DH, Processor Extended State Enumeration

      Extended_State_Leaf : constant := 16#0D#;
      Sub_Leaf_0          : constant := 0;
   begin
      Asm
        ("cpuid"                                                      & NL &
         --  rax:rdx have the supported state mask. Move our mask into
         --  %2:rcx.
         "movq  %2,    %%rcx"                                         & NL &
         "shrq  $32,   %%rcx"                                         & NL &
         "andq  %2,    %%rax"                                         & NL &
         "andq  %%rcx, %%rdx"                                         & NL &
         "movq  $0,    %%rcx"                                         & NL &
         "xsetbv",
         Inputs   =>
           (Unsigned_32'Asm_Input ("a", Extended_State_Leaf),
            Unsigned_32'Asm_Input ("c", Sub_Leaf_0),
            State_Component_Bit_Map'Asm_Input ("r", BB_X86_Context_State)),
         Clobber  => "rbx, rdx",
         Volatile => True);
   end;

   --  Initialize x87 FPU to defaults, masking floating point exceptions

   Asm ("fninit", Volatile => True);

   --  Initialize SSE MXCSR with default 0x1F80
   --  Note: check if we need it to raise divide by zero exceptions.

   Asm ("ldmxcsr %0",
        Inputs   => Unsigned_32'Asm_Input ("m", Runtime_SSE_MXCSR),
        Volatile => True);

   --  Finalize setup of GDT by setting each CPUs TSS record in the
   --  corresponding GDT TSS entry.

   declare
      type TSS_Address is record
         Low  : Base_Address_Low_Part;
         High : Base_Address_High_Long_Part;
      end record;

      for TSS_Address use record
         Low  at 0 range 0 .. 23;
         High at 3 range 0 .. 39;
      end record;

      function To_TSS_Address is new
        Ada.Unchecked_Conversion (System.Address, TSS_Address);
   begin
      for J in GDT.CPU_TSS'Range loop
         GDT.CPU_TSS (J).Base_Address_Low :=
           To_TSS_Address (CPU_Task_State (J)'Address).Low;
         GDT.CPU_TSS (J).Base_Address_High :=
           To_TSS_Address (CPU_Task_State (J)'Address).High;
      end loop;
   end;

   --  Setup CPU specific information

   declare
      use System.Multiprocessors;

      Interrupt_Stacks : array (CPU) of System.Address;
      pragma Import (Asm, Interrupt_Stacks, "interrupt_stack_table");
      --  The Interrupt_Stack_Table defined in the body of
      --  System.BB.Interrupts provides the initial interrupt stack pointer
      --  for each CPU. Unlike other targets, on x86-64 we relocate the
      --  processor's interrupt stack pointer to the x86-64 processor
      --  specific CPU_Specific_Information record because the x86-64
      --  processor will switch stacks for us.

      Exception_Stack_Pointer : System.Address;
      --  Like the interrupt stack pointer above, but for exceptions.

      My_ID : constant CPU := CPU_Range (Local_APIC_ID_Register.ID) + 1;
      --  Local APIC IDs start from 0 while type CPU starts from 1

      My_CPU_Task_State : Task_State_Segment renames CPU_Task_State (My_ID);

      GDT_Location : constant Descriptor_Pointer :=
         (Base_Address => GDT'Address, Limit => GDT'Size / 8 - 1);
      IDT_Location : constant Descriptor_Pointer :=
         (Base_Address => IDT'Address, Limit => IDT'Size / 8 - 1);
      --  Locations of the GDT and IDT tables in the format required by
      --  their respective load instructions.

   begin
      --  Initialize exception stack pointer. The interrupt stack pointer
      --  was initialized in BB.Interrupts.

      Initialize_Stack
        (Exception_Stacks (My_ID)'Address,
         Exception_Stack_Space'Size,
         Exception_Stack_Pointer);

      My_CPU_Task_State :=
        (CPU_ID => My_ID,
         IST1   => Interrupt_Stacks (My_ID),
         IST7   => Exception_Stack_Pointer,
         others => <>);

      --  Find the size of the XSAVE state. We capture this information in
      --  the task's CPU_Task_State record because cpuid can be very
      --  expensive in virtualised environments.

      declare
         XSAVE_Leaf   : constant := 16#0D#;
         Sub_Function : constant := 0;
      begin
         Asm
           ("cpuid",
            Inputs   =>
              (Unsigned_32'Asm_Input ("a", XSAVE_Leaf),
               Unsigned_32'Asm_Input ("c", Sub_Function)),
            Outputs  =>
              Unsigned_64'Asm_Output ("=b", My_CPU_Task_State.XSAVE_Size),
            Clobber  => "rdx",
            Volatile => True);
      end;

      --  Write address of My_CPU_Task_State into %gs via the IA32_GS_BASE
      --  MSR

      Asm
        ("movq    %%rax, %%rdx"                                       & NL &
         "shrq    $32,   %%rdx"                                       & NL &
         "wrmsr",
          Inputs   =>
            (System.Address'Asm_Input ("a", My_CPU_Task_State'Address),
             Unsigned_32'Asm_Input ("c", IA32_GS_BASE)),
          Volatile => True);

      --  Switch GDT to our tasking version

      Asm
        ("lgdt %0",
         Inputs => Descriptor_Pointer'Asm_Input ("m", GDT_Location),
         Volatile => True);

      --  Switch to our new code and data segments

      Asm
        ("pushq   %0"                                                 & NL &
         "pushq   $1f"                                                & NL &
         "lretq"                                                      & NL &
      "1: movl    %1,   %%eax"                                        & NL &
         "movw    %%ax, %%ds"                                         & NL &
         "movw    %%ax, %%es"                                         & NL &
         "movw    %%ax, %%ss"                                         & NL &
         "movw    %%ax, %%fs",
         Inputs   =>
             (Unsigned_16'Asm_Input ("i", GDT.Code'Position),
              Unsigned_16'Asm_Input ("i", GDT.Data'Position)),
         Clobber  => "rax",
         Volatile => True);

      --  Load the Task Register with the CPU's TSS. For the first CPU it's
      --  the first component of the CPU_TSS array.

      Asm
        ("ltr %0",
         Inputs   => Unsigned_16'Asm_Input ("r", GDT.CPU_TSS'Position),
         Volatile => True);

      --  Setup and load the IDT

      Setup_Interrupt_Descriptor_Table;

      Asm
        ("lidt %0",
         Inputs   => Descriptor_Pointer'Asm_Input ("m", IDT_Location),
         Volatile => True);
   end;

   --  Mask Intel 8259A PIC if it happens to be present on our x86-64
   --  machine.

   Write_IO_Byte (Data => 16#FF#, Port => 16#A1#);
   Write_IO_Byte (Data => 16#FF#, Port => 16#21#);

   --  Initialize Local APIC

   --  Set the Spurious Interrupt Register, which is required to allow the
   --  CPU to receive interrupts from the APIC.

   Local_APIC_Spurious_Interrupt_Register :=
     (Spurious_Vector           => Spurious_Interrupt_Vector,
      APIC_Enabled              => True,
      Focus_Processor_Checking  => False,
      Suppress_EOI_Broadcast    => False);

   --  By default all interrupt entries in the Local Vector Table are
   --  masked. Let's assume that is the case for now. In the future it may
   --  be useful to enable the Error, LINT0 and LINT1 vectors (for the
   --  latter need to decide whether to hard code or read the ACPI table).

   --  Determine system clock frequencies

   Determine_Clock_Frequencies;

   --  Setup Local APIC Timer

   --  Disable warnings for Local_APIC_Timer_Divide_Configuration

   pragma Warnings (Off, "condition is always*");

   Local_APIC_Timer_Divide_Configuration :=
     (case APIC_Timer_Divider is
         when 1  => Divide_by_1,  when 2   => Divide_by_2,
         when 4  => Divide_by_4,  when 8   => Divide_by_8,
         when 16 => Divide_by_16, when 32  => Divide_by_32,
         when 64 => Divide_by_64, when 128 => Divide_by_128,
         when others => raise Program_Error with
           "Invalid Local APIC Timer Divider value");

   Local_APIC_LVT_Timer_Register :=
     (Timer_Mode => One_Shot,
      Mask       => False,
      Delivery   => Idle,
      Vector     => APIC_Timer_Vector);

   --  Update APIC_Frequency_In_kHz to reflect the chosen divider

   APIC_Frequency_In_kHz := APIC_Frequency_In_kHz / APIC_Timer_Divider;

   --  Clear any pending interrupts that may have occurred before or during
   --  the setup.

   Local_APIC_End_of_Interrupt := Signal;
end Initialize_CPU;
