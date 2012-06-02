pragma Page;

with Package_Declaration.Child;
use Package_Declaration.Child;

with Package_Declaration.Types;

procedure Procedure_Body is

   use type Package_Declaration.Child.Ptr;

   type Enumeration_Type_Definition is (Enumeration_Literal, 'X');

   function "+" (X : Enumeration_Type_Definition)
                return Enumeration_Type_Definition;

   function "+" (X : Enumeration_Type_Definition)
                return Enumeration_Type_Definition is
   begin
      return 'X';
   end "+";

   function Function_Renaming_Declaration return Enumeration_Type_Definition
     renames 'X';

   subtype Subtype_Declaration is Integer range 1 .. 1;
   subtype Extension is Derived_Record_Extension_Definition;

   Variable_Declaration : aliased Subtype_Declaration;
   Constant_Declaration : constant Subtype_Declaration := 1;

   task Single_Task_Declaration is
      entry Run;
   end Single_Task_Declaration;

   task body Single_Task_Declaration is
   begin
      accept Run;

      select
         accept Run;
      or
         terminate;
      end select;
   end Single_Task_Declaration;

   protected Single_Protected_Declaration is
      entry Run;
   end Single_Protected_Declaration;

   protected body Single_Protected_Declaration is
      entry Run when True is
      begin
         null;
      end;
   end Single_Protected_Declaration;

   procedure Procedure_Body_Stub (X : access Integer) is separate;
   function Function_Body_Stub return Integer is separate;

   package Package_Body_Stub is
      procedure X;
   end Package_Body_Stub;

   package body Package_Body_Stub is separate;

   task Task_Body_Stub;
   task body Task_Body_Stub is separate;

   protected Protected_Body_Stub is
      entry Run;
      procedure X;
   private
      Int : Integer := 0;
   end Protected_Body_Stub;

   protected body Protected_Body_Stub is separate;


   Integer_Number_Declaration : constant := 1;
   Real_Number_Declaration    : constant := 3.1415;
   String_Literal             : constant String := "aaa";
   Ptr   : Package_Declaration.Child.Ptr := new Extension;
   Char  : Character := 'A';
   Slice : String (Subtype_Declaration);
   Num   : Float;

   Extension_Aggregate : Extension := (Incomplete_Type_Declaration with 1);

   Positional_Array_Aggregate : String := ('A', 'B', 'C');
   Named_Array_Aggregate      : String := (2 => 'A', 1 => 'B', 3 => 'C');
begin
   for I in 1 .. 2 loop
      Variable_Declaration := Constant_Declaration;
   end loop;

   for Loop_Parameter_Specification in reverse Slice'Range loop
      if Loop_Parameter_Specification = 1 and then Char not in 'A' .. 'B' then
         Ptr   := Ptr.all.X;
         Ptr   := (Ptr.X);
         Char  := String_Literal (Function_Body_Stub);
         Slice := String_Literal (Slice'First .. 1);
         Extension_Aggregate := (null, 1);
      elsif Ptr.all not in Extension then
         Ptr := new Extension'(Ptr, 2);

         raise Program_Error;
      else
         goto Label;
      end if;

      case Char is
         when 'A' =>
            loop
               Ptr := (Ptr.X);
               exit when Ptr.all in Extension;
            end loop;
         when others =>
            null;
      end case;

      while Ptr /= null or else Char in 'X' .. 'Y' loop
         Num := Float (Function_Body_Stub);
         Char := Character'('X');
      end loop;

      <<Label>>
      declare
         Int : aliased Integer := 1;
      begin
         Int := Int + 1;
         Procedure_Body_Stub (Int'Access);
      end;

      Single_Protected_Declaration.Run;
      delay 1.0;

      select
         Single_Task_Declaration.Run;
      or
         delay 1.0;
      end select;

      select
         Single_Task_Declaration.Run;
      else
         Char := 'B';
      end select;

      select
         delay 1.0;
      then abort
         Variable_Declaration := Constant_Declaration;
      end select;

   end loop;
exception when Choice_Parameter_Specification : others =>
   null;
end Procedure_Body;


------------------------------------------------------------------------------
--  Copyright (c) 2006, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
