package Package_Declaration.Types is

   type Signed_Integer_Type_Definition is range 1 .. 10;
   type Modular_Type_Definition is mod 256;
   type Floating_Point_Definition is digits 8;
   type Ordinary_Fixed_Point_Definition is
     delta 0.01 range 0.0 .. 1_000_000.0;
   type Decimal_Fixed_Point_Definition is delta 0.01 digits 8;

   type Constrained_Array_Definition is array (Boolean) of Integer;
   type Unconstrained_Array_Definition is array (Integer range <>) of Integer;

   X : Unconstrained_Array_Definition (1 .. 10);

   type Unknown_Discriminant_Part (<>) is tagged private;

   generic
      type Formal_Private_Type_Definition is private;
      type Formal_Tagged_Private_Type_Definition is tagged private;
      type Formal_Derived_Type_Definition is
        new Formal_Private_Type_Definition;
      type Formal_Discrete_Type_Definition is (<>);
      type Formal_Signed_Integer_Type_Definition is range <>;
      type Formal_Modular_Type_Definition is mod <>;
      type Formal_Floating_Point_Definition is digits <>;
      type Formal_Ordinary_Fixed_Point_Definition is delta <>;
      type Formal_Decimal_Fixed_Point_Definition is delta <> digits <>;
      type Formal_Unconstrained_Array_Definition is
        array (Integer range <>) of Formal_Private_Type_Definition;

      type Formal_Constrained_Array_Definition is
        array (Boolean) of Formal_Private_Type_Definition;

      type Formal_Access_Type_Definition is
        access all Formal_Private_Type_Definition;
   package Pkg is
     subtype X is Formal_Private_Type_Definition;
   end Pkg;

   type Pool_Specific_Access_To_Variable is access Boolean;
   type Access_To_Variable is access all Boolean;
   type Access_To_Constant is access constant Boolean;
   type Access_To_Procedure is access procedure;
   type Access_To_Protected_Procedure is access protected procedure;
   type Access_To_Function is access function return Boolean;
   type Access_To_Protected_Function is
     access protected function return Boolean;

   type Attribute_Definition_Clause is range 1 .. 10_000;
   for Attribute_Definition_Clause'Size use 16;

   type Enumeration_Representation_Clause is (A, B, C);
   for Enumeration_Representation_Clause use (A => 1, B => 2, C => 3);

   type Record_Representation_Clause is record
      X : Integer;
      C : Character;
   end record;

   for Record_Representation_Clause use record
      X at 0  range 0 .. 31;
      C at 32 range 0 .. 7;
   end record;

   At_Clause : Integer;
   for At_Clause use at X'Address;

private

   type Unknown_Discriminant_Part (A : Boolean) is tagged record
      case A is
         when True =>
            X : Integer;
         when others =>
            null;
      end case;
   end record;

   subtype Range_Attribute_Reference is Integer range Natural'Range;
   subtype Simple_Expression_Range is Integer range 1 .. 10;
   subtype Digits_Constraint is Floating_Point_Definition digits 4;
   subtype Delta_Constraint is Ordinary_Fixed_Point_Definition delta 0.1;
   subtype Index_Constraint is Unconstrained_Array_Definition (X'Range);
   subtype Discriminant_Constraint is Unknown_Discriminant_Part (A => False);

end Package_Declaration.Types;


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
