package Package_Declaration.Child is

   type Incomplete_Type_Declaration;

   type Ptr is access all Incomplete_Type_Declaration'Class;

   type Incomplete_Type_Declaration is tagged record
      X : Ptr;
   end record;

   procedure Primitive_Operation_1
     (Object : in out Incomplete_Type_Declaration);

   function "+"
     (Left  : Incomplete_Type_Declaration;
      Right : Integer) return Integer;

   function "="
     (Left  : Incomplete_Type_Declaration;
      Right : Incomplete_Type_Declaration) return Boolean;

   type Derived_Record_Extension_Definition is new Incomplete_Type_Declaration
     with record
        Y : Integer;
     end record;

   function "+" (X  : in Integer) return Derived_Record_Extension_Definition;

   type Private_Type_Declaration (Discriminant_Specification : Character) is
     private;

   type Private_Extension_Declaration is
     new Incomplete_Type_Declaration with private;

   Deferred_Constant_Declaration : constant Private_Type_Declaration;

   type Limited_Trait is limited null record;

   type Limited_Private_Trait is limited private;

   procedure Abstract_Trait (X : Limited_Private_Trait) is abstract;

   type Abstract_Private_Trait is abstract tagged private;

   procedure Out_Mode (X : out Abstract_Private_Trait) is abstract;
   procedure In_Out_Mode (X : in out Abstract_Private_Trait) is abstract;

   type Abstract_Limited_Trait is abstract tagged limited null record;
   type Abstract_Limited_Private_Trait is abstract tagged limited private;

private

   type Private_Type_Declaration (Discriminant_Specification : Character)
   is record
      Component_Declaration : Boolean;
   end record;

   Deferred_Constant_Declaration : constant Private_Type_Declaration :=
     ('X', False);

   type Private_Extension_Declaration is
     new Incomplete_Type_Declaration with record
        Y : Integer;
     end record;

   type Limited_Private_Trait is null record;

   type Abstract_Private_Trait is abstract tagged null record;

   type Abstract_Limited_Private_Trait is abstract tagged limited null record;

end Package_Declaration.Child;


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
