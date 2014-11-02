private package Package_Declaration.Private_Child is

   task type Task_Type_Declaration is
      entry Entry_Declaration;
   end Task_Type_Declaration;

   protected type Protected_Type_Declaration is
      procedure X (Current : in out Boolean);
      entry Entry_Body_Declaration (Boolean);
   private
      State : Boolean;
   end Protected_Type_Declaration;

   procedure Procedure_Declaration (Parameter_Specification : in Integer);

   Exception_Declaration : exception;

   generic
      type X is range <>;
   package Pkg is
      subtype Y is X;
   end Pkg;

   generic
      type Formal_Type_Declaration is private;
      Formal_Object_Declaration : in Integer := 1;

      with procedure Formal_Procedure_Declaration (X : in Integer) is
        Procedure_Declaration;

      with function Formal_Function_Declaration return Boolean is True;

      with package Formal_Package_Declaration is new Pkg (Integer);

      with package Formal_Package_Declaration_With_Box is new Pkg (<>);

   package Generic_Package_Declaration is
      type X is range 1 .. 10;
   end Generic_Package_Declaration;

   generic
   procedure Generic_Procedure_Declaration;

   generic
   function Generic_Function_Declaration return Boolean;

   generic package Generic_Package_Renaming_Declaration
     renames Generic_Package_Declaration;

   generic procedure Generic_Procedure_Renaming_Declaration
     renames Generic_Procedure_Declaration;

   generic function Generic_Function_Renaming_Declaration
     renames Generic_Function_Declaration;

end Package_Declaration.Private_Child;


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
