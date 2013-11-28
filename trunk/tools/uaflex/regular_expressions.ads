------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: $ $Date: $


with Symbols;   use Symbols;
with Positions;
with Automatons;

package Regular_Expressions is

   type Expression is private;

   function New_Literal     (S     : Symbol_Set)
     return Expression;

   function New_Rule        (Item  : Expression;
                             Token : Natural)
     return Expression;

   function New_Alternative (Left  :  Expression;
                             Right :  Expression)
     return Expression;

   function New_Sequence    (Left  :  Expression;
                             Right :  Expression)
     return Expression;

   function New_Iteration   (Item       : Expression;
                             Allow_Zero : Boolean := True)
     return Expression;

   function New_Optional    (Item       : Expression)
     return Expression;

   function New_Apply       (Item       : Expression)
     return Expression;

   procedure Add_To_DFA
     (Result   : in out Automatons.DFA;
      Item     : in     Expression;
      Starting : in     Positive);

   Syntax_Error : exception;

private

   function Literal_Count (Item : Expression) return Natural;

   type Expression_Node;
   type Expression is access all Expression_Node;

   type Expression_Kind is
      (Literal, Alternative, Sequence, Iteration, Optional, Apply);

   type Expression_Node (Kind : Expression_Kind) is record
      Nullable : Boolean;
      case Kind is
         when Literal =>
            Chars : Symbol_Set;
            Token : Natural := 0;
         when Alternative | Sequence =>
            Left, Right : Expression;
         when Iteration =>
            Item       : Expression;
            Allow_Zero : Boolean;
         when Optional | Apply =>
            Child : Expression;
      end case;
   end record;

end Regular_Expressions;


------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the Maxim Reznik, IE nor the names of its
--       contributors may be used to endorse or promote products derived from
--       this software without specific prior written permission.
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
