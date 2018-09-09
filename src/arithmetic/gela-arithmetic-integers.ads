------------------------------------------------------------------------------
--                           G E L A   X A S I S                            --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision$ $Date$
--  Purpose:
--  Multyprecission integer arithmetic

with Ada.Containers;

with League.Stream_Element_Vectors;

package Gela.Arithmetic.Integers is
   pragma Preelaborate;

   type Value is private;

   function Literal (Text : String) return Value;

   function "and" (Left, Right : Value) return Value;
   function "or" (Left, Right : Value) return Value;
   function "xor" (Left, Right : Value) return Value;

   function "=" (Left, Right : Value) return Boolean;
   function ">" (Left, Right : Value) return Boolean;
   function "<" (Left, Right : Value) return Boolean;
   function ">=" (Left, Right : Value) return Boolean;
   function "<=" (Left, Right : Value) return Boolean;

   function "-" (Left : Value) return Value;
   function "+" (Left, Right : Value) return Value;
   function "-" (Left, Right : Value) return Value;
   function "*" (Left, Right : Value) return Value;
   function "/" (Left, Right : Value) return Value;
   function "mod" (Left, Right : Value) return Value;
   function "rem" (Left, Right : Value) return Value;
   function "**" (Left, Right : Value) return Value;
   function "abs" (Left : Value) return Value;
   function "not" (Left : Value) return Value;

   function Image (Left : Value) return String;
   function Hash (Left : Value) return Ada.Containers.Hash_Type;

   function Zero return Value;
   function One  return Value;
   function Two  return Value;
   function Ten  return Value;

private

   type Value is new League.Stream_Element_Vectors.Stream_Element_Vector
     with null record;

end Gela.Arithmetic.Integers;


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