package body Gela.Source_Buffers is

   function "-" (Left, Right : Cursor) return Integer is
      Result : constant Interfaces.C.ptrdiff_t := Left - Right;
   begin
      return Integer (Result);
   end "-";

   function "+" (Left : Cursor; Right : Integer) return Cursor is
   begin
      return Left + Interfaces.C.Ptrdiff_T (Right);
   end "+";

   -------------
   -- Element --
   -------------

   function Element (Object : Cursor) return Code_Unit is
   begin
      return Object.all;
   end Element;

   ----------
   -- Next --
   ----------

   procedure Next (Object : in out Cursor) renames Increment;

end Gela.Source_Buffers;

------------------------------------------------------------------------------
--  Copyright (c) 2008, Maxim Reznik
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
--
--  Authors:
--    Andry Ogorodnik
--    Maxim Reznik
--    Vadim Godunko
------------------------------------------------------------------------------
