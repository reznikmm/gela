------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: $
--  Purpose:
--  Global object stored in this package.
--  Some of them saved in per thread scope in multithread implementation.

with Asis.Gela.Pools;

with Asis.Errors;
with Asis.Gela.Contexts;
with Asis.Gela.Compilations;

package Asis.Gela.Current_State is

   --  Asis.Implementation
   -------------------------------------------------
   Initialized        : Boolean := False;
   Finalized          : Boolean := False;
   Max_Text           : constant := 2048;
   Current_Parameters : Wide_String (1 .. Max_Text);
   Parameters_Length  : Natural := 0;
   Current_Status     : Asis.Errors.Error_Kinds;
   Current_Diagnosis  : Wide_String (1 .. Max_Text);
   Diagnosis_Length   : Natural := 0;
   -------------------------------------------------
   --  End of Asis.Implementation

   --  Asis.Context
   Contexts : Asis.Gela.Contexts.Context_List;
   --  End of Asis.Context

   ----------------------
   -- Per thread state --
   ----------------------

   --  Asis.Gela.Pools
   function Get_Pool return Pools.Pool_State;
   pragma Inline (Get_Pool);

   procedure Set_Pool (Value : Pools.Pool_State);
   --  End of Asis.Gela.Pools

   --  Asis.Gela.Current_Compilation???
   function Get_Compilation return Compilations.Compilation;
   pragma Inline (Get_Compilation);

   procedure Set_Compilation (Value : Compilations.Compilation);
   --  End of Asis.Gela.Current_Compilation???

   -----------------------------
   -- End of per thread state --
   -----------------------------

end Asis.Gela.Current_State;

------------------------------------------------------------------------------
--  Copyright (c) 2009, Maxim Reznik
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
