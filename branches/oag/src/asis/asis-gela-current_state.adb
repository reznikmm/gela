------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: asis-gela-scanners.adb 2679 2009-01-10 12:07:17Z maxr $:
--  Purpose:
--  Single thread state objects

package body Asis.Gela.Current_State is

   Pool        : Pools.Pool_State;
   Compilation : Compilations.Compilation;

   --------------
   -- Get_Pool --
   --------------

   function Get_Pool return Pools.Pool_State is
   begin
      return Pool;
   end Get_Pool;

   --------------
   -- Set_Pool --
   --------------

   procedure Set_Pool (Value : Pools.Pool_State) is
   begin
      Pool := Value;
   end Set_Pool;

   ---------------------
   -- Get_Compilation --
   ---------------------

   function Get_Compilation return Compilations.Compilation is
   begin
      return Compilation;
   end Get_Compilation;

   ---------------------
   -- Set_Compilation --
   ---------------------

   procedure Set_Compilation (Value : Compilations.Compilation) is
   begin
      Compilation := Value;
   end Set_Compilation;

end Asis.Gela.Current_State;

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
------------------------------------------------------------------------------
