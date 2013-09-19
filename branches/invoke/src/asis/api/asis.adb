------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  Purpose:
--  Procedural wrapper over Object-Oriented ASIS implementation

with Gela.Contexts; pragma Unreferenced (Gela.Contexts);
with Gela.Compilations; pragma Unreferenced (Gela.Compilations);
with Gela.Unit_Containers; pragma Unreferenced (Gela.Unit_Containers);
with Gela.Compilation_Unit_Lists;
pragma Unreferenced (Gela.Compilation_Unit_Lists);

package body Asis is

   -------------
   -- To_List --
   -------------

   function To_List
     (Self : Gela.Types.Compilation_Unit_List)
      return Asis.Compilation_Unit_List
   is
      Count  : constant Natural := Self.Object.Units_Count (Self.Payload);
      Result : Asis.Compilation_Unit_List (1 .. Asis.ASIS_Natural (Count));
      First  : Gela.Types.Compilation_Unit_Cursor :=
        Self.Object.First (Self.Payload);
   begin
      for J in Result'Range loop
         Result (J) := Asis.Compilation_Unit
           (First.Object.Element (First.Payload));
         First := First.Object.Next (First.Payload);
      end loop;

      return Result;
   end To_List;

end Asis;

------------------------------------------------------------------------------
--  Copyright (c) 2013, Maxim Reznik
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
