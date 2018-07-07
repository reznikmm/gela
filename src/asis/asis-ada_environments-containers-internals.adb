package body Asis.Ada_Environments.Containers.Internals is

   -------------
   -- Convert --
   -------------

   function Convert
     (Context : Asis.Context;
      Self    : Gela.Unit_Containers.Unit_Container_Access)
      return Asis.Ada_Environments.Containers.Container is
   begin
      return (Context, Self);
   end Convert;

   -------------
   -- To_List --
   -------------

   function To_List
     (Set : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access)
      return Asis.Compilation_Unit_List
   is
      Index : ASIS_Natural := ASIS_Natural (Set.Length);
      Pos   : Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor'Class :=
        Set.First;
   begin
      return Result : Asis.Compilation_Unit_List (1 .. Index) do
         Index := 1;

         while Pos.Has_Element loop
            Result (Index) := (Data => Pos.Element);
            Index := Index + 1;
            Pos.Next;
         end loop;
      end return;
   end To_List;

end Asis.Ada_Environments.Containers.Internals;

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
