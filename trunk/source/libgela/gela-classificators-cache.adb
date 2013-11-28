------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: $ $Date: $

package body Gela.Classificators.Cache is

   type Second_Table is
     array (Code_Point range 0 .. 255) of
     Character_Class_Buffers.Character_Class;

   type Second_Table_Access is access all Second_Table;

   type First_Table is
     array (Code_Point range 0 .. Code_Point'Last / 256) of
     Second_Table_Access;

   Table : First_Table;

   procedure Initialize (Index : in Code_Point);

   -------------------------
   -- Get_Character_Class --
   -------------------------

   function Get_Character_Class
     (Code : Code_Point)
     return Character_Class_Buffers.Character_Class
   is
      Second_Table : Second_Table_Access := Table (Code / 256);
   begin
      if Second_Table = null then
         Initialize (Code / 256);
         Second_Table := Table (Code / 256);
      end if;

      return Second_Table (Code mod 256);
   end Get_Character_Class;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Index : in Code_Point) is
      Round  : constant Code_Point := Index * 256;
      Object : constant Second_Table_Access := new Second_Table;
   begin
      for J in Round .. Round + 255 loop
         Object (J mod 256) := To_Character_Class (J);
      end loop;

      Table (Index) := Object;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Table (0) = null then
         Initialize (0);
      end if;
   end Initialize;

end Gela.Classificators.Cache;
cat ../../COPYING


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
