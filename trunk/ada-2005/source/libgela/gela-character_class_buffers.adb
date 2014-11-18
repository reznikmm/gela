------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision$ $Date$

package body Gela.Character_Class_Buffers is

   ---------
   -- Put --
   ---------

   procedure Put
     (Object : in out Character_Class_Buffer;
      Item   : in     Character_Class;
      Full   : in out Boolean)
   is
   begin
      Object.Data (Object.Free) := Item;
      Object.Free := Object.Free + 1;
      Full := Full or
              (Object.Free <= Array_Index'Last / 2) /=
              (Object.Index <= Array_Index'Last / 2);
      if Full then
         Object.Data (Object.Free) := End_Of_Buffer;
      end if;
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (Object : in out Character_Class_Buffer;
      Item   :    out Character_Class)
   is
   begin
      Object.Index := Object.Index + 1;
      Item := Object.Data (Object.Index);
      if Item = End_Of_Buffer then
         Object.Index := Object.Index - 1;
      end if;
   end Get;

   ----------
   -- Mark --
   ----------

   procedure Mark (Object : in out Character_Class_Buffer) is
   begin
      Object.Mark := Object.Index;
   end Mark;

   ------------------
   -- Back_To_Mark --
   ------------------

   procedure Back_To_Mark (Object : in out Character_Class_Buffer) is
   begin
      Object.Index := Object.Mark;
   end Back_To_Mark;

end Gela.Character_Class_Buffers;

------------------------------------------------------------------------------
--  Copyright (c) 2008, Maxim Reznik
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
--
------------------------------------------------------------------------------