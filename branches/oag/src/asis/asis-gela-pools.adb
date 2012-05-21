with Ada.Unchecked_Deallocation;

with Asis.Gela.Current_State;

package body Asis.Gela.Pools is

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool                     : in out Storage_Pool;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     Storage_Count;
      Alignment                : in     Storage_Count)
   is
      Too_Large : exception;
      Head      : constant Block_Access := Current_State.Get_Pool.Head;
      Block     : Block_Access;
      Offset    : Storage_Count := 0;
   begin
      if Size_In_Storage_Elements > Block_Size then
         raise Too_Large;
      end if;

      if Head /= null then
         Offset := (Head.Free + Alignment - 1) / Alignment * Alignment;
      end if;

      if Head = null or Offset + Size_In_Storage_Elements > Block_Size then
         Block           := new Storage_Block;
         Block.Next      := Head;
         Offset          := 0;
         Current_State.Set_Pool ((Head => Block));
      else
         Block := Head;
      end if;

      Storage_Address := Block.Data (Offset)'Address;
      Block.Free      := Offset + Size_In_Storage_Elements;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool                     : in out Storage_Pool;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     Storage_Count;
      Alignment                : in     Storage_Count)
   is
      Individual_Deallocation : exception;
   begin
      raise Individual_Deallocation;
   end Deallocate;

   --------------------
   -- Deallocate_All --
   --------------------

   procedure Deallocate_All (Pool  : in out Pool_State) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Storage_Block, Block_Access);

      Head  : Block_Access := Pool.Head;
      Next  : Block_Access;
   begin
      while Head /= null loop
         Next := Head.Next;
         Free (Head);
         Head := Next;
      end loop;
   end Deallocate_All;

   --------------
   -- New_Pool --
   --------------

   function New_Pool return Pool_State is
   begin
      return (Head => null);
   end New_Pool;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (Pool : in Storage_Pool) return Storage_Count is
   begin
      return Storage_Count'Last;
   end Storage_Size;

end Asis.Gela.Pools;



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
