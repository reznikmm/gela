------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: $
--  Purpose:
--  Generic extendable vectors of items with save/load support.

with System.Storage_Pools;

generic
   type Item_Type is private;
   type Index_Type is range <>;

   Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
package Gela.Containers.Vectors is
   pragma Preelaborate;

   type Vector is limited private;

   procedure Add
     (Object : in out Vector;
      Item   : in     Item_Type);

   procedure Add
     (Object : in out Vector;
      Item   : in     Item_Type;
      Index  :    out Index_Type);

   procedure Clear (Object : in out Vector);

   function Length (Object : Vector) return Index_Type'Base;

   function Get (Object : Vector; Index : Index_Type) return Item_Type;
   pragma Inline (Get);

   procedure Set
     (Object : in out Vector;
      Index  : in     Index_Type;
      Item   : in     Item_Type);
   pragma Inline (Set);

   generic
      type Array_Index is range <>;
      type Array_Type is array (Array_Index range <>) of Item_Type;
   package Arrays is
      procedure Add
        (Object : in out Vector;
         Value  : in     Array_Type);

      procedure Get
        (Object : in     Vector;
         Index  : in     Index_Type;
         Value  :    out Array_Type);
   end Arrays;

private
   subtype Index_Natural is Index_Type'Base range 0 .. Index_Type'Last;

   Default_Size : constant Index_Type := 1024; --  8 * 4096 / Item_Type'Size;
   Level_Size   : constant Index_Type := 1024;

   type Table is array (Index_Natural range <>) of Item_Type;
   type Table_Access is access Table;
   for Table_Access'Storage_Pool use Pool;

   subtype First_Table is Table (0 .. Default_Size - 1);
   type First_Table_Access is access First_Table;
   for First_Table_Access'Storage_Pool use Pool;

   type Second_Table is array (0 .. Level_Size - 1) of First_Table_Access;
   type Second_Table_Access is access Second_Table;
   for Second_Table_Access'Storage_Pool use Pool;

   type Third_Table is array (0 .. Level_Size - 1) of Second_Table_Access;

   type Vector is record
      Length       : Index_Natural := 0;
      Saved_Part   : Table_Access;
      Third_Level  : Third_Table;
   end record;

end Gela.Containers.Vectors;

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
