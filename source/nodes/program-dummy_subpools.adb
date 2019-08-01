--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Program.Dummy_Subpools is

   type Dummy_Subpool_Access is access all Dummy_Subpool;

   procedure Free is new Ada.Unchecked_Deallocation
     (Dummy_Subpool, Dummy_Subpool_Access);

   ---------------------------
   -- Allocate_From_Subpool --
   ---------------------------

   overriding procedure Allocate_From_Subpool
     (Self      : in out Dummy_Storage_Pool;
      Address   : out System.Address;
      Size      : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count;
      Subpool   : not null System.Storage_Pools.Subpools.Subpool_Handle)
   is
      pragma Unreferenced (Self, Subpool);

      Default : System.Storage_Pools.Root_Storage_Pool'Class renames
        Dummy_Subpool_Access'Storage_Pool;
   begin
      Default.Allocate
        (Storage_Address          => Address,
         Size_In_Storage_Elements => Size,
         Alignment                => Alignment);
   end Allocate_From_Subpool;

   --------------------
   -- Create_Subpool --
   --------------------

   overriding function Create_Subpool
     (Self : in out Dummy_Storage_Pool)
      return not null System.Storage_Pools.Subpools.Subpool_Handle
   is
      Dummy : constant Dummy_Subpool_Access := new Dummy_Subpool;
   begin
      return Result : constant not null
        System.Storage_Pools.Subpools.Subpool_Handle :=
          System.Storage_Pools.Subpools.Subpool_Handle (Dummy)
      do
         System.Storage_Pools.Subpools.Set_Pool_Of_Subpool (Result, Self);
         Self.Last_Subpool := Result;
      end return;
   end Create_Subpool;

   ------------------------
   -- Deallocate_Subpool --
   ------------------------

   overriding procedure Deallocate_Subpool
     (Self    : in out Dummy_Storage_Pool;
      Subpool : in out System.Storage_Pools.Subpools.Subpool_Handle)
   is
      Dummy : Dummy_Subpool_Access := Dummy_Subpool_Access (Subpool);
   begin
      Free (Dummy);
      Self.Last_Subpool := null;
   end Deallocate_Subpool;

   ------------------------------
   -- Default_Subpool_For_Pool --
   ------------------------------

   overriding function Default_Subpool_For_Pool
     (Self : in out Dummy_Storage_Pool)
      return not null System.Storage_Pools.Subpools.Subpool_Handle is
   begin
      return Self.Last_Subpool;
   end Default_Subpool_For_Pool;

end Program.Dummy_Subpools;
