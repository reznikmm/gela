--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Elements;
with System.Storage_Pools.Subpools;

package Program.Dummy_Subpools is
   pragma Preelaborate;

   type Dummy_Storage_Pool is
     new System.Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools
       with private;

private

   type Dummy_Subpool is
     new System.Storage_Pools.Subpools.Root_Subpool with null record;

   type Dummy_Storage_Pool is
     new System.Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools with
   record
      Last_Subpool : System.Storage_Pools.Subpools.Subpool_Handle;
   end record;

   overriding procedure Allocate_From_Subpool
     (Self      : in out Dummy_Storage_Pool;
      Address   : out System.Address;
      Size      : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count;
      Subpool   : not null System.Storage_Pools.Subpools.Subpool_Handle);

   overriding procedure Deallocate_Subpool
     (Self    : in out Dummy_Storage_Pool;
      Subpool : in out System.Storage_Pools.Subpools.Subpool_Handle);

   overriding function Create_Subpool
     (Self : in out Dummy_Storage_Pool)
      return not null System.Storage_Pools.Subpools.Subpool_Handle;

   overriding function Default_Subpool_For_Pool
     (Self : in out Dummy_Storage_Pool)
      return not null System.Storage_Pools.Subpools.Subpool_Handle;

end Program.Dummy_Subpools;
