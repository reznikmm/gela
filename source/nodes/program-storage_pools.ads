--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Dummy_Subpools;

package Program.Storage_Pools is
   pragma Preelaborate;

   subtype Storage_Pool is Program.Dummy_Subpools.Dummy_Storage_Pool;

   type Storage_Pool_Access is not null access all Storage_Pool;

   Pool_Access : constant Storage_Pool_Access
     with Import, External_Name => "pool_access";

   Pool : Storage_Pool renames Pool_Access.all;

end Program.Storage_Pools;
