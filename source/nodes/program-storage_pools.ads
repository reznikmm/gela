--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Dummy_Subpools;

package Program.Storage_Pools is

   subtype Storage_Pool is Program.Dummy_Subpools.Dummy_Storage_Pool;

   Pool : Storage_Pool;

end Program.Storage_Pools;
