--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Storage_Pools.Instance is
   Pool : aliased Storage_Pool;

   Pool_Access : constant Storage_Pool_Access := Pool'Access
     with Export, External_Name => "pool_access";

end Program.Storage_Pools.Instance;
