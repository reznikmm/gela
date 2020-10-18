--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package Program.Storage_Pools.Instance is
   Pool : aliased Storage_Pool;

   Pool_Access : constant Storage_Pool_Access := Pool'Access
     with Export, External_Name => "pool_access";

end Program.Storage_Pools.Instance;
