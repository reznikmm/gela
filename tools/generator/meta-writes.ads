--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Meta.Classes;
with Meta.Read;

package Meta.Writes is

   procedure Write_Elements (Vector : Meta.Read.Class_Vectors.Vector);
   procedure Write_One_Elements (Item : Meta.Classes.Class);

end Meta.Writes;