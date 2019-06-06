--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Meta.Classes;
with Meta.Read;

package Meta.Writes is

   procedure Write_Elements (Vector : Meta.Read.Class_Vectors.Vector);
   procedure Write_Elements_Body (Vector : Meta.Read.Class_Vectors.Vector);

   procedure Write_One_Element
     (Item      : Meta.Classes.Class;
      With_List : Boolean);

end Meta.Writes;
