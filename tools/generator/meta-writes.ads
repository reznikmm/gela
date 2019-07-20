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
   procedure Write_Visitors (Vector : Meta.Read.Class_Vectors.Vector);
   procedure Write_Iterators (Vector : Meta.Read.Class_Vectors.Vector);
   procedure Write_Factories (Vector : Meta.Read.Class_Vectors.Vector);
   procedure Write_Factories_Body (Vector : Meta.Read.Class_Vectors.Vector);

   procedure Write_One_Element
     (Item      : Meta.Classes.Class;
      With_List : Boolean);

   procedure Write_One_Node
     (Vector : Meta.Read.Class_Vectors.Vector;
      Item   : Meta.Classes.Class);

   procedure Write_One_Node_Body
     (Vector : Meta.Read.Class_Vectors.Vector;
      Item   : Meta.Classes.Class);

end Meta.Writes;
