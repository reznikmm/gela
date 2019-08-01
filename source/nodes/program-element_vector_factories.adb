--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Vectors;
with Program.Storage_Pools;

package body Program.Element_Vector_Factories is

   type Element_Vector_Access is not null access Program.Nodes.Vectors.Vector
     with Storage_Pool => Program.Storage_Pools.Pool;

   ---------------------------
   -- Create_Element_Vector --
   ---------------------------

   not overriding function Create_Element_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Element_Vectors.Element_Vector_Access
   is
      Result : constant Element_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Vectors.Vector'
          (Program.Nodes.Vectors.Create (Each));
   begin
      return Program.Element_Vectors.Element_Vector_Access (Result);
   end Create_Element_Vector;

end Program.Element_Vector_Factories;
