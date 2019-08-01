--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Pools.Subpools;

with Program.Element_Vectors;

package Program.Element_Vector_Factories is
   type Element_Vector_Factory
     (Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle) is
       tagged limited private;

   not overriding function Create_Element_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Element_Vectors.Element_Vector_Access;

private
   type Element_Vector_Factory
     (Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle) is
       tagged limited null record;

end Program.Element_Vector_Factories;
