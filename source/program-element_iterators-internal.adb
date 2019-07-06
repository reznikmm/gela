--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Indexed_Components;
with Program.Elements.Expressions;
with Program.Element_Visitors;

separate (Program.Element_Iterators)
package body Internal is
   function F is new Generic_Child
     (Element      => Program.Elements.Indexed_Components.Indexed_Component,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Indexed_Components.Prefix);

   function F2 is new Generic_Vector
     (Parent        => Program.Elements.Indexed_Components.Indexed_Component,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Indexed_Components.Expressions);

   Indexed_Component_Getters : aliased constant Getter_Array :=
     (1 => (False, Prefix, F'Access),
      2 => (True, Expressions, F2'Access));

   type Visitor is new Program.Element_Visitors.Element_Visitor with record
      Result : access constant Getter_Array;
   end record;

   procedure Indexed_Component
    (Self    : in out Visitor;
     Element : not null Program.Elements.Indexed_Components
         .Indexed_Component_Access);

   procedure Indexed_Component
    (Self    : in out Visitor;
     Element : not null Program.Elements.Indexed_Components
     .Indexed_Component_Access)
   is
      pragma Unreferenced (Element);
   begin
      Self.Result := Indexed_Component_Getters'Access;
   end Indexed_Component;

   function Get
     (Parent : Program.Elements.Element_Access)
      return access constant Getter_Array
   is
      V : Visitor;
   begin
      Parent.Visit (V);
      return V.Result;
   end Get;

end Internal;
