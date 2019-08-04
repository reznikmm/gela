--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Attribute_References;
with Program.Elements.Discrete_Range_Attribute_References;
with Program.Element_Visitors;

package Program.Nodes.Discrete_Range_Attribute_References is

   pragma Pure (Program.Nodes.Discrete_Range_Attribute_References);

   type Discrete_Range_Attribute_Reference is
     new Program.Nodes.Node
         and Program.Elements.Discrete_Range_Attribute_References
           .Discrete_Range_Attribute_Reference
         and Program.Elements.Discrete_Range_Attribute_References
           .Discrete_Range_Attribute_Reference_Text
     with private;

   function Create
    (Range_Attribute : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access)
      return Discrete_Range_Attribute_Reference;

   type Implicit_Discrete_Range_Attribute_Reference is
     new Program.Nodes.Node
         and Program.Elements.Discrete_Range_Attribute_References
           .Discrete_Range_Attribute_Reference
     with private;

   function Create
    (Range_Attribute      : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Discrete_Range_Attribute_Reference
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Discrete_Range_Attribute_Reference is
     abstract new Program.Nodes.Node
       and Program.Elements.Discrete_Range_Attribute_References
         .Discrete_Range_Attribute_Reference
     with record
        Range_Attribute : not null Program.Elements.Attribute_References
          .Attribute_Reference_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Discrete_Range_Attribute_Reference'Class);

   overriding procedure Visit
    (Self    : not null access Base_Discrete_Range_Attribute_Reference;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Range_Attribute
    (Self : Base_Discrete_Range_Attribute_Reference)
      return not null Program.Elements.Attribute_References
          .Attribute_Reference_Access;

   overriding function Is_Discrete_Range_Attribute_Reference
    (Self : Base_Discrete_Range_Attribute_Reference)
      return Boolean;

   overriding function Is_Discrete_Range
    (Self : Base_Discrete_Range_Attribute_Reference)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Discrete_Range_Attribute_Reference)
      return Boolean;

   type Discrete_Range_Attribute_Reference is
     new Base_Discrete_Range_Attribute_Reference
       and Program.Elements.Discrete_Range_Attribute_References
         .Discrete_Range_Attribute_Reference_Text
     with null record;

   overriding function To_Discrete_Range_Attribute_Reference_Text
    (Self : aliased in out Discrete_Range_Attribute_Reference)
      return Program.Elements.Discrete_Range_Attribute_References
          .Discrete_Range_Attribute_Reference_Text_Access;

   type Implicit_Discrete_Range_Attribute_Reference is
     new Base_Discrete_Range_Attribute_Reference
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Discrete_Range_Attribute_Reference_Text
    (Self : aliased in out Implicit_Discrete_Range_Attribute_Reference)
      return Program.Elements.Discrete_Range_Attribute_References
          .Discrete_Range_Attribute_Reference_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discrete_Range_Attribute_Reference)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discrete_Range_Attribute_Reference)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discrete_Range_Attribute_Reference)
      return Boolean;

end Program.Nodes.Discrete_Range_Attribute_References;
