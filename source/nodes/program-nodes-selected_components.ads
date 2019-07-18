--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Selected_Components;
with Program.Element_Visitors;

package Program.Nodes.Selected_Components is

   pragma Pure (Program.Nodes.Selected_Components);

   type Selected_Component is
     new Program.Nodes.Node
         and Program.Elements.Selected_Components.Selected_Component
         and Program.Elements.Selected_Components.Selected_Component_Text
     with private;

   function Create
    (Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Selector  : not null Program.Elements.Expressions.Expression_Access)
      return Selected_Component;

   type Implicit_Selected_Component is
     new Program.Nodes.Node
         and Program.Elements.Selected_Components.Selected_Component
     with private;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Selector             : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Selected_Component
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Selected_Component is
     abstract new Program.Nodes.Node
       and Program.Elements.Selected_Components.Selected_Component
     with record
        Prefix   : not null Program.Elements.Expressions.Expression_Access;
        Selector : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Selected_Component'Class);

   overriding procedure Visit
    (Self    : not null access Base_Selected_Component;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Prefix
    (Self : Base_Selected_Component)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Selector
    (Self : Base_Selected_Component)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Selected_Component
    (Self : Base_Selected_Component)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Selected_Component)
      return Boolean;

   type Selected_Component is
     new Base_Selected_Component
       and Program.Elements.Selected_Components.Selected_Component_Text
     with record
        Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Selected_Component_Text
    (Self : aliased in out Selected_Component)
      return Program.Elements.Selected_Components
          .Selected_Component_Text_Access;

   overriding function Dot_Token
    (Self : Selected_Component)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Selected_Component is
     new Base_Selected_Component
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Selected_Component_Text
    (Self : aliased in out Implicit_Selected_Component)
      return Program.Elements.Selected_Components
          .Selected_Component_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Selected_Component)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Selected_Component)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Selected_Component)
      return Boolean;

end Program.Nodes.Selected_Components;
