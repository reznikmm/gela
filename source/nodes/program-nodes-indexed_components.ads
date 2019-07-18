--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Indexed_Components;
with Program.Element_Visitors;

package Program.Nodes.Indexed_Components is

   pragma Pure (Program.Nodes.Indexed_Components);

   type Indexed_Component is
     new Program.Nodes.Node
         and Program.Elements.Indexed_Components.Indexed_Component
         and Program.Elements.Indexed_Components.Indexed_Component_Text
     with private;

   function Create
    (Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expressions         : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Indexed_Component;

   type Implicit_Indexed_Component is
     new Program.Nodes.Node
         and Program.Elements.Indexed_Components.Indexed_Component
     with private;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Expressions          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Indexed_Component
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Indexed_Component is
     abstract new Program.Nodes.Node
       and Program.Elements.Indexed_Components.Indexed_Component
     with record
        Prefix      : not null Program.Elements.Expressions.Expression_Access;
        Expressions : not null Program.Elements.Expressions
          .Expression_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Indexed_Component'Class);

   overriding procedure Visit
    (Self    : not null access Base_Indexed_Component;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Prefix
    (Self : Base_Indexed_Component)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Expressions
    (Self : Base_Indexed_Component)
      return not null Program.Elements.Expressions.Expression_Vector_Access;

   overriding function Is_Indexed_Component
    (Self : Base_Indexed_Component)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Indexed_Component)
      return Boolean;

   type Indexed_Component is
     new Base_Indexed_Component
       and Program.Elements.Indexed_Components.Indexed_Component_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Indexed_Component_Text
    (Self : aliased in out Indexed_Component)
      return Program.Elements.Indexed_Components.Indexed_Component_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Indexed_Component)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Indexed_Component)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Indexed_Component is
     new Base_Indexed_Component
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Indexed_Component_Text
    (Self : aliased in out Implicit_Indexed_Component)
      return Program.Elements.Indexed_Components.Indexed_Component_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Indexed_Component)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Indexed_Component)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Indexed_Component)
      return Boolean;

end Program.Nodes.Indexed_Components;
