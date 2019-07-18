--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Parenthesized_Expressions;
with Program.Element_Visitors;

package Program.Nodes.Parenthesized_Expressions is

   pragma Pure (Program.Nodes.Parenthesized_Expressions);

   type Parenthesized_Expression is
     new Program.Nodes.Node
         and Program.Elements.Parenthesized_Expressions
           .Parenthesized_Expression
         and Program.Elements.Parenthesized_Expressions
           .Parenthesized_Expression_Text
     with private;

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression          : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Parenthesized_Expression;

   type Implicit_Parenthesized_Expression is
     new Program.Nodes.Node
         and Program.Elements.Parenthesized_Expressions
           .Parenthesized_Expression
     with private;

   function Create
    (Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Parenthesized_Expression
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Parenthesized_Expression is
     abstract new Program.Nodes.Node
       and Program.Elements.Parenthesized_Expressions.Parenthesized_Expression
     with record
        Expression : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Parenthesized_Expression'Class);

   overriding procedure Visit
    (Self    : not null access Base_Parenthesized_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Expression
    (Self : Base_Parenthesized_Expression)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Parenthesized_Expression
    (Self : Base_Parenthesized_Expression)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Parenthesized_Expression)
      return Boolean;

   type Parenthesized_Expression is
     new Base_Parenthesized_Expression
       and Program.Elements.Parenthesized_Expressions
         .Parenthesized_Expression_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Parenthesized_Expression_Text
    (Self : aliased in out Parenthesized_Expression)
      return Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Parenthesized_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Parenthesized_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Parenthesized_Expression is
     new Base_Parenthesized_Expression
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Parenthesized_Expression_Text
    (Self : aliased in out Implicit_Parenthesized_Expression)
      return Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Parenthesized_Expression)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Parenthesized_Expression)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Parenthesized_Expression)
      return Boolean;

end Program.Nodes.Parenthesized_Expressions;
