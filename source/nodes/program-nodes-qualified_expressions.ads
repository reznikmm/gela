--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Qualified_Expressions;
with Program.Element_Visitors;

package Program.Nodes.Qualified_Expressions is

   pragma Preelaborate;

   type Qualified_Expression is
     new Program.Nodes.Node
         and Program.Elements.Qualified_Expressions.Qualified_Expression
         and Program.Elements.Qualified_Expressions.Qualified_Expression_Text
     with private;

   function Create
    (Subtype_Mark        : not null Program.Elements.Expressions
         .Expression_Access;
     Apostrophe_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Operand             : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Qualified_Expression;

   type Implicit_Qualified_Expression is
     new Program.Nodes.Node
         and Program.Elements.Qualified_Expressions.Qualified_Expression
     with private;

   function Create
    (Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Operand              : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Qualified_Expression
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Qualified_Expression is
     abstract new Program.Nodes.Node
       and Program.Elements.Qualified_Expressions.Qualified_Expression
     with record
        Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
        Operand      : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Qualified_Expression'Class);

   overriding procedure Visit
    (Self    : not null access Base_Qualified_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Subtype_Mark
    (Self : Base_Qualified_Expression)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Operand
    (Self : Base_Qualified_Expression)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Qualified_Expression_Element
    (Self : Base_Qualified_Expression)
      return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Qualified_Expression)
      return Boolean;

   type Qualified_Expression is
     new Base_Qualified_Expression
       and Program.Elements.Qualified_Expressions.Qualified_Expression_Text
     with record
        Apostrophe_Token    : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Qualified_Expression_Text
    (Self : aliased in out Qualified_Expression)
      return Program.Elements.Qualified_Expressions
          .Qualified_Expression_Text_Access;

   overriding function Apostrophe_Token
    (Self : Qualified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Qualified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Qualified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Qualified_Expression is
     new Base_Qualified_Expression
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Qualified_Expression_Text
    (Self : aliased in out Implicit_Qualified_Expression)
      return Program.Elements.Qualified_Expressions
          .Qualified_Expression_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Qualified_Expression)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Qualified_Expression)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Qualified_Expression)
      return Boolean;

end Program.Nodes.Qualified_Expressions;
