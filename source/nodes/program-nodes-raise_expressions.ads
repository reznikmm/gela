--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Raise_Expressions;
with Program.Element_Visitors;

package Program.Nodes.Raise_Expressions is

   pragma Preelaborate;

   type Raise_Expression is
     new Program.Nodes.Node
         and Program.Elements.Raise_Expressions.Raise_Expression
         and Program.Elements.Raise_Expressions.Raise_Expression_Text
     with private;

   function Create
    (Raise_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Name     : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Associated_Message : Program.Elements.Expressions.Expression_Access)
      return Raise_Expression;

   type Implicit_Raise_Expression is
     new Program.Nodes.Node
         and Program.Elements.Raise_Expressions.Raise_Expression
     with private;

   function Create
    (Exception_Name       : not null Program.Elements.Expressions
         .Expression_Access;
     Associated_Message   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Raise_Expression
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Raise_Expression is
     abstract new Program.Nodes.Node
       and Program.Elements.Raise_Expressions.Raise_Expression
     with record
        Exception_Name     : not null Program.Elements.Expressions
          .Expression_Access;
        Associated_Message : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Raise_Expression'Class);

   overriding procedure Visit
    (Self    : not null access Base_Raise_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Exception_Name
    (Self : Base_Raise_Expression)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Associated_Message
    (Self : Base_Raise_Expression)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Raise_Expression
    (Self : Base_Raise_Expression)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Raise_Expression)
      return Boolean;

   type Raise_Expression is
     new Base_Raise_Expression
       and Program.Elements.Raise_Expressions.Raise_Expression_Text
     with record
        Raise_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
        With_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Raise_Expression_Text
    (Self : aliased in out Raise_Expression)
      return Program.Elements.Raise_Expressions.Raise_Expression_Text_Access;

   overriding function Raise_Token
    (Self : Raise_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Raise_Expression)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Raise_Expression is
     new Base_Raise_Expression
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Raise_Expression_Text
    (Self : aliased in out Implicit_Raise_Expression)
      return Program.Elements.Raise_Expressions.Raise_Expression_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Raise_Expression)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Raise_Expression)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Raise_Expression)
      return Boolean;

end Program.Nodes.Raise_Expressions;
