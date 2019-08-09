--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Numeric_Literals;
with Program.Element_Visitors;

package Program.Nodes.Numeric_Literals is

   pragma Preelaborate;

   type Numeric_Literal is
     new Program.Nodes.Node
         and Program.Elements.Numeric_Literals.Numeric_Literal
         and Program.Elements.Numeric_Literals.Numeric_Literal_Text
     with private;

   function Create
    (Numeric_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Numeric_Literal;

   type Implicit_Numeric_Literal is
     new Program.Nodes.Node
         and Program.Elements.Numeric_Literals.Numeric_Literal
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Numeric_Literal
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Numeric_Literal is
     abstract new Program.Nodes.Node
       and Program.Elements.Numeric_Literals.Numeric_Literal
     with null record;

   procedure Initialize (Self : aliased in out Base_Numeric_Literal'Class);

   overriding procedure Visit
    (Self    : not null access Base_Numeric_Literal;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Numeric_Literal_Element
    (Self : Base_Numeric_Literal)
      return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Numeric_Literal)
      return Boolean;

   type Numeric_Literal is
     new Base_Numeric_Literal
       and Program.Elements.Numeric_Literals.Numeric_Literal_Text
     with record
        Numeric_Literal_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Numeric_Literal_Text
    (Self : aliased in out Numeric_Literal)
      return Program.Elements.Numeric_Literals.Numeric_Literal_Text_Access;

   overriding function Numeric_Literal_Token
    (Self : Numeric_Literal)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Image (Self : Numeric_Literal) return Text;

   type Implicit_Numeric_Literal is
     new Base_Numeric_Literal
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Numeric_Literal_Text
    (Self : aliased in out Implicit_Numeric_Literal)
      return Program.Elements.Numeric_Literals.Numeric_Literal_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Numeric_Literal)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Numeric_Literal)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Numeric_Literal)
      return Boolean;

   overriding function Image (Self : Implicit_Numeric_Literal) return Text;

end Program.Nodes.Numeric_Literals;
