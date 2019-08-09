--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Null_Literals;
with Program.Element_Visitors;

package Program.Nodes.Null_Literals is

   pragma Preelaborate;

   type Null_Literal is
     new Program.Nodes.Node and Program.Elements.Null_Literals.Null_Literal
         and Program.Elements.Null_Literals.Null_Literal_Text
     with private;

   function Create
    (Null_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Null_Literal;

   type Implicit_Null_Literal is
     new Program.Nodes.Node and Program.Elements.Null_Literals.Null_Literal
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Null_Literal
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Null_Literal is
     abstract new Program.Nodes.Node
       and Program.Elements.Null_Literals.Null_Literal
     with null record;

   procedure Initialize (Self : aliased in out Base_Null_Literal'Class);

   overriding procedure Visit
    (Self    : not null access Base_Null_Literal;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Null_Literal_Element
    (Self : Base_Null_Literal)
      return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Null_Literal)
      return Boolean;

   type Null_Literal is
     new Base_Null_Literal and Program.Elements.Null_Literals.Null_Literal_Text
     with record
        Null_Literal_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Null_Literal_Text
    (Self : aliased in out Null_Literal)
      return Program.Elements.Null_Literals.Null_Literal_Text_Access;

   overriding function Null_Literal_Token
    (Self : Null_Literal)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Null_Literal is
     new Base_Null_Literal
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Null_Literal_Text
    (Self : aliased in out Implicit_Null_Literal)
      return Program.Elements.Null_Literals.Null_Literal_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Null_Literal)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Null_Literal)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Null_Literal)
      return Boolean;

end Program.Nodes.Null_Literals;
