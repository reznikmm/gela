--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Character_Literals;
with Program.Element_Visitors;

package Program.Nodes.Character_Literals is

   pragma Preelaborate;

   type Character_Literal is
     new Program.Nodes.Node
         and Program.Elements.Character_Literals.Character_Literal
         and Program.Elements.Character_Literals.Character_Literal_Text
     with private;

   function Create
    (Character_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Character_Literal;

   type Implicit_Character_Literal is
     new Program.Nodes.Node
         and Program.Elements.Character_Literals.Character_Literal
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Character_Literal
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Character_Literal is
     abstract new Program.Nodes.Node
       and Program.Elements.Character_Literals.Character_Literal
     with null record;

   procedure Initialize (Self : aliased in out Base_Character_Literal'Class);

   overriding procedure Visit
    (Self    : not null access Base_Character_Literal;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Character_Literal
    (Self : Base_Character_Literal)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Character_Literal)
      return Boolean;

   type Character_Literal is
     new Base_Character_Literal
       and Program.Elements.Character_Literals.Character_Literal_Text
     with record
        Character_Literal_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Character_Literal_Text
    (Self : aliased in out Character_Literal)
      return Program.Elements.Character_Literals.Character_Literal_Text_Access;

   overriding function Character_Literal_Token
    (Self : Character_Literal)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Image (Self : Character_Literal) return Text;

   type Implicit_Character_Literal is
     new Base_Character_Literal
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Character_Literal_Text
    (Self : aliased in out Implicit_Character_Literal)
      return Program.Elements.Character_Literals.Character_Literal_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Character_Literal)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Character_Literal)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Character_Literal)
      return Boolean;

   overriding function Image (Self : Implicit_Character_Literal) return Text;

end Program.Nodes.Character_Literals;
