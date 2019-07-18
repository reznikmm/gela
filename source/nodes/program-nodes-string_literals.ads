--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.String_Literals;
with Program.Element_Visitors;

package Program.Nodes.String_Literals is

   pragma Pure (Program.Nodes.String_Literals);

   type String_Literal is
     new Program.Nodes.Node and Program.Elements.String_Literals.String_Literal
         and Program.Elements.String_Literals.String_Literal_Text
     with private;

   function Create
    (String_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return String_Literal;

   type Implicit_String_Literal is
     new Program.Nodes.Node and Program.Elements.String_Literals.String_Literal
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_String_Literal
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_String_Literal is
     abstract new Program.Nodes.Node
       and Program.Elements.String_Literals.String_Literal
     with null record;

   procedure Initialize (Self : aliased in out Base_String_Literal'Class);

   overriding procedure Visit
    (Self    : not null access Base_String_Literal;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_String_Literal
    (Self : Base_String_Literal)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_String_Literal)
      return Boolean;

   type String_Literal is
     new Base_String_Literal
       and Program.Elements.String_Literals.String_Literal_Text
     with record
        String_Literal_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_String_Literal_Text
    (Self : aliased in out String_Literal)
      return Program.Elements.String_Literals.String_Literal_Text_Access;

   overriding function String_Literal_Token
    (Self : String_Literal)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_String_Literal is
     new Base_String_Literal
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_String_Literal_Text
    (Self : aliased in out Implicit_String_Literal)
      return Program.Elements.String_Literals.String_Literal_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_String_Literal)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_String_Literal)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_String_Literal)
      return Boolean;

end Program.Nodes.String_Literals;
