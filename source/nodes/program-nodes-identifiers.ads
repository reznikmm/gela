--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Element_Visitors;

package Program.Nodes.Identifiers is

   pragma Preelaborate;

   type Identifier is
     new Program.Nodes.Node and Program.Elements.Identifiers.Identifier
         and Program.Elements.Identifiers.Identifier_Text
     with private;

   function Create
    (Identifier_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Identifier;

   type Implicit_Identifier is
     new Program.Nodes.Node and Program.Elements.Identifiers.Identifier
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Identifier
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Identifier is
     abstract new Program.Nodes.Node
       and Program.Elements.Identifiers.Identifier
     with null record;

   procedure Initialize (Self : aliased in out Base_Identifier'Class);

   overriding procedure Visit
    (Self    : not null access Base_Identifier;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Identifier_Element
    (Self : Base_Identifier)
      return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Identifier)
      return Boolean;

   type Identifier is
     new Base_Identifier and Program.Elements.Identifiers.Identifier_Text
     with record
        Identifier_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Identifier_Text
    (Self : aliased in out Identifier)
      return Program.Elements.Identifiers.Identifier_Text_Access;

   overriding function Identifier_Token
    (Self : Identifier)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Image (Self : Identifier) return Text;

   type Implicit_Identifier is
     new Base_Identifier
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Identifier_Text
    (Self : aliased in out Implicit_Identifier)
      return Program.Elements.Identifiers.Identifier_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Identifier)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Identifier)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Identifier)
      return Boolean;

   overriding function Image (Self : Implicit_Identifier) return Text;

end Program.Nodes.Identifiers;
