--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Element_Visitors;

package Program.Nodes.Defining_Identifiers is

   pragma Preelaborate;

   type Defining_Identifier is
     new Program.Nodes.Node
         and Program.Elements.Defining_Identifiers.Defining_Identifier
         and Program.Elements.Defining_Identifiers.Defining_Identifier_Text
     with private;

   function Create
    (Identifier_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Defining_Identifier;

   type Implicit_Defining_Identifier is
     new Program.Nodes.Node
         and Program.Elements.Defining_Identifiers.Defining_Identifier
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Defining_Identifier
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Defining_Identifier is
     abstract new Program.Nodes.Node
       and Program.Elements.Defining_Identifiers.Defining_Identifier
     with null record;

   procedure Initialize (Self : aliased in out Base_Defining_Identifier'Class);

   overriding procedure Visit
    (Self    : not null access Base_Defining_Identifier;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Defining_Identifier
    (Self : Base_Defining_Identifier)
      return Boolean;

   overriding function Is_Defining_Name
    (Self : Base_Defining_Identifier)
      return Boolean;

   type Defining_Identifier is
     new Base_Defining_Identifier
       and Program.Elements.Defining_Identifiers.Defining_Identifier_Text
     with record
        Identifier_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Defining_Identifier_Text
    (Self : aliased in out Defining_Identifier)
      return Program.Elements.Defining_Identifiers
          .Defining_Identifier_Text_Access;

   overriding function Identifier_Token
    (Self : Defining_Identifier)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Image (Self : Defining_Identifier) return Text;

   type Implicit_Defining_Identifier is
     new Base_Defining_Identifier
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Defining_Identifier_Text
    (Self : aliased in out Implicit_Defining_Identifier)
      return Program.Elements.Defining_Identifiers
          .Defining_Identifier_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Defining_Identifier)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Defining_Identifier)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Defining_Identifier)
      return Boolean;

   overriding function Image (Self : Implicit_Defining_Identifier) return Text;

end Program.Nodes.Defining_Identifiers;
