--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Attribute_References;
with Program.Element_Visitors;

package Program.Nodes.Attribute_References is

   pragma Pure (Program.Nodes.Attribute_References);

   type Attribute_Reference is
     new Program.Nodes.Node
         and Program.Elements.Attribute_References.Attribute_Reference
         and Program.Elements.Attribute_References.Attribute_Reference_Text
     with private;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Apostrophe_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Attribute_Designator : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expressions          : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Attribute_Reference;

   type Implicit_Attribute_Reference is
     new Program.Nodes.Node
         and Program.Elements.Attribute_References.Attribute_Reference
     with private;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Attribute_Designator : not null Program.Elements.Identifiers
         .Identifier_Access;
     Expressions          : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Attribute_Reference
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Attribute_Reference is
     abstract new Program.Nodes.Node
       and Program.Elements.Attribute_References.Attribute_Reference
     with record
        Prefix               : not null Program.Elements.Expressions
          .Expression_Access;
        Attribute_Designator : not null Program.Elements.Identifiers
          .Identifier_Access;
        Expressions          : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Attribute_Reference'Class);

   overriding procedure Visit
    (Self    : not null access Base_Attribute_Reference;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Prefix
    (Self : Base_Attribute_Reference)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Attribute_Designator
    (Self : Base_Attribute_Reference)
      return not null Program.Elements.Identifiers.Identifier_Access;

   overriding function Expressions
    (Self : Base_Attribute_Reference)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Attribute_Reference
    (Self : Base_Attribute_Reference)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Attribute_Reference)
      return Boolean;

   type Attribute_Reference is
     new Base_Attribute_Reference
       and Program.Elements.Attribute_References.Attribute_Reference_Text
     with record
        Apostrophe_Token    : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Attribute_Reference_Text
    (Self : aliased in out Attribute_Reference)
      return Program.Elements.Attribute_References
          .Attribute_Reference_Text_Access;

   overriding function Apostrophe_Token
    (Self : Attribute_Reference)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Attribute_Reference)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Attribute_Reference)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Attribute_Reference is
     new Base_Attribute_Reference
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Attribute_Reference_Text
    (Self : aliased in out Implicit_Attribute_Reference)
      return Program.Elements.Attribute_References
          .Attribute_Reference_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Attribute_Reference)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Attribute_Reference)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Attribute_Reference)
      return Boolean;

end Program.Nodes.Attribute_References;
