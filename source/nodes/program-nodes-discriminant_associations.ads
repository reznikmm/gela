--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Discriminant_Associations;
with Program.Element_Visitors;

package Program.Nodes.Discriminant_Associations is

   pragma Pure (Program.Nodes.Discriminant_Associations);

   type Discriminant_Association is
     new Program.Nodes.Node
         and Program.Elements.Discriminant_Associations
           .Discriminant_Association
         and Program.Elements.Discriminant_Associations
           .Discriminant_Association_Text
     with private;

   function Create
    (Selector_Names : Program.Elements.Identifiers.Identifier_Vector_Access;
     Arrow_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Expression     : not null Program.Elements.Expressions.Expression_Access)
      return Discriminant_Association;

   type Implicit_Discriminant_Association is
     new Program.Nodes.Node
         and Program.Elements.Discriminant_Associations
           .Discriminant_Association
     with private;

   function Create
    (Selector_Names       : Program.Elements.Identifiers
         .Identifier_Vector_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Discriminant_Association
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Discriminant_Association is
     abstract new Program.Nodes.Node
       and Program.Elements.Discriminant_Associations.Discriminant_Association
     with record
        Selector_Names : Program.Elements.Identifiers.Identifier_Vector_Access;
        Expression     : not null Program.Elements.Expressions
          .Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Discriminant_Association'Class);

   overriding procedure Visit
    (Self    : not null access Base_Discriminant_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Selector_Names
    (Self : Base_Discriminant_Association)
      return Program.Elements.Identifiers.Identifier_Vector_Access;

   overriding function Expression
    (Self : Base_Discriminant_Association)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Discriminant_Association
    (Self : Base_Discriminant_Association)
      return Boolean;

   overriding function Is_Association
    (Self : Base_Discriminant_Association)
      return Boolean;

   type Discriminant_Association is
     new Base_Discriminant_Association
       and Program.Elements.Discriminant_Associations
         .Discriminant_Association_Text
     with record
        Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Discriminant_Association_Text
    (Self : aliased in out Discriminant_Association)
      return Program.Elements.Discriminant_Associations
          .Discriminant_Association_Text_Access;

   overriding function Arrow_Token
    (Self : Discriminant_Association)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Discriminant_Association is
     new Base_Discriminant_Association
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Discriminant_Association_Text
    (Self : aliased in out Implicit_Discriminant_Association)
      return Program.Elements.Discriminant_Associations
          .Discriminant_Association_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discriminant_Association)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discriminant_Association)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discriminant_Association)
      return Boolean;

end Program.Nodes.Discriminant_Associations;
