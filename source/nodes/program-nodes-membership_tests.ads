--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Membership_Tests;
with Program.Element_Visitors;

package Program.Nodes.Membership_Tests is

   pragma Preelaborate;

   type Membership_Test is
     new Program.Nodes.Node
         and Program.Elements.Membership_Tests.Membership_Test
         and Program.Elements.Membership_Tests.Membership_Test_Text
     with private;

   function Create
    (Expression : not null Program.Elements.Expressions.Expression_Access;
     Not_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     In_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices    : not null Program.Element_Vectors.Element_Vector_Access)
      return Membership_Test;

   type Implicit_Membership_Test is
     new Program.Nodes.Node
         and Program.Elements.Membership_Tests.Membership_Test
     with private;

   function Create
    (Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not              : Boolean := False)
      return Implicit_Membership_Test
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Membership_Test is
     abstract new Program.Nodes.Node
       and Program.Elements.Membership_Tests.Membership_Test
     with record
        Expression : not null Program.Elements.Expressions.Expression_Access;
        Choices    : not null Program.Element_Vectors.Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Membership_Test'Class);

   overriding procedure Visit
    (Self    : not null access Base_Membership_Test;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Expression
    (Self : Base_Membership_Test)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Choices
    (Self : Base_Membership_Test)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_Membership_Test
    (Self : Base_Membership_Test)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Membership_Test)
      return Boolean;

   type Membership_Test is
     new Base_Membership_Test
       and Program.Elements.Membership_Tests.Membership_Test_Text
     with record
        Not_Token : Program.Lexical_Elements.Lexical_Element_Access;
        In_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Membership_Test_Text
    (Self : aliased in out Membership_Test)
      return Program.Elements.Membership_Tests.Membership_Test_Text_Access;

   overriding function Not_Token
    (Self : Membership_Test)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function In_Token
    (Self : Membership_Test)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not (Self : Membership_Test) return Boolean;

   type Implicit_Membership_Test is
     new Base_Membership_Test
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not              : Boolean;
     end record;

   overriding function To_Membership_Test_Text
    (Self : aliased in out Implicit_Membership_Test)
      return Program.Elements.Membership_Tests.Membership_Test_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Membership_Test)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Membership_Test)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Membership_Test)
      return Boolean;

   overriding function Has_Not
    (Self : Implicit_Membership_Test)
      return Boolean;

end Program.Nodes.Membership_Tests;
