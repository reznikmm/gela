--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Constraints;
with Program.Elements.Digits_Constraints;
with Program.Element_Visitors;

package Program.Nodes.Digits_Constraints is

   pragma Preelaborate;

   type Digits_Constraint is
     new Program.Nodes.Node
         and Program.Elements.Digits_Constraints.Digits_Constraint
         and Program.Elements.Digits_Constraints.Digits_Constraint_Text
     with private;

   function Create
    (Digits_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Range_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access)
      return Digits_Constraint;

   type Implicit_Digits_Constraint is
     new Program.Nodes.Node
         and Program.Elements.Digits_Constraints.Digits_Constraint
     with private;

   function Create
    (Digits_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return Implicit_Digits_Constraint
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Digits_Constraint is
     abstract new Program.Nodes.Node
       and Program.Elements.Digits_Constraints.Digits_Constraint
     with record
        Digits_Expression     : not null Program.Elements.Expressions
          .Expression_Access;
        Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Digits_Constraint'Class);

   overriding procedure Visit
    (Self    : not null access Base_Digits_Constraint;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Digits_Expression
    (Self : Base_Digits_Constraint)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Real_Range_Constraint
    (Self : Base_Digits_Constraint)
      return Program.Elements.Constraints.Constraint_Access;

   overriding function Is_Digits_Constraint_Element
    (Self : Base_Digits_Constraint)
      return Boolean;

   overriding function Is_Constraint_Element
    (Self : Base_Digits_Constraint)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Digits_Constraint)
      return Boolean;

   type Digits_Constraint is
     new Base_Digits_Constraint
       and Program.Elements.Digits_Constraints.Digits_Constraint_Text
     with record
        Digits_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Range_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Digits_Constraint_Text
    (Self : aliased in out Digits_Constraint)
      return Program.Elements.Digits_Constraints.Digits_Constraint_Text_Access;

   overriding function Digits_Token
    (Self : Digits_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Range_Token
    (Self : Digits_Constraint)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Digits_Constraint is
     new Base_Digits_Constraint
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Digits_Constraint_Text
    (Self : aliased in out Implicit_Digits_Constraint)
      return Program.Elements.Digits_Constraints.Digits_Constraint_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Digits_Constraint)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Digits_Constraint)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Digits_Constraint)
      return Boolean;

end Program.Nodes.Digits_Constraints;
