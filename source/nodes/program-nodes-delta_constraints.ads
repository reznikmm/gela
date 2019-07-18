--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Constraints;
with Program.Elements.Delta_Constraints;
with Program.Element_Visitors;

package Program.Nodes.Delta_Constraints is

   pragma Pure (Program.Nodes.Delta_Constraints);

   type Delta_Constraint is
     new Program.Nodes.Node
         and Program.Elements.Delta_Constraints.Delta_Constraint
         and Program.Elements.Delta_Constraints.Delta_Constraint_Text
     with private;

   function Create
    (Delta_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Range_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access)
      return Delta_Constraint;

   type Implicit_Delta_Constraint is
     new Program.Nodes.Node
         and Program.Elements.Delta_Constraints.Delta_Constraint
     with private;

   function Create
    (Delta_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return Implicit_Delta_Constraint
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Delta_Constraint is
     abstract new Program.Nodes.Node
       and Program.Elements.Delta_Constraints.Delta_Constraint
     with record
        Delta_Expression      : not null Program.Elements.Expressions
          .Expression_Access;
        Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Delta_Constraint'Class);

   overriding procedure Visit
    (Self    : not null access Base_Delta_Constraint;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Delta_Expression
    (Self : Base_Delta_Constraint)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Real_Range_Constraint
    (Self : Base_Delta_Constraint)
      return Program.Elements.Constraints.Constraint_Access;

   overriding function Is_Delta_Constraint
    (Self : Base_Delta_Constraint)
      return Boolean;

   overriding function Is_Constraint
    (Self : Base_Delta_Constraint)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Delta_Constraint)
      return Boolean;

   type Delta_Constraint is
     new Base_Delta_Constraint
       and Program.Elements.Delta_Constraints.Delta_Constraint_Text
     with record
        Delta_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
        Range_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Delta_Constraint_Text
    (Self : aliased in out Delta_Constraint)
      return Program.Elements.Delta_Constraints.Delta_Constraint_Text_Access;

   overriding function Delta_Token
    (Self : Delta_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Range_Token
    (Self : Delta_Constraint)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Delta_Constraint is
     new Base_Delta_Constraint
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Delta_Constraint_Text
    (Self : aliased in out Implicit_Delta_Constraint)
      return Program.Elements.Delta_Constraints.Delta_Constraint_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Delta_Constraint)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Delta_Constraint)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Delta_Constraint)
      return Boolean;

end Program.Nodes.Delta_Constraints;
