--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Discriminant_Associations;
with Program.Elements.Discriminant_Constraints;
with Program.Element_Visitors;

package Program.Nodes.Discriminant_Constraints is

   pragma Preelaborate;

   type Discriminant_Constraint is
     new Program.Nodes.Node
         and Program.Elements.Discriminant_Constraints.Discriminant_Constraint
         and Program.Elements.Discriminant_Constraints
           .Discriminant_Constraint_Text
     with private;

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Discriminants       : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Discriminant_Constraint;

   type Implicit_Discriminant_Constraint is
     new Program.Nodes.Node
         and Program.Elements.Discriminant_Constraints.Discriminant_Constraint
     with private;

   function Create
    (Discriminants        : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Discriminant_Constraint
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Discriminant_Constraint is
     abstract new Program.Nodes.Node
       and Program.Elements.Discriminant_Constraints.Discriminant_Constraint
     with record
        Discriminants : not null Program.Elements.Discriminant_Associations
          .Discriminant_Association_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Discriminant_Constraint'Class);

   overriding procedure Visit
    (Self    : not null access Base_Discriminant_Constraint;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Discriminants
    (Self : Base_Discriminant_Constraint)
      return not null Program.Elements.Discriminant_Associations
          .Discriminant_Association_Vector_Access;

   overriding function Is_Discriminant_Constraint
    (Self : Base_Discriminant_Constraint)
      return Boolean;

   overriding function Is_Constraint
    (Self : Base_Discriminant_Constraint)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Discriminant_Constraint)
      return Boolean;

   type Discriminant_Constraint is
     new Base_Discriminant_Constraint
       and Program.Elements.Discriminant_Constraints
         .Discriminant_Constraint_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Discriminant_Constraint_Text
    (Self : aliased in out Discriminant_Constraint)
      return Program.Elements.Discriminant_Constraints
          .Discriminant_Constraint_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Discriminant_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Discriminant_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Discriminant_Constraint is
     new Base_Discriminant_Constraint
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Discriminant_Constraint_Text
    (Self : aliased in out Implicit_Discriminant_Constraint)
      return Program.Elements.Discriminant_Constraints
          .Discriminant_Constraint_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discriminant_Constraint)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discriminant_Constraint)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discriminant_Constraint)
      return Boolean;

end Program.Nodes.Discriminant_Constraints;
