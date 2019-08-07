--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Index_Constraints;
with Program.Element_Visitors;

package Program.Nodes.Index_Constraints is

   pragma Preelaborate;

   type Index_Constraint is
     new Program.Nodes.Node
         and Program.Elements.Index_Constraints.Index_Constraint
         and Program.Elements.Index_Constraints.Index_Constraint_Text
     with private;

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ranges              : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Index_Constraint;

   type Implicit_Index_Constraint is
     new Program.Nodes.Node
         and Program.Elements.Index_Constraints.Index_Constraint
     with private;

   function Create
    (Ranges               : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Index_Constraint
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Index_Constraint is
     abstract new Program.Nodes.Node
       and Program.Elements.Index_Constraints.Index_Constraint
     with record
        Ranges : not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Index_Constraint'Class);

   overriding procedure Visit
    (Self    : not null access Base_Index_Constraint;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Ranges
    (Self : Base_Index_Constraint)
      return not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Vector_Access;

   overriding function Is_Index_Constraint
    (Self : Base_Index_Constraint)
      return Boolean;

   overriding function Is_Constraint
    (Self : Base_Index_Constraint)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Index_Constraint)
      return Boolean;

   type Index_Constraint is
     new Base_Index_Constraint
       and Program.Elements.Index_Constraints.Index_Constraint_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Index_Constraint_Text
    (Self : aliased in out Index_Constraint)
      return Program.Elements.Index_Constraints.Index_Constraint_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Index_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Index_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Index_Constraint is
     new Base_Index_Constraint
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Index_Constraint_Text
    (Self : aliased in out Implicit_Index_Constraint)
      return Program.Elements.Index_Constraints.Index_Constraint_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Index_Constraint)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Index_Constraint)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Index_Constraint)
      return Boolean;

end Program.Nodes.Index_Constraints;
