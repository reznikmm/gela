--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Elements.Constraints;
with Program.Elements.Discrete_Subtype_Indications;
with Program.Element_Visitors;

package Program.Nodes.Discrete_Subtype_Indications is

   pragma Preelaborate;

   type Discrete_Subtype_Indication is
     new Program.Nodes.Node
         and Program.Elements.Discrete_Subtype_Indications
           .Discrete_Subtype_Indication
         and Program.Elements.Discrete_Subtype_Indications
           .Discrete_Subtype_Indication_Text
     with private;

   function Create
    (Subtype_Mark                   : not null Program.Elements.Expressions
         .Expression_Access;
     Constraint                     : Program.Elements.Constraints
         .Constraint_Access;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return Discrete_Subtype_Indication;

   type Implicit_Discrete_Subtype_Indication is
     new Program.Nodes.Node
         and Program.Elements.Discrete_Subtype_Indications
           .Discrete_Subtype_Indication
     with private;

   function Create
    (Subtype_Mark                   : not null Program.Elements.Expressions
         .Expression_Access;
     Constraint                     : Program.Elements.Constraints
         .Constraint_Access;
     Is_Part_Of_Implicit            : Boolean := False;
     Is_Part_Of_Inherited           : Boolean := False;
     Is_Part_Of_Instance            : Boolean := False;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return Implicit_Discrete_Subtype_Indication
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Discrete_Subtype_Indication is
     abstract new Program.Nodes.Node
       and Program.Elements.Discrete_Subtype_Indications
         .Discrete_Subtype_Indication
     with record
        Subtype_Mark                   : not null Program.Elements.Expressions
          .Expression_Access;
        Constraint                     : Program.Elements.Constraints
          .Constraint_Access;
        Is_Discrete_Subtype_Definition : Boolean;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Discrete_Subtype_Indication'Class);

   overriding procedure Visit
    (Self    : not null access Base_Discrete_Subtype_Indication;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Subtype_Mark
    (Self : Base_Discrete_Subtype_Indication)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Constraint
    (Self : Base_Discrete_Subtype_Indication)
      return Program.Elements.Constraints.Constraint_Access;

   overriding function Is_Discrete_Subtype_Definition
    (Self : Base_Discrete_Subtype_Indication)
      return Boolean;

   overriding function Is_Discrete_Subtype_Indication
    (Self : Base_Discrete_Subtype_Indication)
      return Boolean;

   overriding function Is_Discrete_Range
    (Self : Base_Discrete_Subtype_Indication)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Discrete_Subtype_Indication)
      return Boolean;

   type Discrete_Subtype_Indication is
     new Base_Discrete_Subtype_Indication
       and Program.Elements.Discrete_Subtype_Indications
         .Discrete_Subtype_Indication_Text
     with null record;

   overriding function To_Discrete_Subtype_Indication_Text
    (Self : aliased in out Discrete_Subtype_Indication)
      return Program.Elements.Discrete_Subtype_Indications
          .Discrete_Subtype_Indication_Text_Access;

   type Implicit_Discrete_Subtype_Indication is
     new Base_Discrete_Subtype_Indication
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Discrete_Subtype_Indication_Text
    (Self : aliased in out Implicit_Discrete_Subtype_Indication)
      return Program.Elements.Discrete_Subtype_Indications
          .Discrete_Subtype_Indication_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discrete_Subtype_Indication)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discrete_Subtype_Indication)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discrete_Subtype_Indication)
      return Boolean;

end Program.Nodes.Discrete_Subtype_Indications;
