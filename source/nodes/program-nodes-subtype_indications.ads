--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Constraints;
with Program.Elements.Subtype_Indications;
with Program.Element_Visitors;

package Program.Nodes.Subtype_Indications is

   pragma Preelaborate;

   type Subtype_Indication is
     new Program.Nodes.Node
         and Program.Elements.Subtype_Indications.Subtype_Indication
         and Program.Elements.Subtype_Indications.Subtype_Indication_Text
     with private;

   function Create
    (Not_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
     Constraint   : Program.Elements.Constraints.Constraint_Access)
      return Subtype_Indication;

   type Implicit_Subtype_Indication is
     new Program.Nodes.Node
         and Program.Elements.Subtype_Indications.Subtype_Indication
     with private;

   function Create
    (Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Constraint           : Program.Elements.Constraints.Constraint_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Subtype_Indication
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Subtype_Indication is
     abstract new Program.Nodes.Node
       and Program.Elements.Subtype_Indications.Subtype_Indication
     with record
        Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
        Constraint   : Program.Elements.Constraints.Constraint_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Subtype_Indication'Class);

   overriding procedure Visit
    (Self    : not null access Base_Subtype_Indication;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Subtype_Mark
    (Self : Base_Subtype_Indication)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Constraint
    (Self : Base_Subtype_Indication)
      return Program.Elements.Constraints.Constraint_Access;

   overriding function Is_Subtype_Indication
    (Self : Base_Subtype_Indication)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Subtype_Indication)
      return Boolean;

   type Subtype_Indication is
     new Base_Subtype_Indication
       and Program.Elements.Subtype_Indications.Subtype_Indication_Text
     with record
        Not_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Subtype_Indication_Text
    (Self : aliased in out Subtype_Indication)
      return Program.Elements.Subtype_Indications
          .Subtype_Indication_Text_Access;

   overriding function Not_Token
    (Self : Subtype_Indication)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Subtype_Indication)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not_Null (Self : Subtype_Indication) return Boolean;

   type Implicit_Subtype_Indication is
     new Base_Subtype_Indication
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not_Null         : Boolean;
     end record;

   overriding function To_Subtype_Indication_Text
    (Self : aliased in out Implicit_Subtype_Indication)
      return Program.Elements.Subtype_Indications
          .Subtype_Indication_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Subtype_Indication)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Subtype_Indication)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Subtype_Indication)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Subtype_Indication)
      return Boolean;

end Program.Nodes.Subtype_Indications;
