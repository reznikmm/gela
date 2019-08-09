--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Subtype_Indications is

   function Create
    (Not_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
     Constraint   : Program.Elements.Constraints.Constraint_Access)
      return Subtype_Indication is
   begin
      return Result : Subtype_Indication :=
        (Not_Token => Not_Token, Null_Token => Null_Token,
         Subtype_Mark => Subtype_Mark, Constraint => Constraint,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Constraint           : Program.Elements.Constraints.Constraint_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Subtype_Indication is
   begin
      return Result : Implicit_Subtype_Indication :=
        (Subtype_Mark => Subtype_Mark, Constraint => Constraint,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Not_Null => Has_Not_Null, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Subtype_Mark
    (Self : Base_Subtype_Indication)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Subtype_Mark;
   end Subtype_Mark;

   overriding function Constraint
    (Self : Base_Subtype_Indication)
      return Program.Elements.Constraints.Constraint_Access is
   begin
      return Self.Constraint;
   end Constraint;

   overriding function Not_Token
    (Self : Subtype_Indication)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Subtype_Indication)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Has_Not_Null
    (Self : Subtype_Indication)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Subtype_Indication)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Subtype_Indication)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Subtype_Indication)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not_Null
    (Self : Implicit_Subtype_Indication)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   procedure Initialize
    (Self : aliased in out Base_Subtype_Indication'Class) is
   begin
      Set_Enclosing_Element (Self.Subtype_Mark, Self'Unchecked_Access);
      if Self.Constraint.Assigned then
         Set_Enclosing_Element (Self.Constraint, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Subtype_Indication_Element
    (Self : Base_Subtype_Indication)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Subtype_Indication_Element;

   overriding function Is_Definition_Element
    (Self : Base_Subtype_Indication)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Subtype_Indication;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Subtype_Indication (Self);
   end Visit;

   overriding function To_Subtype_Indication_Text
    (Self : aliased in out Subtype_Indication)
      return Program.Elements.Subtype_Indications
          .Subtype_Indication_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Subtype_Indication_Text;

   overriding function To_Subtype_Indication_Text
    (Self : aliased in out Implicit_Subtype_Indication)
      return Program.Elements.Subtype_Indications
          .Subtype_Indication_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Subtype_Indication_Text;

end Program.Nodes.Subtype_Indications;
