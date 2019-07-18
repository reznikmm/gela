--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Index_Constraints is

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ranges              : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Index_Constraint is
   begin
      return Result : Index_Constraint :=
        (Left_Bracket_Token => Left_Bracket_Token, Ranges => Ranges,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Ranges               : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Index_Constraint is
   begin
      return Result : Implicit_Index_Constraint :=
        (Ranges => Ranges, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Ranges
    (Self : Base_Index_Constraint)
      return not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Vector_Access is
   begin
      return Self.Ranges;
   end Ranges;

   overriding function Left_Bracket_Token
    (Self : Index_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Index_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Index_Constraint)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Index_Constraint)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Index_Constraint)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Index_Constraint'Class) is
   begin
      for Item in Self.Ranges.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Index_Constraint
    (Self : Base_Index_Constraint)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Index_Constraint;

   overriding function Is_Constraint
    (Self : Base_Index_Constraint)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Constraint;

   overriding function Is_Definition
    (Self : Base_Index_Constraint)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Index_Constraint;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Index_Constraint (Self);
   end Visit;

   overriding function To_Index_Constraint_Text
    (Self : aliased in out Index_Constraint)
      return Program.Elements.Index_Constraints.Index_Constraint_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Index_Constraint_Text;

   overriding function To_Index_Constraint_Text
    (Self : aliased in out Implicit_Index_Constraint)
      return Program.Elements.Index_Constraints.Index_Constraint_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Index_Constraint_Text;

end Program.Nodes.Index_Constraints;
