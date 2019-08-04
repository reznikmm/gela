--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Discrete_Simple_Expression_Ranges is

   function Create
    (Lower_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return Discrete_Simple_Expression_Range is
   begin
      return Result : Discrete_Simple_Expression_Range :=
        (Lower_Bound => Lower_Bound, Double_Dot_Token => Double_Dot_Token,
         Upper_Bound => Upper_Bound,
         Is_Discrete_Subtype_Definition => Is_Discrete_Subtype_Definition,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Lower_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Upper_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit            : Boolean := False;
     Is_Part_Of_Inherited           : Boolean := False;
     Is_Part_Of_Instance            : Boolean := False;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return Implicit_Discrete_Simple_Expression_Range is
   begin
      return Result : Implicit_Discrete_Simple_Expression_Range :=
        (Lower_Bound => Lower_Bound, Upper_Bound => Upper_Bound,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Is_Discrete_Subtype_Definition => Is_Discrete_Subtype_Definition,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Lower_Bound
    (Self : Base_Discrete_Simple_Expression_Range)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Lower_Bound;
   end Lower_Bound;

   overriding function Upper_Bound
    (Self : Base_Discrete_Simple_Expression_Range)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Upper_Bound;
   end Upper_Bound;

   overriding function Is_Discrete_Subtype_Definition
    (Self : Base_Discrete_Simple_Expression_Range)
      return Boolean is
   begin
      return Self.Is_Discrete_Subtype_Definition;
   end Is_Discrete_Subtype_Definition;

   overriding function Double_Dot_Token
    (Self : Discrete_Simple_Expression_Range)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Double_Dot_Token;
   end Double_Dot_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discrete_Simple_Expression_Range)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discrete_Simple_Expression_Range)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discrete_Simple_Expression_Range)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Discrete_Simple_Expression_Range'Class) is
   begin
      Set_Enclosing_Element (Self.Lower_Bound, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Upper_Bound, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Discrete_Simple_Expression_Range
    (Self : Base_Discrete_Simple_Expression_Range)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discrete_Simple_Expression_Range;

   overriding function Is_Discrete_Range
    (Self : Base_Discrete_Simple_Expression_Range)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discrete_Range;

   overriding function Is_Definition
    (Self : Base_Discrete_Simple_Expression_Range)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Discrete_Simple_Expression_Range;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Discrete_Simple_Expression_Range (Self);
   end Visit;

   overriding function To_Discrete_Simple_Expression_Range_Text
    (Self : aliased in out Discrete_Simple_Expression_Range)
      return Program.Elements.Discrete_Simple_Expression_Ranges
          .Discrete_Simple_Expression_Range_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Discrete_Simple_Expression_Range_Text;

   overriding function To_Discrete_Simple_Expression_Range_Text
    (Self : aliased in out Implicit_Discrete_Simple_Expression_Range)
      return Program.Elements.Discrete_Simple_Expression_Ranges
          .Discrete_Simple_Expression_Range_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Discrete_Simple_Expression_Range_Text;

end Program.Nodes.Discrete_Simple_Expression_Ranges;
