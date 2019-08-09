--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Slices is

   function Create
    (Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Slice_Range         : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Slice is
   begin
      return Result : Slice :=
        (Prefix => Prefix, Left_Bracket_Token => Left_Bracket_Token,
         Slice_Range => Slice_Range,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Slice_Range          : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Slice is
   begin
      return Result : Implicit_Slice :=
        (Prefix => Prefix, Slice_Range => Slice_Range,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Prefix
    (Self : Base_Slice)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Prefix;
   end Prefix;

   overriding function Slice_Range
    (Self : Base_Slice)
      return not null Program.Elements.Discrete_Ranges.Discrete_Range_Access is
   begin
      return Self.Slice_Range;
   end Slice_Range;

   overriding function Left_Bracket_Token
    (Self : Slice)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Slice)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Slice)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Slice)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Slice)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Slice'Class) is
   begin
      Set_Enclosing_Element (Self.Prefix, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Slice_Range, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Slice_Element (Self : Base_Slice) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Slice_Element;

   overriding function Is_Expression_Element
    (Self : Base_Slice)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Slice;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Slice (Self);
   end Visit;

   overriding function To_Slice_Text
    (Self : aliased in out Slice)
      return Program.Elements.Slices.Slice_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Slice_Text;

   overriding function To_Slice_Text
    (Self : aliased in out Implicit_Slice)
      return Program.Elements.Slices.Slice_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Slice_Text;

end Program.Nodes.Slices;
