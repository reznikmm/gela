--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.If_Expressions is

   function Create
    (If_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition       : not null Program.Elements.Expressions.Expression_Access;
     Then_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Then_Expression : not null Program.Elements.Expressions.Expression_Access;
     Elsif_Paths     : Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access;
     Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Expression : Program.Elements.Expressions.Expression_Access)
      return If_Expression is
   begin
      return Result : If_Expression :=
        (If_Token => If_Token, Condition => Condition,
         Then_Token => Then_Token, Then_Expression => Then_Expression,
         Elsif_Paths => Elsif_Paths, Else_Token => Else_Token,
         Else_Expression => Else_Expression, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Then_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Elsif_Paths          : Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Expression      : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_If_Expression is
   begin
      return Result : Implicit_If_Expression :=
        (Condition => Condition, Then_Expression => Then_Expression,
         Elsif_Paths => Elsif_Paths, Else_Expression => Else_Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Condition
    (Self : Base_If_Expression)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Condition;
   end Condition;

   overriding function Then_Expression
    (Self : Base_If_Expression)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Then_Expression;
   end Then_Expression;

   overriding function Elsif_Paths
    (Self : Base_If_Expression)
      return Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access is
   begin
      return Self.Elsif_Paths;
   end Elsif_Paths;

   overriding function Else_Expression
    (Self : Base_If_Expression)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Else_Expression;
   end Else_Expression;

   overriding function If_Token
    (Self : If_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.If_Token;
   end If_Token;

   overriding function Then_Token
    (Self : If_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Then_Token;
   end Then_Token;

   overriding function Else_Token
    (Self : If_Expression)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Else_Token;
   end Else_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_If_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_If_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_If_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_If_Expression'Class) is
   begin
      Set_Enclosing_Element (Self.Condition, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Then_Expression, Self'Unchecked_Access);
      for Item in Self.Elsif_Paths.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.Else_Expression.Assigned then
         Set_Enclosing_Element (Self.Else_Expression, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_If_Expression
    (Self : Base_If_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_If_Expression;

   overriding function Is_Expression
    (Self : Base_If_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_If_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.If_Expression (Self);
   end Visit;

   overriding function To_If_Expression_Text
    (Self : aliased in out If_Expression)
      return Program.Elements.If_Expressions.If_Expression_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_If_Expression_Text;

   overriding function To_If_Expression_Text
    (Self : aliased in out Implicit_If_Expression)
      return Program.Elements.If_Expressions.If_Expression_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_If_Expression_Text;

end Program.Nodes.If_Expressions;
