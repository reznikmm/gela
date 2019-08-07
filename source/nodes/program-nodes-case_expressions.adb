--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Case_Expressions is

   function Create
    (Case_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Vector_Access)
      return Case_Expression is
   begin
      return Result : Case_Expression :=
        (Case_Token => Case_Token,
         Selecting_Expression => Selecting_Expression, Is_Token => Is_Token,
         Paths => Paths, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Paths                : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Case_Expression is
   begin
      return Result : Implicit_Case_Expression :=
        (Selecting_Expression => Selecting_Expression, Paths => Paths,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Selecting_Expression
    (Self : Base_Case_Expression)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Selecting_Expression;
   end Selecting_Expression;

   overriding function Paths
    (Self : Base_Case_Expression)
      return not null Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Vector_Access is
   begin
      return Self.Paths;
   end Paths;

   overriding function Case_Token
    (Self : Case_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Case_Token;
   end Case_Token;

   overriding function Is_Token
    (Self : Case_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Case_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Case_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Case_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Case_Expression'Class) is
   begin
      Set_Enclosing_Element (Self.Selecting_Expression, Self'Unchecked_Access);
      for Item in Self.Paths.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Case_Expression
    (Self : Base_Case_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Case_Expression;

   overriding function Is_Expression
    (Self : Base_Case_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Case_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Case_Expression (Self);
   end Visit;

   overriding function To_Case_Expression_Text
    (Self : aliased in out Case_Expression)
      return Program.Elements.Case_Expressions.Case_Expression_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Case_Expression_Text;

   overriding function To_Case_Expression_Text
    (Self : aliased in out Implicit_Case_Expression)
      return Program.Elements.Case_Expressions.Case_Expression_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Case_Expression_Text;

end Program.Nodes.Case_Expressions;
