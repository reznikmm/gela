--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Case_Expression_Paths is

   function Create
    (When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : not null Program.Elements.Expressions.Expression_Access)
      return Case_Expression_Path is
   begin
      return Result : Case_Expression_Path :=
        (When_Token => When_Token, Choices => Choices,
         Arrow_Token => Arrow_Token, Expression => Expression,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Case_Expression_Path is
   begin
      return Result : Implicit_Case_Expression_Path :=
        (Choices => Choices, Expression => Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Choices
    (Self : Base_Case_Expression_Path)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Choices;
   end Choices;

   overriding function Expression
    (Self : Base_Case_Expression_Path)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function When_Token
    (Self : Case_Expression_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.When_Token;
   end When_Token;

   overriding function Arrow_Token
    (Self : Case_Expression_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Case_Expression_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Case_Expression_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Case_Expression_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Case_Expression_Path'Class) is
   begin
      for Item in Self.Choices.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Case_Expression_Path
    (Self : Base_Case_Expression_Path)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Case_Expression_Path;

   overriding function Is_Path
    (Self : Base_Case_Expression_Path)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Path;

   overriding procedure Visit
    (Self    : not null access Base_Case_Expression_Path;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Case_Expression_Path (Self);
   end Visit;

   overriding function To_Case_Expression_Path_Text
    (Self : aliased in out Case_Expression_Path)
      return Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Case_Expression_Path_Text;

   overriding function To_Case_Expression_Path_Text
    (Self : aliased in out Implicit_Case_Expression_Path)
      return Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Case_Expression_Path_Text;

end Program.Nodes.Case_Expression_Paths;
