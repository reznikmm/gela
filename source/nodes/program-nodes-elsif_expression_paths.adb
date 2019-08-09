--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Elsif_Expression_Paths is

   function Create
    (Elsif_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Condition   : not null Program.Elements.Expressions.Expression_Access;
     Then_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : not null Program.Elements.Expressions.Expression_Access)
      return Elsif_Expression_Path is
   begin
      return Result : Elsif_Expression_Path :=
        (Elsif_Token => Elsif_Token, Condition => Condition,
         Then_Token => Then_Token, Expression => Expression,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Elsif_Expression_Path is
   begin
      return Result : Implicit_Elsif_Expression_Path :=
        (Condition => Condition, Expression => Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Condition
    (Self : Base_Elsif_Expression_Path)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Condition;
   end Condition;

   overriding function Expression
    (Self : Base_Elsif_Expression_Path)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Elsif_Token
    (Self : Elsif_Expression_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Elsif_Token;
   end Elsif_Token;

   overriding function Then_Token
    (Self : Elsif_Expression_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Then_Token;
   end Then_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Elsif_Expression_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Elsif_Expression_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Elsif_Expression_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Elsif_Expression_Path'Class) is
   begin
      Set_Enclosing_Element (Self.Condition, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Elsif_Expression_Path_Element
    (Self : Base_Elsif_Expression_Path)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Elsif_Expression_Path_Element;

   overriding function Is_Path_Element
    (Self : Base_Elsif_Expression_Path)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Path_Element;

   overriding procedure Visit
    (Self    : not null access Base_Elsif_Expression_Path;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Elsif_Expression_Path (Self);
   end Visit;

   overriding function To_Elsif_Expression_Path_Text
    (Self : aliased in out Elsif_Expression_Path)
      return Program.Elements.Elsif_Expression_Paths
          .Elsif_Expression_Path_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Elsif_Expression_Path_Text;

   overriding function To_Elsif_Expression_Path_Text
    (Self : aliased in out Implicit_Elsif_Expression_Path)
      return Program.Elements.Elsif_Expression_Paths
          .Elsif_Expression_Path_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Elsif_Expression_Path_Text;

end Program.Nodes.Elsif_Expression_Paths;
