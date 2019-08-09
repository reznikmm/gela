--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Exit_Statements is

   function Create
    (Exit_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exit_Loop_Name  : Program.Elements.Expressions.Expression_Access;
     When_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Condition       : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Exit_Statement is
   begin
      return Result : Exit_Statement :=
        (Exit_Token => Exit_Token, Exit_Loop_Name => Exit_Loop_Name,
         When_Token => When_Token, Condition => Condition,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Exit_Loop_Name       : Program.Elements.Expressions.Expression_Access;
     Condition            : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Exit_Statement is
   begin
      return Result : Implicit_Exit_Statement :=
        (Exit_Loop_Name => Exit_Loop_Name, Condition => Condition,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Exit_Loop_Name
    (Self : Base_Exit_Statement)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Exit_Loop_Name;
   end Exit_Loop_Name;

   overriding function Condition
    (Self : Base_Exit_Statement)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Condition;
   end Condition;

   overriding function Exit_Token
    (Self : Exit_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Exit_Token;
   end Exit_Token;

   overriding function When_Token
    (Self : Exit_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.When_Token;
   end When_Token;

   overriding function Semicolon_Token
    (Self : Exit_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Exit_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Exit_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Exit_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Exit_Statement'Class) is
   begin
      if Self.Exit_Loop_Name.Assigned then
         Set_Enclosing_Element (Self.Exit_Loop_Name, Self'Unchecked_Access);
      end if;
      if Self.Condition.Assigned then
         Set_Enclosing_Element (Self.Condition, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Exit_Statement_Element
    (Self : Base_Exit_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Exit_Statement_Element;

   overriding function Is_Statement_Element
    (Self : Base_Exit_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement_Element;

   overriding procedure Visit
    (Self    : not null access Base_Exit_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Exit_Statement (Self);
   end Visit;

   overriding function To_Exit_Statement_Text
    (Self : aliased in out Exit_Statement)
      return Program.Elements.Exit_Statements.Exit_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Exit_Statement_Text;

   overriding function To_Exit_Statement_Text
    (Self : aliased in out Implicit_Exit_Statement)
      return Program.Elements.Exit_Statements.Exit_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Exit_Statement_Text;

end Program.Nodes.Exit_Statements;
