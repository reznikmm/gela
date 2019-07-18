--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Call_Statements is

   function Create
    (Called_Name         : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Call_Statement is
   begin
      return Result : Call_Statement :=
        (Called_Name => Called_Name, Left_Bracket_Token => Left_Bracket_Token,
         Parameters => Parameters, Right_Bracket_Token => Right_Bracket_Token,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Called_Name          : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Call_Statement is
   begin
      return Result : Implicit_Call_Statement :=
        (Called_Name => Called_Name, Parameters => Parameters,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Called_Name
    (Self : Base_Call_Statement)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Called_Name;
   end Called_Name;

   overriding function Parameters
    (Self : Base_Call_Statement)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Left_Bracket_Token
    (Self : Call_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Call_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Semicolon_Token
    (Self : Call_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Call_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Call_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Call_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Call_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Called_Name, Self'Unchecked_Access);
      for Item in Self.Parameters.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Call_Statement
    (Self : Base_Call_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Call_Statement;

   overriding function Is_Statement
    (Self : Base_Call_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_Call_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Call_Statement (Self);
   end Visit;

   overriding function To_Call_Statement_Text
    (Self : aliased in out Call_Statement)
      return Program.Elements.Call_Statements.Call_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Call_Statement_Text;

   overriding function To_Call_Statement_Text
    (Self : aliased in out Implicit_Call_Statement)
      return Program.Elements.Call_Statements.Call_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Call_Statement_Text;

end Program.Nodes.Call_Statements;
