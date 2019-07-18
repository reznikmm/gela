--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Select_Statements is

   function Create
    (Select_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                 : not null Program.Elements.Select_Paths
         .Select_Path_Vector_Access;
     Then_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abort_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Then_Abort_Statements : not null Program.Element_Vectors
         .Element_Vector_Access;
     Else_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Statements       : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Select_Token_2        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Select_Statement is
   begin
      return Result : Select_Statement :=
        (Select_Token => Select_Token, Paths => Paths,
         Then_Token => Then_Token, Abort_Token => Abort_Token,
         Then_Abort_Statements => Then_Abort_Statements,
         Else_Token => Else_Token, Else_Statements => Else_Statements,
         End_Token => End_Token, Select_Token_2 => Select_Token_2,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Paths                 : not null Program.Elements.Select_Paths
         .Select_Path_Vector_Access;
     Then_Abort_Statements : not null Program.Element_Vectors
         .Element_Vector_Access;
     Else_Statements       : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return Implicit_Select_Statement is
   begin
      return Result : Implicit_Select_Statement :=
        (Paths => Paths, Then_Abort_Statements => Then_Abort_Statements,
         Else_Statements => Else_Statements,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Paths
    (Self : Base_Select_Statement)
      return not null Program.Elements.Select_Paths
          .Select_Path_Vector_Access is
   begin
      return Self.Paths;
   end Paths;

   overriding function Then_Abort_Statements
    (Self : Base_Select_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Then_Abort_Statements;
   end Then_Abort_Statements;

   overriding function Else_Statements
    (Self : Base_Select_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Else_Statements;
   end Else_Statements;

   overriding function Select_Token
    (Self : Select_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Select_Token;
   end Select_Token;

   overriding function Then_Token
    (Self : Select_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Then_Token;
   end Then_Token;

   overriding function Abort_Token
    (Self : Select_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abort_Token;
   end Abort_Token;

   overriding function Else_Token
    (Self : Select_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Else_Token;
   end Else_Token;

   overriding function End_Token
    (Self : Select_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Select_Token_2
    (Self : Select_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Select_Token_2;
   end Select_Token_2;

   overriding function Semicolon_Token
    (Self : Select_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Select_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Select_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Select_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Select_Statement'Class) is
   begin
      for Item in Self.Paths.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Then_Abort_Statements.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Else_Statements.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Select_Statement
    (Self : Base_Select_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Select_Statement;

   overriding function Is_Statement
    (Self : Base_Select_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_Select_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Select_Statement (Self);
   end Visit;

   overriding function To_Select_Statement_Text
    (Self : aliased in out Select_Statement)
      return Program.Elements.Select_Statements.Select_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Select_Statement_Text;

   overriding function To_Select_Statement_Text
    (Self : aliased in out Implicit_Select_Statement)
      return Program.Elements.Select_Statements.Select_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Select_Statement_Text;

end Program.Nodes.Select_Statements;
