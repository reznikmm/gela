--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Accept_Statements is

   function Create
    (Accept_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Name               : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token       : Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Index              : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token      : Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token_2     : Program.Lexical_Elements
         .Lexical_Element_Access;
     Parameters               : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2    : Program.Lexical_Elements
         .Lexical_Element_Access;
     Do_Token                 : Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : Program.Element_Vectors.Element_Vector_Access;
     Exception_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Handlers       : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token                : Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Accept_Statement is
   begin
      return Result : Accept_Statement :=
        (Accept_Token => Accept_Token, Entry_Name => Entry_Name,
         Left_Bracket_Token => Left_Bracket_Token, Entry_Index => Entry_Index,
         Right_Bracket_Token => Right_Bracket_Token,
         Left_Bracket_Token_2 => Left_Bracket_Token_2,
         Parameters => Parameters,
         Right_Bracket_Token_2 => Right_Bracket_Token_2, Do_Token => Do_Token,
         Statements => Statements, Exception_Token => Exception_Token,
         Exception_Handlers => Exception_Handlers, End_Token => End_Token,
         End_Statement_Identifier => End_Statement_Identifier,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Entry_Name               : not null Program.Elements.Identifiers
         .Identifier_Access;
     Entry_Index              : Program.Elements.Expressions.Expression_Access;
     Parameters               : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Statements               : Program.Element_Vectors.Element_Vector_Access;
     Exception_Handlers       : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return Implicit_Accept_Statement is
   begin
      return Result : Implicit_Accept_Statement :=
        (Entry_Name => Entry_Name, Entry_Index => Entry_Index,
         Parameters => Parameters, Statements => Statements,
         Exception_Handlers => Exception_Handlers,
         End_Statement_Identifier => End_Statement_Identifier,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Entry_Name
    (Self : Base_Accept_Statement)
      return not null Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.Entry_Name;
   end Entry_Name;

   overriding function Entry_Index
    (Self : Base_Accept_Statement)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Entry_Index;
   end Entry_Index;

   overriding function Parameters
    (Self : Base_Accept_Statement)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Statements
    (Self : Base_Accept_Statement)
      return Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Statements;
   end Statements;

   overriding function Exception_Handlers
    (Self : Base_Accept_Statement)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is
   begin
      return Self.Exception_Handlers;
   end Exception_Handlers;

   overriding function End_Statement_Identifier
    (Self : Base_Accept_Statement)
      return Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.End_Statement_Identifier;
   end End_Statement_Identifier;

   overriding function Accept_Token
    (Self : Accept_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Accept_Token;
   end Accept_Token;

   overriding function Left_Bracket_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Left_Bracket_Token_2
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token_2;
   end Left_Bracket_Token_2;

   overriding function Right_Bracket_Token_2
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token_2;
   end Right_Bracket_Token_2;

   overriding function Do_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Do_Token;
   end Do_Token;

   overriding function Exception_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Exception_Token;
   end Exception_Token;

   overriding function End_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Semicolon_Token
    (Self : Accept_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Accept_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Accept_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Accept_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Accept_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Entry_Name, Self'Unchecked_Access);
      if Self.Entry_Index.Assigned then
         Set_Enclosing_Element (Self.Entry_Index, Self'Unchecked_Access);
      end if;
      for Item in Self.Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Statements.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Exception_Handlers.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.End_Statement_Identifier.Assigned then
         Set_Enclosing_Element
           (Self.End_Statement_Identifier, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Accept_Statement
    (Self : Base_Accept_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Accept_Statement;

   overriding function Is_Statement
    (Self : Base_Accept_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_Accept_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Accept_Statement (Self);
   end Visit;

   overriding function To_Accept_Statement_Text
    (Self : aliased in out Accept_Statement)
      return Program.Elements.Accept_Statements.Accept_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Accept_Statement_Text;

   overriding function To_Accept_Statement_Text
    (Self : aliased in out Implicit_Accept_Statement)
      return Program.Elements.Accept_Statements.Accept_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Accept_Statement_Text;

end Program.Nodes.Accept_Statements;
