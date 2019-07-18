--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Block_Statements is

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     Declare_Token            : Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations             : not null Program.Element_Vectors
         .Element_Vector_Access;
     Begin_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Handlers       : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Block_Statement is
   begin
      return Result : Block_Statement :=
        (Statement_Identifier => Statement_Identifier,
         Colon_Token => Colon_Token, Declare_Token => Declare_Token,
         Declarations => Declarations, Begin_Token => Begin_Token,
         Statements => Statements, Exception_Token => Exception_Token,
         Exception_Handlers => Exception_Handlers, End_Token => End_Token,
         End_Statement_Identifier => End_Statement_Identifier,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Declarations             : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers       : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return Implicit_Block_Statement is
   begin
      return Result : Implicit_Block_Statement :=
        (Statement_Identifier => Statement_Identifier,
         Declarations => Declarations, Statements => Statements,
         Exception_Handlers => Exception_Handlers,
         End_Statement_Identifier => End_Statement_Identifier,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Statement_Identifier
    (Self : Base_Block_Statement)
      return Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Statement_Identifier;
   end Statement_Identifier;

   overriding function Declarations
    (Self : Base_Block_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Declarations;
   end Declarations;

   overriding function Statements
    (Self : Base_Block_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Statements;
   end Statements;

   overriding function Exception_Handlers
    (Self : Base_Block_Statement)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is
   begin
      return Self.Exception_Handlers;
   end Exception_Handlers;

   overriding function End_Statement_Identifier
    (Self : Base_Block_Statement)
      return Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.End_Statement_Identifier;
   end End_Statement_Identifier;

   overriding function Colon_Token
    (Self : Block_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Declare_Token
    (Self : Block_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Declare_Token;
   end Declare_Token;

   overriding function Begin_Token
    (Self : Block_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Begin_Token;
   end Begin_Token;

   overriding function Exception_Token
    (Self : Block_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Exception_Token;
   end Exception_Token;

   overriding function End_Token
    (Self : Block_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Semicolon_Token
    (Self : Block_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Block_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Block_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Block_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Block_Statement'Class) is
   begin
      if Self.Statement_Identifier.Assigned then
         Set_Enclosing_Element
           (Self.Statement_Identifier, Self'Unchecked_Access);
      end if;
      for Item in Self.Declarations.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Statements.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Exception_Handlers.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.End_Statement_Identifier.Assigned then
         Set_Enclosing_Element
           (Self.End_Statement_Identifier, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Block_Statement
    (Self : Base_Block_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Block_Statement;

   overriding function Is_Statement
    (Self : Base_Block_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_Block_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Block_Statement (Self);
   end Visit;

   overriding function To_Block_Statement_Text
    (Self : aliased in out Block_Statement)
      return Program.Elements.Block_Statements.Block_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Block_Statement_Text;

   overriding function To_Block_Statement_Text
    (Self : aliased in out Implicit_Block_Statement)
      return Program.Elements.Block_Statements.Block_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Block_Statement_Text;

end Program.Nodes.Block_Statements;
