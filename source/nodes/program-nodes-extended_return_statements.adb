--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Extended_Return_Statements is

   function Create
    (Return_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Return_Object      : not null Program.Elements
         .Return_Object_Specifications.Return_Object_Specification_Access;
     Do_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Statements         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token_2     : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Extended_Return_Statement is
   begin
      return Result : Extended_Return_Statement :=
        (Return_Token => Return_Token, Return_Object => Return_Object,
         Do_Token => Do_Token, Statements => Statements,
         Exception_Token => Exception_Token,
         Exception_Handlers => Exception_Handlers, End_Token => End_Token,
         Return_Token_2 => Return_Token_2, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Return_Object        : not null Program.Elements
         .Return_Object_Specifications.Return_Object_Specification_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Extended_Return_Statement is
   begin
      return Result : Implicit_Extended_Return_Statement :=
        (Return_Object => Return_Object, Statements => Statements,
         Exception_Handlers => Exception_Handlers,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Return_Object
    (Self : Base_Extended_Return_Statement)
      return not null Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access is
   begin
      return Self.Return_Object;
   end Return_Object;

   overriding function Statements
    (Self : Base_Extended_Return_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Statements;
   end Statements;

   overriding function Exception_Handlers
    (Self : Base_Extended_Return_Statement)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is
   begin
      return Self.Exception_Handlers;
   end Exception_Handlers;

   overriding function Return_Token
    (Self : Extended_Return_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Return_Token;
   end Return_Token;

   overriding function Do_Token
    (Self : Extended_Return_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Do_Token;
   end Do_Token;

   overriding function Exception_Token
    (Self : Extended_Return_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Exception_Token;
   end Exception_Token;

   overriding function End_Token
    (Self : Extended_Return_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Return_Token_2
    (Self : Extended_Return_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Return_Token_2;
   end Return_Token_2;

   overriding function Semicolon_Token
    (Self : Extended_Return_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Extended_Return_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Extended_Return_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Extended_Return_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Extended_Return_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Return_Object, Self'Unchecked_Access);
      for Item in Self.Statements.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Exception_Handlers.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Extended_Return_Statement
    (Self : Base_Extended_Return_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Extended_Return_Statement;

   overriding function Is_Statement
    (Self : Base_Extended_Return_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_Extended_Return_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Extended_Return_Statement (Self);
   end Visit;

   overriding function To_Extended_Return_Statement_Text
    (Self : aliased in out Extended_Return_Statement)
      return Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Extended_Return_Statement_Text;

   overriding function To_Extended_Return_Statement_Text
    (Self : aliased in out Implicit_Extended_Return_Statement)
      return Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Extended_Return_Statement_Text;

end Program.Nodes.Extended_Return_Statements;
