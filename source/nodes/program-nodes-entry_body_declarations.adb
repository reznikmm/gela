--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Entry_Body_Declarations is

   function Create
    (Entry_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Entry_Index           : not null Program.Elements
         .Entry_Index_Specifications.Entry_Index_Specification_Access;
     Right_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Left_Bracket_Token_2  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters            : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2 : Program.Lexical_Elements.Lexical_Element_Access;
     When_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Barrier         : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations          : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements            : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers    : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name              : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Entry_Body_Declaration is
   begin
      return Result : Entry_Body_Declaration :=
        (Entry_Token => Entry_Token, Name => Name,
         Left_Bracket_Token => Left_Bracket_Token, Entry_Index => Entry_Index,
         Right_Bracket_Token => Right_Bracket_Token,
         Left_Bracket_Token_2 => Left_Bracket_Token_2,
         Parameters => Parameters,
         Right_Bracket_Token_2 => Right_Bracket_Token_2,
         When_Token => When_Token, Entry_Barrier => Entry_Barrier,
         Is_Token => Is_Token, Declarations => Declarations,
         Begin_Token => Begin_Token, Statements => Statements,
         Exception_Token => Exception_Token,
         Exception_Handlers => Exception_Handlers, End_Token => End_Token,
         End_Name => End_Name, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Entry_Index          : not null Program.Elements
         .Entry_Index_Specifications.Entry_Index_Specification_Access;
     Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Entry_Barrier        : not null Program.Elements.Expressions
         .Expression_Access;
     Declarations         : Program.Element_Vectors.Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Entry_Body_Declaration is
   begin
      return Result : Implicit_Entry_Body_Declaration :=
        (Name => Name, Entry_Index => Entry_Index, Parameters => Parameters,
         Entry_Barrier => Entry_Barrier, Declarations => Declarations,
         Statements => Statements, Exception_Handlers => Exception_Handlers,
         End_Name => End_Name, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Entry_Body_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Entry_Index
    (Self : Base_Entry_Body_Declaration)
      return not null Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access is
   begin
      return Self.Entry_Index;
   end Entry_Index;

   overriding function Parameters
    (Self : Base_Entry_Body_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Entry_Barrier
    (Self : Base_Entry_Body_Declaration)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Entry_Barrier;
   end Entry_Barrier;

   overriding function Declarations
    (Self : Base_Entry_Body_Declaration)
      return Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Declarations;
   end Declarations;

   overriding function Statements
    (Self : Base_Entry_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Statements;
   end Statements;

   overriding function Exception_Handlers
    (Self : Base_Entry_Body_Declaration)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is
   begin
      return Self.Exception_Handlers;
   end Exception_Handlers;

   overriding function End_Name
    (Self : Base_Entry_Body_Declaration)
      return Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.End_Name;
   end End_Name;

   overriding function Entry_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Entry_Token;
   end Entry_Token;

   overriding function Left_Bracket_Token
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Left_Bracket_Token_2
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token_2;
   end Left_Bracket_Token_2;

   overriding function Right_Bracket_Token_2
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token_2;
   end Right_Bracket_Token_2;

   overriding function When_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.When_Token;
   end When_Token;

   overriding function Is_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function Begin_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Begin_Token;
   end Begin_Token;

   overriding function Exception_Token
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Exception_Token;
   end Exception_Token;

   overriding function End_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Semicolon_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Entry_Body_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Entry_Body_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Entry_Body_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Entry_Body_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Entry_Index, Self'Unchecked_Access);
      for Item in Self.Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Entry_Barrier, Self'Unchecked_Access);
      for Item in Self.Declarations.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Statements.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Exception_Handlers.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.End_Name.Assigned then
         Set_Enclosing_Element (Self.End_Name, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Entry_Body_Declaration_Element
    (Self : Base_Entry_Body_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Entry_Body_Declaration_Element;

   overriding function Is_Declaration_Element
    (Self : Base_Entry_Body_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration_Element;

   overriding procedure Visit
    (Self    : not null access Base_Entry_Body_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Entry_Body_Declaration (Self);
   end Visit;

   overriding function To_Entry_Body_Declaration_Text
    (Self : aliased in out Entry_Body_Declaration)
      return Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Entry_Body_Declaration_Text;

   overriding function To_Entry_Body_Declaration_Text
    (Self : aliased in out Implicit_Entry_Body_Declaration)
      return Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Entry_Body_Declaration_Text;

end Program.Nodes.Entry_Body_Declarations;
