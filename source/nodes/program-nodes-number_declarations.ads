--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Number_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Number_Declarations is

   pragma Pure (Program.Nodes.Number_Declarations);

   type Number_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Number_Declarations.Number_Declaration
         and Program.Elements.Number_Declarations.Number_Declaration_Text
     with private;

   function Create
    (Names            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Constant_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Assignment_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression       : not null Program.Elements.Expressions
         .Expression_Access;
     Semicolon_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Number_Declaration;

   type Implicit_Number_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Number_Declarations.Number_Declaration
     with private;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Number_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Number_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Number_Declarations.Number_Declaration
     with record
        Names      : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;
        Expression : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Number_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Number_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Names
    (Self : Base_Number_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;

   overriding function Expression
    (Self : Base_Number_Declaration)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Number_Declaration
    (Self : Base_Number_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Number_Declaration)
      return Boolean;

   type Number_Declaration is
     new Base_Number_Declaration
       and Program.Elements.Number_Declarations.Number_Declaration_Text
     with record
        Colon_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Constant_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Assignment_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Number_Declaration_Text
    (Self : aliased in out Number_Declaration)
      return Program.Elements.Number_Declarations
          .Number_Declaration_Text_Access;

   overriding function Colon_Token
    (Self : Number_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Constant_Token
    (Self : Number_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Assignment_Token
    (Self : Number_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Number_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Number_Declaration is
     new Base_Number_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Number_Declaration_Text
    (Self : aliased in out Implicit_Number_Declaration)
      return Program.Elements.Number_Declarations
          .Number_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Number_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Number_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Number_Declaration)
      return Boolean;

end Program.Nodes.Number_Declarations;
