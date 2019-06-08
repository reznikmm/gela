--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Entry_Index_Specifications;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Expressions;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Identifiers;

package Program.Elements.Entry_Body_Declarations is

   pragma Pure (Program.Elements.Entry_Body_Declarations);

   type Entry_Body_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Entry_Body_Declaration_Access is
     access all Entry_Body_Declaration'Class with Storage_Size => 0;

   not overriding function Entry_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Entry_Body_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Entry_Index
    (Self : Entry_Body_Declaration)
      return not null Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Left_Bracket_Token_2
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Parameters
    (Self : Entry_Body_Declaration)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Right_Bracket_Token_2
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function When_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Entry_Barrier
    (Self : Entry_Body_Declaration)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Is_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Declarations
    (Self : Entry_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Begin_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Statements
    (Self : Entry_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Exception_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Exception_Handlers
    (Self : Entry_Body_Declaration)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is abstract;

   not overriding function End_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Name
    (Self : Entry_Body_Declaration)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Entry_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Entry_Body_Declarations;
