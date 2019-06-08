--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Discrete_Subtype_Definitions;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Entry_Declarations is

   pragma Pure (Program.Elements.Entry_Declarations);

   type Entry_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Entry_Declaration_Access is access all Entry_Declaration'Class
     with Storage_Size => 0;

   not overriding function Not_Token
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Overriding_Token
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Entry_Token
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Entry_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Entry_Family_Definition
    (Self : Entry_Declaration)
      return Program.Elements.Discrete_Subtype_Definitions
          .Discrete_Subtype_Definition_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Left_Bracket_Token_2
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Parameters
    (Self : Entry_Declaration)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Right_Bracket_Token_2
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspects
    (Self : Entry_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Entry_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Entry_Declarations;
