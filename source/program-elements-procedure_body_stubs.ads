--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Procedure_Body_Stubs is

   pragma Pure (Program.Elements.Procedure_Body_Stubs);

   type Procedure_Body_Stub is
     limited interface and Program.Elements.Declarations.Declaration;

   type Procedure_Body_Stub_Access is access all Procedure_Body_Stub'Class
     with Storage_Size => 0;

   not overriding function Not_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Overriding_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Procedure_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Procedure_Body_Stub)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Parameters
    (Self : Procedure_Body_Stub)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Is_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Separate_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspects
    (Self : Procedure_Body_Stub)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Procedure_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Procedure_Body_Stubs;
