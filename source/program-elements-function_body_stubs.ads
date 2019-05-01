--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;

package Program.Elements.Function_Body_Stubs is

   pragma Pure (Program.Elements.Function_Body_Stubs);

   type Function_Body_Stub is
     limited interface and Program.Elements.Declarations.Declaration;

   type Function_Body_Stub_Access is access all Function_Body_Stub'Class
     with Storage_Size => 0;

   not overriding function Not_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Overriding_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Function_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Function_Body_Stub)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Return_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token_2
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Result_Subtype
    (Self : Function_Body_Stub)
      return Program.Elements.Element_Access is abstract;

   not overriding function Is_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Separate_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Function_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Function_Body_Stubs;
