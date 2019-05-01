--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Names;

package Program.Elements.Generic_Function_Declarations is

   pragma Pure (Program.Elements.Generic_Function_Declarations);

   type Generic_Function_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generic_Function_Declaration_Access is
     access all Generic_Function_Declaration'Class with Storage_Size => 0;

   not overriding function Generic_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Function_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Generic_Function_Declaration)
      return Program.Elements.Defining_Names.Defining_Name_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Return_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Result_Subtype
    (Self : Generic_Function_Declaration)
      return Program.Elements.Element_Access is abstract;

   not overriding function With_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Generic_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Generic_Function_Declarations;
