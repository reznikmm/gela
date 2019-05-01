--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;

package Program.Elements.Exception_Declarations is

   pragma Pure (Program.Elements.Exception_Declarations);

   type Exception_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Exception_Declaration_Access is access all Exception_Declaration'Class
     with Storage_Size => 0;

   not overriding function Colon_Token
    (Self : Exception_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Exception_Token
    (Self : Exception_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Exception_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Exception_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Exception_Declarations;
