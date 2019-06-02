--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Exception_Renaming_Declarations is

   pragma Pure (Program.Elements.Exception_Renaming_Declarations);

   type Exception_Renaming_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Exception_Renaming_Declaration_Access is
     access all Exception_Renaming_Declaration'Class with Storage_Size => 0;

   not overriding function Colon_Token
    (Self : Exception_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Exception_Token
    (Self : Exception_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Renames_Token
    (Self : Exception_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Renamed_Exception
    (Self : Exception_Renaming_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function With_Token
    (Self : Exception_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Exception_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Exception_Renaming_Declarations;
