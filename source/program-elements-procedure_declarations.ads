--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Names;

package Program.Elements.Procedure_Declarations is

   pragma Pure (Program.Elements.Procedure_Declarations);

   type Procedure_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Procedure_Declaration_Access is access all Procedure_Declaration'Class
     with Storage_Size => 0;

   not overriding function Not_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Overriding_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Procedure_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Procedure_Declaration)
      return Program.Elements.Defining_Names.Defining_Name_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Is_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Abstract_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Procedure_Declarations;
