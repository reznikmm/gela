--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;

package Program.Elements.Function_Renaming_Declarations is

   pragma Pure (Program.Elements.Function_Renaming_Declarations);

   type Function_Renaming_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Function_Renaming_Declaration_Access is
     access all Function_Renaming_Declaration'Class with Storage_Size => 0;

   not overriding function Not_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Overriding_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Function_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Function_Renaming_Declaration)
      return Program.Elements.Defining_Names.Defining_Name_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Return_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token_2
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Result_Subtype
    (Self : Function_Renaming_Declaration)
      return Program.Elements.Element_Access is abstract;

   not overriding function Renames_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Renamed_Entity
    (Self : Function_Renaming_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function With_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Function_Renaming_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Function_Renaming_Declarations;
