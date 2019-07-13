--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;

package Program.Elements.Null_Statements is

   pragma Pure (Program.Elements.Null_Statements);

   type Null_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Null_Statement_Access is access all Null_Statement'Class
     with Storage_Size => 0;

   type Null_Statement_Text is limited interface;

   type Null_Statement_Text_Access is access all Null_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Null_Statement_Text
    (Self : aliased in out Null_Statement)
      return Null_Statement_Text_Access is abstract;

   not overriding function Null_Token
    (Self : Null_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Null_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Null_Statements;
