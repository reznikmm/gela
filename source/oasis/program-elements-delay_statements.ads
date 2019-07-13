--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Delay_Statements is

   pragma Pure (Program.Elements.Delay_Statements);

   type Delay_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Delay_Statement_Access is access all Delay_Statement'Class
     with Storage_Size => 0;

   not overriding function Expression
    (Self : Delay_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Delay_Statement_Text is limited interface;

   type Delay_Statement_Text_Access is access all Delay_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Delay_Statement_Text
    (Self : aliased in out Delay_Statement)
      return Delay_Statement_Text_Access is abstract;

   not overriding function Delay_Token
    (Self : Delay_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Until_Token
    (Self : Delay_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Delay_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Delay_Statements;
