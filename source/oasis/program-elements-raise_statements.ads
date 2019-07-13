--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Raise_Statements is

   pragma Pure (Program.Elements.Raise_Statements);

   type Raise_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Raise_Statement_Access is access all Raise_Statement'Class
     with Storage_Size => 0;

   not overriding function Raised_Exception
    (Self : Raise_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Associated_Message
    (Self : Raise_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Raise_Statement_Text is limited interface;

   type Raise_Statement_Text_Access is access all Raise_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Raise_Statement_Text
    (Self : aliased in out Raise_Statement)
      return Raise_Statement_Text_Access is abstract;

   not overriding function Raise_Token
    (Self : Raise_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Raise_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Raise_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Raise_Statements;
