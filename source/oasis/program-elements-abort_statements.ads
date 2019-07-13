--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Abort_Statements is

   pragma Pure (Program.Elements.Abort_Statements);

   type Abort_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Abort_Statement_Access is access all Abort_Statement'Class
     with Storage_Size => 0;

   not overriding function Aborted_Tasks
    (Self : Abort_Statement)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   type Abort_Statement_Text is limited interface;

   type Abort_Statement_Text_Access is access all Abort_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Abort_Statement_Text
    (Self : aliased in out Abort_Statement)
      return Abort_Statement_Text_Access is abstract;

   not overriding function Abort_Token
    (Self : Abort_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Abort_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Abort_Statements;
