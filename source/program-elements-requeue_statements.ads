--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Requeue_Statements is

   pragma Pure (Program.Elements.Requeue_Statements);

   type Requeue_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Requeue_Statement_Access is access all Requeue_Statement'Class
     with Storage_Size => 0;

   not overriding function Entry_Name
    (Self : Requeue_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Has_With_Abort
    (Self : Requeue_Statement)
      return Boolean is abstract;

   type Requeue_Statement_Text is limited interface;

   type Requeue_Statement_Text_Access is
     access all Requeue_Statement_Text'Class with Storage_Size => 0;

   not overriding function To_Requeue_Statement_Text
    (Self : aliased Requeue_Statement)
      return Requeue_Statement_Text_Access is abstract;

   not overriding function Requeue_Token
    (Self : Requeue_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Requeue_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Abort_Token
    (Self : Requeue_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Requeue_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Requeue_Statements;
