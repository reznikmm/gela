--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;

package Program.Elements.Terminate_Alternative_Statements is

   pragma Pure (Program.Elements.Terminate_Alternative_Statements);

   type Terminate_Alternative_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Terminate_Alternative_Statement_Access is
     access all Terminate_Alternative_Statement'Class with Storage_Size => 0;

   type Terminate_Alternative_Statement_Text is limited interface;

   type Terminate_Alternative_Statement_Text_Access is
     access all Terminate_Alternative_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Terminate_Alternative_Statement_Text
    (Self : aliased Terminate_Alternative_Statement)
      return Terminate_Alternative_Statement_Text_Access is abstract;

   not overriding function Terminate_Token
    (Self : Terminate_Alternative_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Terminate_Alternative_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Terminate_Alternative_Statements;
