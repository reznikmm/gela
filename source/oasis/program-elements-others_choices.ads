--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;

package Program.Elements.Others_Choices is

   pragma Pure (Program.Elements.Others_Choices);

   type Others_Choice is
     limited interface and Program.Elements.Definitions.Definition;

   type Others_Choice_Access is access all Others_Choice'Class
     with Storage_Size => 0;

   type Others_Choice_Text is limited interface;

   type Others_Choice_Text_Access is access all Others_Choice_Text'Class
     with Storage_Size => 0;

   not overriding function To_Others_Choice_Text
    (Self : aliased in out Others_Choice)
      return Others_Choice_Text_Access is abstract;

   not overriding function Others_Token
    (Self : Others_Choice_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Others_Choices;
