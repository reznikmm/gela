--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;

package Program.Elements.Others_Choices is

   pragma Pure (Program.Elements.Others_Choices);

   type Others_Choice is
     limited interface and Program.Elements.Definitions.Definition;

   type Others_Choice_Access is access all Others_Choice'Class
     with Storage_Size => 0;

   not overriding function Others_Token
    (Self : Others_Choice)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Others_Choices;
