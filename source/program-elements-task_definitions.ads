--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;
with Program.Elements.Identifiers;

package Program.Elements.Task_Definitions is

   pragma Pure (Program.Elements.Task_Definitions);

   type Task_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Task_Definition_Access is access all Task_Definition'Class
     with Storage_Size => 0;

   not overriding function Private_Token
    (Self : Task_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : Task_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Name
    (Self : Task_Definition)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

end Program.Elements.Task_Definitions;
