--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;
with Program.Elements.Identifiers;

package Program.Elements.Protected_Definitions is

   pragma Pure (Program.Elements.Protected_Definitions);

   type Protected_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Protected_Definition_Access is access all Protected_Definition'Class
     with Storage_Size => 0;

   not overriding function Private_Token
    (Self : Protected_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : Protected_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Name
    (Self : Protected_Definition)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

end Program.Elements.Protected_Definitions;
