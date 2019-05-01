--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;

package Program.Elements.Incomplete_Type_Definitions is

   pragma Pure (Program.Elements.Incomplete_Type_Definitions);

   type Incomplete_Type_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Incomplete_Type_Definition_Access is
     access all Incomplete_Type_Definition'Class with Storage_Size => 0;

   not overriding function Tagged_Token
    (Self : Incomplete_Type_Definition)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Incomplete_Type_Definitions;
