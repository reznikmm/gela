--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Names;
with Program.Tokens;

package Program.Elements.Defining_Identifiers is

   pragma Pure (Program.Elements.Defining_Identifiers);

   type Defining_Identifier is
     limited interface and Program.Elements.Defining_Names.Defining_Name;

   type Defining_Identifier_Access is access all Defining_Identifier'Class
     with Storage_Size => 0;

   not overriding function Identifier_Token
    (Self : Defining_Identifier)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Defining_Identifiers;
