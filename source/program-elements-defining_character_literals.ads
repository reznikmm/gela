--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Names;
with Program.Tokens;

package Program.Elements.Defining_Character_Literals is

   pragma Pure (Program.Elements.Defining_Character_Literals);

   type Defining_Character_Literal is
     limited interface and Program.Elements.Defining_Names.Defining_Name;

   type Defining_Character_Literal_Access is
     access all Defining_Character_Literal'Class with Storage_Size => 0;

   not overriding function Character_Literal_Token
    (Self : Defining_Character_Literal)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Defining_Character_Literals;
