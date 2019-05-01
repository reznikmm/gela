--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Tokens;

package Program.Elements.Formal_Ordinary_Fixed_Point_Definitions is

   pragma Pure (Program.Elements.Formal_Ordinary_Fixed_Point_Definitions);

   type Formal_Ordinary_Fixed_Point_Definition is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Ordinary_Fixed_Point_Definition_Access is
     access all Formal_Ordinary_Fixed_Point_Definition'Class
     with Storage_Size => 0;

   not overriding function Delta_Token
    (Self : Formal_Ordinary_Fixed_Point_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Box_Token
    (Self : Formal_Ordinary_Fixed_Point_Definition)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Formal_Ordinary_Fixed_Point_Definitions;
