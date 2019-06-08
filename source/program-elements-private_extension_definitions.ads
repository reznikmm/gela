--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;
with Program.Elements.Subtype_Indications;
with Program.Elements.Expressions;

package Program.Elements.Private_Extension_Definitions is

   pragma Pure (Program.Elements.Private_Extension_Definitions);

   type Private_Extension_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Private_Extension_Definition_Access is
     access all Private_Extension_Definition'Class with Storage_Size => 0;

   not overriding function Abstract_Token
    (Self : Private_Extension_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Limited_Token
    (Self : Private_Extension_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Synchronized_Token
    (Self : Private_Extension_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Private_Extension_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Ancestor
    (Self : Private_Extension_Definition)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is abstract;

   not overriding function And_Token
    (Self : Private_Extension_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Progenitors
    (Self : Private_Extension_Definition)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function With_Token
    (Self : Private_Extension_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Private_Token
    (Self : Private_Extension_Definition)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Private_Extension_Definitions;
