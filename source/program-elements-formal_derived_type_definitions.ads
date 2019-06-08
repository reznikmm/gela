--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Formal_Derived_Type_Definitions is

   pragma Pure (Program.Elements.Formal_Derived_Type_Definitions);

   type Formal_Derived_Type_Definition is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Derived_Type_Definition_Access is
     access all Formal_Derived_Type_Definition'Class with Storage_Size => 0;

   not overriding function Abstract_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Limited_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Synchronized_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Subtype_Mark
    (Self : Formal_Derived_Type_Definition)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function And_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Progenitors
    (Self : Formal_Derived_Type_Definition)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function With_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Private_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Formal_Derived_Type_Definitions;
