--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Real_Range_Specifications is

   pragma Pure (Program.Elements.Real_Range_Specifications);

   type Real_Range_Specification is
     limited interface and Program.Elements.Definitions.Definition;

   type Real_Range_Specification_Access is
     access all Real_Range_Specification'Class with Storage_Size => 0;

   not overriding function Range_Token
    (Self : Real_Range_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Lower_Bound
    (Self : Real_Range_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Double_Dot_Token
    (Self : Real_Range_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Upper_Bound
    (Self : Real_Range_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Real_Range_Specifications;
