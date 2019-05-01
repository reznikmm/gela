--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Aspect_Specifications is

   pragma Pure (Program.Elements.Aspect_Specifications);

   type Aspect_Specification is
     limited interface and Program.Elements.Definitions.Definition;

   type Aspect_Specification_Access is access all Aspect_Specification'Class
     with Storage_Size => 0;

   not overriding function Aspect_Mark
    (Self : Aspect_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Arrow_Token
    (Self : Aspect_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspect_Definition
    (Self : Aspect_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Aspect_Specifications;
