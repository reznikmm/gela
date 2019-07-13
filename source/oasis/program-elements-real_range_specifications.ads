--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Real_Range_Specifications is

   pragma Pure (Program.Elements.Real_Range_Specifications);

   type Real_Range_Specification is
     limited interface and Program.Elements.Definitions.Definition;

   type Real_Range_Specification_Access is
     access all Real_Range_Specification'Class with Storage_Size => 0;

   not overriding function Lower_Bound
    (Self : Real_Range_Specification)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Upper_Bound
    (Self : Real_Range_Specification)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Real_Range_Specification_Text is limited interface;

   type Real_Range_Specification_Text_Access is
     access all Real_Range_Specification_Text'Class with Storage_Size => 0;

   not overriding function To_Real_Range_Specification_Text
    (Self : aliased in out Real_Range_Specification)
      return Real_Range_Specification_Text_Access is abstract;

   not overriding function Range_Token
    (Self : Real_Range_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Double_Dot_Token
    (Self : Real_Range_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Real_Range_Specifications;
