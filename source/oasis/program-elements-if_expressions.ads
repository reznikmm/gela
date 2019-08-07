--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Elsif_Paths;

package Program.Elements.If_Expressions is

   pragma Pure (Program.Elements.If_Expressions);

   type If_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type If_Expression_Access is access all If_Expression'Class
     with Storage_Size => 0;

   not overriding function Condition
    (Self : If_Expression)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Then_Expression
    (Self : If_Expression)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Elsif_Paths
    (Self : If_Expression)
      return Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access is abstract;

   not overriding function Else_Expression
    (Self : If_Expression)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type If_Expression_Text is limited interface;

   type If_Expression_Text_Access is access all If_Expression_Text'Class
     with Storage_Size => 0;

   not overriding function To_If_Expression_Text
    (Self : aliased in out If_Expression)
      return If_Expression_Text_Access is abstract;

   not overriding function If_Token
    (Self : If_Expression_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Then_Token
    (Self : If_Expression_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Else_Token
    (Self : If_Expression_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.If_Expressions;
