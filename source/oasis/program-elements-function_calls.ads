--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Associations;

package Program.Elements.Function_Calls is

   pragma Pure (Program.Elements.Function_Calls);

   type Function_Call is
     limited interface and Program.Elements.Expressions.Expression;

   type Function_Call_Access is access all Function_Call'Class
     with Storage_Size => 0;

   not overriding function Prefix
    (Self : Function_Call)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Parameters
    (Self : Function_Call)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is abstract;

   type Function_Call_Text is limited interface;

   type Function_Call_Text_Access is access all Function_Call_Text'Class
     with Storage_Size => 0;

   not overriding function To_Function_Call_Text
    (Self : aliased in out Function_Call)
      return Function_Call_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Function_Call_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Function_Call_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Function_Calls;
