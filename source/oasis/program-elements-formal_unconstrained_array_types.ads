--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Component_Definitions;

package Program.Elements.Formal_Unconstrained_Array_Types is

   pragma Pure (Program.Elements.Formal_Unconstrained_Array_Types);

   type Formal_Unconstrained_Array_Type is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Unconstrained_Array_Type_Access is
     access all Formal_Unconstrained_Array_Type'Class with Storage_Size => 0;

   not overriding function Index_Subtypes
    (Self : Formal_Unconstrained_Array_Type)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function Component_Definition
    (Self : Formal_Unconstrained_Array_Type)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access is abstract;

   type Formal_Unconstrained_Array_Type_Text is limited interface;

   type Formal_Unconstrained_Array_Type_Text_Access is
     access all Formal_Unconstrained_Array_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Formal_Unconstrained_Array_Type_Text
    (Self : aliased in out Formal_Unconstrained_Array_Type)
      return Formal_Unconstrained_Array_Type_Text_Access is abstract;

   not overriding function Array_Token
    (Self : Formal_Unconstrained_Array_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Formal_Unconstrained_Array_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Formal_Unconstrained_Array_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Of_Token
    (Self : Formal_Unconstrained_Array_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Formal_Unconstrained_Array_Types;
