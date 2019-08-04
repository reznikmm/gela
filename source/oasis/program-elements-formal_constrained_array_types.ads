--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Component_Definitions;

package Program.Elements.Formal_Constrained_Array_Types is

   pragma Pure (Program.Elements.Formal_Constrained_Array_Types);

   type Formal_Constrained_Array_Type is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Constrained_Array_Type_Access is
     access all Formal_Constrained_Array_Type'Class with Storage_Size => 0;

   not overriding function Index_Subtypes
    (Self : Formal_Constrained_Array_Type)
      return not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Vector_Access is abstract;

   not overriding function Component_Definition
    (Self : Formal_Constrained_Array_Type)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access is abstract;

   type Formal_Constrained_Array_Type_Text is limited interface;

   type Formal_Constrained_Array_Type_Text_Access is
     access all Formal_Constrained_Array_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Formal_Constrained_Array_Type_Text
    (Self : aliased in out Formal_Constrained_Array_Type)
      return Formal_Constrained_Array_Type_Text_Access is abstract;

   not overriding function Array_Token
    (Self : Formal_Constrained_Array_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Formal_Constrained_Array_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Formal_Constrained_Array_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Of_Token
    (Self : Formal_Constrained_Array_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Formal_Constrained_Array_Types;
