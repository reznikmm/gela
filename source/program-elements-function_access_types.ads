--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Access_Types;
with Program.Elements.Formal_Access_Types;
with Program.Elements.Anonymous_Access_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Specifications;

package Program.Elements.Function_Access_Types is

   pragma Pure (Program.Elements.Function_Access_Types);

   type Function_Access_Type is
     limited interface and Program.Elements.Access_Types.Access_Type
       and Program.Elements.Formal_Access_Types.Formal_Access_Type
       and Program.Elements.Anonymous_Access_Definitions
         .Anonymous_Access_Definition;

   type Function_Access_Type_Access is access all Function_Access_Type'Class
     with Storage_Size => 0;

   not overriding function Parameters
    (Self : Function_Access_Type)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Result_Subtype
    (Self : Function_Access_Type)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Function_Access_Type)
      return Boolean is abstract;

   not overriding function Has_Protected
    (Self : Function_Access_Type)
      return Boolean is abstract;

   not overriding function Has_Not_Null_2
    (Self : Function_Access_Type)
      return Boolean is abstract;

   type Function_Access_Type_Text is limited interface;

   type Function_Access_Type_Text_Access is
     access all Function_Access_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Function_Access_Type_Text
    (Self : aliased Function_Access_Type)
      return Function_Access_Type_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Access_Token
    (Self : Function_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Protected_Token
    (Self : Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Function_Token
    (Self : Function_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Return_Token
    (Self : Function_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Not_Token_2
    (Self : Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token_2
    (Self : Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Function_Access_Types;
