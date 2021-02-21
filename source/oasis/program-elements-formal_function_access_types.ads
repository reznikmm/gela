--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Formal_Access_Types;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Specifications;

package Program.Elements.Formal_Function_Access_Types is

   pragma Pure (Program.Elements.Formal_Function_Access_Types);

   type Formal_Function_Access_Type is
     limited interface
       and Program.Elements.Formal_Access_Types.Formal_Access_Type;

   type Formal_Function_Access_Type_Access is
     access all Formal_Function_Access_Type'Class with Storage_Size => 0;

   not overriding function Parameters
    (Self : Formal_Function_Access_Type)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Result_Subtype
    (Self : Formal_Function_Access_Type)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Formal_Function_Access_Type)
      return Boolean is abstract;

   not overriding function Has_Protected
    (Self : Formal_Function_Access_Type)
      return Boolean is abstract;

   not overriding function Has_Not_Null_2
    (Self : Formal_Function_Access_Type)
      return Boolean is abstract;

   type Formal_Function_Access_Type_Text is limited interface;

   type Formal_Function_Access_Type_Text_Access is
     access all Formal_Function_Access_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Formal_Function_Access_Type_Text
    (Self : in out Formal_Function_Access_Type)
      return Formal_Function_Access_Type_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Formal_Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Formal_Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Access_Token
    (Self : Formal_Function_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Protected_Token
    (Self : Formal_Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Function_Token
    (Self : Formal_Function_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Formal_Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Formal_Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Return_Token
    (Self : Formal_Function_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Not_Token_2
    (Self : Formal_Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token_2
    (Self : Formal_Function_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Formal_Function_Access_Types;
