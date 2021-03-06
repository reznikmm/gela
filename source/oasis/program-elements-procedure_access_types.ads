--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Access_Types;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Specifications;

package Program.Elements.Procedure_Access_Types is

   pragma Pure (Program.Elements.Procedure_Access_Types);

   type Procedure_Access_Type is
     limited interface and Program.Elements.Access_Types.Access_Type;

   type Procedure_Access_Type_Access is access all Procedure_Access_Type'Class
     with Storage_Size => 0;

   not overriding function Parameters
    (Self : Procedure_Access_Type)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Procedure_Access_Type)
      return Boolean is abstract;

   not overriding function Has_Protected
    (Self : Procedure_Access_Type)
      return Boolean is abstract;

   type Procedure_Access_Type_Text is limited interface;

   type Procedure_Access_Type_Text_Access is
     access all Procedure_Access_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Procedure_Access_Type_Text
    (Self : in out Procedure_Access_Type)
      return Procedure_Access_Type_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Procedure_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Procedure_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Access_Token
    (Self : Procedure_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Protected_Token
    (Self : Procedure_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Procedure_Token
    (Self : Procedure_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Procedure_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Procedure_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Procedure_Access_Types;
