--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Anonymous_Access_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Specifications;

package Program.Elements.Anonymous_Access_To_Functions is

   pragma Pure (Program.Elements.Anonymous_Access_To_Functions);

   type Anonymous_Access_To_Function is
     limited interface
       and Program.Elements.Anonymous_Access_Definitions
         .Anonymous_Access_Definition;

   type Anonymous_Access_To_Function_Access is
     access all Anonymous_Access_To_Function'Class with Storage_Size => 0;

   not overriding function Parameters
    (Self : Anonymous_Access_To_Function)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Result_Subtype
    (Self : Anonymous_Access_To_Function)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Anonymous_Access_To_Function)
      return Boolean is abstract;

   not overriding function Has_Protected
    (Self : Anonymous_Access_To_Function)
      return Boolean is abstract;

   not overriding function Has_Not_Null_2
    (Self : Anonymous_Access_To_Function)
      return Boolean is abstract;

   type Anonymous_Access_To_Function_Text is limited interface;

   type Anonymous_Access_To_Function_Text_Access is
     access all Anonymous_Access_To_Function_Text'Class with Storage_Size => 0;

   not overriding function To_Anonymous_Access_To_Function_Text
    (Self : aliased in out Anonymous_Access_To_Function)
      return Anonymous_Access_To_Function_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Anonymous_Access_To_Function_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Anonymous_Access_To_Function_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Access_Token
    (Self : Anonymous_Access_To_Function_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Protected_Token
    (Self : Anonymous_Access_To_Function_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Function_Token
    (Self : Anonymous_Access_To_Function_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Anonymous_Access_To_Function_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Anonymous_Access_To_Function_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Return_Token
    (Self : Anonymous_Access_To_Function_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Not_Token_2
    (Self : Anonymous_Access_To_Function_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token_2
    (Self : Anonymous_Access_To_Function_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Anonymous_Access_To_Functions;
