--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Signed_Integer_Types is

   pragma Pure (Program.Elements.Signed_Integer_Types);

   type Signed_Integer_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Signed_Integer_Type_Access is access all Signed_Integer_Type'Class
     with Storage_Size => 0;

   not overriding function Lower_Bound
    (Self : Signed_Integer_Type)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Upper_Bound
    (Self : Signed_Integer_Type)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Signed_Integer_Type_Text is limited interface;

   type Signed_Integer_Type_Text_Access is
     access all Signed_Integer_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Signed_Integer_Type_Text
    (Self : aliased in out Signed_Integer_Type)
      return Signed_Integer_Type_Text_Access is abstract;

   not overriding function Range_Token
    (Self : Signed_Integer_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Double_Dot_Token
    (Self : Signed_Integer_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Signed_Integer_Types;
