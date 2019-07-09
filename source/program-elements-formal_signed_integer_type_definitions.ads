--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Lexical_Elements;

package Program.Elements.Formal_Signed_Integer_Type_Definitions is

   pragma Pure (Program.Elements.Formal_Signed_Integer_Type_Definitions);

   type Formal_Signed_Integer_Type_Definition is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Signed_Integer_Type_Definition_Access is
     access all Formal_Signed_Integer_Type_Definition'Class
     with Storage_Size => 0;

   type Formal_Signed_Integer_Type_Definition_Text is limited interface;

   type Formal_Signed_Integer_Type_Definition_Text_Access is
     access all Formal_Signed_Integer_Type_Definition_Text'Class
     with Storage_Size => 0;

   not overriding function To_Formal_Signed_Integer_Type_Definition_Text
    (Self : aliased Formal_Signed_Integer_Type_Definition)
      return Formal_Signed_Integer_Type_Definition_Text_Access is abstract;

   not overriding function Range_Token
    (Self : Formal_Signed_Integer_Type_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Box_Token
    (Self : Formal_Signed_Integer_Type_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Formal_Signed_Integer_Type_Definitions;
