--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Names;
with Program.Lexical_Elements;

package Program.Elements.Defining_Operator_Symbols is

   pragma Pure (Program.Elements.Defining_Operator_Symbols);

   type Defining_Operator_Symbol is
     limited interface and Program.Elements.Defining_Names.Defining_Name;

   type Defining_Operator_Symbol_Access is
     access all Defining_Operator_Symbol'Class with Storage_Size => 0;

   type Defining_Operator_Symbol_Text is limited interface;

   type Defining_Operator_Symbol_Text_Access is
     access all Defining_Operator_Symbol_Text'Class with Storage_Size => 0;

   not overriding function To_Defining_Operator_Symbol_Text
    (Self : aliased in out Defining_Operator_Symbol)
      return Defining_Operator_Symbol_Text_Access is abstract;

   not overriding function Operator_Symbol_Token
    (Self : Defining_Operator_Symbol_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Defining_Operator_Symbols;
