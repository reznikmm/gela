--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Short_Circuit_Operations is

   pragma Pure (Program.Elements.Short_Circuit_Operations);

   type Short_Circuit_Operation is
     limited interface and Program.Elements.Expressions.Expression;

   type Short_Circuit_Operation_Access is
     access all Short_Circuit_Operation'Class with Storage_Size => 0;

   not overriding function Left
    (Self : Short_Circuit_Operation)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Right
    (Self : Short_Circuit_Operation)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Has_And_Then
    (Self : Short_Circuit_Operation)
      return Boolean is abstract;

   not overriding function Has_Or_Else
    (Self : Short_Circuit_Operation)
      return Boolean is abstract;

   type Short_Circuit_Operation_Text is limited interface;

   type Short_Circuit_Operation_Text_Access is
     access all Short_Circuit_Operation_Text'Class with Storage_Size => 0;

   not overriding function To_Short_Circuit_Operation_Text
    (Self : aliased in out Short_Circuit_Operation)
      return Short_Circuit_Operation_Text_Access is abstract;

   not overriding function And_Token
    (Self : Short_Circuit_Operation_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Then_Token
    (Self : Short_Circuit_Operation_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Or_Token
    (Self : Short_Circuit_Operation_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Else_Token
    (Self : Short_Circuit_Operation_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Short_Circuit_Operations;
