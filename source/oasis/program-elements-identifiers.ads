--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Identifiers is

   pragma Pure (Program.Elements.Identifiers);

   type Identifier is
     limited interface and Program.Elements.Expressions.Expression;

   type Identifier_Access is access all Identifier'Class
     with Storage_Size => 0;

   type Identifier_Text is limited interface;

   type Identifier_Text_Access is access all Identifier_Text'Class
     with Storage_Size => 0;

   not overriding function To_Identifier_Text
    (Self : aliased Identifier)
      return Identifier_Text_Access is abstract;

   not overriding function Identifier_Token
    (Self : Identifier_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   type Identifier_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Identifier_Vector_Access is access all Identifier_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Identifier_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Identifier;

   function To_Identifier
    (Self  : Identifier_Vector'Class;
     Index : Positive)
      return not null Identifier_Access
     is (Self.Element (Index).To_Identifier);

end Program.Elements.Identifiers;
