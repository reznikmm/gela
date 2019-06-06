--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;
with Program.Element_Vectors;

package Program.Elements.Identifiers is

   pragma Pure (Program.Elements.Identifiers);

   type Identifier is
     limited interface and Program.Elements.Expressions.Expression;

   type Identifier_Access is access all Identifier'Class
     with Storage_Size => 0;

   not overriding function Identifier_Token
    (Self : Identifier)
      return Program.Tokens.Token_Access is abstract;

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
