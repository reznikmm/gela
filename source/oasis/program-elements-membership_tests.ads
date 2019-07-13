--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Element_Vectors;

package Program.Elements.Membership_Tests is

   pragma Pure (Program.Elements.Membership_Tests);

   type Membership_Test is
     limited interface and Program.Elements.Expressions.Expression;

   type Membership_Test_Access is access all Membership_Test'Class
     with Storage_Size => 0;

   not overriding function Expression
    (Self : Membership_Test)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Choices
    (Self : Membership_Test)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Has_Not (Self : Membership_Test) return Boolean
     is abstract;

   type Membership_Test_Text is limited interface;

   type Membership_Test_Text_Access is access all Membership_Test_Text'Class
     with Storage_Size => 0;

   not overriding function To_Membership_Test_Text
    (Self : aliased in out Membership_Test)
      return Membership_Test_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Membership_Test_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function In_Token
    (Self : Membership_Test_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Membership_Tests;
