--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Elsif_Expression_Paths is

   pragma Pure (Program.Elements.Elsif_Expression_Paths);

   type Elsif_Expression_Path is
     limited interface and Program.Elements.Paths.Path;

   type Elsif_Expression_Path_Access is access all Elsif_Expression_Path'Class
     with Storage_Size => 0;

   not overriding function Condition
    (Self : Elsif_Expression_Path)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Expression
    (Self : Elsif_Expression_Path)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Elsif_Expression_Path_Text is limited interface;

   type Elsif_Expression_Path_Text_Access is
     access all Elsif_Expression_Path_Text'Class with Storage_Size => 0;

   not overriding function To_Elsif_Expression_Path_Text
    (Self : aliased in out Elsif_Expression_Path)
      return Elsif_Expression_Path_Text_Access is abstract;

   not overriding function Elsif_Token
    (Self : Elsif_Expression_Path_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Then_Token
    (Self : Elsif_Expression_Path_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Elsif_Expression_Paths;
