--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Elsif_Paths is

   pragma Pure (Program.Elements.Elsif_Paths);

   type Elsif_Path is limited interface and Program.Elements.Paths.Path;

   type Elsif_Path_Access is access all Elsif_Path'Class
     with Storage_Size => 0;

   not overriding function Elsif_Token
    (Self : Elsif_Path)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Condition_Expression
    (Self : Elsif_Path)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Then_Token
    (Self : Elsif_Path)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Elsif_Paths;
