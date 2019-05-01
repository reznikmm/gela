--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Select_Paths is

   pragma Pure (Program.Elements.Select_Paths);

   type Select_Path is limited interface and Program.Elements.Paths.Path;

   type Select_Path_Access is access all Select_Path'Class
     with Storage_Size => 0;

   not overriding function When_Token
    (Self : Select_Path)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Guard
    (Self : Select_Path)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Arrow_Token
    (Self : Select_Path)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Select_Paths;
