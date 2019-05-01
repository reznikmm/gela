--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;
with Program.Tokens;

package Program.Elements.Case_Paths is

   pragma Pure (Program.Elements.Case_Paths);

   type Case_Path is limited interface and Program.Elements.Paths.Path;

   type Case_Path_Access is access all Case_Path'Class with Storage_Size => 0;

   not overriding function When_Token
    (Self : Case_Path)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Arrow_Token
    (Self : Case_Path)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Case_Paths;
