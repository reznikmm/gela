--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;
with Program.Tokens;

package Program.Elements.Then_Abort_Paths is

   pragma Pure (Program.Elements.Then_Abort_Paths);

   type Then_Abort_Path is limited interface and Program.Elements.Paths.Path;

   type Then_Abort_Path_Access is access all Then_Abort_Path'Class
     with Storage_Size => 0;

   not overriding function Then_Token
    (Self : Then_Abort_Path)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Abort_Token
    (Self : Then_Abort_Path)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Then_Abort_Paths;
