--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Tokens is
   pragma Pure (Program.Tokens);

   type Token is limited interface;

   type Token_Access is access all Token'Class with Storage_Size => 0;
end Program.Tokens;
