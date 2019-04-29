--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Package_Body_Stubs is

   pragma Pure (Program.Elements.Package_Body_Stubs);

   type Package_Body_Stub is
     limited interface and Program.Elements.Declarations.Declaration;

   type Package_Body_Stub_Access is access all Package_Body_Stub'Class
     with Storage_Size => 0;

end Program.Elements.Package_Body_Stubs;
