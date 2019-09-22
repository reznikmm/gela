--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

limited private with Program.Parsers.Nodes;

package Program.Parsers is
   pragma Preelaborate;

   procedure Parse;

private

   type Parse_Context is record
      Factory : not null access Program.Parsers.Nodes.Node_Factory;
   end record;

end Program.Parsers;
