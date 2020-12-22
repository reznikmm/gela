--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements;
with Program.Symbols;

package Program.Node_Symbols is
   pragma Preelaborate;

   function Get_Symbol (Name : access Program.Elements.Element'Class)
     return Program.Symbols.Symbol;
   --  Return a symbol for given direct name or defining name. Return symbol of
   --  the selector for expanded [defining] name.

end Program.Node_Symbols;
