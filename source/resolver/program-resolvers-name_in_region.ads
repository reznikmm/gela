--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package Program.Resolvers.Name_In_Region is
   pragma Preelaborate;

   procedure Resolve_Name
     (Region : Program.Visibility.View;
      Name   : Program.Elements.Expressions.Expression_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access);
   --  Resolve direct Name in a given region (no overloading).

end Program.Resolvers.Name_In_Region;
