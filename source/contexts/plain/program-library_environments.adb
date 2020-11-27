--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Library_Environments is

   -----------------
   -- Public_View --
   -----------------

   function Public_View
     (Self : Library_Environment'Class;
      Name : Program.Symbol_Lists.Symbol_List)
        return Program.Visibility.Snapshot_Access is
   begin
      return Self.Public_Views (Name);
   end Public_View;

   ---------------------
   -- Put_Public_View --
   ---------------------

   procedure Put_Public_View
     (Self  : in out Library_Environment'Class;
      Name  : Program.Symbol_Lists.Symbol_List;
      Value : Program.Visibility.Snapshot_Access) is
   begin
      Self.Public_Views.Insert (Name, Value);
   end Put_Public_View;

end Program.Library_Environments;
