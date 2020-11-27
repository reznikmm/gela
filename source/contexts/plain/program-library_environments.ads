--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

with Program.Visibility;
with Program.Symbol_Lists;

package Program.Library_Environments is
   pragma Preelaborate;

   type Library_Environment is tagged limited private;

   function Public_View
     (Self : Library_Environment'Class;
      Name : Program.Symbol_Lists.Symbol_List)
        return Program.Visibility.Snapshot_Access;
   --  Return a library level view of a unit with given Name as it's visible
   --  from a public child unit.

   procedure Put_Public_View
     (Self  : in out Library_Environment'Class;
      Name  : Program.Symbol_Lists.Symbol_List;
      Value : Program.Visibility.Snapshot_Access);
   --  Remember a library level view of a unit with given Name as it's visible
   --  from a public child unit.

private

   package Snapshot_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Program.Symbol_Lists.Symbol_List,
      Element_Type    => Program.Visibility.Snapshot_Access,
      Hash            => Program.Symbol_Lists.Hash,
      Equivalent_Keys => Program.Symbol_Lists."=",
      "="             => Program.Visibility."=");

   type Library_Environment is tagged limited record
      Public_Views : Snapshot_Maps.Map;
   end record;

end Program.Library_Environments;
