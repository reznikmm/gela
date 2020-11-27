--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

with Program.Symbols.Tables;

package Program.Symbol_Lists is
   pragma Preelaborate;

   type Symbol_List is private;
   --  A symbol or several symbols separated by a dot ('.');

   function Empty_Symbol_List return Symbol_List
     with Inline;
   --  An empty symbol list

   function Hash (Value : Symbol_List) return Ada.Containers.Hash_Type
     with Inline;

   type Symbol_List_Table
     (Table : not null access Program.Symbols.Tables.Symbol_Table'Class) is
       tagged limited private;

   function Find
     (Self  : Symbol_List_Table'Class;
      Value : Program.Text) return Symbol_List;
   --  Return a Symbol_List for given Text or Empty_Symbol_List if no such
   --  value in the table.

   procedure Find_Or_Create
     (Self   : in out Symbol_List_Table'Class;
      Value  : Program.Text;
      Result : out Symbol_List);
   --  Return existing symbol list or create new one otherise. The parent
   --  table should have all symbols from the list.

   function Symbol_List_Text
     (Self : Symbol_List_Table'Class;
      List : Symbol_List) return Program.Text;
   --  Return text of the list. For debug purposes only

private

   type Symbol_List is range 0 .. Integer'Last;

   type Symbol_List_Item is record
      Prefix : Symbol_List;
      Symbol : Program.Symbols.Symbol;
   end record;

   function Hash (Value : Symbol_List_Item) return Ada.Containers.Hash_Type;

   package Symbol_List_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_List_Item,
      Element_Type    => Symbol_List,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Symbol_List_Table
     (Table : not null access Program.Symbols.Tables.Symbol_Table'Class) is
     tagged limited
   record
      Map : Symbol_List_Maps.Map;
   end record;

end Program.Symbol_Lists;
