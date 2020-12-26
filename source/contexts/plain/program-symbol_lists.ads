--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

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

   type Symbol_List_Table_Access is access all Symbol_List_Table'Class
     with Storage_Size => 0;

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

   procedure Find_Or_Create
     (Self   : in out Symbol_List_Table'Class;
      Prefix : Symbol_List := Empty_Symbol_List;
      Suffix : Program.Symbols.Symbol;
      Result : out Symbol_List);
   --  Find or build a new list by appending Suffix to Prefix (if any).

   function Symbol_List_Text
     (Self : Symbol_List_Table'Class;
      List : Symbol_List) return Program.Text;
   --  Return text of the list

   function Prefix
     (Self : Symbol_List_Table'Class;
      List : Program.Symbol_Lists.Symbol_List)
      return Program.Symbol_Lists.Symbol_List
        with Pre => List /= Empty_Symbol_List;

   function Suffix
     (Self : Symbol_List_Table'Class;
      List : Program.Symbol_Lists.Symbol_List)
      return Program.Symbols.Symbol
        with Pre => List /= Empty_Symbol_List;

private

   type Symbol_List is range 0 .. Integer'Last;
   subtype Symbol_List_Positive is Symbol_List range 1 .. Symbol_List'Last;

   type Symbol_List_Item is record
      Prefix : Symbol_List;
      Symbol : Program.Symbols.Symbol;
   end record;

   function Hash (Value : Symbol_List_Item) return Ada.Containers.Hash_Type;

   package Symbol_List_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_List_Item,
      Element_Type    => Symbol_List_Positive,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package Symbol_List_Vectors is new Ada.Containers.Vectors
     (Symbol_List_Positive, Symbol_List_Item);

   type Symbol_List_Table
     (Table : not null access Program.Symbols.Tables.Symbol_Table'Class) is
     tagged limited
   record
      Map  : Symbol_List_Maps.Map;
      Back : Symbol_List_Vectors.Vector;
   end record;

end Program.Symbol_Lists;
