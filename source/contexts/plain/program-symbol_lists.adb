--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Strings.Wide_Wide_Fixed;

package body Program.Symbol_Lists is

   -----------------------
   -- Empty_Symbol_List --
   -----------------------

   function Empty_Symbol_List return Symbol_List is
   begin
      return 0;
   end Empty_Symbol_List;

   ----------
   -- Find --
   ----------

   function Find
     (Self  : Symbol_List_Table'Class;
      Value : Program.Text) return Symbol_List
   is
      Cursor : Symbol_List_Maps.Cursor;
      Item   : Symbol_List_Item :=
        (Empty_Symbol_List, Program.Symbols.No_Symbol);
      Result : Symbol_List;
      Prev   : Positive;
      Dot    : Natural := Value'First - 1;
   begin
      loop
         Prev := Dot + 1;
         Dot := Ada.Strings.Wide_Wide_Fixed.Index (Value, ".", Prev);
         exit when Dot not in Value'Range;
         Item.Symbol := Self.Table.Find (Value (Prev .. Dot - 1));
         Cursor := Self.Map.Find (Item);

         if Symbol_List_Maps.Has_Element (Cursor) then
            Item.Prefix := Symbol_List_Maps.Element (Cursor);
         else
            return Empty_Symbol_List;
         end if;
      end loop;

      Item.Symbol := Self.Table.Find (Value (Dot + 1 .. Value'Last));
      Cursor := Self.Map.Find (Item);

      if Symbol_List_Maps.Has_Element (Cursor) then
         Result := Symbol_List_Maps.Element (Cursor);
      else
         return Empty_Symbol_List;
      end if;

      return Result;
   end Find;

   --------------------
   -- Find_Or_Create --
   --------------------

   procedure Find_Or_Create
     (Self   : in out Symbol_List_Table'Class;
      Value  : Program.Text;
      Result : out Symbol_List)
   is
      Prefix : Symbol_List := Empty_Symbol_List;
      Suffix : Program.Symbols.Symbol;
      Prev   : Positive;
      Dot    : Natural := Value'First - 1;
   begin
      loop
         Prev := Dot + 1;
         Dot := Ada.Strings.Wide_Wide_Fixed.Index (Value, ".", Prev);
         exit when Dot not in Value'Range;
         Suffix := Self.Table.Find (Value (Prev .. Dot - 1));
         Self.Find_Or_Create (Prefix, Suffix, Result => Prefix);
      end loop;

      Suffix := Self.Table.Find (Value (Dot + 1 .. Value'Last));
      Self.Find_Or_Create (Prefix, Suffix, Result);
   end Find_Or_Create;

   --------------------
   -- Find_Or_Create --
   --------------------

   procedure Find_Or_Create
     (Self   : in out Symbol_List_Table'Class;
      Prefix : Symbol_List := Empty_Symbol_List;
      Suffix : Program.Symbols.Symbol;
      Result : out Symbol_List)
   is
      Item   : constant Symbol_List_Item := (Prefix, Suffix);
      Cursor : constant Symbol_List_Maps.Cursor := Self.Map.Find (Item);
   begin
      if Symbol_List_Maps.Has_Element (Cursor) then
         Result := Symbol_List_Maps.Element (Cursor);
      else
         Self.Back.Append (Item);
         Result := Self.Back.Last_Index;
         Self.Map.Insert (Item, Result);
      end if;
   end Find_Or_Create;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Symbol_List) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Mod (Value);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Symbol_List_Item) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Ada.Containers.Hash_Type'Mod (Value.Prefix)
        + 100003 * Program.Symbols.Hash (Value.Symbol);
   end Hash;

   ------------
   -- Prefix --
   ------------

   function Prefix
     (Self : Symbol_List_Table'Class;
      List : Program.Symbol_Lists.Symbol_List)
      return Program.Symbol_Lists.Symbol_List is
   begin
      return Self.Back (List).Prefix;
   end Prefix;

   ------------
   -- Suffix --
   ------------

   function Suffix
     (Self : Symbol_List_Table'Class;
      List : Program.Symbol_Lists.Symbol_List)
      return Program.Symbols.Symbol is
   begin
      return Self.Back (List).Symbol;
   end Suffix;

   ----------------------
   -- Symbol_List_Text --
   ----------------------

   function Symbol_List_Text
     (Self : Symbol_List_Table'Class;
      List : Symbol_List) return Program.Text
   is
      Prefix : Symbol_List;
      Symbol : Program.Symbols.Symbol;
   begin
      if List = Empty_Symbol_List then
         return "";
      end if;

      Prefix := Self.Prefix (List);
      Symbol := Self.Suffix (List);

      if Prefix = Empty_Symbol_List then
         return Self.Table.Symbol_Text (Symbol);
      else
         return Self.Symbol_List_Text (Prefix) & "." &
           Self.Table.Symbol_Text (Symbol);
      end if;
   end Symbol_List_Text;

end Program.Symbol_Lists;
