--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Iterator_Interfaces;

package Program.Interpretations.Symbols is
   pragma Preelaborate;

   type Cursor is record
      Index  : Natural;
      Symbol : Program.Symbols.Symbol;
   end record;

   function Has_Element (Self : Cursor) return Boolean is (Self.Index > 0);

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator is new Iterators.Forward_Iterator with private;

   function Each (Set : Interpretation_Set) return Iterator;

private

   type Iterator is new Iterators.Forward_Iterator with record
      Set : Interpretation_Set;
   end record;

   overriding function First (Self : Iterator) return Cursor;

   overriding function Next
     (Self     : Iterator;
      Position : Cursor) return Cursor;

end Program.Interpretations.Symbols;
