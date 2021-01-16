--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Iterator_Interfaces;

package Program.Interpretations.Names is
   pragma Preelaborate;

   type Cursor is private;

   function View (Self : Cursor) return Program.Visibility.View;

   function "+" (Self : Cursor) return Program.Visibility.View renames View;

   function Has_Element (Self : Cursor) return Boolean;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator is new Iterators.Forward_Iterator with private;

   function Each (Set : Interpretation_Set) return Iterator;

private

   type Cursor_State (Is_Symbol : Boolean := False) is record
      case Is_Symbol is
         when True =>
            Iter   : Program.Visibility.Direct_Visible_Name_Iterator;
            Cursor : Program.Visibility.View_Cursor;
         when False =>
            View   : Program.Visibility.View;
      end case;
   end record;

   type Cursor is record
      Index : Natural;
      State : Cursor_State;
   end record;

   type Iterator is new Iterators.Forward_Iterator with record
      Set : Interpretation_Set;
   end record;

   overriding function First (Self : Iterator) return Cursor;

   overriding function Next
     (Self     : Iterator;
      Position : Cursor) return Cursor;

end Program.Interpretations.Names;
