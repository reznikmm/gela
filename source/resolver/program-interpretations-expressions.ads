--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Iterator_Interfaces;

package Program.Interpretations.Expressions is
   pragma Preelaborate;

   type Cursor is private;

   function Type_View (Self : Cursor) return Program.Visibility.View;

   function "+" (Self : Cursor) return Program.Visibility.View
     renames Type_View;

   function Solution (Self : Cursor) return Program.Interpretations.Solution;

   function "-" (Self : Cursor) return Program.Interpretations.Solution
     renames Solution;

   function Has_Element (Self : Cursor) return Boolean;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator is new Iterators.Forward_Iterator with private;

   function Each (Set : Interpretation_Set) return Iterator;
   --  Return each interpretation for an expression.

   function Each_Of_Type
     (Set           : Interpretation_Set;
      Expected_Type : Program.Visibility.View) return Iterator;
   --  Return expression interpretation of given Expected_Type.

private

   type Optional_View (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            View : Program.Visibility.View;
         when False =>
            null;
      end case;
   end record;

   type Cursor_State (Kind : Interpretation_Kind := Symbol) is record
      case Kind is
         when Symbol =>
            Iter   : Program.Visibility.Directly_Visible_Name_Iterator;
            Cursor : Program.Visibility.View_Cursor;
         when Name =>
            View   : Program.Visibility.View;
         when Expression =>
            Type_View : Program.Visibility.View;
            Tuple     : Solution_Tuple_Access;
      end case;
   end record;

   type Cursor is record
      Index : Natural;
      State : Cursor_State;
   end record;

   type Iterator is new Iterators.Forward_Iterator with record
      Set      : Interpretation_Set;
      Expected : Optional_View;
   end record;

   overriding function First (Self : Iterator) return Cursor;

   overriding function Next
     (Self     : Iterator;
      Position : Cursor) return Cursor;

end Program.Interpretations.Expressions;
