--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Iterator_Interfaces;

with Program.Elements;
with Program.Lexical_Elements;

package Program.Element_Vectors is
   pragma Pure (Program.Element_Vectors);

   type Element_Vector is limited interface;
   --  Vector of elements

   type Element_Vector_Access is access all Element_Vector'Class
     with Storage_Size => 0;

   not overriding function Get_Length (Self : Element_Vector)
     return Positive is abstract;
   --  Number of elements in the vector. Not null vector has at least one item.

   function Length (Self : access Element_Vector'Class)
     return Natural is (if Self = null then 0 else Self.Get_Length);
   --  Return number of elements in the vector

   function Is_Empty (Self : access Element_Vector'Class)
     return Boolean is (Self = null);
   --  Check if the vector is empty

   not overriding function Element
     (Self  : Element_Vector;
      Index : Positive)
     return not null Program.Elements.Element_Access is abstract;
   --  Return an element of the vector

   not overriding function Delimiter
     (Self  : Element_Vector;
      Index : Positive)
     return Program.Lexical_Elements.Lexical_Element_Access is abstract
       with Post'Class =>
         (if Index = Self.Get_Length then Delimiter'Result in null);
   --  Return a delimiter token after an element of the vector

   type Element_Cursor is record
      Element  : Program.Elements.Element_Access;
      --  An element of the vector for given cursor

      Delimiter : Program.Lexical_Elements.Lexical_Element_Access;
      --  A delimiter after the element pointed by the cursor

      Index     : Natural;
      --  Position in the vector, starting from one

      Is_Last   : Boolean;
      --  Set if the cursor points to the last element in the list
   end record;

   function Has_Element (Self : Element_Cursor) return Boolean
     is (Self.Element.Assigned);
   --  Check if the cursor points an element

   package Iterators is new Ada.Iterator_Interfaces
     (Element_Cursor, Has_Element);

   type Iterator is new Iterators.Reversible_Iterator with private;

   overriding function First
     (Self : Iterator) return Program.Element_Vectors.Element_Cursor;

   overriding function Next
     (Self   : Iterator;
      Cursor : Program.Element_Vectors.Element_Cursor)
         return Program.Element_Vectors.Element_Cursor;

   overriding function Last
     (Self : Iterator) return Program.Element_Vectors.Element_Cursor;

   overriding function Previous
     (Self   : Iterator;
      Cursor : Program.Element_Vectors.Element_Cursor)
         return Program.Element_Vectors.Element_Cursor;

   type Element_Checker is access
     function (Self : not null Program.Elements.Element_Access) return Boolean;

   function Each_Element
     (Self   : access Element_Vector'Class;
      Filter : Element_Checker := null) return Iterator;
   --  Return an iterator to enumerate elements in the Vector which satisfy
   --  given condition, if provided

   function Single_Element
     (Element : Program.Elements.Element_Access)
        return Iterators.Forward_Iterator'Class;

   function Two_Elements
     (Left, Right : Program.Elements.Element_Access)
        return Iterators.Forward_Iterator'Class;

private

   type Iterator is new Iterators.Reversible_Iterator with record
      Vector    : access constant Element_Vector'Class;
      Condition : Element_Checker;
   end record;

end Program.Element_Vectors;
