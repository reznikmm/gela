--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Lexical_Elements is

   pragma Pure (Program.Lexical_Elements);

   type Lexical_Element is limited interface;

   type Lexical_Element_Access is
     access all Lexical_Element'Class with Storage_Size => 0;

   function Assigned (Self : access Lexical_Element'Class) return Boolean
     is (Self /= null);

   type Lexical_Element_Vector is limited interface;
   --  Vector of lexical elements.

   type Lexical_Element_Vector_Access is
     access all Lexical_Element_Vector'Class
       with Storage_Size => 0;

   not overriding function First_Index (Self : Lexical_Element_Vector)
     return Positive is abstract;

   not overriding function Last_Index (Self : Lexical_Element_Vector)
     return Positive is abstract;
   --  The vector always has at least one element.

   function Length (Self : Lexical_Element_Vector'Class)
     return Positive is (Self.Last_Index - Self.First_Index + 1);
   --  Return number of elements in the vector

   not overriding function Element
     (Self  : Lexical_Element_Vector;
      Index : Positive)
      return not null Lexical_Element_Access
        is abstract
     with Pre'Class => Index in Self.First_Index .. Self.Last_Index;
   --  Return an element of the vector

   function First
     (Self  : Lexical_Element_Vector'Class)
      return not null Program.Lexical_Elements.Lexical_Element_Access
        with Inline;
   --  Get the first element of the vector

   function Last
     (Self  : Lexical_Element_Vector'Class)
      return not null Program.Lexical_Elements.Lexical_Element_Access
        with Inline;
   --  Get the last element of the vector

end Program.Lexical_Elements;
