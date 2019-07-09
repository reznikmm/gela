--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Iterator_Interfaces;

with Program.Elements;
with Program.Tokens;

package Program.Element_Vectors is
   pragma Pure (Program.Element_Vectors);

   type Element_Vector is limited interface;
   --  Vector of elements

   type Element_Vector_Access is access all Element_Vector'Class
     with Storage_Size => 0;

   not overriding function Length (Self : Element_Vector)
     return Natural is abstract;
   --  Return number of elements in the vector

   function Is_Empty (Self : Element_Vector'Class)
     return Boolean is (Self.Length = 0);
   --  Check if the vector is empty

   not overriding function Element
     (Self  : Element_Vector;
      Index : Positive)
     return not null Program.Elements.Element_Access is abstract;
   --  Return an element of the vector

   not overriding function Delimiter
     (Self  : Element_Vector;
      Index : Positive)
     return Program.Tokens.Token_Access is abstract
       with Post'Class =>
         (if Index = Self.Length then Delimiter'Result in null);
   --  Return a delimiter token after an element of the vector

   type Element_Cursor
     (Vector : not null access Element_Vector'Class) is tagged
   record
      Position : Natural := 0;
   end record;

   not overriding function Has_Element (Self : Element_Cursor) return Boolean
     is (Self.Position /= 0);
   --  Check if the cursor points an element

   function Is_Last
     (Self : Element_Cursor'Class) return Boolean is
       (Self.Position = Self.Vector.Length);
   --  Check if the cursor points the last element in the list

   function Element (Self : Element_Cursor'Class)
     return not null Program.Elements.Element_Access
       is (Self.Vector.Element (Self.Position));
   --  Return an element of the vector for given cursor

   function Delimiter (Self : Element_Cursor'Class)
     return Program.Tokens.Token_Access
       is (Self.Vector.Delimiter (Self.Position));
   --  Return a delimiter after the element pointed by the cursor

   package Iterators is new Ada.Iterator_Interfaces
     (Element_Cursor, Has_Element);

   not overriding function Each (Self : aliased Element_Vector)
     return Iterators.Forward_Iterator'Class is abstract;
   --  Return an iterator to enumerate all elements in the Vector

end Program.Element_Vectors;
