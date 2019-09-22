--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Iterator_Interfaces;

with Program.Compilation_Units;

package Program.Compilation_Unit_Vectors is
   pragma Pure;

   type Compilation_Unit_Vector is limited interface;
   --  Vector of Compilation_Units

   type Compilation_Unit_Vector_Access is
     access constant Compilation_Unit_Vector'Class
       with Storage_Size => 0;

   not overriding function Get_Length (Self : Compilation_Unit_Vector)
     return Positive is abstract;
   --  Number of elements in the vector. Not null vector has at least one item.

   function Length (Self : access Compilation_Unit_Vector'Class)
     return Natural is (if Self = null then 0 else Self.Get_Length);
   --  Return number of elements in the vector

   function Is_Empty (Self : access Compilation_Unit_Vector'Class)
     return Boolean is (Self = null);
   --  Check if the vector is empty

   not overriding function Element
     (Self  : Compilation_Unit_Vector;
      Index : Positive)
        return not null Program.Compilation_Units.Compilation_Unit_Access
          is abstract;
   --  Return an element of the vector

   not overriding function Find_Unit
     (Self  : Compilation_Unit_Vector;
      Name  : Text)
        return Program.Compilation_Units.Compilation_Unit_Access
          is abstract;
   --  Returns the compilation_unit with the name, contained in the library.
   --
   --  A null is returned if no such compilation_unit exists.

   function Find
     (Self  : access Compilation_Unit_Vector'Class;
      Name  : Text)
        return Program.Compilation_Units.Compilation_Unit_Access
          is (if Self.Is_Empty then null else Self.Find_Unit (Name));
   --  Returns the compilation_unit with the name, contained in the library.
   --
   --  A null is returned if no such compilation_unit exists.

   type Compilation_Unit_Cursor is record
      Unit    : Program.Compilation_Units.Compilation_Unit_Access;
      --  An element of the vector for given cursor

      Index   : Natural;
      --  Position in the vector, starting from one

      Is_Last : Boolean;
      --  Set if the cursor points to the last Compilation_Unit in the list
   end record;

   function Has_Element (Self : Compilation_Unit_Cursor) return Boolean
     is (Self.Unit.Assigned);
   --  Check if the cursor points an element

   package Iterators is new Ada.Iterator_Interfaces
     (Compilation_Unit_Cursor, Has_Element);

   type Iterator is new Iterators.Reversible_Iterator with private;

   overriding function First (Self : Iterator)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Cursor;

   overriding function Next
     (Self   : Iterator;
      Cursor : Program.Compilation_Unit_Vectors.Compilation_Unit_Cursor)
         return Program.Compilation_Unit_Vectors.Compilation_Unit_Cursor;

   overriding function Last (Self : Iterator)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Cursor;

   overriding function Previous
     (Self   : Iterator;
      Cursor : Program.Compilation_Unit_Vectors.Compilation_Unit_Cursor)
         return Program.Compilation_Unit_Vectors.Compilation_Unit_Cursor;

   type Compilation_Unit_Checker is access
     function (Self : Program.Compilation_Units.Compilation_Unit'Class)
       return Boolean;

   function Each_Unit
     (Self      : access Compilation_Unit_Vector'Class;
      When_Unit : Compilation_Unit_Checker := null) return Iterator;
   --  Return an iterator to enumerate elements in the Vector which satisfy
   --  given condition, if provided or all element otherwise

private

   type Iterator is new Iterators.Reversible_Iterator with record
      Vector    : access constant Compilation_Unit_Vector'Class;
      Condition : Compilation_Unit_Checker;
   end record;

end Program.Compilation_Unit_Vectors;
