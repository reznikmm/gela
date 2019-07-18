--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Vectors is

   package Iterators is
      type Iterator is new Program.Element_Vectors.Iterators.Forward_Iterator
      with record
         Vector : not null access constant
           Program.Element_Vectors.Element_Vector'Class;
      end record;

      overriding function First
        (Self : Iterator) return Program.Element_Vectors.Element_Cursor;

      overriding function Next
        (Self   : Iterator;
         Cursor : Program.Element_Vectors.Element_Cursor)
         return Program.Element_Vectors.Element_Cursor;
   end Iterators;

   ---------------
   -- Iterators --
   ---------------

   package body Iterators is

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Iterator) return Program.Element_Vectors.Element_Cursor is
      begin
         return Program.Element_Vectors.Element_Cursor'(Self.Vector, 1);
      end First;

      ----------
      -- Next --
      ----------

      overriding function Next
        (Self   : Iterator;
         Cursor : Program.Element_Vectors.Element_Cursor)
         return Program.Element_Vectors.Element_Cursor is
      begin
         return Program.Element_Vectors.Element_Cursor'
           (Self.Vector, Cursor.Position);
      end Next;
   end Iterators;

   -------------------
   -- Create_Vector --
   -------------------

   function Create_Vector
     (Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
      return Vector
   is
      Elements, Tokens : Natural := 0;
      Has_Tokens : Boolean := False;
      Index : Positive := 1;
   begin
      for J in Each loop
         Elements := Elements + 1;

         if J.Delimiter.Assigned then
            Has_Tokens := True;
         end if;
      end loop;

      if Has_Tokens then
         Tokens := Elements - 1;
      end if;

      return Result : Vector (Elements, Tokens) do
         for J in Each loop
            Result.Element_List (Index) := J.Element;

            if Index in Result.Token_List'Range then
               Result.Token_List (Index) := J.Delimiter;
            end if;

            Index := Index + 1;
         end loop;
      end return;
   end Create_Vector;

   ---------------
   -- Delimiter --
   ---------------

   overriding function Delimiter
     (Self  : Vector;
      Index : Positive)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      if Index in Self.Token_List'Range then
         return Self.Token_List (Index);
      else
         return null;
      end if;
   end Delimiter;

   ------------
   -- Length --
   ------------

   overriding function Length (Self : Vector) return Natural is
   begin
      return Self.Elements;
   end Length;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Vector;
      Index : Positive)
        return not null Program.Elements.Element_Access is
   begin
      return Self.Element_List (Index);
   end Element;

   ----------
   -- Each --
   ----------

   overriding function Each
     (Self : aliased Vector)
      return Program.Element_Vectors.Iterators.Forward_Iterator'Class is
   begin
      return Iterators.Iterator'(Vector => Self'Unchecked_Access);
   end Each;

end Program.Nodes.Vectors;
