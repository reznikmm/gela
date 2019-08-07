--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Element_Vectors is

   ------------------
   -- Each_Element --
   ------------------

   function Each_Element
     (Self : access Element_Vector'Class) return Iterator is
   begin
      return (Vector => Self);
   end Each_Element;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Iterator) return Program.Element_Vectors.Element_Cursor is
   begin
      if Self.Vector = null then
         return (null, null, 1, False);
      else
         return (Self.Vector.Element (1),
                 Self.Vector.Delimiter (1),
                 Index   => 1,
                 Is_Last => Self.Vector.Length = 1);
      end if;
   end First;

   ----------
   -- Last --
   ----------

   overriding function Last
     (Self : Iterator) return Program.Element_Vectors.Element_Cursor is
   begin
      if Self.Vector = null then
         return (null, null, 1, False);
      else
         return (Self.Vector.Element (Self.Vector.Length),
                 Self.Vector.Delimiter (Self.Vector.Length),
                 Index   => Self.Vector.Length,
                 Is_Last => True);
      end if;
   end Last;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self   : Iterator;
      Cursor : Program.Element_Vectors.Element_Cursor)
         return Program.Element_Vectors.Element_Cursor is
   begin
      if Cursor.Index >= Self.Vector.Length then
         return (null, null, Self.Vector.Length + 1, False);
      else
         return (Self.Vector.Element (Cursor.Index + 1),
                 Self.Vector.Delimiter (Cursor.Index + 1),
                 Cursor.Index + 1,
                 Self.Vector.Length = Cursor.Index + 1);
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Self   : Iterator;
      Cursor : Program.Element_Vectors.Element_Cursor)
         return Program.Element_Vectors.Element_Cursor is
   begin
      if Cursor.Index >= Self.Vector.Length then
         return (null, null, Self.Vector.Length + 1, False);
      else
         return (Self.Vector.Element (Cursor.Index - 1),
                 Self.Vector.Delimiter (Cursor.Index - 1),
                 Cursor.Index - 1,
                 False);
      end if;
   end Previous;

end Program.Element_Vectors;
