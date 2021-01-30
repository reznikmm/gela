--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Element_Vectors is

   procedure Skip_Elements
     (Self  : Iterator;
      Pos   : in out Element_Cursor;
      Step  : Integer);
   --  Find a good Element starting from Pos.Index and set other fields of Pos

   package Single is
      type Iterator is new Iterators.Forward_Iterator with record
         Element : Program.Elements.Element_Access;
      end record;

      overriding function First (Self : Iterator) return Element_Cursor;

      overriding function Next
        (Self : Iterator;
         Pos  : Element_Cursor) return Element_Cursor;

   end Single;

   package body Single is

      overriding function First (Self : Iterator) return Element_Cursor is
      begin
         return (Self.Element, null, 1, Is_Last => True);
      end First;

      overriding function Next
        (Self : Iterator;
         Pos  : Element_Cursor) return Element_Cursor
      is
         pragma Unreferenced (Self);
      begin
         if Pos.Index = 1 then
            return (null, null, 0, False);
         else
            return Pos;
         end if;
      end Next;

   end Single;

   package Two is
      type Iterator is new Iterators.Forward_Iterator with record
         Left, Right : Program.Elements.Element_Access;
         Delimiter   : Program.Lexical_Elements.Lexical_Element_Access;
      end record;

      overriding function First (Self : Iterator) return Element_Cursor;

      overriding function Next
        (Self : Iterator;
         Pos  : Element_Cursor) return Element_Cursor;

   end Two;

   package body Two is

      overriding function First (Self : Iterator) return Element_Cursor is
      begin
         return (Self.Left, Self.Delimiter, 1, Is_Last => False);
      end First;

      overriding function Next
        (Self : Iterator;
         Pos  : Element_Cursor) return Element_Cursor is
      begin
         if Pos.Index = 2 then
            return (null, null, 0, False);
         elsif Pos.Index = 1 then
            return (Self.Right, null, 2, Is_Last => True);
         else
            return Pos;
         end if;
      end Next;

   end Two;

   ------------------
   -- Each_Element --
   ------------------

   function Each_Element
     (Self   : access Element_Vector'Class;
      Filter : Element_Checker := null) return Iterator is
   begin
      return (Vector => Self, Condition => Filter);
   end Each_Element;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Iterator) return Program.Element_Vectors.Element_Cursor is
   begin
      return Result : Element_Cursor := (Index => 1, others => <>) do
         Self.Skip_Elements (Result, Step => 1);
      end return;
   end First;

   ----------
   -- Last --
   ----------

   overriding function Last
     (Self : Iterator) return Program.Element_Vectors.Element_Cursor is
   begin
      return Result : Element_Cursor :=
        (Index => Self.Vector.Length, others => <>)
      do
         Self.Skip_Elements (Result, Step => -1);
      end return;
   end Last;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self   : Iterator;
      Cursor : Program.Element_Vectors.Element_Cursor)
         return Program.Element_Vectors.Element_Cursor is
   begin
      return Result : Element_Cursor := Cursor do
         Result.Index := Result.Index + 1;
         Self.Skip_Elements (Result, Step => 1);
      end return;
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Self   : Iterator;
      Cursor : Program.Element_Vectors.Element_Cursor)
         return Program.Element_Vectors.Element_Cursor is
   begin
      return Result : Element_Cursor := Cursor do
         Result.Index := Result.Index - 1;
         Self.Skip_Elements (Result, Step => -1);
      end return;
   end Previous;

   --------------------
   -- Single_Element --
   --------------------

   function Single_Element
     (Element : Program.Elements.Element_Access)
        return Iterators.Forward_Iterator'Class
   is
   begin
      return Single.Iterator'(Element => Element);
   end Single_Element;

   -------------------
   -- Skip_Elements --
   -------------------

   procedure Skip_Elements
     (Self  : Iterator;
      Pos   : in out Element_Cursor;
      Step  : Integer) is
   begin
      while Pos.Index in 1 .. Self.Vector.Length
        and then Self.Condition /= null
        and then not Self.Condition (Self.Vector.Element (Pos.Index))
      loop
         Pos.Index := Pos.Index + Step;
      end loop;

      if Pos.Index in 1 .. Self.Vector.Length then
         Pos := (Self.Vector.Element (Pos.Index),
                 Self.Vector.Delimiter (Pos.Index),
                 Pos.Index,
                 Pos.Index = Self.Vector.Length);
      else
         Pos := (null, null, 0, False);
      end if;
   end Skip_Elements;

   ------------------
   -- Two_Elements --
   ------------------

   function Two_Elements
     (Left, Right : Program.Elements.Element_Access)
        return Iterators.Forward_Iterator'Class
   is
   begin
      return Two.Iterator'(Left, Right, null);
   end Two_Elements;

end Program.Element_Vectors;
