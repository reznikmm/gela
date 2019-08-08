--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Element_Vectors is

   procedure Skip_Elements
     (Self  : Iterator;
      Pos   : in out Element_Cursor;
      Step  : Integer);
   --  Find a good Element starting from Pos.Index and set other fields of Pos

   ------------------
   -- Each_Element --
   ------------------

   function Each_Element
     (Self         : access Element_Vector'Class;
      When_Element : Element_Checker := null) return Iterator is
   begin
      return (Vector => Self, Condition => When_Element);
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
        and then not Self.Condition (Self.Vector.Element (Pos.Index).all)
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

end Program.Element_Vectors;
