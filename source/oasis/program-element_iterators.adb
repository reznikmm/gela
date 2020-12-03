--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vectors;

package body Program.Element_Iterators is

   package Internal is

      function Get
        (Parent : Program.Elements.Element_Access)
           return access constant Getter_Array;

   end Internal;

   generic
      type Element is limited interface and Program.Elements.Element;

      type Child is limited interface and Program.Elements.Element;

      type Child_Access is access all Child'Class;

      with function Get_Child (Self : Element) return Child_Access is abstract;
   function Generic_Child (Self : not null Program.Elements.Element_Access)
     return Program.Elements.Element_Access;

   generic
      type Parent is limited interface and Program.Elements.Element;

      type Vector is
        limited interface and Program.Element_Vectors.Element_Vector;

      type Vector_Access is access all Vector'Class;

      with function Get_Vector
        (Self : Parent) return Vector_Access is abstract;
   function Generic_Vector (Self : not null Program.Elements.Element_Access)
     return Program.Element_Vectors.Element_Vector_Access;

   -------------
   -- Cursors --
   -------------

   package body Cursors is

      -------------
      -- Element --
      -------------

      function Element
        (Self : Child_Cursor) return Program.Elements.Element_Access is
      begin
         return Self.Element;
      end Element;

      -----------------
      -- Has_Element --
      -----------------

      function Has_Element (Self : Child_Cursor) return Boolean is
      begin
         return Self.Element.Assigned;
      end Has_Element;

      ---------------------------
      -- Has_Enclosing_Element --
      ---------------------------

      function Has_Enclosing_Element
        (Self : Enclosing_Element_Cursor) return Boolean is
      begin
         return Self.Element.Assigned;
      end Has_Enclosing_Element;

      -----------
      -- Index --
      -----------

      function Index (Self : Child_Cursor) return Positive is
      begin
         return Self.Item_Index;
      end Index;

      -----------
      -- Total --
      -----------

      function Total (Self : Child_Cursor) return Positive is
      begin
         return Self.Total_Items;
      end Total;

      --------------
      -- Property --
      --------------

      function Property (Self : Child_Cursor) return Property_Name is
      begin
         return Self.Property;
      end Property;

      package body Internal is

         procedure Step
           (Self   : Child_Iterator'Class;
            Cursor : in out Child_Cursor;
            Reset  : Boolean)
         is
            Vector : Program.Element_Vectors.Element_Vector_Access;
         begin
            if Reset then
               Cursor.Getter_Index := 1;
               Cursor.Item_Index := 1;
            else
               Cursor.Item_Index := Cursor.Item_Index + 1;
            end if;

            Cursor.Element := null;

            Find_Next_Element :
            while Cursor.Getter_Index in Self.Getters'Range loop
               Cursor.Property := Self.Getters (Cursor.Getter_Index).Property;

               case Self.Getters (Cursor.Getter_Index).Is_Vector is
                  when True =>
                     Vector := Self.Getters (Cursor.Getter_Index).Get_Vector
                       (Self.Parent);

                     while Vector not in null
                       and then Cursor.Item_Index <= Vector.Length
                     loop
                        Cursor.Element := Vector.Element (Cursor.Item_Index);

                        exit Find_Next_Element
                          when Check (Cursor, Self.Filter (1 .. Self.Last));

                        Cursor.Item_Index := Cursor.Item_Index + 1;
                     end loop;

                  when False =>
                     if Cursor.Item_Index > 1 then
                        Cursor.Item_Index := 1;
                     else
                        Cursor.Element :=
                          Self.Getters (Cursor.Getter_Index).Get_Child
                          (Self.Parent);

                        exit Find_Next_Element
                          when Cursor.Element.Assigned and then
                            Check (Cursor, Self.Filter (1 .. Self.Last));
                     end if;
               end case;

               Cursor.Getter_Index := Cursor.Getter_Index + 1;
            end loop Find_Next_Element;
         end Step;

      end Internal;

   end Cursors;

   -----------
   -- Check --
   -----------

   function Check
     (Cursor : Cursors.Child_Cursor;
      List   : Checker_Chain_Array) return Boolean is
   begin
      for Item of List loop
         case Item.Is_Cursor is
            when True =>
               return Item.Cursor_Filter (Cursor);
            when False =>
               return Item.Element_Filter (Cursor.Element);
         end case;
      end loop;

      return True;
   end Check;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Child_Iterator) return Cursors.Child_Cursor is
   begin
      return Result : Cursors.Child_Cursor do
         Cursors.Internal.Step (Self, Result, True);
      end return;
   end First;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Enclosing_Element_Iterator)
      return Cursors.Enclosing_Element_Cursor is
   begin
      return (Element => Self.First, Level => 1);
   end First;

   --------------------
   -- Generic_Vector --
   --------------------

   function Generic_Vector
     (Self : not null Program.Elements.Element_Access)
      return Program.Element_Vectors.Element_Vector_Access
   is
      Result : constant Vector_Access := Get_Vector (Parent'Class (Self.all));
   begin
      return Program.Element_Vectors.Element_Vector_Access (Result);
   end Generic_Vector;

   -------------------
   -- Generic_Child --
   -------------------

   function Generic_Child
     (Self : not null Program.Elements.Element_Access)
      return Program.Elements.Element_Access
   is
      Result : constant Child_Access := Get_Child (Element'Class (Self.all));
   begin
      return Program.Elements.Element_Access (Result);
   end Generic_Child;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Enclosing_Element_Iterator;
      Position : Cursors.Enclosing_Element_Cursor)
      return Cursors.Enclosing_Element_Cursor
   is
      pragma Unreferenced (Self);
   begin
      return (Element => Position.Element.Enclosing_Element,
              Level   => Position.Level + 1);
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Child_Iterator;
      Position : Cursors.Child_Cursor) return Cursors.Child_Cursor is
   begin
      return Result : Cursors.Child_Cursor := Position do
         Cursors.Internal.Step (Self, Result, False);
      end return;
   end Next;

   -------------
   -- Only_If --
   -------------

   function Only_If
     (Parent : Child_Iterator;
      Filter : not null Cursor_Checker)
      return Child_Iterator is
   begin
      return Result : Child_Iterator :=
        (Parent.Parent, Parent.Getters, Parent.Filter, Parent.Last + 1)
      do
         Result.Filter (Result.Last) := (True, Filter);
      end return;
   end Only_If;

   -------------
   -- Only_If --
   -------------

   function Only_If
     (Parent : Child_Iterator;
      Filter : not null Element_Checker)
      return Child_Iterator is
   begin
      return Result : Child_Iterator :=
        (Parent.Parent, Parent.Getters, Parent.Filter, Parent.Last + 1)
      do
         Result.Filter (Result.Last) := (False, Filter);
      end return;
   end Only_If;

   -----------------------
   -- To_Child_Iterator --
   -----------------------

   function To_Child_Iterator
     (Parent : not null Program.Elements.Element_Access;
      Filter : Element_Checker := null)
      return Child_Iterator is
   begin
      return Result : Child_Iterator := (Parent  => Parent,
                                         Getters => Internal.Get (Parent),
                                         Last    => 0,
                                         Filter  => <>)
      do
         if Filter /= null then
            Result.Filter (1) := (False, Filter);
            Result.Last := 1;
         end if;
      end return;
   end To_Child_Iterator;

   -----------------------------------
   -- To_Enclosing_Element_Iterator --
   -----------------------------------

   function To_Enclosing_Element_Iterator
     (Parent : not null Program.Elements.Element_Access)
      return Enclosing_Element_Iterator is
   begin
      return Enclosing_Element_Iterator'(First => Parent);
   end To_Enclosing_Element_Iterator;

   package body Internal is separate;

end Program.Element_Iterators;
