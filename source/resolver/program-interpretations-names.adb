--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Interpretations.Names is

   procedure Step
     (Self   : Iterator'Class;
      Cursor : in out Names.Cursor);

   ----------
   -- Each --
   ----------

   function Each
     (Set : Interpretation_Set) return Iterator is
   begin
      return (Set => Set);
   end Each;

   -----------
   -- First --
   -----------

   overriding function First (Self : Iterator) return Cursor is
   begin
      return Result : Cursor := (Index => Self.Set.From, State => <>) do
         Self.Step (Result);
      end return;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Cursor) return Boolean is
   begin
      return Self.Index > 0;
   end Has_Element;

   ----------
   -- Next --
   ----------

   overriding function Next (Self : Iterator; Position : Cursor) return Cursor
   is
   begin
      return Result : Cursor := Position do
         if Position.State.Is_Symbol then
            Result.State.Cursor := Result.State.Iter.Next
              (Result.State.Cursor);

            if Program.Visibility.Has_Element (Result.State.Cursor) then
               return;
            end if;
         end if;

         Result.Index := Result.Index + 1;
         Self.Step (Result);
      end return;
   end Next;

   ----------
   -- View --
   ----------

   function View (Self : Cursor) return Program.Visibility.View is
   begin
      if Self.State.Is_Symbol then
         return Program.Visibility.Get_View (Self.State.Cursor);
      else
         return Self.State.View;
      end if;
   end View;

   ----------
   -- Step --
   ----------

   procedure Step
     (Self   : Iterator'Class;
      Cursor : in out Names.Cursor)
   is
      Env : constant Program.Visibility.Context_Access := Self.Set.Context.Env;
   begin
      while Cursor.Index <= Self.Set.To loop
         declare
            Item : Interpretation renames Self.Set.Context.Data (Cursor.Index);
         begin
            case Item.Kind is
               when Name =>
                  Cursor.State := (Is_Symbol => False, View => Item.Name_View);

                  return;
               when Symbol =>
                  Cursor.State :=
                    (Is_Symbol => True,
                     Iter      => Env.Direct_Visible (Item.Symbol),
                     Cursor    => <>);

                  Cursor.State.Cursor := Cursor.State.Iter.First;

                  if Program.Visibility.Has_Element (Cursor.State.Cursor) then
                     return;
                  end if;

               when Expression =>
                  null;
            end case;
         end;

         Cursor.Index := Cursor.Index + 1;
      end loop;

      Cursor.Index := 0;
   end Step;

end Program.Interpretations.Names;
