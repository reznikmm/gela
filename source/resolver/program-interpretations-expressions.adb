--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Interpretations.Expressions is

   procedure Step
     (Self   : Iterator'Class;
      Cursor : in out Expressions.Cursor);

   ----------
   -- Each --
   ----------

   function Each (Set : Interpretation_Set) return Iterator is
   begin
      return (Set      => Set,
              Expected => (Is_Set => False));
   end Each;

   ------------------
   -- Each_Of_Type --
   ------------------

   function Each_Of_Type
     (Set           : Interpretation_Set;
      Expected_Type : Program.Visibility.View)
     return Iterator is
   begin
      return (Set      => Set,
              Expected => (True, Expected_Type));
   end Each_Of_Type;

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

   overriding function Next
     (Self : Iterator; Position : Cursor) return Cursor is
   begin
      return Result : Cursor := Position do
         case Position.State.Kind is
            when Symbol =>
               Result.State.Cursor := Result.State.Iter.Next
                 (Result.State.Cursor);

               if Program.Visibility.Has_Element (Result.State.Cursor) then
                  return;
               end if;
            when Name | Expression =>
               null;
         end case;

         Result.Index := Result.Index + 1;
         Self.Step (Result);
      end return;
   end Next;

   --------------
   -- Solution --
   --------------

   function Solution (Self : Cursor) return Program.Interpretations.Solution is
   begin
      case Self.State.Kind is
         when Symbol =>
            return (Defining_Name_Solution,
                    Program.Visibility.Get_View (Self.State.Cursor));
         when Name =>
            return (Defining_Name_Solution, Self.State.View);
         when Expression =>
            return (Tuple_Solution, Self.State.Tuple);
      end case;

   end Solution;

   ----------
   -- Step --
   ----------

   procedure Step
     (Self   : Iterator'Class;
      Cursor : in out Expressions.Cursor)
   is
      use type Program.Visibility.View_Cursor;

      function Check_Name (View : Program.Visibility.View) return Boolean;
      --  Check if View is an expression of expected type

      function Check_Type (View : Program.Visibility.View) return Boolean;
      --  Check expression type is expected type

      function Check_Name (View : Program.Visibility.View) return Boolean is
         use all type Program.Visibility.View_Kind;
      begin
         return View.Kind in
           Enumeration_Literal_View | Character_Literal_View | Parameter_View
           and then Check_Type (Program.Visibility.Type_Of (View));
      end Check_Name;

      function Check_Type (View : Program.Visibility.View) return Boolean is
      begin
         if Self.Expected.Is_Set then
            return Program.Visibility.Is_Expected_Type
              (View, Self.Expected.View);
         end if;

         return True;
      end Check_Type;

      Env : constant Program.Visibility.Context_Access := Self.Set.Context.Env;
   begin
      while Cursor.Index <= Self.Set.To loop
         declare
            Item : Interpretation renames Self.Set.Context.Data (Cursor.Index);
         begin
            case Item.Kind is
               when Symbol =>
                  Cursor.State :=
                    (Kind   => Symbol,
                     Iter   => Env.Directly_Visible (Item.Symbol),
                     Cursor => <>);

                  Cursor.State.Cursor := Cursor.State.Iter.First;

                  while Program.Visibility.Has_Element (Cursor.State.Cursor)
                  loop
                     if Check_Name (+Cursor.State.Cursor) then
                        return;
                     end if;

                     Cursor.State.Cursor := Cursor.State.Iter.Next
                       (Cursor.State.Cursor);
                  end loop;

               when Name =>
                  if Check_Name (Item.Name_View) then
                     Cursor.State := (Kind => Name, View => Item.Name_View);

                     return;
                  end if;
               when Expression =>
                  if Check_Type (Item.Type_View) then
                     Cursor.State :=
                       (Kind      => Expression,
                        Type_View => Item.Type_View,
                        Tuple     => Solution_Tuple_Access (Item.Solutions));

                     return;
                  end if;
            end case;
         end;

         Cursor.Index := Cursor.Index + 1;
      end loop;

      Cursor.Index := 0;
   end Step;

   ---------------
   -- Type_View --
   ---------------

   function Type_View (Self : Cursor) return Program.Visibility.View is
      Object : Program.Visibility.View;
   begin
      case Self.State.Kind is
         when Symbol =>
            Object := Program.Visibility.Get_View (Self.State.Cursor);
         when Name =>
            Object := Self.State.View;
         when Expression =>
            return Self.State.Type_View;
      end case;

      return Program.Visibility.Type_Of (Object);
   end Type_View;

end Program.Interpretations.Expressions;
