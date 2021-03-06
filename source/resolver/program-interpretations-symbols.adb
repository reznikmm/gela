--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Interpretations.Symbols is

   procedure Step
     (Self   : Iterator'Class;
      Cursor : in out Symbols.Cursor);

   ----------
   -- Each --
   ----------

   function Each (Set : Interpretation_Set) return Iterator is
   begin
      return (Set => Set);
   end Each;

   -----------
   -- First --
   -----------

   overriding function First (Self : Iterator) return Cursor is
   begin
      return Result : Cursor := (Index => Self.Set.From, Symbol => <>) do
         Self.Step (Result);
      end return;
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self : Iterator; Position : Cursor) return Cursor is
   begin
      return Result : Cursor := Position do
         Result.Index := Result.Index + 1;
         Self.Step (Result);
      end return;
   end Next;

   ----------
   -- Step --
   ----------

   procedure Step
     (Self   : Iterator'Class;
      Cursor : in out Symbols.Cursor) is
   begin
      while Cursor.Index <= Self.Set.To loop
         declare
            Item : Interpretation renames Self.Set.Context.Data (Cursor.Index);
         begin
            case Item.Kind is
               when Symbol =>
                  Cursor.Symbol := Item.Symbol;

               when Name | Expression | Expression_Category =>
                  null;
            end case;
         end;

         Cursor.Index := Cursor.Index + 1;
      end loop;

      Cursor.Index := 0;
   end Step;

end Program.Interpretations.Symbols;
