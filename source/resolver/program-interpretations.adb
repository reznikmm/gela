--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Program.Interpretations is

   -----------------------
   -- Add_Defining_Name --
   -----------------------

   procedure Add_Defining_Name
     (Self      : in out Interpretation_Set'Class;
      Name_View : Program.Visibility.View;
      Down      : Solution_Array := Empty_Solution_Array)
   is
      pragma Unreferenced (Down);
      Item : constant Interpretation := (Name, Name_View);
   begin
      Self.Context.Data.Append (Item);
      Self.To := Self.Context.Data.Last_Index;
   end Add_Defining_Name;

   --------------------
   -- Add_Expression --
   --------------------

   procedure Add_Expression
     (Self : in out Interpretation_Set'Class;
      Tipe : Program.Visibility.View;
      Down : Solution_Array := Empty_Solution_Array)
   is
      Item : constant Interpretation :=
        (Expression, Tipe, new Solution_Array'(Down));
   begin
      --  Only non-interrupted set allowed
      pragma Assert
        (Self.To < Self.From or Self.To = Self.Context.Data.Last_Index);

      Self.Context.Data.Append (Item);

      if Self.To < Self.From then
         Self.From := Self.Context.Data.Last_Index;
      end if;

      Self.To := Self.Context.Data.Last_Index;
   end Add_Expression;

   -----------------------------
   -- Add_Expression_Category --
   -----------------------------

   procedure Add_Expression_Category
     (Self    : in out Interpretation_Set'Class;
      Matcher : not null Program.Type_Matchers.Type_Matcher_Access)
   is
      Item : constant Interpretation := (Expression_Category, Matcher);
   begin
      --  Only non-interrupted set allowed
      pragma Assert
        (Self.To < Self.From or Self.To = Self.Context.Data.Last_Index);

      Self.Context.Data.Append (Item);

      if Self.To < Self.From then
         Self.From := Self.Context.Data.Last_Index;
      end if;

      Self.To := Self.Context.Data.Last_Index;
   end Add_Expression_Category;

   ----------------
   -- Add_Symbol --
   ----------------

   procedure Add_Symbol
     (Self   : in out Interpretation_Set'Class;
      Symbol : Program.Symbols.Symbol)
   is
      Item : constant Interpretation := (Interpretations.Symbol, Symbol);
   begin
      Self.Context.Data.Append (Item);
      Self.To := Self.Context.Data.Last_Index;
   end Add_Symbol;

   -------------------------------
   -- Create_Interpretation_Set --
   -------------------------------

   function Create_Interpretation_Set
     (Self  : in out Context'Class) return Interpretation_Set is
   begin
      return (Self'Unchecked_Access,
              From => Self.Data.Last_Index + 1,
              To =>   Self.Data.Last_Index);
   end Create_Interpretation_Set;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Context) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Solution_Array, Solution_Array_Access);
   begin
      for X of Self.Data loop
         if X.Kind in Expression then
            Free (X.Solutions);
         end if;
      end loop;
   end Finalize;

end Program.Interpretations;
