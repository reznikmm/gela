--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

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
      pragma Unreferenced (Down);
      Item : constant Interpretation := (Expression, Tipe);
   begin
      Self.Context.Data.Append (Item);
      Self.To := Self.Context.Data.Last_Index;
   end Add_Expression;

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

end Program.Interpretations;
