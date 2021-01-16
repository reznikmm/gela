--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

private with Ada.Containers.Vectors;

with Program.Visibility;
with Program.Symbols;

package Program.Interpretations is
   pragma Preelaborate;

   type Solution_Kind is
     (Placeholder_Solution,
      Defining_Name_Solution,
      Expression_Solution);

   type Solution (Kind : Solution_Kind := Solution_Kind'First) is record
      case Kind is
         when Placeholder_Solution =>
            null;
         when Defining_Name_Solution =>
            Name_View : Program.Visibility.View;
         when Expression_Solution =>
            Type_View : Program.Visibility.View;
      end case;
   end record;

   type Solution_Array is array (Positive range <>) of Solution;

   Empty_Solution_Array : constant Solution_Array := (1 .. 0 => <>);

   type Context (Env : not null Program.Visibility.Context_Access) is
     tagged limited private;

   type Context_Access is access all Context'Class with Storage_Size => 0;

   type Interpretation_Set is tagged private;
   type Interpretation_Set_Array is
     array (Positive range <>) of Interpretation_Set;

   function Create_Interpretation_Set
     (Self  : in out Context'Class) return Interpretation_Set;

   procedure Add_Symbol
     (Self   : in out Interpretation_Set'Class;
      Symbol : Program.Symbols.Symbol);
   --  Extend Self with symbol interpretation

   procedure Add_Defining_Name
     (Self      : in out Interpretation_Set'Class;
      Name_View : Program.Visibility.View;
      Down      : Solution_Array := Empty_Solution_Array);

   type Apply_Kind is
     (Unknown,
      Function_Call,
      Type_Convertion,
      Indexed_Component);

   procedure Add_Expression
     (Self  : in out Interpretation_Set'Class;
      Tipe  : Program.Visibility.View;
--      Apply : Apply_Kind := Unknown;  ???
      Down  : Solution_Array := Empty_Solution_Array);

private

   type Interpretation_Kind is (Symbol, Name, Expression);

   type Interpretation (Kind : Interpretation_Kind := Symbol) is record
      case Kind is
         when Symbol =>
            Symbol : Program.Symbols.Symbol;
         when Name =>
            Name_View : Program.Visibility.View;
         when Expression =>
            Type_View : Program.Visibility.View;
      end case;
   end record;

   package Interpretation_Vectors is new Ada.Containers.Vectors
     (Positive, Interpretation);

   type Context (Env : not null Program.Visibility.Context_Access) is
     tagged limited
   record
      Data : Interpretation_Vectors.Vector;
   end record;

   type Interpretation_Set is tagged record
      Context : Context_Access;
      From    : Positive;
      To      : Natural;
   end record;

end Program.Interpretations;
