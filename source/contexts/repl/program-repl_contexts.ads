--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
--
--  REPL is abbreviation for "Read, Execute, Print Loop", a kind of an
--  interactive programming environment.
--
--  This package provides a support for building REPL.

with Program.Contexts;
with Program.Compilation_Unit_Vectors;

package Program.REPL_Contexts is

   type REPL_Context is new Program.Contexts.Context with private;
   --  Context for REPL environment.
   --
   --  Context contains user input as sequence of cells. Each cell is
   --  a declaration, a statement or an expression. Sequence of cells is
   --  converted to corresponding Ada code. This code is wrapped into a main
   --  subprogram - a library level procedure. Compilation unit for main
   --  and any its dependency are available through OASIS Context API.

   procedure Initialize
     (Self      : in out REPL_Context'Class;
      Main_Name : Program.Text := "Main");
   --  Initialize the context. Use Main_Name for the top level procedure.
   --  This procedure will contain a code corresponding to the input.

   function Cell_Count (Self : REPL_Context'Class) return Natural;
   --  Total number of cells in the context.

   function Is_Valid_Cell
     (Self  : REPL_Context'Class;
      Index : Positive) return Boolean;
   --  Return status of the cell with given index. Invalid cells have no
   --  corresponding Ada code.

   procedure Append_Cell
     (Self : in out REPL_Context'Class;
      Text : Program.Text);
   --  Create new cell and append it to cell list as last element.

   procedure Update_Cell
     (Self  : in out REPL_Context'Class;
      Index : Positive;
      Text  : Program.Text);
   --  Replace text of the cell with given index.

   procedure Delete_Cell
     (Self  : in out REPL_Context'Class;
      Index : Positive);
   --  Drop the cell with given index from cell list.

private

   type REPL_Context is new Program.Contexts.Context with null record;

   overriding function Library_Unit_Declarations (Self : REPL_Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;

   overriding function Compilation_Unit_Bodies (Self : REPL_Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;

end Program.REPL_Contexts;
