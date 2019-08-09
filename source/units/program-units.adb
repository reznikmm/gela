--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Units is

   -----------------
   -- Compilation --
   -----------------

   overriding function Compilation
     (Self : access Unit)
      return Program.Compilations.Compilation_Access is
   begin
      return Self.Compilation;
   end Compilation;

   -----------------------------
   -- Context_Clause_Elements --
   -----------------------------

   overriding function Context_Clause_Elements
     (Self : access Unit)
      return Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Context_Clause;
   end Context_Clause_Elements;

   ---------------
   -- Full_Name --
   ---------------

   overriding function Full_Name (Self : access Unit) return Text is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
        (Self.Full_Name);
   end Full_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Unit'Class;
      Compilation      : Program.Compilations.Compilation_Access;
      Full_Name        : Text;
      Context_Clause   : Program.Element_Vectors.Element_Vector_Access;
      Unit_Declaration : not null Program.Elements.Element_Access) is
   begin
      Self.Compilation := Compilation;
      Self.Full_Name :=
        Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String
          (Full_Name);
      Self.Context_Clause := Context_Clause;
      Self.Unit_Declaration := Unit_Declaration;
   end Initialize;

   ----------------------
   -- Unit_Declaration --
   ----------------------

   overriding function Unit_Declaration
     (Self : access Unit)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Unit_Declaration;
   end Unit_Declaration;

   -------------------------------
   -- Is_Library_Unit_Body_Unit --
   -------------------------------

   overriding function Is_Library_Unit_Body_Unit
     (Self : Unit) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Library_Unit_Body_Unit;

   --------------------------
   -- Is_Library_Item_Unit --
   --------------------------

   overriding function Is_Library_Item_Unit (Self : Unit) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Library_Item_Unit;

   --------------------------------------
   -- Is_Library_Unit_Declaration_Unit --
   --------------------------------------

   overriding function Is_Library_Unit_Declaration_Unit
     (Self : Unit) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Library_Unit_Declaration_Unit;

   ---------------------
   -- Is_Subunit_Unit --
   ---------------------

   overriding function Is_Subunit_Unit (Self : Unit) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Subunit_Unit;

end Program.Units;
