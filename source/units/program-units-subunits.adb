--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Units.Bodies;

package body Program.Units.Subunits is

   --------------------
   -- Append_Subunit --
   --------------------

   procedure Append_Subunit
     (Self  : in out Subunit;
      Value : Program.Compilation_Units.Compilation_Unit_Access)
   is
   begin
      Self.Subunits.Append (Value);
   end Append_Subunit;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Subunit;
      Compilation      : Program.Compilations.Compilation_Access;
      Full_Name        : Text;
      Context_Clause   : Program.Element_Vectors.Element_Vector_Access;
      Unit_Declaration : not null Program.Elements.Element_Access;
      Parent_Body      : not null Program.Compilation_Units
                                    .Compilation_Unit_Access) is
   begin
      Self.Initialize
        (Compilation      => Compilation,
         Full_Name        => Full_Name,
         Context_Clause   => Context_Clause,
         Unit_Declaration => Unit_Declaration);

      Self.Parent_Body := Parent_Body;

      if Parent_Body.all in Program.Units.Bodies.Unit_Body then
         Program.Units.Bodies.Unit_Body (Parent_Body.all).Append_Subunit
           (Self'Unchecked_Access);
      else
         Subunit (Parent_Body.all).Append_Subunit (Self'Unchecked_Access);
      end if;

      Self.Subunits.Clear;
   end Initialize;

   --------------
   -- Subunits --
   --------------

   overriding function Subunits (Self : access Subunit)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      if Self.Subunits.Is_Empty then
         return null;
      else
         return Self.Subunits'Access;
      end if;
   end Subunits;

   -----------------
   -- Parent_Body --
   -----------------

   overriding function Parent_Body (Self : access Subunit)
     return not null Program.Compilation_Units.Compilation_Unit_Access is
   begin
      return Self.Parent_Body;
   end Parent_Body;

end Program.Units.Subunits;
