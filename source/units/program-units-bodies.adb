--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Units.Declarations;

package body Program.Units.Bodies is

   --------------------
   -- Append_Subunit --
   --------------------

   procedure Append_Subunit
     (Self  : in out Unit_Body;
      Value : Program.Compilation_Units.Compilation_Unit_Access)
   is
   begin
      Self.Subunits.Append (Value);
   end Append_Subunit;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   overriding function Corresponding_Declaration (Self : access Unit_Body)
    return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access is
   begin
      return Self.Declaration;
   end Corresponding_Declaration;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Unit_Body;
      Compilation      : Program.Compilations.Compilation_Access;
      Full_Name        : Text;
      Context_Clause   : Program.Element_Vectors.Element_Vector_Access;
      Unit_Declaration : not null Program.Elements.Element_Access;
      Parent           : Program.Library_Unit_Declarations
                           .Library_Unit_Declaration_Access;
      Declaration      : Program.Library_Unit_Declarations
                           .Library_Unit_Declaration_Access)
   is
   begin
      Self.Initialize
        (Compilation      => Compilation,
         Full_Name        => Full_Name,
         Context_Clause   => Context_Clause,
         Unit_Declaration => Unit_Declaration);

      Self.Parent := Parent;

      if Parent not in null then
         Program.Units.Declarations.Unit_Declaration (Parent.all)
           .Append_Child (Self'Unchecked_Access);
      end if;

      if Declaration not in null then
         Program.Units.Declarations.Unit_Declaration (Declaration.all)
           .Set_Body (Self'Unchecked_Access);
      end if;

      Self.Declaration := Declaration;

      Self.Subunits.Clear;
   end Initialize;

   ------------
   -- Parent --
   ------------

   overriding function Parent (Self : access Unit_Body)
     return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access
   is
   begin
      return Self.Parent;
   end Parent;

   --------------
   -- Subunits --
   --------------

   overriding function Subunits (Self : access Unit_Body)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      if Self.Subunits.Is_Empty then
         return null;
      else
         return Self.Subunits'Access;
      end if;
   end Subunits;

end Program.Units.Bodies;
