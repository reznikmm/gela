--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Compilation_Unit_Vectors;

package Program.Subunits is
   pragma Pure;

   type Subunit is limited interface
     and Program.Compilation_Units.Compilation_Unit;
   --  Subunits are like child units, with these (important) differences:
   --  subunits support the separate compilation of bodies only (not
   --  declarations); the parent contains a body_stub to indicate the existence
   --  and place of each of its subunits; declarations appearing in the
   --  parent's body can be visible within the subunits

   type Subunit_Access is access all Subunit'Class
     with Storage_Size => 0;

   not overriding function Subunits (Self : access Subunit)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access
       is abstract;
--   with Post'Class =>
--     (Subunits'Result.Is_Empty
--     or else (for all X in Subunits'Result.Each_Unit => X.Unit.Is_Subunit));
   --  Returns a complete list of subunit values, with one value for each body
   --  stub that appears in the given Subunit. Returns an empty list if the
   --  parent unit does not contain any body stubs.

   not overriding function Parent_Body (Self : access Subunit)
     return not null Program.Compilation_Units.Compilation_Unit_Access
       is abstract
     with Post'Class =>
       (Parent_Body'Result.Is_Subunit
          or Parent_Body'Result.Is_Library_Unit_Body);
   --  Returns the Compilation_Unit containing the body stub of the given
   --  Subunit.

end Program.Subunits;
