--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Compilation_Unit_Vectors;
with Program.Library_Items;
limited with Program.Library_Unit_Declarations;

package Program.Library_Unit_Bodies is
   pragma Pure;

   type Library_Unit_Body is limited interface
     and Program.Library_Items.Library_Item;
   --  A library_unit_body is a compilation unit that is the subprogram or
   --  package body.
   --
   --  A unit interpreted only as the completion of a subprogram, or a unit
   --  interpreted as both the declaration and body of a library subprogram.
   --  Reference Manual 10.1.4(4)

   type Library_Unit_Body_Access is access all Library_Unit_Body'Class
     with Storage_Size => 0;

   not overriding function Corresponding_Declaration
     (Self : access Library_Unit_Body)
       return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access
         is abstract;
   --  Returns the corresponding library_unit_declaration, if any, for the
   --  library_unit_body. The corresponding library unit is the unit upon
   --  which the library_unit_body depends semantically.
   --
   --  Returns null for library_unit_body arguments that do not have a
   --  corresponding library unit contained in the Context.

   not overriding function Subunits (Self : Library_Unit_Body)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access
       is abstract
     with Post'Class =>
       (Subunits'Result.Is_Empty
        or else (for all X in Subunits'Result.Each_Unit => X.Unit.Is_Subunit));
   --  Returns a complete list of subunit values, with one value for each body
   --  stub that appears in the given Library_Unit_Body. Returns an empty list
   --  if the parent unit does not contain any body stubs.

end Program.Library_Unit_Bodies;
