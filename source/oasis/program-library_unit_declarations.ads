--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Compilation_Unit_Vectors;
with Program.Library_Items;

package Program.Library_Unit_Declarations is
   pragma Pure;

   type Library_Unit_Declaration is limited interface
     and Program.Library_Items.Library_Item;
   --  library_unit_declaration is a compilation unit that is the declaration
   --  or renaming of a library unit.

   type Library_Unit_Declaration_Access is
     access all Library_Unit_Declaration'Class
       with Storage_Size => 0;

   not overriding function Corresponding_Body (Self : Library_Unit_Declaration)
     return Program.Compilation_Units.Compilation_Unit_Access
       is abstract;
   --  Returns the corresponding library_unit_body, if any, for the
   --  library_unit_declaration. The corresponding library_unit_body is the
   --  unit that depends semantically on the library_unit_declaration.
   --
   --  Returns null for library_unit_declaration arguments that
   --  do not have a corresponding library_unit_body contained in the Context.

   not overriding function Corresponding_Childern
     (Self : Library_Unit_Declaration)
       return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access
         is abstract
     with Post'Class =>
       (Corresponding_Childern'Result.Is_Empty
        or else (for all X in Corresponding_Childern'Result.Each_Unit
                   => X.Unit.Is_Library_Item));
   --  Returns a list of the child units for the given parent library unit.
   --
   --  Both the declaration and body (if any) of each child unit are returned.
   --  Descendants beyond immediate children (i.e., children of children) are
   --  not returned by this query.

end Program.Library_Unit_Declarations;
