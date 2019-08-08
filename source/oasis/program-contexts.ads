--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilation_Unit_Vectors;

package Program.Contexts is
   pragma Pure;

   type Context is limited interface;
   --  The Context is a view of a particular implementation of an Ada
   --  environment. We require an application to identify that view of the
   --  Ada environment. An Context identifies an Ada environment as defined
   --  by ISO/IEC 8652:1995. The Ada environment is well defined for Ada
   --  implementations. ISO/IEC 8652:1995 provides for an implementation-
   --  defined method to enter compilation units into the Ada environment.
   --
   --  Defined by the implementation, an context is a way to identify a set of
   --  Compilation Units to be processed by an application. This may include
   --  things such as the pathname, search rules, etc., which are attributes
   --  of the Ada environment and consequently becomes part of the Context
   --  only because it is a "view" of the Ada environment.

   type Context_Access is access all Context'Class
     with Storage_Size => 0;

   not overriding function Library_Unit_Declarations (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access
       is abstract
     with Post'Class =>
       (Library_Unit_Declarations'Result.Is_Empty
        or else (for all X in Library_Unit_Declarations'Result.Each_Unit
                   => X.Unit.Is_Library_Unit_Declaration));
--  Returns a list of all library_unit_declaration and
--  library_unit_renaming_declaration elements contained in Context.
--  Individual units will appear only once in an order that is not defined.

   not overriding function Compilation_Unit_Bodies (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access
       is abstract
     with Post'Class =>
       (Compilation_Unit_Bodies'Result.Is_Empty
        or else (for all X in Compilation_Unit_Bodies'Result.Each_Unit
                   => X.Unit.Is_Library_Unit_Body or X.Unit.Is_Subunit));
--  Returns a list of all library_unit_body and subunit elements contained in
--  Context. Individual units will appear only once in an order that is not
--  defined.

end Program.Contexts;
