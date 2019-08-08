--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Contexts;

package Program.Compilations is
   pragma Pure;

   --  A specific Compilation value is valid (usable) for as long as the
   --  Context variable, used to create it, remains open. Once an Context is
   --  closed, all associated Compilation values become invalid. It is
   --  erroneous to use an invalid Compilation value.

   type Compilation is limited interface;
   --  The Ada Compilation abstraction:
   --
   --  The text of a program is submitted to the compiler in one or more
   --  compilations.  Each compilation is a succession of compilation units.

   type Compilation_Access is access all Compilation'Class
     with Storage_Size => 0;

   function Is_Assigned (Self : access Compilation'Class) return Boolean
     is (Self /= null);

   not overriding function Context (Self : Compilation)
     return not null Program.Contexts.Context_Access is abstract;
   --  Return corresponding context

   not overriding function Text_Name
     (Self : Compilation) return Text is abstract;
   --  Returns the name of the text, or other structure, that was the source of
   --  the compilation that resulted in this Compilation. Returns a null string
   --  if the text name is not available for any reason.

   not overriding function Object_Name
     (Self : Compilation) return Text is abstract;
   --  Returns the name of the object, or other structure, that contains the
   --  binary result of the compilation for this Compilation. Returns a null
   --  string if the object name is not available for any reason.

   not overriding function Line_Count
     (Self : Compilation) return Natural is abstract;

   not overriding function Line
     (Self : Compilation) return Text is abstract;

   not overriding function Lexical_Element_Count
     (Self : Compilation) return Natural is abstract;

   not overriding function Lexical_Element (Self : Compilation)
     return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   --  TODO: Compilation_Pragmas?
end Program.Compilations;
