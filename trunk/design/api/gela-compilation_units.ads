--  This package provides Compilation_Unit interfaces and their methods.

limited with Gela.Compilation_Unit_Sets;
--  limited with Gela.Compilations;
limited with Gela.Contexts;
--  limited with Gela.Unit_Containers;
--  with Gela.Lexical_Types;
--  with Gela.Elements.Compilation_Units;
with Gela.Symbols;
limited with Gela.Compilation_Units.Visiters;

package Gela.Compilation_Units is
   pragma Preelaborate;

   --  Compilation Unit hierarchy is:
   --  Compilation_Unit (abstract)
   --    Library_Unit_Declaration
   --    Library_Unit_Body
   --    Subunit

   type Compilation_Unit is limited interface;
   --  Compilation unit of some context
   type Compilation_Unit_Access is access all Compilation_Unit'Class;
   for Compilation_Unit_Access'Storage_Size use 0;

   function Assigned (Self : access Compilation_Unit'Class) return Boolean
     is (Self /= null);

   not overriding function Name
     (Self : access Compilation_Unit)
      return Gela.Symbols.Symbol_Access is abstract;
   --  Return name of compilation unit

   not overriding function Context
     (Self : access Compilation_Unit)
      return Gela.Contexts.Context_Access is abstract;
   --  Return corresponding context

   not overriding function Parent
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Compilation_Unit_Access is abstract;
   --  For Standard package return null.
   --  For library unit return parent package declaration.
   --  For subunit return parent body/subunit

   not overriding function Is_Library_Unit_Declaration
     (Self : access Compilation_Unit) return Boolean is abstract;

   not overriding function Is_Library_Unit_Body
     (Self : access Compilation_Unit) return Boolean is abstract;

   not overriding function Is_Subunit
     (Self : access Compilation_Unit) return Boolean is abstract;

   not overriding procedure Visit
     (Self    : access Compilation_Unit;
      Visiter : in out Gela.Compilation_Units.Visiters.Visiter'Class)
   is abstract;

--     not overriding function Container
--       (Self : access Compilation_Unit)
--        return Gela.Unit_Containers.Unit_Container_Access is abstract;
--     --  Return container of compilation unit if any
--
--     not overriding function Compilation
--       (Self : access Compilation_Unit)
--        return Gela.Compilations.Compilation_Access is abstract;
--     --  Return compilation of compilation unit
--
--     not overriding function Tree
--       (Self : access Compilation_Unit)
--        return Gela.Elements.Compilation_Units.Compilation_Unit_Access
--          is abstract;
--     --  Return abstract syntax tree of compilation unit

   type Library_Unit_Declaration is limited interface and Compilation_Unit;
   --  Compilation library unit declaration. This includes rename declarations

   type Library_Unit_Declaration_Access is
     access all Library_Unit_Declaration'Class;
   for Library_Unit_Declaration_Access'Storage_Size use 0;

   not overriding function Children
     (Self : access Library_Unit_Declaration)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;
   --  Return library unit declarations for children and subprogram bodies
   --  that interpreted as both the declaration and the body of a library
   --  subprogram.

   type Library_Unit_Body is limited interface and Compilation_Unit;
   --  Compilation unit body

   type Library_Unit_Body_Access is access all Library_Unit_Body'Class;
   --  Compilation library unit body
   for Library_Unit_Body_Access'Storage_Size use 0;

   not overriding function Subunits
     (Self : access Library_Unit_Body)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;
   --  Return set of subunits of given body

   not overriding function Corresponding_Declaration
     (Self : access Library_Unit_Body)
      return Gela.Compilation_Units.Library_Unit_Declaration_Access
       is abstract;
   --  Return declaration for given body if any

   not overriding function Corresponding_Body
     (Self : access Library_Unit_Declaration)
      return Gela.Compilation_Units.Library_Unit_Body_Access
       is abstract;
   --  Return body of library declaration if any

   type Subunit is limited interface and Compilation_Unit;
   type Subunit_Access is access all Subunit'Class;
   for Subunit_Access'Storage_Size use 0;

   not overriding function Subunits
     (Self : access Subunit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
      is abstract;
   --  Return set of subunits of given subunit

end Gela.Compilation_Units;
