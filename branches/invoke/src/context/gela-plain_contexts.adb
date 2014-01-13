
package body Gela.Plain_Contexts is

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   overriding function Compilation_Unit_Bodies
     (Self  : access Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Compilation_Unit_Bodies;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize (Self : in out Context) is
   begin
      Self.Symbols.Initialize;
   end Initialize;

   -----------
   -- Lexer --
   -----------

   overriding function Lexer
     (Self  : access Context) return Gela.Lexers.Lexer_Access is
   begin
      return Self.Lexer'Access;
   end Lexer;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   overriding function Library_Unit_Declarations
     (Self  : access Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Library_Unit_Declarations;

   -------------------
   -- Naming_Schema --
   -------------------

   overriding function Naming_Schema
     (Self  : access Context) return Gela.Naming_Schemas.Naming_Schema_Access
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Naming_Schema;

   -------------------
   -- Source_Finder --
   -------------------

   overriding function Source_Finder
     (Self  : access Context) return Gela.Source_Finders.Source_Finder_Access
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Source_Finder;

   -------------
   -- Symbols --
   -------------

   overriding function Symbols
     (Self : access Context) return Gela.Symbol_Sets.Symbol_Set_Access is
   begin
      return Self.Symbols'Access;
   end Symbols;

   ---------------------
   -- Unit_Containers --
   ---------------------

   overriding function Unit_Containers
     (Self  : access Context) return Gela.Unit_Containers.Unit_Container_List
   is
      pragma Unreferenced (Self);
   begin
      return (1 .. 0 => <>);
   end Unit_Containers;

end Gela.Plain_Contexts;
