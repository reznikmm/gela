with Gela.Lexical_Handler;
with Gela.Path_Source_Finders;
with Gela.Plain_Dependency_Lists;
with Gela.Plain_Compilation_Managers;

package body Gela.Plain_Contexts is

   -------------------------
   -- Compilation_Manager --
   -------------------------

   overriding function Compilation_Manager
     (Self  : access Context)
      return Gela.Compilation_Managers.Compilation_Manager_Access is
   begin
      return Self.Manager;
   end Compilation_Manager;

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

   ----------------------
   -- Create_Body_Unit --
   ----------------------

   overriding function Create_Body_Unit
     (Self        : in out Context;
      Declaration : Gela.Compilation_Units.Library_Unit_Declaration_Access;
      Name        : Gela.Lexical_Types.Symbol;
      Node        : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
      return Gela.Compilation_Units.Body_Unit_Access
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Declaration);
      pragma Unreferenced (Name);
      pragma Unreferenced (Node);
   begin
      return null;
   end Create_Body_Unit;

   ------------------------------------------
   -- Create_Body_Unit_Without_Declaration --
   ------------------------------------------

   overriding function Create_Body_Unit_Without_Declaration
     (Self   : in out Context;
      Parent : Gela.Compilation_Units.Package_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
      return Gela.Compilation_Units.Body_Unit_Access
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Parent);
      pragma Unreferenced (Name);
      pragma Unreferenced (Node);
   begin
      return null;
   end Create_Body_Unit_Without_Declaration;

   -------------------------------------
   -- Create_Library_Unit_Declaration --
   -------------------------------------

   overriding function Create_Library_Unit_Declaration
     (Self   : in out Context;
      Parent : Gela.Compilation_Units.Package_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access)
      return Gela.Compilation_Units.Library_Unit_Declaration_Access
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Parent);
      pragma Unreferenced (Name);
      pragma Unreferenced (Node);
   begin
      return null;
   end Create_Library_Unit_Declaration;

   --------------------
   -- Create_Subunit --
   --------------------

   overriding function Create_Subunit
     (Self   : in out Context;
      Parent : Gela.Compilation_Units.Body_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node        : Gela.Elements.Subunits.Subunit_Access)
      return Gela.Compilation_Units.Subunit_Access
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Parent);
      pragma Unreferenced (Name);
      pragma Unreferenced (Node);
   begin
      return null;
   end Create_Subunit;

   --------------------
   -- Create_Subunit --
   --------------------

   overriding function Create_Subunit
     (Self   : in out Context;
      Parent : Gela.Compilation_Units.Subunit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Subunits.Subunit_Access)
      return Gela.Compilation_Units.Subunit_Access
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Parent);
      pragma Unreferenced (Name);
      pragma Unreferenced (Node);
   begin
      return null;
   end Create_Subunit;

   ---------------------
   -- Dependency_List --
   ---------------------

   overriding function Dependency_List
     (Self  : access Context)
      return Gela.Dependency_Lists.Dependency_List_Access is
   begin
      return Self.Dependency_List;
   end Dependency_List;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self : in out Context;
      Path : League.Strings.Universal_String;
      Comp : League.Strings.Universal_String)
   is
      Deps    : constant Gela.Plain_Dependency_Lists.Dependency_List_Access :=
        new Gela.Plain_Dependency_Lists.Dependency_List
          (Self'Unchecked_Access);

      Manager : constant Gela.Plain_Compilation_Managers.
        Compilation_Manager_Access :=
          new Gela.Plain_Compilation_Managers.Compilation_Manager
            (Self'Unchecked_Access, Self'Unchecked_Access);
   begin
      Gela.Lexical_Handler.Initialize;
      Self.Symbols.Initialize;
      Self.Finder := Gela.Path_Source_Finders.Create
        (Path    => Path,
         Context => Self'Unchecked_Access);
      Self.Dependency_List :=
        Gela.Dependency_Lists.Dependency_List_Access (Deps);
      Self.Manager :=
        Gela.Compilation_Managers.Compilation_Manager_Access (Manager);

      if not Comp.Is_Empty then
         Manager.Read_Compilation (Comp);
      end if;
   end Initialize;

   -----------
   -- Lexer --
   -----------

   overriding function Lexer
     (Self  : access Context) return Gela.Lexers.Lexer_Access is
   begin
      return Self.Lexer'Unchecked_Access;
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
     (Self  : access Context)
      return Gela.Naming_Schemas.Naming_Schema_Access is
   begin
      return Self.Schema'Unchecked_Access;
   end Naming_Schema;

   -------------------
   -- Source_Finder --
   -------------------

   overriding function Source_Finder
     (Self  : access Context) return Gela.Source_Finders.Source_Finder_Access
   is
   begin
      return Self.Finder;
   end Source_Finder;

   -------------
   -- Symbols --
   -------------

   overriding function Symbols
     (Self : access Context) return Gela.Symbol_Sets.Symbol_Set_Access is
   begin
      return Self.Symbols'Unchecked_Access;
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
