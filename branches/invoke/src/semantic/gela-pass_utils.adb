------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Contexts;
with Gela.Compilation_Managers;
with Gela.Dependency_Lists;

package body Gela.Pass_Utils is

   --------------------
   -- Create_Subunit --
   --------------------

   function Create_Subunit
     (Comp         : Gela.Compilations.Compilation_Access;
      Unit         : Gela.Elements.Subunits.Subunit_Access;
      Parent_Name  : Gela.Lexical_Types.Symbol;
      Full_Name    : Gela.Lexical_Types.Symbol;
      Unit_Kind    : Gela.Semantic_Types.Unit_Kinds;
      With_List    : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Unit_Kind);
      Context : constant Gela.Contexts.Context_Access := Comp.Context;
      Manager : constant Gela.Compilation_Managers.Compilation_Manager_Access
        := Context.Compilation_Manager;
      Deps    : constant Gela.Dependency_Lists.Dependency_List_Access :=
        Context.Dependency_List;
   begin
      Deps.Add_Subunit
        (Parent       => Parent_Name,
         Name         => Full_Name,
         Withed       => With_List,
         Limited_With => Limited_With,
         Unit         => Unit);

      Manager.Read_Dependency (Deps);

      return 0;
   end Create_Subunit;

   ----------------------
   -- Create_Unit_Body --
   ----------------------

   function Create_Unit_Body
     (Comp         : Gela.Compilations.Compilation_Access;
      Unit         : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access;
      Full_Name    : Gela.Lexical_Types.Symbol;
      Unit_Kind    : Gela.Semantic_Types.Unit_Kinds;
      With_List    : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Unit_Kind);
      Context : constant Gela.Contexts.Context_Access := Comp.Context;
      Manager : constant Gela.Compilation_Managers.Compilation_Manager_Access
        := Context.Compilation_Manager;
      Deps    : constant Gela.Dependency_Lists.Dependency_List_Access :=
        Context.Dependency_List;
   begin
      Deps.Add_Body_Unit
        (Name         => Full_Name,
         Withed       => With_List,
         Limited_With => Limited_With,
         Unit         => Unit);

      Manager.Read_Dependency (Deps);

      return 0;
   end Create_Unit_Body;

   -----------------------------
   -- Create_Unit_Declaration --
   -----------------------------

   function Create_Unit_Declaration
     (Comp          : Gela.Compilations.Compilation_Access;
      Unit          : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Private_Index : Gela.Lexical_Types.Token_Count;
      Full_Name     : Gela.Lexical_Types.Symbol;
      Unit_Kind     : Gela.Semantic_Types.Unit_Kinds;
      With_List     : Gela.Lexical_Types.Symbol_List;
      Limited_With  : Gela.Lexical_Types.Symbol_List)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Private_Index);
      pragma Unreferenced (Unit_Kind);
      Context : constant Gela.Contexts.Context_Access := Comp.Context;
      Manager : constant Gela.Compilation_Managers.Compilation_Manager_Access
        := Context.Compilation_Manager;
      Deps    : constant Gela.Dependency_Lists.Dependency_List_Access :=
        Context.Dependency_List;
   begin
      Deps.Add_Library_Unit_Declaration
        (Name         => Full_Name,
         Withed       => With_List,
         Limited_With => Limited_With,
         Unit         => Unit);

      Manager.Read_Dependency (Deps);

      return 0;
   end Create_Unit_Declaration;

end Gela.Pass_Utils;
