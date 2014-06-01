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

   ----------------------------
   -- Add_Name_Create_Region --
   ----------------------------

   function Add_Name_Create_Region
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      Symbol       : Gela.Lexical_Types.Symbol;
      Name         : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index
   is
      Env_1 : Gela.Semantic_Types.Env_Index;
      Env_2 : Gela.Semantic_Types.Env_Index;
   begin
      Env_1 := Comp.Context.Environment_Set.Add_Defining_Name
        (Index  => Env,
         Symbol => Symbol,
         Name   => Name);
      Env_2 := Comp.Context.Environment_Set.Enter_Declarative_Region
        (Index  => Env_1,
         Region => Name);

      return Env_2;
   end Add_Name_Create_Region;

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

   ------------------------------------
   -- Numeric_Literal_Interpretation --
   ------------------------------------

   procedure Numeric_Literal_Interpretation
     (Comp   : Gela.Compilations.Compilation_Access;
      Token  : Gela.Lexical_Types.Token_Count;
      Result : out Gela.Interpretations.Interpretation_Set_Index)
   is
      Value : constant Gela.Lexical_Types.Token := Comp.Get_Token (Token);
      Tipe  : Gela.Semantic_Types.Type_Index;
   begin
      if Comp.Source.Index (Value.First, Value.Last, '.') = 0 then
         Tipe := Comp.Context.Types.Universal_Integer;
      else
         Tipe := Comp.Context.Types.Universal_Real;
      end if;

      Comp.Context.Interpretation_Manager.Expression (Tipe, Result);
   end Numeric_Literal_Interpretation;

   ---------------------
   -- Resolve_To_Type --
   ---------------------

   procedure Resolve_To_Type
     (Self    : Gela.Interpretations.Interpretation_Manager_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is
      Type_Index : Gela.Semantic_Types.Type_Index;
   begin
      Self.Get_Subtype
        (Env    => Env,
         Set    => Type_Up,
         Result => Type_Index);

      Self.Resolve_To_Type
        (Env    => Env,
         Set    => Expr_Up,
         Value  => Type_Index,
         Result => Result);
   end Resolve_To_Type;
   ----------------------
   -- Shall_Be_Subtype --
   ----------------------

   procedure Shall_Be_Subtype
     (Self   : Gela.Interpretations.Interpretation_Manager_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index) is
   begin
      Self.Chosen_Interpretation
        (Env    => Env,
         Set    => Set,
         Result => Result);
   end Shall_Be_Subtype;

end Gela.Pass_Utils;
