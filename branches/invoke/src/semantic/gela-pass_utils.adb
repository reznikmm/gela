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
with Gela.Compilation_Unit_Sets;
with Gela.Compilation_Units;
with Gela.Dependency_Lists;
with Gela.Element_Visiters;
with Gela.Elements.Compilation_Units;
with Gela.Elements.Generic_Package_Declarations;
with Gela.Elements.Package_Declarations;
with Gela.Plain_Type_Managers;
with Gela.Symbol_Sets;

package body Gela.Pass_Utils is

   procedure Preprocess_Standard
     (Comp          : Gela.Compilations.Compilation_Access;
      Unit          : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access);

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

   ---------------
   -- Add_Names --
   ---------------

   function Add_Names
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      List         : Gela.Lexical_Types.Symbol_List;
      Names        : Gela.Elements.Defining_Identifiers
                       .Defining_Identifier_Sequence_Access)
      return Gela.Semantic_Types.Env_Index
   is
      Head   : Gela.Lexical_Types.Symbol_List := List;
      Env_1  : Gela.Semantic_Types.Env_Index := Env;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Identifiers.Defining_Identifier_Access;
      Cursor : Gela.Elements.Defining_Identifiers
        .Defining_Identifier_Sequence_Cursor := Names.First;
      Set    : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Comp.Context.Symbols;
   begin
      while Cursor.Has_Element loop
         Name := Cursor.Element;
         Symbol := Set.Tail (Head);
         Head := Set.Head (Head);
         Cursor.Next;

         Env_1 := Comp.Context.Environment_Set.Add_Defining_Name
           (Index  => Env_1,
            Symbol => Symbol,
            Name   => Gela.Elements.Defining_Names
                        .Defining_Name_Access (Name));
      end loop;

      return Env_1;
   end Add_Names;

   -----------------------------
   -- Add_Names_Create_Region --
   -----------------------------

   function Add_Names_Create_Region
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      List         : Gela.Lexical_Types.Symbol_List;
      Names        : Gela.Elements.Defining_Identifiers
      .Defining_Identifier_Sequence_Access)
      return Gela.Semantic_Types.Env_Index
   is
      Env_1  : Gela.Semantic_Types.Env_Index;
      Env_2  : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Elements.Defining_Identifiers.Defining_Identifier_Access;
      Cursor : constant Gela.Elements.Defining_Identifiers
                          .Defining_Identifier_Sequence_Cursor := Names.First;
   begin
      Name := Cursor.Element;
      Env_1 := Add_Names (Comp, Env, List, Names);

      Env_2 := Comp.Context.Environment_Set.Enter_Declarative_Region
        (Index  => Env_1,
         Region => Gela.Elements.Defining_Names.Defining_Name_Access (Name));

      return Env_2;
   end Add_Names_Create_Region;

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

      use type Gela.Lexical_Types.Symbol;

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

      if Full_Name = Gela.Lexical_Types.Predefined_Symbols.Standard then
         Preprocess_Standard (Comp, Unit);
      end if;

      Manager.Read_Dependency (Deps);

      return 0;
   end Create_Unit_Declaration;

   --------------------------------
   -- Parents_Declarative_Region --
   --------------------------------

   function Parents_Declarative_Region
     (Comp          : Gela.Compilations.Compilation_Access;
      Private_Index : Gela.Lexical_Types.Token_Count;
      Full_Name     : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Private_Index);
      use type Gela.Lexical_Types.Symbol;

      package Get is

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Env_Index := 0;
         end record;

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations
                              .Package_Declaration_Access);

         overriding procedure Generic_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Declarations
            .Generic_Package_Declaration_Access);

      end Get;

      package body Get is

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations
                              .Package_Declaration_Access) is
         begin
            Self.Result := Node.Declarative_Region;
         end Package_Declaration;

         overriding procedure Generic_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Declarations
                              .Generic_Package_Declaration_Access) is
         begin
            Self.Result := Node.Declarative_Region;
         end Generic_Package_Declaration;

      end Get;

      Set    : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Comp.Context.Symbols;
      Parent : constant Gela.Lexical_Types.Symbol := Set.Parent (Full_Name);
      Unit_Set : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
      Unit   : Gela.Compilation_Units.Compilation_Unit_Access;
      Tree   : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
      V      : Get.Visiter;
   begin
      if Parent = Gela.Lexical_Types.No_Symbol then
         return 0;
      end if;

      Unit_Set := Comp.Context.Library_Unit_Declarations;
      Unit := Unit_Set.Find (Parent);
      Tree := Unit.Tree;
      Tree.Visit (V);

      return V.Result;
   end Parents_Declarative_Region;

   -------------------------
   -- Preprocess_Standard --
   -------------------------

   procedure Preprocess_Standard
     (Comp          : Gela.Compilations.Compilation_Access;
      Unit          : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access) is
   begin
      Gela.Plain_Type_Managers.Type_Manager_Access
        (Comp.Context.Types).Initialize (Unit);
   end Preprocess_Standard;

end Gela.Pass_Utils;
