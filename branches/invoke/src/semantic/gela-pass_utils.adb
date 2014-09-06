------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Element_Visiters;
with Gela.Elements.Enumeration_Type_Definitions;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Type_Definitions;
with Gela.Plain_Type_Managers;
with Gela.Symbol_Sets;

package body Gela.Pass_Utils is

   procedure Preprocess_Standard
     (Comp          : Gela.Compilations.Compilation_Access;
      Unit          : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access);

   function Is_Enumeration
     (Decl : Gela.Elements.Element_Access) return Boolean;

   ----------------------------
   -- Add_Name_Create_Region --
   ----------------------------

   function Add_Name_Create_Region
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Decl   : Gela.Elements.Element_Access)
        return Gela.Semantic_Types.Env_Index
   is
      Env_1 : Gela.Semantic_Types.Env_Index;
      Env_2 : Gela.Semantic_Types.Env_Index;
   begin
      Env_1 := Comp.Context.Environment_Set.Add_Defining_Name
        (Index  => Env,
         Symbol => Symbol,
         Name   => Name);

      if Is_Enumeration (Decl) then
         return Env_1;
      end if;

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
     (Comp         : Gela.Compilations.Compilation_Access)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Comp);
   begin
      return 0;
   end Create_Subunit;

   ----------------------
   -- Create_Unit_Body --
   ----------------------

   function Create_Unit_Body
     (Comp         : Gela.Compilations.Compilation_Access)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Comp);
   begin
      return 0;
   end Create_Unit_Body;

   -----------------------------
   -- Create_Unit_Declaration --
   -----------------------------

   function Create_Unit_Declaration
     (Comp          : Gela.Compilations.Compilation_Access;
      Unit          : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Full_Name     : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index
   is
      use type Gela.Lexical_Types.Symbol;
   begin
      if Full_Name = Gela.Lexical_Types.Predefined_Symbols.Standard then
         Preprocess_Standard (Comp, Unit);
      end if;

      return 0;
   end Create_Unit_Declaration;

   --------------------
   -- Is_Enumeration --
   --------------------

   function Is_Enumeration
     (Decl : Gela.Elements.Element_Access) return Boolean
   is
      package Get is

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Boolean := False;
         end record;

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access);

         overriding procedure Enumeration_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Enumeration_Type_Definitions.
              Enumeration_Type_Definition_Access);

      end Get;

      package body Get is

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access)
         is
            View : constant Gela.Elements.Type_Definitions.
              Type_Definition_Access := Node.Type_Declaration_View;
         begin
            View.Visit (Self);
         end Full_Type_Declaration;

         overriding procedure Enumeration_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Enumeration_Type_Definitions.
              Enumeration_Type_Definition_Access)
         is
            pragma Unreferenced (Node);
         begin
            Self.Result := True;
         end Enumeration_Type_Definition;

      end Get;

      use type Gela.Elements.Element_Access;
      V : Get.Visiter;
   begin
      if Decl /= null then
         Decl.Visit (V);
      end if;

      return V.Result;
   end Is_Enumeration;

   ------------------------------
   -- Leave_Declarative_Region --
   ------------------------------

   function Leave_Declarative_Region
     (Comp   : Gela.Compilations.Compilation_Access;
      Index  : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is
   begin
      if Is_Enumeration (Name.Parent) then
         return Index;
      else
         return Comp.Context.Environment_Set.Leave_Declarative_Region (Index);
      end if;
   end Leave_Declarative_Region;

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

      Set    : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Comp.Context.Symbols;
      Parent : constant Gela.Lexical_Types.Symbol := Set.Parent (Full_Name);
      Result : Gela.Semantic_Types.Env_Index;
   begin
      if Parent = Gela.Lexical_Types.No_Symbol then
         return 0;
      end if;

      Result := Comp.Context.Environment_Set.Library_Unit_Environment (Parent);

      return Result;
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
