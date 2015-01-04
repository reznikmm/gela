------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Element_Factories;
with Gela.Element_Visiters;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Context_Items;
with Gela.Elements.Defining_Designators;
with Gela.Elements.Enumeration_Type_Definitions;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Function_Declarations;
with Gela.Elements.Identifiers;
with Gela.Elements.Package_Declarations;
with Gela.Elements.Parameter_Specifications;
with Gela.Elements.Procedure_Bodies;
with Gela.Elements.Program_Unit_Names;
with Gela.Elements.Selected_Identifiers;
with Gela.Elements.Selector_Names;
with Gela.Elements.Subtype_Mark_Or_Access_Definitions;
with Gela.Elements.Type_Definitions;
with Gela.Elements.Use_Package_Clauses;
with Gela.Elements.With_Clauses;
with Gela.Environments;
with Gela.Plain_Type_Managers;
with Gela.Symbol_Sets;
with Gela.Elements.Defining_Operator_Symbols;
with Gela.Defining_Name_Cursors;

package body Gela.Pass_Utils is

   procedure Preprocess_Standard
     (Comp : Gela.Compilations.Compilation_Access;
      Unit : Gela.Elements.Element_Access);

   procedure Postprocess_Standard
     (Comp : Gela.Compilations.Compilation_Access;
      Unit : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Env  : in out Gela.Semantic_Types.Env_Index);

   function Is_Enumeration
     (Decl : Gela.Elements.Element_Access) return Boolean;

   procedure Add_Library_Level_Use_Clauses
     (Comp   : Gela.Compilations.Compilation_Access;
      Decl   : Gela.Elements.Element_Access;
      Env    : in out Gela.Semantic_Types.Env_Index);

   procedure Add_Library_Level_Use_Clauses
     (Comp   : Gela.Compilations.Compilation_Access;
      Decl   : Gela.Elements.Element_Access;
      Env    : in out Gela.Semantic_Types.Env_Index)
   is

      package Get is

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Name : Gela.Elements.Defining_Names.Defining_Name_Access;
         end record;

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access);

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access);

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access);

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations.
              Package_Declaration_Access);

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access);

         overriding procedure Selected_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Identifiers.
              Selected_Identifier_Access);

         overriding procedure Use_Package_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Package_Clauses.
              Use_Package_Clause_Access);

         overriding procedure With_Clause  --  Use env.out instead
           (Self : in out Visiter;
            Node : not null Gela.Elements.With_Clauses.With_Clause_Access);
      end Get;

      package body Get is

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access)
         is
            List : constant Gela.Elements.Context_Items.
              Context_Item_Sequence_Access := Node.Context_Clause_Elements;

            Cursor : Gela.Elements.Element_Sequence_Cursor'Class := List.First;
         begin
            while Cursor.Has_Element loop
               Cursor.Element.Visit (Self);
               Cursor.Next;
            end loop;
         end Compilation_Unit_Body;

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access)
         is
            List : constant Gela.Elements.Context_Items.
              Context_Item_Sequence_Access := Node.Context_Clause_Elements;

            Cursor : Gela.Elements.Element_Sequence_Cursor'Class := List.First;
         begin
            while Cursor.Has_Element loop
               Cursor.Element.Visit (Self);
               Cursor.Next;
            end loop;
         end Compilation_Unit_Declaration;

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access) is
         begin
            Self.Name := Node.Defining_Name;
         end Identifier;

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations.
              Package_Declaration_Access) is
         begin
            Node.Enclosing_Element.Visit (Self);
         end Package_Declaration;

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access) is
         begin
            Node.Enclosing_Element.Visit (Self);
         end Procedure_Body;

         overriding procedure Selected_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Identifiers.
              Selected_Identifier_Access)
         is
            Selector : constant Gela.Elements.Selector_Names.
              Selector_Name_Access := Node.Selector;
         begin
            Selector.Visit (Self);
         end Selected_Identifier;

         overriding procedure Use_Package_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Package_Clauses.
              Use_Package_Clause_Access)
         is
            Set : constant Gela.Environments.Environment_Set_Access :=
              Comp.Context.Environment_Set;
            List : constant Gela.Elements.Program_Unit_Names.
              Program_Unit_Name_Sequence_Access := Node.Clause_Names;
            Cursor : Gela.Elements.Element_Sequence_Cursor'Class := List.First;
         begin
            while Cursor.Has_Element loop
               Cursor.Element.Visit (Self);

               Env := Set.Add_Use_Package
                 (Index => Env,
                  Name  => Self.Name);

               Cursor.Next;
            end loop;
         end Use_Package_Clause;

         overriding procedure With_Clause  --  Use env.out instead
           (Self : in out Visiter;
            Node : not null Gela.Elements.With_Clauses.With_Clause_Access)
         is
            pragma Unreferenced (Self);
            use type Gela.Lexical_Types.Symbol_List;

            List : Gela.Lexical_Types.Symbol_List := Node.With_List;
            Set : constant Gela.Environments.Environment_Set_Access :=
              Comp.Context.Environment_Set;
         begin
            while List /= Gela.Lexical_Types.Empty_Symbol_List loop
               Env := Set.Add_With_Clause
                 (Index  => Env,
                  Symbol => Comp.Context.Symbols.Head (List));
               List := Comp.Context.Symbols.Tail (List);
            end loop;
         end With_Clause;

      end Get;

      V : Get.Visiter;
   begin
      Decl.Visit (V);
   end Add_Library_Level_Use_Clauses;

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
      use type Gela.Semantic_Types.Env_Index;
      use type Gela.Lexical_Types.Symbol;

      Library_Level : constant Boolean :=
        Env = Comp.Context.Environment_Set.Library_Level_Environment;

      Env_0 : Gela.Semantic_Types.Env_Index;
      Env_1 : Gela.Semantic_Types.Env_Index;
      Env_2 : Gela.Semantic_Types.Env_Index;
   begin
      if Library_Level then
         Env_0 := Parents_Declarative_Region (Comp, Symbol);

         if Symbol = Gela.Lexical_Types.Predefined_Symbols.Standard then
            Preprocess_Standard (Comp, Decl);
         end if;

      else
         Env_0 := Env;
      end if;

      Env_1 := Comp.Context.Environment_Set.Add_Defining_Name
        (Index  => Env_0,
         Symbol => Symbol,
         Name   => Name);

      if Is_Enumeration (Decl) then
         return Env_1;
      end if;

      Env_2 := Comp.Context.Environment_Set.Enter_Declarative_Region
        (Index  => Env_1,
         Region => Name);

      if Library_Level then
         Add_Library_Level_Use_Clauses (Comp, Decl, Env_2);
      end if;

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
      Tail   : Gela.Lexical_Types.Symbol_List := List;
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
         Symbol := Set.Head (Tail);
         Tail := Set.Tail (Tail);
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

   --------------------------------
   -- Create_Function_Call_Value --
   --------------------------------

   function Create_Function_Call_Value
     (Comp          : Gela.Compilations.Compilation_Access;
      Name          : Gela.Semantic_Types.Value_Index;
      Arguments     : Gela.Semantic_Types.Value_Index)
      return Gela.Semantic_Types.Value_Index
   is
      Result : Gela.Semantic_Types.Value_Index;
   begin
      Comp.Context.Values.Apply
        (Name  => Name,
         Args  => Arguments,
         Value => Result);

      return Result;
   end Create_Function_Call_Value;

   function Create_Numeric_Value
     (Comp  : Gela.Compilations.Compilation_Access;
      Value : Gela.Lexical_Types.Token_Index)
      return Gela.Semantic_Types.Value_Index
   is
      Token  : constant Gela.Lexical_Types.Token := Comp.Get_Token (Value);
      Source : constant League.Strings.Universal_String := Comp.Source;
      Image  : constant League.Strings.Universal_String :=
        Source.Slice (Token.First, Token.Last);
      Result : Gela.Semantic_Types.Value_Index;
   begin
      Comp.Context.Values.Numeric_Literal (Image, Result);

      return Result;
   end Create_Numeric_Value;

   -------------------------
   -- Create_String_Value --
   -------------------------

   function Create_String_Value
     (Comp          : Gela.Compilations.Compilation_Access;
      Full_Name     : Gela.Lexical_Types.Token_Index)
      return Gela.Semantic_Types.Value_Index
   is
      Token  : constant Gela.Lexical_Types.Token := Comp.Get_Token (Full_Name);
      Source : constant League.Strings.Universal_String := Comp.Source;
      Image  : constant League.Strings.Universal_String :=
        Source.Slice (Token.First, Token.Last);
      Result : Gela.Semantic_Types.Value_Index;
   begin
      Comp.Context.Values.String_Literal
        (Image.Slice (2, Image.Length - 1), Result);

      return Result;
   end Create_String_Value;

   procedure End_Of_Compilation_Unit_Declaration
     (Comp   : Gela.Compilations.Compilation_Access;
      Unit   : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Symbol : Gela.Lexical_Types.Symbol;
      Env    : in out Gela.Semantic_Types.Env_Index)
   is
      use type Gela.Lexical_Types.Symbol;
   begin
      if Symbol = Gela.Lexical_Types.Predefined_Symbols.Standard then
         Postprocess_Standard (Comp, Unit, Env);
      end if;
   end End_Of_Compilation_Unit_Declaration;

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
      if Is_Enumeration (Name.Enclosing_Element) then
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
      Full_Name     : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index
   is
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

   --------------------------
   -- Postprocess_Standard --
   --------------------------

   procedure Postprocess_Standard
     (Comp : Gela.Compilations.Compilation_Access;
      Unit : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Env  : in out Gela.Semantic_Types.Env_Index)
   is
      pragma Unreferenced (Unit);

      function Create_Operator
        (Operator_Symbol : Gela.Lexical_Types.Symbol;
         Type_Symbol     : Gela.Lexical_Types.Symbol)
         return Gela.Elements.Function_Declarations.
                  Function_Declaration_Access;

      function Create_Subtype
        (Type_Symbol : Gela.Lexical_Types.Symbol)
         return Gela.Elements.Subtype_Mark_Or_Access_Definitions.
           Subtype_Mark_Or_Access_Definition_Access;

      function Get_Type
        (Type_Symbol : Gela.Lexical_Types.Symbol)
         return Gela.Elements.Defining_Names.Defining_Name_Access;

      Env_Set : constant Gela.Environments.Environment_Set_Access :=
        Comp.Context.Environment_Set;

      Factory : constant Gela.Element_Factories.Element_Factory_Access :=
        Comp.Factory;

      ---------------------
      -- Create_Operator --
      ---------------------

      function Create_Operator
        (Operator_Symbol : Gela.Lexical_Types.Symbol;
         Type_Symbol     : Gela.Lexical_Types.Symbol)
         return Gela.Elements.Function_Declarations.
                  Function_Declaration_Access
      is
         FD : Gela.Elements.Function_Declarations.Function_Declaration_Access;

         Oper : constant Gela.Elements.Defining_Operator_Symbols.
           Defining_Operator_Symbol_Access :=
             Factory.Defining_Operator_Symbol
               (Operator_Symbol_Token => 0);

         Name : Gela.Elements.Defining_Designators.Defining_Designator_Access;

         Param : Gela.Elements.Parameter_Specifications.
           Parameter_Specification_Access;

         Params : constant Gela.Elements.Parameter_Specifications.
           Parameter_Specification_Sequence_Access :=
             Factory.Parameter_Specification_Sequence;

         Mark : Gela.Elements.Subtype_Mark_Or_Access_Definitions.
           Subtype_Mark_Or_Access_Definition_Access;
      begin
         Oper.Set_Full_Name (Operator_Symbol);

         Name := Gela.Elements.Defining_Designators.Defining_Designator_Access
           (Oper);

         for J in 1 .. 2 loop
            Mark := Create_Subtype (Type_Symbol);

            Param := Factory.Parameter_Specification
              (Names                      =>
                 Factory.Defining_Identifier_Sequence,
               Colon_Token                => 0,
               Aliased_Token              => 0,
               In_Token                   => 0,
               Out_Token                  => 0,
               Not_Token                  => 0,
               Null_Token                 => 0,
               Object_Declaration_Subtype => Mark,
               Assignment_Token           => 0,
               Initialization_Expression  => null);
                     Params.Append (Param);
         end loop;

         Mark := Create_Subtype (Type_Symbol);

         FD := Factory.Function_Declaration
           (Not_Token             => 0,
            Overriding_Token      => 0,
            Function_Token        => 0,
            Names                 => Name,
            Lp_Token              => 0,
            Parameter_Profile     => Params,
            Rp_Token              => 0,
            Return_Token          => 0,
            Return_Not_Token      => 0,
            Return_Null_Token     => 0,
            Result_Subtype        => Mark,
            Is_Token              => 0,
            Abstract_Token        => 0,
            Result_Expression     => null,
            Renames_Token         => 0,
            Renamed_Entity        => null,
            Separate_Token        => 0,
            Aspect_Specifications => Factory.Aspect_Specification_Sequence,
            Semicolon_Token       => 0);

         Env := Env_Set.Add_Defining_Name
           (Index  => Env,
            Symbol => Operator_Symbol,
            Name   => Gela.Elements.Defining_Names.Defining_Name_Access
              (Name));

         return FD;
      end Create_Operator;

      --------------------
      -- Create_Subtype --
      --------------------

      function Create_Subtype
        (Type_Symbol     : Gela.Lexical_Types.Symbol)
         return Gela.Elements.Subtype_Mark_Or_Access_Definitions.
           Subtype_Mark_Or_Access_Definition_Access
      is
         Identifier : Gela.Elements.Identifiers.Identifier_Access;

         Mark : Gela.Elements.Subtype_Mark_Or_Access_Definitions.
           Subtype_Mark_Or_Access_Definition_Access;
      begin
         Identifier := Factory.Identifier (Identifier_Token => 0);
         Identifier.Set_Full_Name (Type_Symbol);
         Identifier.Set_Defining_Name (Get_Type (Type_Symbol));

         Mark := Gela.Elements.Subtype_Mark_Or_Access_Definitions.
           Subtype_Mark_Or_Access_Definition_Access (Identifier);

         return Mark;
      end Create_Subtype;

      --------------
      -- Get_Type --
      --------------

      function Get_Type
        (Type_Symbol : Gela.Lexical_Types.Symbol)
         return Gela.Elements.Defining_Names.Defining_Name_Access
      is
         Pos : constant Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
           := Env_Set.Direct_Visible (Env, Type_Symbol);
      begin
         if Pos.Has_Element then
            return Pos.Element;
         else
            raise Constraint_Error;
         end if;
      end Get_Type;

      FD : Gela.Elements.Function_Declarations.Function_Declaration_Access;
      pragma Unreferenced (FD);
   begin
      FD := Create_Operator
        (Operator_Symbol => Gela.Lexical_Types.Operators.Hyphen_Operator,
         Type_Symbol     => Gela.Lexical_Types.Predefined_Symbols.Integer);

      FD := Create_Operator
        (Operator_Symbol => Gela.Lexical_Types.Operators.Ampersand_Operator,
         Type_Symbol     => Gela.Lexical_Types.Predefined_Symbols.String);
   end Postprocess_Standard;

   -------------------------
   -- Preprocess_Standard --
   -------------------------

   procedure Preprocess_Standard
     (Comp : Gela.Compilations.Compilation_Access;
      Unit : Gela.Elements.Element_Access) is
   begin
      Gela.Plain_Type_Managers.Type_Manager_Access
        (Comp.Context.Types).Initialize (Unit);
   end Preprocess_Standard;

end Gela.Pass_Utils;
