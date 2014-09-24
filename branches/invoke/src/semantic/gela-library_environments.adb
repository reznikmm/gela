--  with Gela.Defining_Name_Cursors;
with Gela.Compilation_Units;
with Gela.Compilation_Unit_Sets;
with Gela.Element_Visiters;

with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Subunits;
with Gela.Elements.Library_Unit_Bodies;
with Gela.Elements.Library_Unit_Declarations;
with Gela.Elements.Proper_Bodies;
with Gela.Elements.Function_Bodies;
with Gela.Elements.Package_Bodies;
with Gela.Elements.Procedure_Bodies;
with Gela.Elements.Function_Declarations;
with Gela.Elements.Function_Instantiations;
with Gela.Elements.Generic_Function_Declarations;
with Gela.Elements.Generic_Function_Renamings;
with Gela.Elements.Generic_Package_Declarations;
with Gela.Elements.Generic_Package_Renamings;
with Gela.Elements.Generic_Procedure_Declarations;
with Gela.Elements.Generic_Procedure_Renamings;
with Gela.Elements.Package_Declarations;
with Gela.Elements.Package_Instantiations;
with Gela.Elements.Package_Renaming_Declarations;
with Gela.Elements.Procedure_Declarations;
with Gela.Elements.Procedure_Instantiations;
with Gela.Elements.Protected_Bodies;
with Gela.Elements.Task_Bodies;
with Gela.Elements.Defining_Designators;
with Gela.Elements.Defining_Program_Unit_Names;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Defining_Expanded_Unit_Names;
with Gela.Elements.Defining_Operator_Symbols;
with Gela.Symbol_Sets;

package body Gela.Library_Environments is

   package Library_Cursor is
      type Defining_Name_Cursor is
        new Gela.Defining_Name_Cursors.Defining_Name_Cursor
      with record
         Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      end record;

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean;

      overriding function Element
        (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access;

      overriding procedure Next (Self : in out Defining_Name_Cursor);

   end Library_Cursor;

   package Get_Defining_Name_Visiter is

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

      overriding procedure Defining_Expanded_Unit_Name
        (Self : in out Visiter;
         Node : not null Gela.Elements.Defining_Expanded_Unit_Names.
           Defining_Expanded_Unit_Name_Access);

      overriding procedure Defining_Identifier
        (Self : in out Visiter;
         Node : not null Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access);

      overriding procedure Defining_Operator_Symbol
        (Self : in out Visiter;
         Node : not null Gela.Elements.Defining_Operator_Symbols.
           Defining_Operator_Symbol_Access);

      overriding procedure Function_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Function_Bodies.Function_Body_Access);

      overriding procedure Function_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Function_Declarations.
           Function_Declaration_Access);

      overriding procedure Function_Instantiation
        (Self : in out Visiter;
         Node : not null Gela.Elements.Function_Instantiations.
           Function_Instantiation_Access);

      overriding procedure Generic_Function_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Function_Declarations.
           Generic_Function_Declaration_Access);

      overriding procedure Generic_Function_Renaming
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Function_Renamings.
           Generic_Function_Renaming_Access);

      overriding procedure Generic_Package_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Package_Declarations.
           Generic_Package_Declaration_Access);

      overriding procedure Generic_Package_Renaming
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Package_Renamings.
           Generic_Package_Renaming_Access);

      overriding procedure Generic_Procedure_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Procedure_Declarations.
           Generic_Procedure_Declaration_Access);

      overriding procedure Generic_Procedure_Renaming
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Procedure_Renamings.
           Generic_Procedure_Renaming_Access);

      overriding procedure Package_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Package_Bodies.Package_Body_Access);

      overriding procedure Package_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Package_Declarations.
           Package_Declaration_Access);

      overriding procedure Package_Instantiation
        (Self : in out Visiter;
         Node : not null Gela.Elements.Package_Instantiations.
           Package_Instantiation_Access);

      overriding procedure Package_Renaming_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Package_Renaming_Declarations.
           Package_Renaming_Declaration_Access);

      overriding procedure Procedure_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Procedure_Bodies.Procedure_Body_Access);

      overriding procedure Procedure_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Procedure_Declarations.
           Procedure_Declaration_Access);

      overriding procedure Procedure_Instantiation
        (Self : in out Visiter;
         Node : not null Gela.Elements.Procedure_Instantiations.
           Procedure_Instantiation_Access);

      overriding procedure Protected_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Protected_Bodies.Protected_Body_Access);

      overriding procedure Subunit
        (Self : in out Visiter;
         Node : not null Gela.Elements.Subunits.Subunit_Access);

      overriding procedure Task_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Task_Bodies.Task_Body_Access);

   end Get_Defining_Name_Visiter;

   -------------------------------
   -- Get_Defining_Name_Visiter --
   -------------------------------

   package body Get_Defining_Name_Visiter is

      ---------------------------
      -- Compilation_Unit_Body --
      ---------------------------

      overriding procedure Compilation_Unit_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Compilation_Unit_Bodies.
           Compilation_Unit_Body_Access)
      is
         Decl : constant Gela.Elements.Library_Unit_Bodies.
           Library_Unit_Body_Access := Node.Unit_Declaration;
      begin
         Decl.Visit (Self);
      end Compilation_Unit_Body;

      ----------------------------------
      -- Compilation_Unit_Declaration --
      ----------------------------------

      overriding procedure Compilation_Unit_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Compilation_Unit_Declarations.
           Compilation_Unit_Declaration_Access)
      is
         Decl : constant Gela.Elements.Library_Unit_Declarations.
           Library_Unit_Declaration_Access := Node.Unit_Declaration;
      begin
         Decl.Visit (Self);
      end Compilation_Unit_Declaration;

      ---------------------------------
      -- Defining_Expanded_Unit_Name --
      ---------------------------------

      overriding procedure Defining_Expanded_Unit_Name
        (Self : in out Visiter;
         Node : not null Gela.Elements.Defining_Expanded_Unit_Names.
           Defining_Expanded_Unit_Name_Access) is
      begin
         Self.Name := Gela.Elements.Defining_Names.Defining_Name_Access (Node);
      end Defining_Expanded_Unit_Name;

      -------------------------
      -- Defining_Identifier --
      -------------------------

      overriding procedure Defining_Identifier
        (Self : in out Visiter;
         Node : not null Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access) is
      begin
         Self.Name := Gela.Elements.Defining_Names.Defining_Name_Access (Node);
      end Defining_Identifier;

      ------------------------------
      -- Defining_Operator_Symbol --
      ------------------------------

      overriding procedure Defining_Operator_Symbol
        (Self : in out Visiter;
         Node : not null Gela.Elements.Defining_Operator_Symbols.
           Defining_Operator_Symbol_Access) is
      begin
         Self.Name := Gela.Elements.Defining_Names.Defining_Name_Access (Node);
      end Defining_Operator_Symbol;

      -------------------
      -- Function_Body --
      -------------------

      overriding procedure Function_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Function_Bodies.Function_Body_Access)
      is
         Name : constant Gela.Elements.Defining_Designators.
           Defining_Designator_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Function_Body;

      --------------------------
      -- Function_Declaration --
      --------------------------

      overriding procedure Function_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Function_Declarations.
           Function_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Designators.
           Defining_Designator_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Function_Declaration;

      ----------------------------
      -- Function_Instantiation --
      ----------------------------

      overriding procedure Function_Instantiation
        (Self : in out Visiter;
         Node : not null Gela.Elements.Function_Instantiations.
           Function_Instantiation_Access)
      is
         Name : constant Gela.Elements.Defining_Designators.
           Defining_Designator_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Function_Instantiation;

      ----------------------------------
      -- Generic_Function_Declaration --
      ----------------------------------

      overriding procedure Generic_Function_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Function_Declarations.
           Generic_Function_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Designators.
           Defining_Designator_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Generic_Function_Declaration;

      -------------------------------
      -- Generic_Function_Renaming --
      -------------------------------

      overriding procedure Generic_Function_Renaming
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Function_Renamings.
           Generic_Function_Renaming_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Generic_Function_Renaming;

      ---------------------------------
      -- Generic_Package_Declaration --
      ---------------------------------

      overriding procedure Generic_Package_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Package_Declarations.
           Generic_Package_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Generic_Package_Declaration;

      ------------------------------
      -- Generic_Package_Renaming --
      ------------------------------

      overriding procedure Generic_Package_Renaming
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Package_Renamings.
           Generic_Package_Renaming_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Generic_Package_Renaming;

      -----------------------------------
      -- Generic_Procedure_Declaration --
      -----------------------------------

      overriding procedure Generic_Procedure_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Procedure_Declarations.
           Generic_Procedure_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Generic_Procedure_Declaration;

      --------------------------------
      -- Generic_Procedure_Renaming --
      --------------------------------

      overriding procedure Generic_Procedure_Renaming
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Procedure_Renamings.
           Generic_Procedure_Renaming_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Generic_Procedure_Renaming;

      ------------------
      -- Package_Body --
      ------------------

      overriding procedure Package_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Package_Bodies.Package_Body_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Package_Body;

      -------------------------
      -- Package_Declaration --
      -------------------------

      overriding procedure Package_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Package_Declarations.
           Package_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Package_Declaration;

      ---------------------------
      -- Package_Instantiation --
      ---------------------------

      overriding procedure Package_Instantiation
        (Self : in out Visiter;
         Node : not null Gela.Elements.Package_Instantiations.
           Package_Instantiation_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Package_Instantiation;

      ----------------------------------
      -- Package_Renaming_Declaration --
      ----------------------------------

      overriding procedure Package_Renaming_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Package_Renaming_Declarations.
           Package_Renaming_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Package_Renaming_Declaration;

      --------------------
      -- Procedure_Body --
      --------------------

      overriding procedure Procedure_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Procedure_Bodies.Procedure_Body_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Procedure_Body;

      ---------------------------
      -- Procedure_Declaration --
      ---------------------------

      overriding procedure Procedure_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Procedure_Declarations.
           Procedure_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Procedure_Declaration;

      -----------------------------
      -- Procedure_Instantiation --
      -----------------------------

      overriding procedure Procedure_Instantiation
        (Self : in out Visiter;
         Node : not null Gela.Elements.Procedure_Instantiations.
           Procedure_Instantiation_Access)
      is
         Name : constant Gela.Elements.Defining_Program_Unit_Names.
           Defining_Program_Unit_Name_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Procedure_Instantiation;

      --------------------
      -- Protected_Body --
      --------------------

      overriding procedure Protected_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Protected_Bodies.Protected_Body_Access)
      is
         Name : constant Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Protected_Body;

      -------------
      -- Subunit --
      -------------

      overriding procedure Subunit
        (Self : in out Visiter;
         Node : not null Gela.Elements.Subunits.Subunit_Access)
      is
         Decl : constant Gela.Elements.Proper_Bodies.Proper_Body_Access :=
           Node.Unit_Declaration;
      begin
         Decl.Visit (Self);
      end Subunit;

      ---------------
      -- Task_Body --
      ---------------

      overriding procedure Task_Body
        (Self : in out Visiter;
         Node : not null Gela.Elements.Task_Bodies.Task_Body_Access)
      is
         Name : constant Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access := Node.Names;
      begin
         Name.Visit (Self);
      end Task_Body;

   end Get_Defining_Name_Visiter;

   --------------------
   -- Library_Cursor --
   --------------------

   package body Library_Cursor is

      -----------------
      -- Has_Element --
      -----------------

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean
      is
         use type Gela.Elements.Defining_Names.Defining_Name_Access;
      begin
         return Self.Name /= null;
      end Has_Element;

      -------------
      -- Element --
      -------------

      overriding function Element
        (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access is
      begin
         return Self.Name;
      end Element;

      ----------
      -- Next --
      ----------

      overriding procedure Next (Self : in out Defining_Name_Cursor) is
      begin
         Self.Name := null;
      end Next;

   end Library_Cursor;

   -----------------------
   -- Add_Defining_Name --
   -----------------------

   overriding function Add_Defining_Name
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is
   begin
      raise Program_Error;
      return 0;
   end Add_Defining_Name;

   ---------------------
   -- Add_Use_Package --
   ---------------------

   overriding function Add_Use_Package
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is
   begin
      raise Program_Error;
      return 0;
   end Add_Use_Package;

   --------------------
   -- Direct_Visible --
   --------------------

   overriding function Direct_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
   is
      use type Gela.Semantic_Types.Env_Index;
      use type Gela.Compilation_Units.Compilation_Unit_Access;
      Unit  : Gela.Compilation_Units.Compilation_Unit_Access;
      Units : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
   begin
      if Index /= Library_Env then
         return Library_Cursor.Defining_Name_Cursor'(Name => null);
      end if;

      Units := Self.Context.Library_Unit_Declarations;
      Unit := Units.Find (Symbol);

      if Unit = null then
         Units := Self.Context.Compilation_Unit_Bodies;
         Unit := Units.Find (Symbol);
      end if;

      return Result : Library_Cursor.Defining_Name_Cursor do
         if Unit /= null then
            declare
               V : Get_Defining_Name_Visiter.Visiter;
            begin
               Unit.Tree.Visit (V);
               Result.Name := V.Name;
            end;
         end if;
      end return;
   end Direct_Visible;

   ------------------------------
   -- Enter_Declarative_Region --
   ------------------------------

   overriding function Enter_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is
   begin
      raise Program_Error;
      return Self.Enter_Declarative_Region (Index, Region);
   end Enter_Declarative_Region;

   ------------------------------
   -- Leave_Declarative_Region --
   ------------------------------

   overriding function Leave_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index)
      return Gela.Semantic_Types.Env_Index is
   begin
      raise Program_Error;
      return Self.Leave_Declarative_Region (Index);
   end Leave_Declarative_Region;

   -------------------------------
   -- Library_Level_Environment --
   -------------------------------

   overriding function Library_Level_Environment
     (Self  : Environment_Set)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Self);
   begin
      return Library_Env;
   end Library_Level_Environment;

   ------------------------------
   -- Library_Unit_Environment --
   ------------------------------

   overriding function Library_Unit_Environment
     (Self   : access Environment_Set;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Symbol);
   begin
      return 0;
   end Library_Unit_Environment;

   ----------------------------------
   -- Set_Library_Unit_Environment --
   ----------------------------------

   overriding procedure Set_Library_Unit_Environment
     (Self   : access Environment_Set;
      Symbol : Gela.Lexical_Types.Symbol;
      Value  : Gela.Semantic_Types.Env_Index) is
   begin
      raise Constraint_Error;
   end Set_Library_Unit_Environment;

   -----------------
   -- Use_Visible --
   -----------------

   overriding function Use_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Index);
      pragma Unreferenced (Symbol);

      --  No use clause considered at library level
      Result : Library_Cursor.Defining_Name_Cursor;
   begin
      return Result;
   end Use_Visible;

   -------------
   -- Visible --
   -------------

   overriding function Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : access Boolean)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
   is
      use type Gela.Semantic_Types.Env_Index;
      use type Gela.Compilation_Units.Compilation_Unit_Access;
      Unit   : Gela.Compilation_Units.Compilation_Unit_Access;
      Units  : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Set    : Gela.Symbol_Sets.Symbol_Set_Access;
   begin
      if Index /= Library_Env then
         return Library_Cursor.Defining_Name_Cursor'(Name => null);
      end if;

      Set := Self.Context.Symbols;
      Units := Self.Context.Library_Unit_Declarations;

      Name := Region.Full_Name;
      Unit := Units.Find (Name);

      if Unit = null then
         Found.all := False;
         return Library_Cursor.Defining_Name_Cursor'(Name => null);
      end if;

      Found.all := True;
      Set.Join (Left => Name, Right => Symbol, Value => Name);
      Unit := Units.Find (Name);

      if Unit = null then
         Units := Self.Context.Compilation_Unit_Bodies;
         Unit := Units.Find (Symbol);
      end if;

      return Result : Library_Cursor.Defining_Name_Cursor do
         if Unit /= null then
            declare
               V : Get_Defining_Name_Visiter.Visiter;
            begin
               Unit.Tree.Visit (V);
               Result.Name := V.Name;
            end;
         end if;
      end return;
   end Visible;

end Gela.Library_Environments;
