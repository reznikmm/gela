with Gela.Compilations;
with Gela.Element_Factories;
with Gela.Element_Visiters;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Compilation_Units;
with Gela.Elements.Compilations;
with Gela.Elements.Subunits;
with Gela.LARL_Parsers;
with Gela.Lexers;
with Gela.Node_Factories;
with Gela.Pass_List;
with Gela.Plain_Compilations;
with Gela.Source_Finders;
with Gela.Elements.Library_Unit_Declarations;
with Gela.Symbol_Sets;
with Gela.Elements.Context_Items;
with Gela.Elements.With_Clauses;
with Gela.Elements.Program_Unit_Names;
with Gela.Elements.Identifiers;
with Gela.Elements.Selected_Identifiers;
with Gela.Elements.Selector_Names;
with Gela.Elements.Library_Unit_Bodies;
with Gela.Elements.Package_Bodies;
with Gela.Elements.Procedure_Bodies;
with Gela.Elements.Function_Bodies;
with Gela.Elements.Defining_Program_Unit_Names;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Defining_Expanded_Unit_Names;
with Gela.Elements.Procedure_Declarations;
with Gela.Elements.Procedure_Instantiations;
with Gela.Elements.Function_Declarations;
with Gela.Elements.Function_Instantiations;
with Gela.Elements.Package_Declarations;
with Gela.Elements.Package_Instantiations;
with Gela.Elements.Generic_Procedure_Declarations;
with Gela.Elements.Generic_Procedure_Renamings;
with Gela.Elements.Generic_Function_Declarations;
with Gela.Elements.Generic_Function_Renamings;
with Gela.Elements.Generic_Package_Declarations;
with Gela.Elements.Generic_Package_Renamings;
with Gela.Elements.Package_Renaming_Declarations;
with Gela.Elements.Defining_Designators;
with Gela.Elements.Proper_Bodies;
with Gela.Elements.Task_Bodies;
with Gela.Elements.Protected_Bodies;

package body Gela.Plain_Compilation_Managers is

   procedure Read
     (Self   : in out Compilation_Manager'Class;
      File   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String);

   procedure Add_Depend_Units
     (Comp   : Gela.Compilations.Compilation_Access;
      Root   : Gela.Elements.Compilations.Compilation_Access);

   procedure Look_Into_Unit
     (Comp   : Gela.Compilations.Compilation_Access;
      Unit   : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
      Value  : out Gela.Dependency_Lists.Unit_Data);

   ----------------------
   -- Add_Depend_Units --
   ----------------------

   procedure Look_Into_Unit
     (Comp   : Gela.Compilations.Compilation_Access;
      Unit   : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
      Value  : out Gela.Dependency_Lists.Unit_Data)
   is

      package Get is

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Is_Package : Boolean := False;
            Is_Subprogram : Boolean := False;
            Symbol : Gela.Lexical_Types.Symbol := 0;
            Withed : Gela.Lexical_Types.Symbol_List :=
              Gela.Lexical_Types.Empty_Symbol_List;
         end record;

         procedure Add_With
           (Self : in out Visiter;
            List : Gela.Elements.Context_Items.Context_Item_Sequence_Access);

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

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access);

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

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access);

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
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access);

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
            Node : not null Gela.Elements.Protected_Bodies.
              Protected_Body_Access);

         overriding procedure Selected_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Identifiers.
              Selected_Identifier_Access);

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access);

         overriding procedure Task_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Bodies.Task_Body_Access);

         overriding procedure With_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.With_Clauses.With_Clause_Access);
      end Get;

      package body Get is

         --------------
         -- Add_With --
         --------------

         procedure Add_With
           (Self : in out Visiter;
            List : Gela.Elements.Context_Items.Context_Item_Sequence_Access)
         is
            Cursor : Gela.Elements.Context_Items.Context_Item_Sequence_Cursor
              := List.First;
         begin
            while Cursor.Has_Element loop
               Cursor.Element.Visit (Self);
               Cursor.Next;
            end loop;
         end Add_With;

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
            Self.Withed := Gela.Lexical_Types.Empty_Symbol_List;
            Self.Add_With (Node.Context_Clause_Elements);
            Decl.Visit (Self);

            Value :=
              (Kind          => Gela.Dependency_Lists.Unit_Body,
               Name          => Self.Symbol,
               Withed        => Self.Withed,
               Limited_With  => Gela.Lexical_Types.Empty_Symbol_List,
               Unit_Body     => Node,
               Is_Subprogram => Self.Is_Subprogram);
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
            Self.Withed := Gela.Lexical_Types.Empty_Symbol_List;
            Self.Add_With (Node.Context_Clause_Elements);
            Decl.Visit (Self);

            Value :=
              (Kind         => Gela.Dependency_Lists.Unit_Declaration,
               Name         => Self.Symbol,
               Withed       => Self.Withed,
               Limited_With => Gela.Lexical_Types.Empty_Symbol_List,
               Is_Package   => Self.Is_Package,
               Unit_Declaration => Node);
         end Compilation_Unit_Declaration;

         ---------------------------------
         -- Defining_Expanded_Unit_Name --
         ---------------------------------

         overriding procedure Defining_Expanded_Unit_Name
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Expanded_Unit_Names.
              Defining_Expanded_Unit_Name_Access)
         is
            Prefix : constant Gela.Elements.Program_Unit_Names.
              Program_Unit_Name_Access := Node.Defining_Prefix;
            Selector : constant Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access := Node.Defining_Selector;
            Symbol : Gela.Lexical_Types.Symbol;
         begin
            Prefix.Visit (Self);
            Symbol := Self.Symbol;
            Selector.Visit (Self);
            Comp.Context.Symbols.Join
              (Left  => Symbol,
               Right => Self.Symbol,
               Value => Self.Symbol);
         end Defining_Expanded_Unit_Name;

         -------------------------
         -- Defining_Identifier --
         -------------------------

         overriding procedure Defining_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access) is
         begin
            Self.Symbol := Comp.Get_Token (Node.Identifier_Token).Symbol;
         end Defining_Identifier;

         -------------------
         -- Function_Body --
         -------------------

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access)
         is
            Name : constant Gela.Elements.Defining_Designators.
              Defining_Designator_Access := Node.Names;
         begin
            Self.Is_Subprogram := True;
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

         overriding procedure Generic_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Declarations.
              Generic_Package_Declaration_Access)
         is
            Name : constant Gela.Elements.Defining_Program_Unit_Names.
              Defining_Program_Unit_Name_Access := Node.Names;
         begin
            Name.Visit (Self);
            Self.Is_Package := True;
         end Generic_Package_Declaration;

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

         ----------------
         -- Identifier --
         ----------------

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access) is
         begin
            Self.Symbol := Comp.Get_Token (Node.Identifier_Token).Symbol;
         end Identifier;

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
            Self.Is_Package := True;
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
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access)
         is
            Name : constant Gela.Elements.Defining_Program_Unit_Names.
              Defining_Program_Unit_Name_Access := Node.Names;
         begin
            Self.Is_Subprogram := True;
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
            Node : not null Gela.Elements.Protected_Bodies.
              Protected_Body_Access)
         is
            Name : constant Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access := Node.Names;
         begin
            Name.Visit (Self);
         end Protected_Body;

         -------------------------
         -- Selected_Identifier --
         -------------------------

         overriding procedure Selected_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Identifiers.
              Selected_Identifier_Access)
         is
            Selector : constant Gela.Elements.Selector_Names.
              Selector_Name_Access := Node.Selector;
            Symbol : Gela.Lexical_Types.Symbol;
         begin
            Node.Prefix.Visit (Self);
            Symbol := Self.Symbol;
            Selector.Visit (Self);
            Comp.Context.Symbols.Join
              (Left  => Symbol,
               Right => Self.Symbol,
               Value => Self.Symbol);
         end Selected_Identifier;

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
            Self.Withed := Gela.Lexical_Types.Empty_Symbol_List;
            Self.Add_With (Node.Context_Clause_Elements);
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

         -----------------
         -- With_Clause --
         -----------------

         overriding procedure With_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.With_Clauses.With_Clause_Access)
         is
            List : constant Gela.Elements.Program_Unit_Names
              .Program_Unit_Name_Sequence_Access := Node.With_Clause_Names;
            Cursor : Gela.Elements.Program_Unit_Names.
              Program_Unit_Name_Sequence_Cursor := List.First;
         begin
            while Cursor.Has_Element loop
               Cursor.Element.Visit (Self);
               Comp.Context.Symbols.Create_List
                 (Head  => Self.Withed,
                  Tail  => Self.Symbol,
                  Value => Self.Withed);

               Cursor.Next;
            end loop;
         end With_Clause;
      end Get;

      V : Get.Visiter;
   begin
      Unit.Visit (V);
   end Look_Into_Unit;

   ----------------------
   -- Add_Depend_Units --
   ----------------------

   procedure Add_Depend_Units
     (Comp   : Gela.Compilations.Compilation_Access;
      Root   : Gela.Elements.Compilations.Compilation_Access)
   is
      Deps  : constant Gela.Dependency_Lists.Dependency_List_Access :=
        Comp.Context.Dependency_List;
      Units : constant Gela.Elements.Compilation_Units
        .Compilation_Unit_Sequence_Access := Root.Units;
      Cursor : Gela.Elements.Compilation_Units
        .Compilation_Unit_Sequence_Cursor := Units.First;
      Value  : Gela.Dependency_Lists.Unit_Data;
   begin
      while Cursor.Has_Element loop
         Look_Into_Unit (Comp, Cursor.Element, Value);
         Deps.Add_Compilation_Unit (Value);
         Cursor.Next;
      end loop;
   end Add_Depend_Units;

   -------------
   -- Context --
   -------------

   overriding function Context
     (Self : Compilation_Manager) return Gela.Contexts.Context_Access is
   begin
      return Self.Context;
   end Context;

   ------------------
   -- Create_Units --
   ------------------

   not overriding procedure Create_Unit
     (Self    : in out Compilation_Manager;
      Item    : Gela.Dependency_Lists.Unit_Data)
   is
      use all type Gela.Dependency_Lists.Unit_Kinds;
      use type Gela.Lexical_Types.Symbol;

      Up     : Gela.Lexical_Types.Symbol;
      Set    : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Self.Context.Symbols;
      Upper  : Gela.Compilation_Units.Package_Unit_Access;
      Decl   : Gela.Compilation_Units.Library_Unit_Declaration_Access;
      Parent : Gela.Compilation_Units.Body_Unit_Access;
      Sub    : Gela.Compilation_Units.Subunit_Access;
   begin
      case Item.Kind is
         when Unit_Declaration =>
            Up := Set.Parent (Item.Name);

            if Up /= Gela.Lexical_Types.No_Symbol then
               Upper := Self.Packages.Element (Up);
            end if;

            Decl := Self.Factory.Create_Library_Unit_Declaration
              (Parent => Upper,
               Name   => Item.Name,
               Node   => Item.Unit_Declaration);

            Self.Specs.Insert (Item.Name, Decl);

            if Item.Is_Package then
               Upper := Gela.Compilation_Units.Package_Unit_Access (Decl);
               Self.Packages.Insert (Item.Name, Upper);
            end if;

         when Unit_Body =>
            if Self.Specs.Contains (Item.Name) then
               Decl := Self.Specs.Element (Item.Name);

               Parent := Self.Factory.Create_Body_Unit
                 (Declaration => Decl,
                  Name   => Item.Name,
                  Node   => Item.Unit_Body);
            else
               Up := Set.Prefix (Item.Name);

               if Up /= Gela.Lexical_Types.No_Symbol then
                  Upper := Self.Packages.Element (Up);
               end if;

               Parent := Self.Factory.Create_Body_Unit_Without_Declaration
                 (Parent => Upper,
                  Name   => Item.Name,
                  Node   => Item.Unit_Body);
            end if;

            Self.Bodies.Insert (Item.Name, Parent);

         when Subunit =>
            if Self.Bodies.Contains (Item.Parent) then
               Parent := Self.Bodies.Element (Item.Parent);

               Sub := Self.Factory.Create_Subunit
                 (Parent => Parent,
                  Name   => Item.Name,
                  Node   => Item.Subunit);
            else
               Sub := Self.Subunits.Element (Item.Parent);

               Sub := Self.Factory.Create_Subunit
                 (Parent => Sub,
                  Name   => Item.Name,
                  Node   => Item.Subunit);
            end if;

            Self.Subunits.Insert (Item.Name, Sub);
      end case;
   end Create_Unit;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Item);
   end Hash;

   ----------
   -- Read --
   ----------

   procedure Read
     (Self   : in out Compilation_Manager'Class;
      File   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String)
   is
      Lexer   : constant Gela.Lexers.Lexer_Access := Self.Context.Lexer;
      Comp    : constant Gela.Plain_Compilations.Compilation_Access :=
        new Gela.Plain_Compilations.Compilation (Self.Context);
      C       : constant Gela.Compilations.Compilation_Access :=
        Gela.Compilations.Compilation_Access (Comp);
      Parser  : Gela.LARL_Parsers.Parser (Self.Context);
      Root    : Gela.Elements.Compilations.Compilation_Access;
      Last    : Gela.Lexical_Types.Token_Index;
      Factory : constant Gela.Node_Factories.Element_Factory_Access :=
        new Gela.Node_Factories.Element_Factory (C);
   begin
      Comp.Initialize
        (Text_Name => File,
         Source    => Source,
         Factory   => Gela.Element_Factories.Element_Factory_Access (Factory));

      Lexer.Scan (Source, Comp);

      Parser.Parse
        (Input      => Comp,
         Factory    => Gela.Element_Factories.Element_Factory_Access (Factory),
         Root       => Root,
         Last_Token => Last);

      Add_Depend_Units (C, Root);
      Self.Read_Dependency (Self.Context.Dependency_List);
   end Read;

   ---------------
   -- Read_Body --
   ---------------

   overriding procedure Read_Body
     (Self   : in out Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol)
   is
      Source_Finder : constant Gela.Source_Finders.Source_Finder_Access :=
        Self.Context.Source_Finder;
      Found  : Boolean;
      File   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String;
   begin
      Source_Finder.Lookup_Body
        (Symbol => Symbol,
         Found  => Found,
         File   => File,
         Source => Source);

      if not Found then
         raise Constraint_Error;
      end if;

      Read (Self, File, Source);
   end Read_Body;

   ----------------------
   -- Read_Compilation --
   ----------------------

   overriding procedure Read_Compilation
     (Self   : in out Compilation_Manager;
      Name   : League.Strings.Universal_String)
   is
      Source_Finder : constant Gela.Source_Finders.Source_Finder_Access :=
        Self.Context.Source_Finder;
      Found  : Boolean;
      File   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String;
   begin
      Source_Finder.Lookup_Compilation
        (Name   => Name,
         Found  => Found,
         File   => File,
         Source => Source);

      if not Found then
         raise Constraint_Error;
      end if;

      Read (Self, File, Source);
   end Read_Compilation;

   ----------------------
   -- Read_Declaration --
   ----------------------

   overriding procedure Read_Declaration
     (Self   : in out Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol)
   is
      Source_Finder : constant Gela.Source_Finders.Source_Finder_Access :=
        Self.Context.Source_Finder;
      Deps   : Gela.Dependency_Lists.Dependency_List_Access;
      Found  : Boolean;
      File   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String;
   begin
      Source_Finder.Lookup_Declaration
        (Symbol => Symbol,
         Found  => Found,
         File   => File,
         Source => Source);

      if Found then
         Read (Self, File, Source);
      else
         Deps := Self.Context.Dependency_List;
         Deps.No_Library_Unit_Declaration (Symbol);
      end if;

   end Read_Declaration;

   ---------------------
   -- Read_Dependency --
   ---------------------

   overriding procedure Read_Dependency
     (Self   : in out Compilation_Manager;
      List   : Gela.Dependency_Lists.Dependency_List_Access)
   is
      use type Gela.Lexical_Types.Symbol;

      Action : Gela.Dependency_Lists.Action;
   begin
      loop
         List.Next_Action (Action);

         case Action.Action_Kind is
            when Gela.Dependency_Lists.Complete =>
               exit;
            when Gela.Dependency_Lists.Unit_Required =>
               case Action.Unit_Kind is
                  when Gela.Dependency_Lists.Unit_Declaration =>
                     Self.Read_Declaration (Action.Full_Name);
                  when Gela.Dependency_Lists.Unit_Body =>
                     Self.Read_Body (Action.Full_Name);
                  when Gela.Dependency_Lists.Subunit =>
                     raise Constraint_Error;
               end case;
            when Gela.Dependency_Lists.Unit_Ready =>
               Self.Create_Unit (Action.Unit);

               case Action.Unit.Kind is
                  when Gela.Dependency_Lists.Subunit =>
                     declare
                        PL : constant Gela.Pass_List.Visiter_Access :=
                          new Gela.Pass_List.Visiter
                            (Action.Unit.Subunit.Enclosing_Compilation);
                     begin
                        PL.Subunit_1 (Action.Unit.Subunit);
                     end;

                  when Gela.Dependency_Lists.Unit_Body =>
                     declare
                        PL : constant Gela.Pass_List.Visiter_Access :=
                          new Gela.Pass_List.Visiter
                            (Action.Unit.Unit_Body.Enclosing_Compilation);
                     begin
                        PL.Compilation_Unit_Body_1 (Action.Unit.Unit_Body);
                     end;
                  when Gela.Dependency_Lists.Unit_Declaration =>
                     declare
                        PL : constant Gela.Pass_List.Visiter_Access :=
                          new Gela.Pass_List.Visiter
                            (Action.Unit.Unit_Declaration.
                               Enclosing_Compilation);
                     begin
                        PL.Compilation_Unit_Declaration_1
                          (Action.Unit.Unit_Declaration);
                     end;
               end case;

         end case;
      end loop;
   end Read_Dependency;

end Gela.Plain_Compilation_Managers;
