with Gela.Compilations;
with Gela.Element_Factories;
with Gela.Elements.Compilations;
with Gela.LARL_Parsers;
with Gela.Lexers;
with Gela.Node_Factories;
with Gela.Pass_List;
with Gela.Plain_Compilations;
with Gela.Source_Finders;
with Gela.Elements.Library_Unit_Declarations;
with Gela.Symbol_Sets;
with Gela.Semantic_Types;

package body Gela.Plain_Compilation_Managers is

   procedure Read
     (Self   : Compilation_Manager'Class;
      File   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String);

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
      Lib    : Gela.Elements.Library_Unit_Declarations.
        Library_Unit_Declaration_Access;
      Set    : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Self.Context.Symbols;
      Upper  : Gela.Compilation_Units.Package_Unit_Access;
      Decl   : Gela.Compilation_Units.Library_Unit_Declaration_Access;
      Parent : Gela.Compilation_Units.Body_Unit_Access;
      Sub    : Gela.Compilation_Units.Subunit_Access;
   begin
      case Item.Kind is
         when Unit_Declaration =>
            Up := Set.Prefix (Item.Name);

            if Up /= Gela.Lexical_Types.No_Symbol then
               Upper := Self.Packages.Element (Up);
            end if;

            Decl := Self.Factory.Create_Library_Unit_Declaration
              (Parent => Upper,
               Name   => Item.Name,
               Node   => Item.Unit_Declaration);

            Self.Specs.Insert (Item.Name, Decl);

            Lib := Item.Unit_Declaration.Unit_Declaration;

            case Lib.Unit_Kind is
               when Gela.Semantic_Types.A_Package |
                    Gela.Semantic_Types.A_Generic_Package |
                    Gela.Semantic_Types.A_Package_Instance =>

                  Upper := Gela.Compilation_Units.Package_Unit_Access (Decl);
                  Self.Packages.Insert (Item.Name, Upper);
               when others =>
                  null;
            end case;

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
     (Self   : Compilation_Manager'Class;
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
         Source    => Source);

      Lexer.Scan (Source, Comp);

      Parser.Parse
        (Input      => Comp,
         Factory    => Gela.Element_Factories.Element_Factory_Access (Factory),
         Root       => Root,
         Last_Token => Last);

      declare
         PL : constant Gela.Pass_List.Visiter_Access :=
           new Gela.Pass_List.Visiter (C);
      begin
         PL.Compilation_1 (Root);
      end;
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
         end case;
      end loop;
   end Read_Dependency;

end Gela.Plain_Compilation_Managers;
