with Gela.Compilations;
with Gela.Element_Factories;
with Gela.Element_Visiters;
with Gela.Elements.Compilations;
with Gela.LARL_Parsers;
with Gela.Lexers;
with Gela.Node_Factories;
with Gela.Pass_1;
with Gela.Pass_2;
with Gela.Pass_List;
with Gela.Plain_Compilations;
with Gela.Source_Finders;

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
         P1 : constant Gela.Pass_1.Visiter_Access :=
           new Gela.Pass_1.Visiter (C, PL);
         P2 : constant Gela.Pass_2.Visiter_Access :=
           new Gela.Pass_2.Visiter (C, PL);
      begin
         PL.Parent := PL;
         PL.P1 := Gela.Element_Visiters.Visiter_Access (P1);
         PL.P2 := Gela.Element_Visiters.Visiter_Access (P2);
         Root.Visit (P1.all);
      end;
   end Read;

   ---------------
   -- Read_Body --
   ---------------

   overriding procedure Read_Body
     (Self   : Compilation_Manager;
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
     (Self   : Compilation_Manager;
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
     (Self   : Compilation_Manager;
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
     (Self   : Compilation_Manager;
      List   : Gela.Dependency_Lists.Dependency_List_Access)
   is
      use type Gela.Lexical_Types.Symbol;

      Name         : Gela.Lexical_Types.Symbol;
      Declartion   : Boolean;
   begin
      loop
         List.Next_Required_Unit
           (Name       => Name,
            Declartion => Declartion);

         exit when Name = Gela.Lexical_Types.No_Symbol;

         if Declartion then
            Self.Read_Declaration (Symbol => Name);
         else
            Self.Read_Body (Symbol => Name);
         end if;
      end loop;
   end Read_Dependency;

end Gela.Plain_Compilation_Managers;
