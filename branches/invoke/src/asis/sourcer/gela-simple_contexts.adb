------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with League.String_Vectors;
with League.Application;

with Gela.Errors.Put_Lines;
with Gela.Lexical.Handler;
with Gela.Compilation_Units; pragma Unreferenced (Gela.Compilation_Units);
with Gela.Simple_Contexts.Loaders;
with Gela.Simple_Compilation_Units;

package body Gela.Simple_Contexts is

   type Unit_List_Access is access all Unit_List;
   type Symbol_Set_Access is access all Mutables.Symbol_Sets.Symbol_Set;

   -------------------------------
   -- Add_Compilation_Unit_Body --
   -------------------------------

   not overriding procedure Add_Compilation_Unit_Body
     (Self      : access Context;
      Full_Name : Gela.Types.Symbol;
      Unit      : Gela.Types.Compilation_Unit) is
   begin
      Self.Bodies.Map.Insert (Full_Name, Unit);
      Self.Units.Insert (Unit.Payload, Unit.Object);
   end Add_Compilation_Unit_Body;

   ----------------------------------
   -- Add_Library_Unit_Declaration --
   ----------------------------------

   not overriding procedure Add_Library_Unit_Declaration
     (Self      : access Context;
      Full_Name : Gela.Types.Symbol;
      Unit      : Gela.Types.Compilation_Unit) is
   begin
      Self.Specs.Map.Insert (Full_Name, Unit);
      Self.Units.Insert (Unit.Payload, Unit.Object);
   end Add_Library_Unit_Declaration;

   ---------------
   -- Associate --
   ---------------

   overriding procedure Associate
     (Self       : access Context;
      Name       : League.Strings.Universal_String;
      Parameters : League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;
      use type Gela.Errors.Error_Handler_Access;
   begin
      Self.Name := Name;
      Self.Parameters := Parameters;
      Self.Is_Open := False;
      Self.File_Name.Clear;
      Self.Path := Self.Default_Path;
      Self.Debug.Clear;
      Self.Parse_Parameters;

      if Self.Errors = null then
         Self.Errors := new Gela.Errors.Put_Lines.Handler;

         Gela.Lexical.Handler.Initialize;

         Self.Loader := new Gela.Simple_Contexts.Loaders.Loader
           (Context_Access (Self));
      end if;
   end Associate;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Self : access Context) is
   begin
      Gela.Source_Finders.Destroy (Self.Finder);
      Self.Is_Open := False;
   end Close;

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   overriding function Compilation_Unit_Bodies
     (Self  : access Context)
      return Gela.Types.Compilation_Unit_List
   is
      List : constant Unit_List_Access := Self.Bodies'Access;
   begin
      return (Gela.Types.Compilation_Unit_List_Access (List), 0);
   end Compilation_Unit_Bodies;

   ---------------------------
   -- Compilation_Unit_Body --
   ---------------------------

   not overriding function Compilation_Unit_Body
     (Self  : access Context;
      Name  : Gela.Types.Symbol)
      return Gela.Types.Compilation_Unit
   is
      Pos : constant Unit_Maps.Cursor := Self.Bodies.Map.Find (Name);
   begin
      if Unit_Maps.Has_Element (Pos) then
         return Unit_Maps.Element (Pos);
      end if;

      return (null, 0);
   end Compilation_Unit_Body;

   ---------------------------
   -- Compilation_Unit_Body --
   ---------------------------

   overriding function Compilation_Unit_Body
     (Self  : access Context;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit is
   begin
      return Self.Compilation_Unit_Body
        (Self.Symbols.Get (Name.To_Simple_Casefold));
   end Compilation_Unit_Body;

   ---------------
   -- Container --
   ---------------

   overriding function Container
     (Self  : access Context;
      Index : Positive) return Gela.Types.Container_Access
   is
      This : constant Context_Access := Context_Access (Self);
   begin
      if Index = 1 then
         return Gela.Types.Container_Access (This);
      else
         raise Constraint_Error;
      end if;
   end Container;

   ----------------------------
   -- Corresponding_Children --
   ----------------------------

   overriding function Corresponding_Children
     (Self   : access Context;
      Parent : Gela.Types.Compilation_Unit)
      return Gela.Types.Compilation_Unit_List
   is
      pragma Unreferenced (Self);
      use Gela.Simple_Compilation_Units;
      Unit : constant Simple_Compilation_Unit_Access :=
        Simple_Compilation_Unit_Access (Parent.Object);
   begin
      return Unit.Children (Parent.Payload);
   end Corresponding_Children;

   -----------------
   -- Debug_Image --
   -----------------

   overriding function Debug_Image
     (Self : access Context) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
--        use type League.Strings.Universal_String;
--        AST : constant League.Strings.Universal_String :=
--          Self.Default_Path & "/src/asis/context/ada-ast.ag";
--        G : constant Gela.Grammars.Grammar :=
--          Gela.Grammars.Reader.Read (AST.To_UTF_8_String);
   begin
      --        return Gela.Mutables.To_XML.Compilation (Self.Comp, G);
      return League.Strings.Empty_Universal_String;
   end Debug_Image;

   ------------------
   -- Default_Path --
   ------------------

   function Default_Path
     (Self : access Context) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
      Gela_Include_Path : constant League.Strings.Universal_String :=
        League.Strings.To_Universal_String ("GELA_INCLUDE_PATH");
   begin
      return League.Application.Environment.Value (Gela_Include_Path);
   end Default_Path;

   ----------------
   -- Dissociate --
   ----------------

   overriding procedure Dissociate (Self : access Context) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Gela.Errors.Error_Handler'Class, Gela.Errors.Error_Handler_Access);
   begin
      Free (Self.Errors);
   end Dissociate;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self    : access Unit_List;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      Symbol : constant Gela.Types.Symbol := Gela.Types.Symbol (Payload);
   begin
      return Self.Map.Element (Symbol);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First
     (Self    : access Unit_List;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor
   is
      pragma Unreferenced (Payload);
      This : constant
        Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_List_Access :=
          Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_List_Access
            (Self);
   begin
      if Self.Map.Is_Empty then
         return (null, 0);
      end if;

      return (Gela.Types.Compilation_Unit_Cursor_Access (This),
              Gela.Types.Payload (Self.Map.First_Key));
   end First;

   -------------
   -- Is_Open --
   -------------

   overriding function Is_Open (Self : access Context) return Boolean is
   begin
      return Self.Is_Open;
   end Is_Open;

   ------------
   -- Length --
   ------------

   overriding function Length (Self : access Context) return Natural is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Length;

   ------------------------------
   -- Library_Unit_Declaration --
   ------------------------------

   not overriding function Library_Unit_Declaration
     (Self  : access Context;
      Name  : Gela.Types.Symbol)
      return Gela.Types.Compilation_Unit
   is
      Pos : constant Unit_Maps.Cursor := Self.Specs.Map.Find (Name);
   begin
      if Unit_Maps.Has_Element (Pos) then
         return Unit_Maps.Element (Pos);
      end if;

      return (null, 0);
   end Library_Unit_Declaration;

   ------------------------------
   -- Library_Unit_Declaration --
   ------------------------------

   overriding function Library_Unit_Declaration
     (Self  : access Context;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit is
   begin
      return Self.Library_Unit_Declaration
        (Self.Symbols.Get (Name.To_Simple_Casefold));
   end Library_Unit_Declaration;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   overriding function Library_Unit_Declarations
     (Self  : access Context)
      return Gela.Types.Compilation_Unit_List
   is
      List : constant Unit_List_Access := Self.Specs'Access;
   begin
      return (Gela.Types.Compilation_Unit_List_Access (List), 0);
   end Library_Unit_Declarations;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : access Context)
      return League.Strings.Universal_String is
   begin
      return Self.Name;
   end Name;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self    : access Unit_List;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor
   is
      use type Gela.Types.Symbol;
      Symbol : constant Gela.Types.Symbol := Gela.Types.Symbol (Payload);
      Next   : constant Unit_Maps.Cursor := Self.Map.Ceiling (Symbol + 1);
   begin
      if Unit_Maps.Has_Element (Next) then
         return (Gela.Types.Compilation_Unit_Cursor_Access (Self),
                 Gela.Types.Payload (Unit_Maps.Key (Next)));
      end if;

      return (null, 0);
   end Next;

   ----------
   -- Open --
   ----------

   overriding procedure Open (Self : access Context) is
   begin
      Self.Finder := Gela.Source_Finders.Create
        (Self.Path, Self.Schema'Access);

      Self.Loader.Try_Read_File_And_Supporters
        (File_Name => Self.File_Name);

      Self.Is_Open := True;
   end Open;

   ----------------
   -- Parameters --
   ----------------

   overriding function Parameters
     (Self : access Context)
      return League.Strings.Universal_String is
   begin
      return Self.Parameters;
   end Parameters;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self : access Context)
      return Gela.Types.Context_Access
   is
      This : constant Context_Access := Context_Access (Self);
   begin
      return Gela.Types.Context_Access (This);
   end Parent;

   ----------------------
   -- Parse_Parameters --
   ----------------------

   procedure Parse_Parameters (Self : access Context) is
      use League.Strings;
      use League.String_Vectors;

      procedure Add_Path (Arg : Universal_String);
      procedure Singe_Unit_Expected;

      --------------
      -- Add_Path --
      --------------

      procedure Add_Path (Arg : Universal_String) is
      begin
         if not Self.Path.Is_Empty then
            Self.Path.Append (":");
         end if;
         Self.Path.Append (Arg);
      end Add_Path;

      -------------------------
      -- Singe_Unit_Expected --
      -------------------------

      procedure Singe_Unit_Expected is
      begin
         Self.On_Error.Singe_File_Expected;
         --  ("Singe unit name expected in Parameters");
      end Singe_Unit_Expected;

      Next_Is_Path : Boolean := False;
      Args         : Universal_String_Vector;
   begin
      Args := Self.Parameters.Split (' ', Skip_Empty);

      for J in 1 .. Args.Length loop
         declare
            Arg : constant Universal_String := Args.Element (J);
         begin
            if Arg.Starts_With ("-A") then
               Self.Debug.Append (Arg.Slice (3, Arg.Length));
            elsif Arg.Starts_With ("-I") then
               if Arg.Length = 2 then
                  Next_Is_Path := True;
               else
                  Add_Path (Arg.Slice (3, Arg.Length));
                  Next_Is_Path := False;
               end if;
            elsif Next_Is_Path then
               Add_Path (Arg);
               Next_Is_Path := False;
            elsif Self.File_Name.Is_Empty then
               Self.File_Name := Arg;
            else
               Singe_Unit_Expected;
            end if;
         end;
      end loop;

      if Self.File_Name.Is_Empty then
         Singe_Unit_Expected;
      end if;
   end Parse_Parameters;

   -------------
   -- Symbols --
   -------------

   not overriding function Symbols
     (Self    : access Context) return Gela.Types.Symbol_Set_Access
   is
      Set : constant Symbol_Set_Access := Self.Symbols'Access;
   begin
      return Gela.Types.Symbol_Set_Access (Set);
   end Symbols;

   -----------------
   -- Units_Count --
   -----------------

   overriding function Units_Count
     (Self    : access Unit_List;
      Payload : Gela.Types.Payload) return Natural
   is
      pragma Unreferenced (Payload);
   begin
      return Natural (Self.Map.Length);
   end Units_Count;

end Gela.Simple_Contexts;
