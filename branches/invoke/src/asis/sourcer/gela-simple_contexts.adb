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
with Gela.Grammars.Conflicts;
with Gela.Grammars.Constructors;
with Gela.Grammars_Convertors;
with Gela.Grammars.Reader;
with Gela.Grammars.LR.LALR;
with Gela.Lexical.Handler;
with Gela.Mutables.Compilations;
with Gela.Mutables.To_XML;

package body Gela.Simple_Contexts is

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
      LALR : League.Strings.Universal_String;
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

         LALR := Self.Default_Path;
         LALR.Append ("/src/asis/sourcer/ada-lalr.ag");

         Self.Grammar := new Gela.Grammars.Grammar'
           (Gela.Grammars_Convertors.Convert
              (Gela.Grammars.Reader.Read (LALR.To_UTF_8_String),
               Left => False));

         declare
            Resolver  : Gela.Grammars.Conflicts.Resolver;
            Augmented : constant Gela.Grammars.Grammar :=
              Gela.Grammars.Constructors.To_Augmented (Self.Grammar.all);
         begin
            Self.Table :=
              Gela.Grammars.LR.LALR.Build (Augmented, Right_Nulled => False);
            Resolver.Resolve (Augmented, Self.Table.all);
            Gela.Lexical.Handler.Initialize;
         end;
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

   ---------------
   -- Container --
   ---------------

   overriding function Container
     (Self  : access Context;
      Index : Positive) return Gela.Types.Container_Access
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Index);
   begin
      return null;
   end Container;

   -----------------
   -- Debug_Image --
   -----------------

   overriding function Debug_Image
     (Self : access Context) return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      AST : constant League.Strings.Universal_String :=
        Self.Default_Path & "/src/asis/context/ada-ast.ag";
      G : constant Gela.Grammars.Grammar :=
        Gela.Grammars.Reader.Read (AST.To_UTF_8_String);
   begin
      return Gela.Mutables.To_XML.Compilation (Self.Comp, G);
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
      procedure Free is new Ada.Unchecked_Deallocation
        (Gela.Grammars.Grammar, Gela.Grammars.Grammar_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Gela.Grammars.LR_Tables.Table, Gela.Grammars.LR_Tables.Table_Access);
   begin
      Free (Self.Errors);
      Free (Self.Grammar);
      Free (Self.Table);
   end Dissociate;

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
      return 0;
   end Length;

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
   -- Open --
   ----------

   overriding procedure Open (Self : access Context) is
      Found : Boolean;
      Name  : League.Strings.Universal_String;
      Text  : League.Strings.Universal_String;
   begin
      Self.Finder := Gela.Source_Finders.Create
        (Self.Path, Self.Schema'Access);

      Self.Finder.Lookup_File (Self.File_Name, Found, Name, Text);

      if not Found then
         Self.On_Error
           ("File not found:" & Self.File_Name.To_UTF_16_Wide_String);
      end if;

      Self.Comp := Gela.Mutables.Compilations.Create
        (Self.File_Name, Text, Self.Errors, Self.Grammar, Self.Table);

      Self.Comp.Start;

      if Self.Comp.Root.Object = null then
         Self.On_Error
           ("Syntax error :" & Self.File_Name.To_UTF_16_Wide_String);
      end if;

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
         Self.On_Error ("Singe unit name expected in Parameters");
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

end Gela.Simple_Contexts;
