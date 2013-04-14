------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Mutables.Compilations is

   --------------------------------------
   -- Compilation_Command_Line_Options --
   --------------------------------------

   overriding function Compilation_Command_Line_Options
     (Self : access Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Options;
   end Compilation_Command_Line_Options;

   ------------------------------
   -- Compilation_CPU_Duration --
   ------------------------------

   overriding function Compilation_CPU_Duration
     (Self : access Compilation)
      return Duration is
   begin
      return Self.CPU_Spent;
   end Compilation_CPU_Duration;

   ------------
   -- Create --
   ------------

   function Create
     (Name   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String)
      return Mutable_Compilation_Access
   is
      use type League.Strings.Universal_String;

      NFKC  : constant League.Strings.Universal_String := Source.To_NFKC;
   begin
      return Result : constant Mutable_Compilation_Access :=
        new Compilation'
        (Name        => Name,
         Object_Name => Name & ".o",
         Options     => League.Strings.Empty_Universal_String,
         Text        => NFKC,
         Updated     => League.Calendars.Clock,
         CPU_Spent   => 0.0,
         Root        => <>,
         Store       => <>,
         Fabric      => <>,
         Errors      => <>,
         Lexer       => <>,
         Parser      => <>,
         Symbols     => <>)
      do
         if Source /= NFKC then
            Result.Errors.Not_In_NFKC_Warning
              (Gela.Types.Compilation_Access (Result));
         end if;

         Result.Fabric.Initialize;
         Result.Lexer.Initialize (Source);
      end return;
   end Create;

   ---------------
   -- Get_Token --
   ---------------

   overriding function Get_Token
     (Self  : access Compilation;
      Index : Positive)
      return Gela.Types.Token
   is
   begin
      return Self.Lexer.Get_Token (Index);
   end Get_Token;

   ---------------
   -- Last_Line --
   ---------------

   overriding function Last_Line
     (Self : access Compilation) return Gela.Lexical.Line_Count is
   begin
      return Self.Lexer.Last_Line;
   end Last_Line;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Self  : access Compilation;
      Index : Gela.Lexical.Line_Index) return Gela.Lexical.Line_Offset is
   begin
      return Self.Lexer.Line (Index);
   end Line;

   -----------------
   -- Object_Name --
   -----------------

   overriding function Object_Name
     (Self : access Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Object_Name;
   end Object_Name;

   -----------
   -- Start --
   -----------

   not overriding procedure Start
     (Self : not null access Compilation)
   is
   begin
      Self.Parser.Parse;
   end Start;

   -------------
   -- Symbols --
   -------------

   overriding function Symbols
     (Self  : access Compilation)
      return Gela.Types.Symbol_Set_Access is
   begin
      return Self.Symbols'Access;
   end Symbols;

   ----------
   -- Text --
   ----------

   overriding function Text
     (Self : access Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Text;
   end Text;

   ---------------
   -- Text_Name --
   ---------------

   overriding function Text_Name
     (Self : access Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Name;
   end Text_Name;

   -------------------------
   -- Time_Of_Last_Update --
   -------------------------

   overriding function Time_Of_Last_Update
     (Self : access Compilation)
      return League.Calendars.Date_Time is
   begin
      return Self.Updated;
   end Time_Of_Last_Update;

end Gela.Mutables.Compilations;
