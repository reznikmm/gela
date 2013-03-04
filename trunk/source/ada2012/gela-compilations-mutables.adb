------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO;

with Gela.Compilations.Mutables.Folded_Sets;
with Gela.Compilations.Mutables.Tokens;
with Gela.Errors.Put_Lines;
with Gela.Lexical.Scanners;
with Gela.Lexical.Handler;

with String_Sources;

package body Gela.Compilations.Mutables is

   procedure Free is new
     Ada.Unchecked_Deallocation (Line_Offset_Array, Line_Offset_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Gela.Types.Payload_Array, Payload_Array_Access);

   type Internal_Data is record
      Token   : aliased Gela.Compilations.Mutables.Tokens.Token;
      Symbols : aliased Gela.Compilations.Mutables.Folded_Sets.Folded_Set;
   end record;

   --------------------------------------
   -- Compilation_Command_Line_Options --
   --------------------------------------

   overriding function Compilation_Command_Line_Options
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Options;
   end Compilation_Command_Line_Options;

   ------------------------------
   -- Compilation_CPU_Duration --
   ------------------------------

   overriding function Compilation_CPU_Duration
     (Self : access Mutable_Compilation)
      return Duration is
   begin
      return Self.CPU_Spent;
   end Compilation_CPU_Duration;

   ------------
   -- Create --
   ------------

   not overriding function Create
     (Name   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String)
      return Mutable_Compilation_Access
   is
      use type League.Strings.Universal_String;

      NFKC  : constant League.Strings.Universal_String := Source.To_NFKC;
   begin
      return Result : constant Mutable_Compilation_Access :=
        new Mutable_Compilation'
        (Name        => Name,
         Object_Name => Name & ".o",
         Options     => League.Strings.Empty_Universal_String,
         Text        => NFKC,
         Lines       => new Line_Offset_Array (1 .. 64),
         Last_Line   => 0,
         Store       => <>,
         Internal    => new Internal_Data,
         Tokens      => new Gela.Types.Payload_Array (1 .. 16),
         Errors      => new Gela.Errors.Put_Lines.Handler,
         Last_Token  => 0,
         Updated     => League.Calendars.Clock,
         CPU_Spent   => 0.0)
      do
         Result.Internal.Token.Compilation := Result;
         Result.Internal.Symbols.Compilation := Result;

         if Source /= NFKC then
            Result.Errors.Not_In_NFKC_Warning
              (Gela.Types.Compilation_Access (Result));
         end if;
      end return;
   end Create;

   -----------------
   -- First_Token --
   -----------------

   overriding function First_Token
     (Self  : access Mutable_Compilation)
      return Gela.Types.Token is
   begin
      return (Self.Internal.Token'Access,
              Self.Tokens (Self.Tokens'First));
   end First_Token;

   ----------------
   -- Folded_Set --
   ----------------

   overriding function Folded_Set
     (Self  : access Mutable_Compilation)
      return Gela.Types.Folded_Set_Access is
   begin
      return Self.Internal.Symbols'Access;
   end Folded_Set;

   ---------------
   -- Last_Line --
   ---------------

   overriding function Last_Line
     (Self : access Mutable_Compilation) return Gela.Lexical.Line_Count is
   begin
      return Self.Last_Line;
   end Last_Line;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Self  : access Mutable_Compilation;
      Index : Gela.Lexical.Line_Index) return Line_Offset is
   begin
      return Self.Lines (Index);
   end Line;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self    : access Mutable_Compilation;
      First   : Text_Index;
      Last    : Text_Index;
      Comment : Text_Index)
   is
      use type Gela.Lexical.Line_Count;
   begin
      Self.Last_Line := Self.Last_Line + 1;

      if Self.Last_Line not in Self.Lines'Range then
         declare
            Saved : Line_Offset_Array_Access := Self.Lines;
         begin
            Self.Lines := new Line_Offset_Array (1 .. 2 * Saved'Last);
            Self.Lines (Saved'Range) := Saved.all;
            Free (Saved);
         end;
      end if;

      Self.Lines (Self.Last_Line) :=
        (First => First, Last => Last, Comment => Comment);
   end New_Line;

   ---------------
   -- New_Token --
   ---------------

   overriding procedure New_Token
     (Self      : access Mutable_Compilation;
      Token     : Gela.Lexical.Tokens.Token;
      Line      : Positive;
      First     : Text_Index;
      Last      : Text_Index;
      Separator : Text_Index;
      Folded    : League.Strings.Universal_String)
   is
      use Gela.Lexical.Tokens;
      Symbol : Gela.Types.Symbol;
   begin
      if Token in
        Identifier_Token | Character_Literal_Token | String_Literal_Token
      then
         Self.Internal.Symbols.Append (Folded, Symbol);
      end if;

      if Self.Last_Token = Self.Tokens'Last then
         declare
            Saved : Payload_Array_Access := Self.Tokens;
         begin
            Self.Tokens :=
              new Gela.Types.Payload_Array (1 .. 2 * Saved'Last);
            Self.Tokens (Saved'Range) := Saved.all;
            Free (Saved);
         end;
      end if;

      Self.Last_Token := Self.Last_Token + 1;
      Self.Internal.Token.Create
        (Self.Tokens (Self.Last_Token),
         Token,
         Gela.Lexical.Line_Index (Line),
         First,
         Last,
         Separator,
         Symbol);
   end New_Token;

   -----------------
   -- Object_Name --
   -----------------

   overriding function Object_Name
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Object_Name;
   end Object_Name;

   -----------
   -- Start --
   -----------

   not overriding procedure Start
     (Self : not null access Mutable_Compilation)
   is
      Scanner : aliased Gela.Lexical.Scanners.Scanner;
      Source  : aliased String_Sources.String_Source;
      Handler : aliased Gela.Lexical.Handler.Handler;
   begin
      Scanner.Set_Source (Source'Unchecked_Access);
      Scanner.Set_Handler (Handler'Unchecked_Access);
      Handler.Set_Fabric (Gela.Lexical.Fabrics.Fabric_Access (Self));
      Source.Create (Self.Text);

      loop
         declare
            Token : Gela.Lexical.Tokens.Token;
         begin
            Scanner.Get_Token (Token);

            Ada.Wide_Wide_Text_IO.Put
              (Gela.Lexical.Tokens.Token'Wide_Wide_Image (Token));

            exit when Token in Gela.Lexical.Tokens.End_Of_Input
              | Gela.Lexical.Tokens.Error;

            Ada.Wide_Wide_Text_IO.Put (": ");

            Ada.Wide_Wide_Text_IO.Put_Line
              (Scanner.Get_Text.To_Wide_Wide_String);
         end;
      end loop;
   end Start;

   ----------
   -- Text --
   ----------

   overriding function Text
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Text;
   end Text;

   ---------------
   -- Text_Name --
   ---------------

   overriding function Text_Name
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Name;
   end Text_Name;

   -------------------------
   -- Time_Of_Last_Update --
   -------------------------

   overriding function Time_Of_Last_Update
     (Self : access Mutable_Compilation)
      return League.Calendars.Date_Time is
   begin
      return Self.Updated;
   end Time_Of_Last_Update;

end Gela.Compilations.Mutables;
