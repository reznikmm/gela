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
with Gela.Elements.Symbol_Tables;
with Gela.Errors.Put_Lines;
with Gela.Lexical.Scanners;
with Gela.Lexical.Handler;

with String_Sources;

package body Gela.Compilations.Mutables is

   procedure Free is new
     Ada.Unchecked_Deallocation (Line_Offset_Array, Line_Offset_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Gela.Elements.Payload_Array, Payload_Array_Access);

   type Internal_Data is record
      Token   : Gela.Compilations.Mutables.Tokens.Token;
      Symbols : Gela.Compilations.Mutables.Folded_Sets.Folded_Set;
   end record;

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
        (Name       => Name,
         Text       => NFKC,
         Lines      => new Line_Offset_Array (1 .. 64),
         Last_Line  => 0,
         Store      => <>,
         Internal   => new Internal_Data,
         Tokens     => new Gela.Elements.Payload_Array (1 .. 16),
         Errors     => new Gela.Errors.Put_Lines.Handler,
         Last_Token => 0)
      do
         Result.Internal.Token.Compilation := Result;
         Result.Internal.Symbols.Compilation := Result;

         if Source /= NFKC then
            Result.Errors.Not_In_NFKC_Warning (Result);
         end if;
      end return;
   end Create;

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
      Symbol : Gela.Elements.Symbol_Tables.Symbol;
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
              new Gela.Elements.Payload_Array (1 .. 2 * Saved'Last);
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

end Gela.Compilations.Mutables;
