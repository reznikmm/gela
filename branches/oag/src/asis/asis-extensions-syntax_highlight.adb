with Asis.Gela.Scanners;
with Asis.Gela.Parser_Tokens;
with Asis.Gela.Scanner_Tables;
with Asis.Gela.Classificators_Ada_UTF_16;

with Gela; use Gela;
with Gela.Source_Buffers.Wide_Strings;

package body Asis.Extensions.Syntax_Highlight is

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Text  : in     Wide_String;
      State : in out Natural)
   is
      use Asis.Gela;
      use Source_Buffers;
      use type Asis.Gela.Parser_Tokens.Token;
      use type Source_Buffers.Cursor;

      Classificator : aliased Classificators_Ada_UTF_16.Classificator;
      Scanner       : Scanners.Scanner (Classificator'Access);
      Source_Buffer : Wide_Strings.Source_Buffer;
      Token         : Parser_Tokens.Token;
      Start         : Source_Buffers.Cursor;
      From, To      : Source_Buffers.Cursor;

      procedure Set_Token (Kind : Token_Kinds) is
      begin
         if Kind = Error then
            --  Mark as Error piece (From .. Text'Last)
            Set_Token
              (From  => (From - Start) / 2 + Text'First,
               Count => Text'Length - (From - Start) / 2,
               Kind  => Kind);
         else
            Set_Token
              (From  => (From - Start) / 2 + Text'First,
               Count => (To   - From) / 2,
               Kind  => Kind);
         end if;
      end Set_Token;

   begin
      Wide_Strings.Initialize (Source_Buffer, Text);
      Start := Wide_Strings.Buffer_Start (Source_Buffer);
      Scanners.Initialize (Scanner, Start);
      Scanners.Enter (Scanner, Scanner_Tables.State (State));

      loop
         Scanners.Next_Token (Scanner, Token);

         exit when Token = Parser_Tokens.End_Of_Input;

         Scanners.Token_Span (Scanner, From, To);

         case Token is
            when Parser_Tokens.Comment_Token =>
               Set_Token (Comment);

            when Parser_Tokens.Identifier_Token =>
               Set_Token (Identifier);
               Scanners.Enter (Scanner, Scanner_Tables.Allow_Keyword);
               State := Natural (Scanner_Tables.Allow_Keyword);

            when Parser_Tokens.Integer_Literal_Token |
              Parser_Tokens.Real_Literal_Token |
              Parser_Tokens.Character_Literal_Token |
              Parser_Tokens.String_Literal_Token
              =>
               Set_Token (Literal);
               Scanners.Enter (Scanner, Scanner_Tables.Allow_Char);
               State := Natural (Scanner_Tables.Allow_Char);

            when Parser_Tokens.Double_Star_Token ..
              Parser_Tokens.Less_Token |
              Parser_Tokens.Left_Parenthesis_Token ..
              Parser_Tokens.Vertical_Line_Token
              =>
               Set_Token (Delimiter);
               Scanners.Enter (Scanner, Scanner_Tables.Allow_Char);
               State := Natural (Scanner_Tables.Allow_Char);

            when Parser_Tokens.Abort_Token .. Parser_Tokens.Raise_Token |
              Parser_Tokens.Record_Token .. Parser_Tokens.Xor_Token =>
               Set_Token (Keyword);
               Scanners.Enter (Scanner, Scanner_Tables.Allow_Char);
               State := Natural (Scanner_Tables.Allow_Char);

            when Parser_Tokens.Range_Token =>
               if State = Natural (Scanner_Tables.Default) then
                  --  Treat range as identifier after apostrophe
                  Set_Token (Identifier);
                  Scanners.Enter (Scanner, Scanner_Tables.Allow_Keyword);
                  State := Natural (Scanner_Tables.Allow_Keyword);
               else
                  Set_Token (Keyword);
                  Scanners.Enter (Scanner, Scanner_Tables.Allow_Char);
                  State := Natural (Scanner_Tables.Allow_Char);
               end if;
            when Parser_Tokens.Apostrophe_Token =>
               Set_Token (Delimiter);
               Scanners.Enter (Scanner, Scanner_Tables.Default);
               State := Natural (Scanner_Tables.Default);

            when Parser_Tokens.Error =>
               Set_Token (Error);
               exit;
            when others =>
               null;
         end case;
      end loop;

      Wide_Strings.Clear (Source_Buffer);
   end Parse;

end Asis.Extensions.Syntax_Highlight;
