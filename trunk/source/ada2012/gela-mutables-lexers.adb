------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Gela.Mutables.Compilations;

package body Gela.Mutables.Lexers is

   procedure Free is new
     Ada.Unchecked_Deallocation (Line_Offset_Array, Line_Offset_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Gela.Types.Payload_Array, Payload_Array_Access);

   ---------------
   -- Get_Token --
   ---------------

   not overriding function Get_Token
     (Self  : access Lexer;
      Index : Positive)
      return Gela.Types.Token is
   begin
      return (Self.Compilation.Fabric.Token'Access, Self.Tokens (Index));
   end Get_Token;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self : access Lexer;
      Text : League.Strings.Universal_String) is
   begin
      if Self.Lines = null then
         Self.Lines := new Line_Offset_Array (1 .. 64);
      end if;

      if Self.Tokens = null then
         Self.Tokens := new Gela.Types.Payload_Array (1 .. 16);
      end if;

      Self.Scanner.Set_Source (Self.Source'Unchecked_Access);
      Self.Scanner.Set_Handler (Self.Handler'Unchecked_Access);
      Self.Handler.Set_Fabric (Gela.Lexical.Fabrics.Fabric_Access (Self));
      Self.Source.Create (Text);
   end Initialize;

   ---------------
   -- Last_Line --
   ---------------

   not overriding function Last_Line
     (Self : access Lexer) return Gela.Lexical.Line_Count is
   begin
      return Self.Last_Line;
   end Last_Line;

   ----------------
   -- Last_Token --
   ----------------

   not overriding function Last_Token
     (Self : access Lexer) return Gela.Types.Payload is
   begin
      return Self.Tokens (Self.Last_Token);
   end Last_Token;

   ----------
   -- Line --
   ----------

   not overriding function Line
     (Self  : access Lexer;
      Index : Gela.Lexical.Line_Index) return Gela.Lexical.Line_Offset is
   begin
      return Self.Lines (Index);
   end Line;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self    : access Lexer;
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
     (Self      : access Lexer;
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
         Gela.Mutables.Compilations.Symbols
           (Self.Compilation).Append (Folded, Symbol);
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
      Self.Tokens (Self.Last_Token) := Self.Compilation.Fabric.Create_Token;

      Self.Compilation.Fabric.Token.Initialize
        (Self.Tokens (Self.Last_Token),
         Token,
         Gela.Lexical.Line_Index (Line),
         First,
         Last,
         Separator);
   end New_Token;

   use Gela.Lexical.Tokens;

   Map : constant array (Gela.Lexical.Tokens.Token)
     of Gela.Grammars.Terminal_Count :=
     (Error                   => 0,
      End_Of_Input            => 0,
      Abort_Token             => 1,
      Abs_Token               => 2,
      Abstract_Token          => 3,
      Accept_Token            => 4,
      Access_Token            => 5,
      Aliased_Token           => 6,
      All_Token               => 7,
      Ampersand_Token         => 8,
      And_Token               => 9,
      Apostrophe_Token        => 10,
      Array_Token             => 11,
      Arrow_Token             => 12,
      Assignment_Token        => 13,
      At_Token                => 14,
      Begin_Token             => 15,
      Body_Token              => 16,
      Box_Token               => 17,
      Case_Token              => 18,
      Character_Literal_Token => 19,
      Colon_Token             => 20,
      Comma_Token             => 21,
      Comment_Token           => 22,
      Constant_Token          => 23,
      Declare_Token           => 24,
      Delay_Token             => 25,
      Delta_Token             => 26,
      Digits_Token            => 27,
      Do_Token                => 28,
      Dot_Token               => 29,
      Double_Dot_Token        => 30,
      Double_Star_Token       => 31,
      Else_Token              => 32,
      Elsif_Token             => 33,
      End_Token               => 34,
      Entry_Token             => 35,
      Equal_Token             => 36,
      Exception_Token         => 37,
      Exit_Token              => 38,
      For_Token               => 39,
      Function_Token          => 40,
      Generic_Token           => 41,
      Goto_Token              => 42,
      Greater_Or_Equal_Token  => 43,
      Greater_Token           => 44,
      Hyphen_Token            => 45,
      Identifier_Token        => 46,
      If_Token                => 47,
      In_Token                => 48,
      Inequality_Token        => 49,
      Interface_Token         => 50,
      Is_Token                => 51,
      Left_Label_Token        => 52,
      Left_Parenthesis_Token  => 53,
      Less_Or_Equal_Token     => 54,
      Less_Token              => 55,
      Limited_Token           => 56,
      Loop_Token              => 57,
      Mod_Token               => 58,
      New_Token               => 59,
      Not_Token               => 60,
      Null_Token              => 61,
      Numeric_Literal_Token   => 62,
      Of_Token                => 63,
      Or_Token                => 64,
      Others_Token            => 65,
      Out_Token               => 66,
      Overriding_Token        => 67,
      Package_Token           => 68,
      Plus_Token              => 69,
      Pragma_Token            => 70,
      Private_Token           => 71,
      Procedure_Token         => 72,
      Protected_Token         => 73,
      Raise_Token             => 74,
      Range_Token             => 75,
      Record_Token            => 76,
      Rem_Token               => 77,
      Renames_Token           => 78,
      Requeue_Token           => 79,
      Return_Token            => 80,
      Reverse_Token           => 81,
      Right_Label_Token       => 82,
      Right_Parenthesis_Token => 83,
      Select_Token            => 84,
      Semicolon_Token         => 85,
      Separate_Token          => 86,
      Slash_Token             => 87,
      Some_Token              => 88,
      Star_Token              => 89,
      String_Literal_Token    => 90,
      Subtype_Token           => 91,
      Synchronized_Token      => 92,
      Tagged_Token            => 93,
      Task_Token              => 94,
      Terminate_Token         => 95,
      Then_Token              => 96,
      Type_Token              => 97,
      Until_Token             => 98,
      Use_Token               => 99,
      Vertical_Line_Token     => 100,
      When_Token              => 101,
      While_Token             => 102,
      With_Token              => 103,
      Xor_Token               => 104);

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self : in out Lexer)
      return Gela.Grammars.Terminal_Count
   is
      Token : Gela.Lexical.Tokens.Token;
   begin
      Self.Scanner.Get_Token (Token);
      return Map (Token);
   end Next;

end Gela.Mutables.Lexers;
