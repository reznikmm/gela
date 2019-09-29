--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Anagram.Grammars.LR_Parsers;

with Program.Parsers.Nodes;
with Program.Parsers.Data;
with Program.Parsers.On_Reduce;

package body Program.Parsers is

   procedure Next_Token
     (Self  : access Parse_Context;
      Token : out Anagram.Grammars.Terminal_Count;
      Value : out Program.Parsers.Nodes.Node);

   procedure Do_Parse is new Anagram.Grammars.LR_Parsers.Parse
     (Node        => Program.Parsers.Nodes.Node,
      Node_Array  => Program.Parsers.Nodes.Node_Array,
      Lexer       => Parse_Context,
      Parser      => Parse_Context,
      Next_Token  => Next_Token,
      Next_Action => Program.Parsers.Data.Next_Action,
      Go_To       => Program.Parsers.Data.Go_To,
      On_Reduce   => Program.Parsers.On_Reduce);

   use all type Program.Lexical_Elements.Lexical_Element_Kind;

   Map : constant array (Program.Lexical_Elements.Lexical_Element_Kind)
     of Anagram.Grammars.Terminal_Count :=
       (Error                => 0,
        End_Of_Input         => 0,
        Abort_Keyword        => 1,
        Abs_Keyword          => 2,
        Abstract_Keyword     => 3,
        Accept_Keyword       => 4,
        Access_Keyword       => 5,
        Aliased_Keyword      => 6,
        All_Keyword          => 7,
        Ampersand            => 8,
        And_Keyword          => 9,
        Apostrophe           => 10,
        Array_Keyword        => 11,
        Arrow                => 12,
        Assignment           => 13,
        At_Keyword           => 14,
        Begin_Keyword        => 15,
        Body_Keyword         => 16,
        Box                  => 17,
        Case_Keyword         => 18,
        Character_Literal    => 19,
        Colon                => 20,
        Comma                => 21,
        Comment              => 22,
        Constant_Keyword     => 23,
        Declare_Keyword      => 24,
        Delay_Keyword        => 25,
        Delta_Keyword        => 26,
        Digits_Keyword       => 27,
        Do_Keyword           => 28,
        Dot                  => 29,
        Double_Dot           => 30,
        Double_Star          => 31,
        Else_Keyword         => 32,
        Elsif_Keyword        => 33,
        End_Keyword          => 34,
        Entry_Keyword        => 35,
        Equal                => 36,
        Exception_Keyword    => 37,
        Exit_Keyword         => 38,
        For_Keyword          => 39,
        Function_Keyword     => 40,
        Generic_Keyword      => 41,
        Goto_Keyword         => 42,
        Greater_Or_Equal     => 43,
        Greater              => 44,
        Hyphen               => 45,
        Identifier           => 46,
        If_Keyword           => 47,
        In_Keyword           => 48,
        Inequality           => 49,
        Interface_Keyword    => 50,
        Is_Keyword           => 51,
        Left_Label           => 52,
        Left_Parenthesis     => 53,
        Less_Or_Equal        => 54,
        Less                 => 55,
        Limited_Keyword      => 56,
        Loop_Keyword         => 57,
        Mod_Keyword          => 58,
        New_Keyword          => 59,
        Not_Keyword          => 60,
        Null_Keyword         => 61,
        Numeric_Literal      => 62,
        Of_Keyword           => 63,
        Or_Keyword           => 64,
        Others_Keyword       => 65,
        Out_Keyword          => 66,
        Overriding_Keyword   => 67,
        Package_Keyword      => 68,
        Plus                 => 69,
        Pragma_Keyword       => 70,
        Private_Keyword      => 71,
        Procedure_Keyword    => 72,
        Protected_Keyword    => 73,
        Raise_Keyword        => 74,
        Range_Keyword        => 75,
        Record_Keyword       => 76,
        Rem_Keyword          => 77,
        Renames_Keyword      => 78,
        Requeue_Keyword      => 79,
        Return_Keyword       => 80,
        Reverse_Keyword      => 81,
        Right_Label          => 82,
        Right_Parenthesis    => 83,
        Select_Keyword       => 84,
        Semicolon            => 85,
        Separate_Keyword     => 86,
        Slash                => 87,
        Some_Keyword         => 88,
        Star                 => 89,
        String_Literal       => 90,
        Subtype_Keyword      => 91,
        Synchronized_Keyword => 92,
        Tagged_Keyword       => 93,
        Task_Keyword         => 94,
        Terminate_Keyword    => 95,
        Then_Keyword         => 96,
        Type_Keyword         => 97,
        Until_Keyword        => 98,
        Use_Keyword          => 99,
        Vertical_Line        => 100,
        When_Keyword         => 101,
        While_Keyword        => 102,
        With_Keyword         => 103,
        Xor_Keyword          => 104);

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Self  : access Parse_Context;
      Token : out Anagram.Grammars.Terminal_Count;
      Value : out Program.Parsers.Nodes.Node)
   is
      Next : Program.Lexical_Elements.Lexical_Element_Access;
      Kind : Program.Lexical_Elements.Lexical_Element_Kind;
   begin
      if Self.Index <= Self.Tokens.Last_Index then
         Next := Self.Tokens.Element (Self.Index);
         Kind := Next.Kind;

         Token := Map (Kind);
         Value := Self.Factory.Token (Next);
         Self.Index := Self.Index + 1;
      else
         Token := 0;
         Value := Program.Parsers.Nodes.No_Token;
      end if;
   end Next_Token;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Compilation : not null Program.Compilations.Compilation_Access;
      Tokens      : not null Lexical_Elements.Lexical_Element_Vector_Access;
      Subpool     : not null System.Storage_Pools.Subpools.Subpool_Handle;
      Units       : out Unit_Vectors.Vector;
      Pragmas     : out Element_Vectors.Vector)
   is
      Factory : aliased Program.Parsers.Nodes.Node_Factory
        (Compilation, Subpool);
      Context : aliased Parse_Context :=
        (Factory => Factory'Unchecked_Access,
         Tokens  => Tokens,
         Index   => 1);

      Root : Program.Parsers.Nodes.Node;
      Ok   : Boolean;
   begin
      Do_Parse (Context'Access, Context'Access, Root, Ok);

      if Ok then
         Program.Parsers.Nodes.Get_Compilation_Units
           (Root, Units, Pragmas);
      else
         raise Constraint_Error with "Parsing error";
      end if;
   end Parse;

end Program.Parsers;
