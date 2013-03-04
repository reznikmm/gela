------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--        Library for dealing with grammars for for Gela project,           --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Lexical.Tokens is
--        A_Less_Than_Operator,              --  <
--  12    An_Equal_Operator,                 --  =
--        A_Greater_Than_Operator,           --  >
--        A_Minus_Operator,                  --  -
--        A_Divide_Operator,                 --  /
--        A_Multiply_Operator,               --  *
--        A_Concatenate_Operator,            --  &
--        A_Unary_Plus_Operator,             --  +
--        A_Less_Than_Or_Equal_Operator,     --  <=
--        A_Greater_Than_Or_Equal_Operator,  --  >=
--        A_Not_Equal_Operator,              --  /=
--        An_Exponentiate_Operator,          --  **

--        An_And_Operator,                   --  and
--        An_Or_Operator,                    --  or
--        An_Xor_Operator,                   --  xor
--        A_Mod_Operator,                    --  mod
--        A_Rem_Operator,                    --  rem
--        An_Abs_Operator,                   --  abs
--  7     A_Not_Operator);                   --  not

   type Token is
     (End_Of_Input,
      Less_Token, Equal_Token, Greater_Token, Hyphen_Token, Slash_Token,
      Star_Token, Ampersand_Token, Plus_Token,
      Less_Or_Equal_Token, Greater_Or_Equal_Token, Inequality_Token,
      Double_Star_Token,
      And_Token, Or_Token, Xor_Token, Mod_Token, Rem_Token, Abs_Token,
      Not_Token,

      Right_Label_Token,
      Box_Token, Left_Label_Token,
      Assignment_Token, Arrow_Token,
      Double_Dot_Token,
      Apostrophe_Token, Left_Parenthesis_Token,
      Right_Parenthesis_Token,
      Comma_Token, Dot_Token,
      Colon_Token, Semicolon_Token,
      Vertical_Line_Token, Abort_Token,
      Abstract_Token, Accept_Token,
      Access_Token, Aliased_Token, All_Token,
      Array_Token, At_Token,
      Begin_Token, Body_Token, Case_Token,
      Constant_Token, Declare_Token, Delay_Token,
      Delta_Token, Digits_Token, Do_Token,
      Else_Token, Elsif_Token, End_Token,
      Entry_Token, Exception_Token, Exit_Token,
      For_Token, Function_Token, Generic_Token,
      Goto_Token, If_Token, In_Token,
      Interface_Token, Is_Token, Limited_Token,
      Loop_Token, New_Token,
      Null_Token, Of_Token,
      Others_Token, Out_Token,
      Overriding_Token, Package_Token, Pragma_Token,
      Private_Token, Procedure_Token, Protected_Token,
      Raise_Token, Range_Token, Record_Token,
      Renames_Token, Requeue_Token,
      Return_Token, Reverse_Token, Select_Token,
      Separate_Token, Some_Token, Subtype_Token, Synchronized_Token,
      Tagged_Token, Task_Token, Terminate_Token,
      Then_Token, Type_Token, Until_Token,
      Use_Token, When_Token, While_Token,
      With_Token,
      Comment_Token, Identifier_Token, Numeric_Literal_Token,
      Character_Literal_Token, String_Literal_Token, Error);

   subtype Operator_Token is Token range Less_Token .. Double_Star_Token;
   subtype Keyword_Operator_Token is Token range And_Token .. Not_Token;

end Gela.Lexical.Tokens;
