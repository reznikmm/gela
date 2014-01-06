
package Gela.Lexical_Types is
   pragma Pure;

   subtype Text_Index is Natural;
   --  Index inside source text
   subtype Colon_Index is Text_Index;
   --  Index inside a text line

   type Line_Count is new Natural;
   subtype Line_Index is Line_Count range 1 .. Line_Count'Last;

   type Line_Span is record
      First   : Text_Index;
      Last    : Text_Index;
      Comment : Text_Index;
   end record;

   type Token_Kind is
     (End_Of_Input,
      Less_Token, Equal_Token, Greater_Token, Hyphen_Token, Slash_Token,
      Star_Token, Ampersand_Token, Plus_Token,
      Less_Or_Equal_Token, Greater_Or_Equal_Token, Inequality_Token,
      Double_Star_Token,
      Or_Token, And_Token, Xor_Token, Mod_Token, Rem_Token, Abs_Token,
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

   subtype Operator_Token is Token_Kind range Less_Token .. Double_Star_Token;
   subtype Keyword_Operator_Token is Token_Kind range Or_Token .. Not_Token;

   type Symbol is mod 2 ** 32;
   No_Symbol : constant Symbol := 0;

   type Symbol_List is mod 2 ** 32;
   Empty_Symbol_List : constant Symbol_List := 0;

   type Token is record
      Line      : Line_Index;
      First     : Text_Index;
      Last      : Text_Index;
      Separator : Text_Index;
      Kind      : Token_Kind;
      Symbol    : Gela.Lexical_Types.Symbol;
   end record;

   type Position is record
      Line  : Line_Index;
      Colon : Colon_Index;
   end record;

   type Token_Count is new Natural;
   subtype Token_Index is Token_Count range 1 .. Token_Count'Last;

end Gela.Lexical_Types;
