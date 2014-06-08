--  This package provides types related to lexic information.

package Gela.Lexical_Types is
   pragma Pure;

   subtype Text_Index is Natural;
   --  Index inside source text
   subtype Colon_Index is Text_Index;
   --  Index inside a text line

   type Line_Count is new Natural;
   --  Line index inside source text
   subtype Line_Index is Line_Count range 1 .. Line_Count'Last;

   type Line_Span is record
      First   : Text_Index;
      Last    : Text_Index;
      Comment : Text_Index;
   end record;
   --  Index of begin, end, comment of some line in source text

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
   --  Unique code corresponding to any name, operator symbol, character
   --  literal or compound name
   No_Symbol : constant Symbol := 0;

   type Symbol_List is mod 2 ** 32;
   --  Code corresponding to list of some symbols
   Empty_Symbol_List : constant Symbol_List := 0;

   type Token is record
      Line      : Line_Index;
      First     : Text_Index;
      Last      : Text_Index;
      Separator : Text_Index;
      Kind      : Token_Kind;
      Symbol    : Gela.Lexical_Types.Symbol;
   end record;
   --  Token related information

   type Position is record
      Line  : Line_Index;
      Colon : Colon_Index;
   end record;
   --  Position inside source code

   type Token_Count is new Natural;
   subtype Token_Index is Token_Count range 1 .. Token_Count'Last;

   type Predefined_Symbol is
     (All_Calls_Remote_Symbol,
      Assert_Symbol,
      Assertion_Policy_Symbol,
      Asynchronous_Symbol,
      Atomic_Symbol,
      Atomic_Components_Symbol,
      Attach_Handler_Symbol,
      Controlled_Symbol,
      Convention_Symbol,
      Detect_Blocking_Symbol,
      Discard_Names_Symbol,
      Elaborate_Symbol,
      Elaborate_All_Symbol,
      Elaborate_Body_Symbol,
      Export_Symbol,
      Import_Symbol,
      Inline_Symbol,
      Inspection_Point_Symbol,
      Interrupt_Handler_Symbol,
      Interrupt_Priority_Symbol,
      Linker_Options_Symbol,
      List_Symbol,
      Locking_Policy_Symbol,
      No_Return_Symbol,
      Normalize_Scalars_Symbol,
      Optimize_Symbol,
      Pack_Symbol,
      Page_Symbol,
      Partition_Elaboration_Policy_Symbol,
      Preelaborable_Initialization_Symbol,
      Preelaborate_Symbol,
      Priority_Symbol,
      Priority_Specific_Dispatching_Symbol,
      Profile_Symbol,
      Pure_Symbol,
      Queuing_Policy_Symbol,
      Relative_Deadline_Symbol,
      Remote_Call_Interface_Symbol,
      Remote_Types_Symbol,
      Restrictions_Symbol,
      Reviewable_Symbol,
      Shared_Passive_Symbol,
      Storage_Size_Symbol,
      Suppress_Symbol,
      Task_Dispatching_Policy_Symbol,
      Unchecked_Union_Symbol,
      Unsuppress_Symbol,
      Volatile_Symbol,
      Volatile_Components_Symbol,
      Access_Symbol,
      Address_Symbol,
      Adjacent_Symbol,
      Aft_Symbol,
      Alignment_Symbol,
      Base_Symbol,
      Bit_Order_Symbol,
      Body_Version_Symbol,
      Callable_Symbol,
      Caller_Symbol,
      Ceiling_Symbol,
      Class_Symbol,
      Component_Size_Symbol,
      Compose_Symbol,
      Constrained_Symbol,
      Copy_Sign_Symbol,
      Count_Symbol,
      Definite_Symbol,
      Delta_Symbol,
      Denorm_Symbol,
      Digits_Symbol,
      Exponent_Symbol,
      External_Tag_Symbol,
      First_Symbol,
      First_Bit_Symbol,
      Floor_Symbol,
      Fore_Symbol,
      Fraction_Symbol,
      Identity_Symbol,
      Image_Symbol,
      Input_Symbol,
      Last_Symbol,
      Last_Bit_Symbol,
      Leading_Part_Symbol,
      Length_Symbol,
      Machine_Symbol,
      Machine_Emax_Symbol,
      Machine_Emin_Symbol,
      Machine_Mantissa_Symbol,
      Machine_Overflows_Symbol,
      Machine_Radix_Symbol,
      Machine_Rounding_Symbol,
      Machine_Rounds_Symbol,
      Max_Symbol,
      Max_Size_In_Storage_Elements_Symbol,
      Min_Symbol,
      Mod_Symbol,
      Model_Symbol,
      Model_Emin_Symbol,
      Model_Epsilon_Symbol,
      Model_Mantissa_Symbol,
      Model_Small_Symbol,
      Modulus_Symbol,
      Output_Symbol,
      Partition_ID_Symbol,
      Pos_Symbol,
      Position_Symbol,
      Pred_Symbol,
      Range_Symbol,
      Read_Symbol,
      Remainder_Symbol,
      Round_Symbol,
      Rounding_Symbol,
      Safe_First_Symbol,
      Safe_Last_Symbol,
      Scale_Symbol,
      Scaling_Symbol,
      Signed_Zeros_Symbol,
      Size_Symbol,
      Small_Symbol,
      Storage_Pool_Symbol,
      Stream_Size_Symbol,
      Succ_Symbol,
      Tag_Symbol,
      Terminated_Symbol,
      Truncation_Symbol,
      Unbiased_Rounding_Symbol,
      Unchecked_Access_Symbol,
      Val_Symbol,
      Valid_Symbol,
      Value_Symbol,
      Version_Symbol,
      Wide_Image_Symbol,
      Wide_Value_Symbol,
      Wide_Wide_Image_Symbol,
      Wide_Wide_Value_Symbol,
      Wide_Wide_Width_Symbol,
      Wide_Width_Symbol,
      Width_Symbol,
      Write_Symbol);

end Gela.Lexical_Types;
