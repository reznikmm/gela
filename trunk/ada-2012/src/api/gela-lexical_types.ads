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

   package Operators is
      subtype Symbol is Gela.Lexical_Types.Symbol range 1 .. 19;

      Less_Operator             : constant Symbol := Token_Kind'Pos
                                                      (Less_Token);
      Equal_Operator            : constant Symbol := Token_Kind'Pos
                                                      (Equal_Token);
      Greater_Operator          : constant Symbol := Token_Kind'Pos
                                                      (Greater_Token);
      Hyphen_Operator           : constant Symbol := Token_Kind'Pos
                                                      (Hyphen_Token);
      Slash_Operator            : constant Symbol := Token_Kind'Pos
                                                      (Slash_Token);
      Star_Operator             : constant Symbol := Token_Kind'Pos
                                                      (Star_Token);
      Ampersand_Operator        : constant Symbol := Token_Kind'Pos
                                                      (Ampersand_Token);
      Plus_Operator             : constant Symbol := Token_Kind'Pos
                                                      (Plus_Token);
      Less_Or_Equal_Operator    : constant Symbol := Token_Kind'Pos
                                                      (Less_Or_Equal_Token);
      Greater_Or_Equal_Operator : constant Symbol := Token_Kind'Pos
                                                      (Greater_Or_Equal_Token);
      Inequality_Operator       : constant Symbol := Token_Kind'Pos
                                                      (Inequality_Token);
      Double_Star_Operator      : constant Symbol := Token_Kind'Pos
                                                      (Double_Star_Token);
      Or_Operator               : constant Symbol := Token_Kind'Pos
                                                      (Or_Token);
      And_Operator              : constant Symbol := Token_Kind'Pos
                                                      (And_Token);
      Xor_Operator              : constant Symbol := Token_Kind'Pos
                                                      (Xor_Token);
      Mod_Operator              : constant Symbol := Token_Kind'Pos
                                                      (Mod_Token);
      Rem_Operator              : constant Symbol := Token_Kind'Pos
                                                      (Rem_Token);
      Abs_Operator              : constant Symbol := Token_Kind'Pos
                                                      (Abs_Token);
      Not_Operator              : constant Symbol := Token_Kind'Pos
                                                      (Not_Token);
   end Operators;

   package Predefined_Symbols is
      subtype Symbol is
        Gela.Lexical_Types.Symbol range 16#11_0000# .. 16#11_0095#;

      All_Calls_Remote              : constant Symbol := 16#11_0000#;
      Assert                        : constant Symbol := 16#11_0001#;
      Assertion_Policy              : constant Symbol := 16#11_0002#;
      Asynchronous                  : constant Symbol := 16#11_0003#;
      Atomic                        : constant Symbol := 16#11_0004#;
      Atomic_Components             : constant Symbol := 16#11_0005#;
      Attach_Handler                : constant Symbol := 16#11_0006#;
      Controlled                    : constant Symbol := 16#11_0007#;
      Convention                    : constant Symbol := 16#11_0008#;
      Detect_Blocking               : constant Symbol := 16#11_0009#;
      Discard_Names                 : constant Symbol := 16#11_000A#;
      Elaborate                     : constant Symbol := 16#11_000B#;
      Elaborate_All                 : constant Symbol := 16#11_000C#;
      Elaborate_Body                : constant Symbol := 16#11_000D#;
      Export                        : constant Symbol := 16#11_000E#;
      Import                        : constant Symbol := 16#11_000F#;
      Inline                        : constant Symbol := 16#11_0010#;
      Inspection_Point              : constant Symbol := 16#11_0011#;
      Interrupt_Handler             : constant Symbol := 16#11_0012#;
      Interrupt_Priority            : constant Symbol := 16#11_0013#;
      Linker_Options                : constant Symbol := 16#11_0014#;
      List                          : constant Symbol := 16#11_0015#;
      Locking_Policy                : constant Symbol := 16#11_0016#;
      No_Return                     : constant Symbol := 16#11_0017#;
      Normalize_Scalars             : constant Symbol := 16#11_0018#;
      Optimize                      : constant Symbol := 16#11_0019#;
      Pack                          : constant Symbol := 16#11_001A#;
      Page                          : constant Symbol := 16#11_001B#;
      Partition_Elaboration_Policy  : constant Symbol := 16#11_001C#;
      Preelaborable_Initialization  : constant Symbol := 16#11_001D#;
      Preelaborate                  : constant Symbol := 16#11_001E#;
      Priority_Specific_Dispatching : constant Symbol := 16#11_001F#;
      Profile                       : constant Symbol := 16#11_0020#;
      Pure                          : constant Symbol := 16#11_0021#;
      Queuing_Policy                : constant Symbol := 16#11_0022#;
      Relative_Deadline             : constant Symbol := 16#11_0023#;
      Remote_Call_Interface         : constant Symbol := 16#11_0024#;
      Remote_Types                  : constant Symbol := 16#11_0025#;
      Restrictions                  : constant Symbol := 16#11_0026#;
      Reviewable                    : constant Symbol := 16#11_0027#;
      Shared_Passive                : constant Symbol := 16#11_0028#;
      Suppress                      : constant Symbol := 16#11_0029#;
      Task_Dispatching_Policy       : constant Symbol := 16#11_002A#;
      Unchecked_Union               : constant Symbol := 16#11_002B#;
      Unsuppress                    : constant Symbol := 16#11_002C#;
      Volatile                      : constant Symbol := 16#11_002D#;
      Volatile_Components           : constant Symbol := 16#11_002E#;
      Access_Symbol                 : constant Symbol := 16#11_002F#;
      Address                       : constant Symbol := 16#11_0030#;
      Adjacent                      : constant Symbol := 16#11_0031#;
      Aft                           : constant Symbol := 16#11_0032#;
      Alignment                     : constant Symbol := 16#11_0033#;
      Base                          : constant Symbol := 16#11_0034#;
      Bit_Order                     : constant Symbol := 16#11_0035#;
      Body_Version                  : constant Symbol := 16#11_0036#;
      Callable                      : constant Symbol := 16#11_0037#;
      Caller                        : constant Symbol := 16#11_0038#;
      Ceiling                       : constant Symbol := 16#11_0039#;
      Class                         : constant Symbol := 16#11_003A#;
      Component_Size                : constant Symbol := 16#11_003B#;
      Compose                       : constant Symbol := 16#11_003C#;
      Constrained                   : constant Symbol := 16#11_003D#;
      Copy_Sign                     : constant Symbol := 16#11_003E#;
      Count                         : constant Symbol := 16#11_003F#;
      Definite                      : constant Symbol := 16#11_0040#;
      Delta_Symbol                  : constant Symbol := 16#11_0041#;
      Denorm                        : constant Symbol := 16#11_0042#;
      Digits_Symbol                 : constant Symbol := 16#11_0043#;
      Exponent                      : constant Symbol := 16#11_0044#;
      External_Tag                  : constant Symbol := 16#11_0045#;
      First                         : constant Symbol := 16#11_0046#;
      First_Bit                     : constant Symbol := 16#11_0047#;
      Floor                         : constant Symbol := 16#11_0048#;
      Fore                          : constant Symbol := 16#11_0049#;
      Fraction                      : constant Symbol := 16#11_004A#;
      Identity                      : constant Symbol := 16#11_004B#;
      Image                         : constant Symbol := 16#11_004C#;
      Input                         : constant Symbol := 16#11_004D#;
      Last                          : constant Symbol := 16#11_004E#;
      Last_Bit                      : constant Symbol := 16#11_004F#;
      Leading_Part                  : constant Symbol := 16#11_0050#;
      Length                        : constant Symbol := 16#11_0051#;
      Machine                       : constant Symbol := 16#11_0052#;
      Machine_Emax                  : constant Symbol := 16#11_0053#;
      Machine_Emin                  : constant Symbol := 16#11_0054#;
      Machine_Mantissa              : constant Symbol := 16#11_0055#;
      Machine_Overflows             : constant Symbol := 16#11_0056#;
      Machine_Radix                 : constant Symbol := 16#11_0057#;
      Machine_Rounding              : constant Symbol := 16#11_0058#;
      Machine_Rounds                : constant Symbol := 16#11_0059#;
      Max                           : constant Symbol := 16#11_005A#;
      Max_Size_In_Storage_Elements  : constant Symbol := 16#11_005B#;
      Min                           : constant Symbol := 16#11_005C#;
      Mod_Symbol                    : constant Symbol := 16#11_005D#;
      Model                         : constant Symbol := 16#11_005E#;
      Model_Emin                    : constant Symbol := 16#11_005F#;
      Model_Epsilon                 : constant Symbol := 16#11_0060#;
      Model_Mantissa                : constant Symbol := 16#11_0061#;
      Model_Small                   : constant Symbol := 16#11_0062#;
      Modulus                       : constant Symbol := 16#11_0063#;
      Output                        : constant Symbol := 16#11_0064#;
      Partition_ID                  : constant Symbol := 16#11_0065#;
      Pos                           : constant Symbol := 16#11_0066#;
      Position                      : constant Symbol := 16#11_0067#;
      Pred                          : constant Symbol := 16#11_0068#;
      Priority                      : constant Symbol := 16#11_0069#;
      Range_Symbol                  : constant Symbol := 16#11_006A#;
      Read                          : constant Symbol := 16#11_006B#;
      Remainder                     : constant Symbol := 16#11_006C#;
      Round                         : constant Symbol := 16#11_006D#;
      Rounding                      : constant Symbol := 16#11_006E#;
      Safe_First                    : constant Symbol := 16#11_006F#;
      Safe_Last                     : constant Symbol := 16#11_0070#;
      Scale                         : constant Symbol := 16#11_0071#;
      Scaling                       : constant Symbol := 16#11_0072#;
      Signed_Zeros                  : constant Symbol := 16#11_0073#;
      Size                          : constant Symbol := 16#11_0074#;
      Small                         : constant Symbol := 16#11_0075#;
      Storage_Pool                  : constant Symbol := 16#11_0076#;
      Storage_Size                  : constant Symbol := 16#11_0077#;
      Stream_Size                   : constant Symbol := 16#11_0078#;
      Succ                          : constant Symbol := 16#11_0079#;
      Tag                           : constant Symbol := 16#11_007A#;
      Terminated                    : constant Symbol := 16#11_007B#;
      Truncation                    : constant Symbol := 16#11_007C#;
      Unbiased_Rounding             : constant Symbol := 16#11_007D#;
      Unchecked_Access              : constant Symbol := 16#11_007E#;
      Val                           : constant Symbol := 16#11_007F#;
      Valid                         : constant Symbol := 16#11_0080#;
      Value                         : constant Symbol := 16#11_0081#;
      Version                       : constant Symbol := 16#11_0082#;
      Wide_Image                    : constant Symbol := 16#11_0083#;
      Wide_Value                    : constant Symbol := 16#11_0084#;
      Wide_Wide_Image               : constant Symbol := 16#11_0085#;
      Wide_Wide_Value               : constant Symbol := 16#11_0086#;
      Wide_Wide_Width               : constant Symbol := 16#11_0087#;
      Wide_Width                    : constant Symbol := 16#11_0088#;
      Width                         : constant Symbol := 16#11_0089#;
      Write                         : constant Symbol := 16#11_008A#;
      Standard                      : constant Symbol := 16#11_008B#;
      Boolean                       : constant Symbol := 16#11_008C#;
      Integer                       : constant Symbol := 16#11_008D#;
      Float                         : constant Symbol := 16#11_008E#;
      Character                     : constant Symbol := 16#11_008F#;
      Wide_Character                : constant Symbol := 16#11_0090#;
      Wide_Wide_Character           : constant Symbol := 16#11_0091#;
      String                        : constant Symbol := 16#11_0092#;
      Wide_String                   : constant Symbol := 16#11_0093#;
      Wide_Wide_String              : constant Symbol := 16#11_0094#;
      Duration                      : constant Symbol := 16#11_0095#;

      subtype Attribute is Symbol range Access_Symbol .. Write;

   end Predefined_Symbols;

end Gela.Lexical_Types;
