--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Containers;

with Program.Lexical_Elements;

package Program.Symbols is
   pragma Pure;

   type Symbol is private;
   --  Symbol is case-insensitive representation of identifiers, operators
   --  and character literals

   function Hash (Value : Symbol) return Ada.Containers.Hash_Type;

   function No_Symbol return Symbol;

   function Is_Operator (Value : Symbol) return Boolean;

   function Less_Symbol             return Symbol;
   function Equal_Symbol            return Symbol;
   function Greater_Symbol          return Symbol;
   function Hyphen_Symbol           return Symbol;
   function Slash_Symbol            return Symbol;
   function Star_Symbol             return Symbol;
   function Ampersand_Symbol        return Symbol;
   function Plus_Symbol             return Symbol;
   function Less_Or_Equal_Symbol    return Symbol;
   function Greater_Or_Equal_Symbol return Symbol;
   function Inequality_Symbol       return Symbol;
   function Double_Star_Symbol      return Symbol;
   function Or_Symbol               return Symbol;
   function And_Symbol              return Symbol;
   function Xor_Symbol              return Symbol;
   function Mod_Symbol              return Symbol;
   function Rem_Symbol              return Symbol;
   function Abs_Symbol              return Symbol;
   function Not_Symbol              return Symbol;

   function Is_Character_Literal (Value : Symbol) return Boolean;

   function All_Calls_Remote              return Symbol;
   function Assert                        return Symbol;
   function Assertion_Policy              return Symbol;
   function Asynchronous                  return Symbol;
   function Atomic                        return Symbol;
   function Atomic_Components             return Symbol;
   function Attach_Handler                return Symbol;
   function Controlled                    return Symbol;
   function Convention                    return Symbol;
   function Detect_Blocking               return Symbol;
   function Discard_Names                 return Symbol;
   function Elaborate                     return Symbol;
   function Elaborate_All                 return Symbol;
   function Elaborate_Body                return Symbol;
   function Export                        return Symbol;
   function Import                        return Symbol;
   function Inline                        return Symbol;
   function Inspection_Point              return Symbol;
   function Interrupt_Handler             return Symbol;
   function Interrupt_Priority            return Symbol;
   function Linker_Options                return Symbol;
   function List                          return Symbol;
   function Locking_Policy                return Symbol;
   function No_Return                     return Symbol;
   function Normalize_Scalars             return Symbol;
   function Optimize                      return Symbol;
   function Pack                          return Symbol;
   function Page                          return Symbol;
   function Partition_Elaboration_Policy  return Symbol;
   function Preelaborable_Initialization  return Symbol;
   function Preelaborate                  return Symbol;
   function Priority_Specific_Dispatching return Symbol;
   function Profile                       return Symbol;
   function Pure                          return Symbol;
   function Queuing_Policy                return Symbol;
   function Relative_Deadline             return Symbol;
   function Remote_Call_Interface         return Symbol;
   function Remote_Types                  return Symbol;
   function Restrictions                  return Symbol;
   function Reviewable                    return Symbol;
   function Shared_Passive                return Symbol;
   function Suppress                      return Symbol;
   function Task_Dispatching_Policy       return Symbol;
   function Unchecked_Union               return Symbol;
   function Unsuppress                    return Symbol;
   function Volatile                      return Symbol;
   function Volatile_Components           return Symbol;
   --  Attributes:
   function Access_Symbol                 return Symbol;
   function Address                       return Symbol;
   function Adjacent                      return Symbol;
   function Aft                           return Symbol;
   function Alignment                     return Symbol;
   function Base                          return Symbol;
   function Bit_Order                     return Symbol;
   function Body_Version                  return Symbol;
   function Callable                      return Symbol;
   function Caller                        return Symbol;
   function Ceiling                       return Symbol;
   function Class                         return Symbol;
   function Component_Size                return Symbol;
   function Compose                       return Symbol;
   function Constrained                   return Symbol;
   function Copy_Sign                     return Symbol;
   function Count                         return Symbol;
   function Definite                      return Symbol;
   function Delta_Symbol                  return Symbol;
   function Denorm                        return Symbol;
   function Digits_Symbol                 return Symbol;
   function Exponent                      return Symbol;
   function External_Tag                  return Symbol;
   function First                         return Symbol;
   function First_Bit                     return Symbol;
   function Floor                         return Symbol;
   function Fore                          return Symbol;
   function Fraction                      return Symbol;
   function Identity                      return Symbol;
   function Image                         return Symbol;
   function Input                         return Symbol;
   function Last                          return Symbol;
   function Last_Bit                      return Symbol;
   function Leading_Part                  return Symbol;
   function Length                        return Symbol;
   function Machine                       return Symbol;
   function Machine_Emax                  return Symbol;
   function Machine_Emin                  return Symbol;
   function Machine_Mantissa              return Symbol;
   function Machine_Overflows             return Symbol;
   function Machine_Radix                 return Symbol;
   function Machine_Rounding              return Symbol;
   function Machine_Rounds                return Symbol;
   function Max                           return Symbol;
   function Max_Size_In_Storage_Elements  return Symbol;
   function Min                           return Symbol;
   function Mod_Keyword                   return Symbol;
   function Model                         return Symbol;
   function Model_Emin                    return Symbol;
   function Model_Epsilon                 return Symbol;
   function Model_Mantissa                return Symbol;
   function Model_Small                   return Symbol;
   function Modulus                       return Symbol;
   function Output                        return Symbol;
   function Partition_ID                  return Symbol;
   function Pos                           return Symbol;
   function Position                      return Symbol;
   function Pred                          return Symbol;
   function Priority                      return Symbol;
   function Range_Keyword                 return Symbol;
   function Read                          return Symbol;
   function Remainder                     return Symbol;
   function Round                         return Symbol;
   function Rounding                      return Symbol;
   function Safe_First                    return Symbol;
   function Safe_Last                     return Symbol;
   function Scale                         return Symbol;
   function Scaling                       return Symbol;
   function Signed_Zeros                  return Symbol;
   function Size                          return Symbol;
   function Small                         return Symbol;
   function Storage_Pool                  return Symbol;
   function Storage_Size                  return Symbol;
   function Stream_Size                   return Symbol;
   function Succ                          return Symbol;
   function Tag                           return Symbol;
   function Terminated                    return Symbol;
   function Truncation                    return Symbol;
   function Unbiased_Rounding             return Symbol;
   function Unchecked_Access              return Symbol;
   function Val                           return Symbol;
   function Valid                         return Symbol;
   function Value                         return Symbol;
   function Version                       return Symbol;
   function Wide_Image                    return Symbol;
   function Wide_Value                    return Symbol;
   function Wide_Wide_Image               return Symbol;
   function Wide_Wide_Value               return Symbol;
   function Wide_Wide_Width               return Symbol;
   function Wide_Width                    return Symbol;
   function Width                         return Symbol;
   function Write                         return Symbol;

   --  Other names:

   package S renames Standard;

   function Standard                      return Symbol;
   function Boolean                       return Symbol;
   function Integer                       return Symbol;
   function Float                         return Symbol;
   function Character                     return Symbol;
   function Wide_Character                return Symbol;
   function Wide_Wide_Character           return Symbol;
   function String                        return Symbol;
   function Wide_String                   return Symbol;
   function Wide_Wide_String              return Symbol;
   function Duration                      return Symbol;
   function Root_Integer                  return Symbol;
   function Root_Real                     return Symbol;
   function Left                          return Symbol;
   function Right                         return Symbol;

private

   type Symbol is mod 2 ** 32;
   --  Symbol is case-insensitive representation of identifiers, operators
   --  and character literals

   function Hash (Value : Symbol) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type'Mod (Value));

   function No_Symbol return Symbol is (0);

   function Is_Operator (Value : Symbol) return S.Boolean is
      (Value in 1 .. 19);

   use Program.Lexical_Elements;

   function Less_Symbol             return Symbol is
     (Lexical_Element_Kind'Pos (Less) + 1);

   function Equal_Symbol            return Symbol is
     (Lexical_Element_Kind'Pos (Equal) + 1);

   function Greater_Symbol          return Symbol is
     (Lexical_Element_Kind'Pos (Greater) + 1);

   function Hyphen_Symbol           return Symbol is
     (Lexical_Element_Kind'Pos (Hyphen) + 1);

   function Slash_Symbol            return Symbol is
     (Lexical_Element_Kind'Pos (Slash) + 1);

   function Star_Symbol             return Symbol is
     (Lexical_Element_Kind'Pos (Star) + 1);

   function Ampersand_Symbol        return Symbol is
     (Lexical_Element_Kind'Pos (Ampersand) + 1);

   function Plus_Symbol             return Symbol is
     (Lexical_Element_Kind'Pos (Plus) + 1);

   function Less_Or_Equal_Symbol    return Symbol is
     (Lexical_Element_Kind'Pos (Less_Or_Equal) + 1);

   function Greater_Or_Equal_Symbol return Symbol is
     (Lexical_Element_Kind'Pos (Greater_Or_Equal) + 1);

   function Inequality_Symbol       return Symbol is
     (Lexical_Element_Kind'Pos (Inequality) + 1);

   function Double_Star_Symbol      return Symbol is
     (Lexical_Element_Kind'Pos (Double_Star) + 1);

   function Or_Symbol               return Symbol is
     (Lexical_Element_Kind'Pos (Or_Keyword) + 1);

   function And_Symbol              return Symbol is
     (Lexical_Element_Kind'Pos (And_Keyword) + 1);

   function Xor_Symbol              return Symbol is
     (Lexical_Element_Kind'Pos (Xor_Keyword) + 1);

   function Mod_Symbol              return Symbol is
     (Lexical_Element_Kind'Pos (Mod_Keyword) + 1);

   function Rem_Symbol              return Symbol is
     (Lexical_Element_Kind'Pos (Rem_Keyword) + 1);

   function Abs_Symbol              return Symbol is
     (Lexical_Element_Kind'Pos (Abs_Keyword) + 1);

   function Not_Symbol              return Symbol is
     (Lexical_Element_Kind'Pos (Not_Keyword) + 1);

   function Is_Character_Literal (Value : Symbol) return S.Boolean is
      (Value in 16#00_0020# .. 16#10_FFFF#);

   subtype X_Symbol is Symbol range 16#11_0000# .. 16#11_0099#;

   function All_Calls_Remote              return Symbol is (16#11_0000#);
   function Assert                        return Symbol is (16#11_0001#);
   function Assertion_Policy              return Symbol is (16#11_0002#);
   function Asynchronous                  return Symbol is (16#11_0003#);
   function Atomic                        return Symbol is (16#11_0004#);
   function Atomic_Components             return Symbol is (16#11_0005#);
   function Attach_Handler                return Symbol is (16#11_0006#);
   function Controlled                    return Symbol is (16#11_0007#);
   function Convention                    return Symbol is (16#11_0008#);
   function Detect_Blocking               return Symbol is (16#11_0009#);
   function Discard_Names                 return Symbol is (16#11_000A#);
   function Elaborate                     return Symbol is (16#11_000B#);
   function Elaborate_All                 return Symbol is (16#11_000C#);
   function Elaborate_Body                return Symbol is (16#11_000D#);
   function Export                        return Symbol is (16#11_000E#);
   function Import                        return Symbol is (16#11_000F#);
   function Inline                        return Symbol is (16#11_0010#);
   function Inspection_Point              return Symbol is (16#11_0011#);
   function Interrupt_Handler             return Symbol is (16#11_0012#);
   function Interrupt_Priority            return Symbol is (16#11_0013#);
   function Linker_Options                return Symbol is (16#11_0014#);
   function List                          return Symbol is (16#11_0015#);
   function Locking_Policy                return Symbol is (16#11_0016#);
   function No_Return                     return Symbol is (16#11_0017#);
   function Normalize_Scalars             return Symbol is (16#11_0018#);
   function Optimize                      return Symbol is (16#11_0019#);
   function Pack                          return Symbol is (16#11_001A#);
   function Page                          return Symbol is (16#11_001B#);
   function Partition_Elaboration_Policy  return Symbol is (16#11_001C#);
   function Preelaborable_Initialization  return Symbol is (16#11_001D#);
   function Preelaborate                  return Symbol is (16#11_001E#);
   function Priority_Specific_Dispatching return Symbol is (16#11_001F#);
   function Profile                       return Symbol is (16#11_0020#);
   function Pure                          return Symbol is (16#11_0021#);
   function Queuing_Policy                return Symbol is (16#11_0022#);
   function Relative_Deadline             return Symbol is (16#11_0023#);
   function Remote_Call_Interface         return Symbol is (16#11_0024#);
   function Remote_Types                  return Symbol is (16#11_0025#);
   function Restrictions                  return Symbol is (16#11_0026#);
   function Reviewable                    return Symbol is (16#11_0027#);
   function Shared_Passive                return Symbol is (16#11_0028#);
   function Suppress                      return Symbol is (16#11_0029#);
   function Task_Dispatching_Policy       return Symbol is (16#11_002A#);
   function Unchecked_Union               return Symbol is (16#11_002B#);
   function Unsuppress                    return Symbol is (16#11_002C#);
   function Volatile                      return Symbol is (16#11_002D#);
   function Volatile_Components           return Symbol is (16#11_002E#);
   --  Attributes:
   function Access_Symbol                 return Symbol is (16#11_002F#);
   function Address                       return Symbol is (16#11_0030#);
   function Adjacent                      return Symbol is (16#11_0031#);
   function Aft                           return Symbol is (16#11_0032#);
   function Alignment                     return Symbol is (16#11_0033#);
   function Base                          return Symbol is (16#11_0034#);
   function Bit_Order                     return Symbol is (16#11_0035#);
   function Body_Version                  return Symbol is (16#11_0036#);
   function Callable                      return Symbol is (16#11_0037#);
   function Caller                        return Symbol is (16#11_0038#);
   function Ceiling                       return Symbol is (16#11_0039#);
   function Class                         return Symbol is (16#11_003A#);
   function Component_Size                return Symbol is (16#11_003B#);
   function Compose                       return Symbol is (16#11_003C#);
   function Constrained                   return Symbol is (16#11_003D#);
   function Copy_Sign                     return Symbol is (16#11_003E#);
   function Count                         return Symbol is (16#11_003F#);
   function Definite                      return Symbol is (16#11_0040#);
   function Delta_Symbol                  return Symbol is (16#11_0041#);
   function Denorm                        return Symbol is (16#11_0042#);
   function Digits_Symbol                 return Symbol is (16#11_0043#);
   function Exponent                      return Symbol is (16#11_0044#);
   function External_Tag                  return Symbol is (16#11_0045#);
   function First                         return Symbol is (16#11_0046#);
   function First_Bit                     return Symbol is (16#11_0047#);
   function Floor                         return Symbol is (16#11_0048#);
   function Fore                          return Symbol is (16#11_0049#);
   function Fraction                      return Symbol is (16#11_004A#);
   function Identity                      return Symbol is (16#11_004B#);
   function Image                         return Symbol is (16#11_004C#);
   function Input                         return Symbol is (16#11_004D#);
   function Last                          return Symbol is (16#11_004E#);
   function Last_Bit                      return Symbol is (16#11_004F#);
   function Leading_Part                  return Symbol is (16#11_0050#);
   function Length                        return Symbol is (16#11_0051#);
   function Machine                       return Symbol is (16#11_0052#);
   function Machine_Emax                  return Symbol is (16#11_0053#);
   function Machine_Emin                  return Symbol is (16#11_0054#);
   function Machine_Mantissa              return Symbol is (16#11_0055#);
   function Machine_Overflows             return Symbol is (16#11_0056#);
   function Machine_Radix                 return Symbol is (16#11_0057#);
   function Machine_Rounding              return Symbol is (16#11_0058#);
   function Machine_Rounds                return Symbol is (16#11_0059#);
   function Max                           return Symbol is (16#11_005A#);
   function Max_Size_In_Storage_Elements  return Symbol is (16#11_005B#);
   function Min                           return Symbol is (16#11_005C#);
   function Mod_Keyword                   return Symbol is (16#11_005D#);
   function Model                         return Symbol is (16#11_005E#);
   function Model_Emin                    return Symbol is (16#11_005F#);
   function Model_Epsilon                 return Symbol is (16#11_0060#);
   function Model_Mantissa                return Symbol is (16#11_0061#);
   function Model_Small                   return Symbol is (16#11_0062#);
   function Modulus                       return Symbol is (16#11_0063#);
   function Output                        return Symbol is (16#11_0064#);
   function Partition_ID                  return Symbol is (16#11_0065#);
   function Pos                           return Symbol is (16#11_0066#);
   function Position                      return Symbol is (16#11_0067#);
   function Pred                          return Symbol is (16#11_0068#);
   function Priority                      return Symbol is (16#11_0069#);
   function Range_Keyword                 return Symbol is (16#11_006A#);
   function Read                          return Symbol is (16#11_006B#);
   function Remainder                     return Symbol is (16#11_006C#);
   function Round                         return Symbol is (16#11_006D#);
   function Rounding                      return Symbol is (16#11_006E#);
   function Safe_First                    return Symbol is (16#11_006F#);
   function Safe_Last                     return Symbol is (16#11_0070#);
   function Scale                         return Symbol is (16#11_0071#);
   function Scaling                       return Symbol is (16#11_0072#);
   function Signed_Zeros                  return Symbol is (16#11_0073#);
   function Size                          return Symbol is (16#11_0074#);
   function Small                         return Symbol is (16#11_0075#);
   function Storage_Pool                  return Symbol is (16#11_0076#);
   function Storage_Size                  return Symbol is (16#11_0077#);
   function Stream_Size                   return Symbol is (16#11_0078#);
   function Succ                          return Symbol is (16#11_0079#);
   function Tag                           return Symbol is (16#11_007A#);
   function Terminated                    return Symbol is (16#11_007B#);
   function Truncation                    return Symbol is (16#11_007C#);
   function Unbiased_Rounding             return Symbol is (16#11_007D#);
   function Unchecked_Access              return Symbol is (16#11_007E#);
   function Val                           return Symbol is (16#11_007F#);
   function Valid                         return Symbol is (16#11_0080#);
   function Value                         return Symbol is (16#11_0081#);
   function Version                       return Symbol is (16#11_0082#);
   function Wide_Image                    return Symbol is (16#11_0083#);
   function Wide_Value                    return Symbol is (16#11_0084#);
   function Wide_Wide_Image               return Symbol is (16#11_0085#);
   function Wide_Wide_Value               return Symbol is (16#11_0086#);
   function Wide_Wide_Width               return Symbol is (16#11_0087#);
   function Wide_Width                    return Symbol is (16#11_0088#);
   function Width                         return Symbol is (16#11_0089#);
   function Write                         return Symbol is (16#11_008A#);

   function Standard                      return Symbol is (16#11_008B#);
   function Boolean                       return Symbol is (16#11_008C#);
   function Integer                       return Symbol is (16#11_008D#);
   function Float                         return Symbol is (16#11_008E#);
   function Character                     return Symbol is (16#11_008F#);
   function Wide_Character                return Symbol is (16#11_0090#);
   function Wide_Wide_Character           return Symbol is (16#11_0091#);
   function String                        return Symbol is (16#11_0092#);
   function Wide_String                   return Symbol is (16#11_0093#);
   function Wide_Wide_String              return Symbol is (16#11_0094#);
   function Duration                      return Symbol is (16#11_0095#);
   function Root_Integer                  return Symbol is (16#11_0096#);
   function Root_Real                     return Symbol is (16#11_0097#);
   function Left                          return Symbol is (16#11_0098#);
   function Right                         return Symbol is (16#11_0099#);

end Program.Symbols;
