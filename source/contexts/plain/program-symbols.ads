--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements; use Program.Lexical_Elements;

package Program.Symbols is
   pragma Pure;

   type Symbol is mod 2 ** 32;
   --  Symbol is case-insensitive representation of identifiers, operators
   --  and character literals

   function No_Symbol return Symbol is (0);

   subtype Operator_Symbol is Symbol range 1 .. 19;

   function Less_Symbol             return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Less) + 1);

   function Equal_Symbol            return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Equal) + 1);

   function Greater_Symbol          return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Greater) + 1);

   function Hyphen_Symbol           return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Hyphen) + 1);

   function Slash_Symbol            return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Slash) + 1);

   function Star_Symbol             return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Star) + 1);

   function Ampersand_Symbol        return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Ampersand) + 1);

   function Plus_Symbol             return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Plus) + 1);

   function Less_Or_Equal_Symbol    return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Less_Or_Equal) + 1);

   function Greater_Or_Equal_Symbol return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Greater_Or_Equal) + 1);

   function Inequality_Symbol       return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Inequality) + 1);

   function Double_Star_Symbol      return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Double_Star) + 1);

   function Or_Symbol               return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Or_Keyword) + 1);

   function And_Symbol              return Operator_Symbol is
     (Lexical_Element_Kind'Pos (And_Keyword) + 1);

   function Xor_Symbol              return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Xor_Keyword) + 1);

   function Mod_Symbol              return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Mod_Keyword) + 1);

   function Rem_Symbol              return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Rem_Keyword) + 1);

   function Abs_Symbol              return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Abs_Keyword) + 1);

   function Not_Symbol              return Operator_Symbol is
     (Lexical_Element_Kind'Pos (Not_Keyword) + 1);

   subtype Character_Literal_Symbol is Symbol range 16#00_0020# .. 16#10_FFFF#;

   subtype X_Symbol is Symbol range 16#11_0000# .. 16#11_0097#;

   function All_Calls_Remote              return X_Symbol is (16#11_0000#);
   function Assert                        return X_Symbol is (16#11_0001#);
   function Assertion_Policy              return X_Symbol is (16#11_0002#);
   function Asynchronous                  return X_Symbol is (16#11_0003#);
   function Atomic                        return X_Symbol is (16#11_0004#);
   function Atomic_Components             return X_Symbol is (16#11_0005#);
   function Attach_Handler                return X_Symbol is (16#11_0006#);
   function Controlled                    return X_Symbol is (16#11_0007#);
   function Convention                    return X_Symbol is (16#11_0008#);
   function Detect_Blocking               return X_Symbol is (16#11_0009#);
   function Discard_Names                 return X_Symbol is (16#11_000A#);
   function Elaborate                     return X_Symbol is (16#11_000B#);
   function Elaborate_All                 return X_Symbol is (16#11_000C#);
   function Elaborate_Body                return X_Symbol is (16#11_000D#);
   function Export                        return X_Symbol is (16#11_000E#);
   function Import                        return X_Symbol is (16#11_000F#);
   function Inline                        return X_Symbol is (16#11_0010#);
   function Inspection_Point              return X_Symbol is (16#11_0011#);
   function Interrupt_Handler             return X_Symbol is (16#11_0012#);
   function Interrupt_Priority            return X_Symbol is (16#11_0013#);
   function Linker_Options                return X_Symbol is (16#11_0014#);
   function List                          return X_Symbol is (16#11_0015#);
   function Locking_Policy                return X_Symbol is (16#11_0016#);
   function No_Return                     return X_Symbol is (16#11_0017#);
   function Normalize_Scalars             return X_Symbol is (16#11_0018#);
   function Optimize                      return X_Symbol is (16#11_0019#);
   function Pack                          return X_Symbol is (16#11_001A#);
   function Page                          return X_Symbol is (16#11_001B#);
   function Partition_Elaboration_Policy  return X_Symbol is (16#11_001C#);
   function Preelaborable_Initialization  return X_Symbol is (16#11_001D#);
   function Preelaborate                  return X_Symbol is (16#11_001E#);
   function Priority_Specific_Dispatching return X_Symbol is (16#11_001F#);
   function Profile                       return X_Symbol is (16#11_0020#);
   function Pure                          return X_Symbol is (16#11_0021#);
   function Queuing_Policy                return X_Symbol is (16#11_0022#);
   function Relative_Deadline             return X_Symbol is (16#11_0023#);
   function Remote_Call_Interface         return X_Symbol is (16#11_0024#);
   function Remote_Types                  return X_Symbol is (16#11_0025#);
   function Restrictions                  return X_Symbol is (16#11_0026#);
   function Reviewable                    return X_Symbol is (16#11_0027#);
   function Shared_Passive                return X_Symbol is (16#11_0028#);
   function Suppress                      return X_Symbol is (16#11_0029#);
   function Task_Dispatching_Policy       return X_Symbol is (16#11_002A#);
   function Unchecked_Union               return X_Symbol is (16#11_002B#);
   function Unsuppress                    return X_Symbol is (16#11_002C#);
   function Volatile                      return X_Symbol is (16#11_002D#);
   function Volatile_Components           return X_Symbol is (16#11_002E#);
   --  Attributes:
   function Access_Symbol                 return X_Symbol is (16#11_002F#);
   function Address                       return X_Symbol is (16#11_0030#);
   function Adjacent                      return X_Symbol is (16#11_0031#);
   function Aft                           return X_Symbol is (16#11_0032#);
   function Alignment                     return X_Symbol is (16#11_0033#);
   function Base                          return X_Symbol is (16#11_0034#);
   function Bit_Order                     return X_Symbol is (16#11_0035#);
   function Body_Version                  return X_Symbol is (16#11_0036#);
   function Callable                      return X_Symbol is (16#11_0037#);
   function Caller                        return X_Symbol is (16#11_0038#);
   function Ceiling                       return X_Symbol is (16#11_0039#);
   function Class                         return X_Symbol is (16#11_003A#);
   function Component_Size                return X_Symbol is (16#11_003B#);
   function Compose                       return X_Symbol is (16#11_003C#);
   function Constrained                   return X_Symbol is (16#11_003D#);
   function Copy_Sign                     return X_Symbol is (16#11_003E#);
   function Count                         return X_Symbol is (16#11_003F#);
   function Definite                      return X_Symbol is (16#11_0040#);
   function Delta_Symbol                  return X_Symbol is (16#11_0041#);
   function Denorm                        return X_Symbol is (16#11_0042#);
   function Digits_Symbol                 return X_Symbol is (16#11_0043#);
   function Exponent                      return X_Symbol is (16#11_0044#);
   function External_Tag                  return X_Symbol is (16#11_0045#);
   function First                         return X_Symbol is (16#11_0046#);
   function First_Bit                     return X_Symbol is (16#11_0047#);
   function Floor                         return X_Symbol is (16#11_0048#);
   function Fore                          return X_Symbol is (16#11_0049#);
   function Fraction                      return X_Symbol is (16#11_004A#);
   function Identity                      return X_Symbol is (16#11_004B#);
   function Image                         return X_Symbol is (16#11_004C#);
   function Input                         return X_Symbol is (16#11_004D#);
   function Last                          return X_Symbol is (16#11_004E#);
   function Last_Bit                      return X_Symbol is (16#11_004F#);
   function Leading_Part                  return X_Symbol is (16#11_0050#);
   function Length                        return X_Symbol is (16#11_0051#);
   function Machine                       return X_Symbol is (16#11_0052#);
   function Machine_Emax                  return X_Symbol is (16#11_0053#);
   function Machine_Emin                  return X_Symbol is (16#11_0054#);
   function Machine_Mantissa              return X_Symbol is (16#11_0055#);
   function Machine_Overflows             return X_Symbol is (16#11_0056#);
   function Machine_Radix                 return X_Symbol is (16#11_0057#);
   function Machine_Rounding              return X_Symbol is (16#11_0058#);
   function Machine_Rounds                return X_Symbol is (16#11_0059#);
   function Max                           return X_Symbol is (16#11_005A#);
   function Max_Size_In_Storage_Elements  return X_Symbol is (16#11_005B#);
   function Min                           return X_Symbol is (16#11_005C#);
   function Mod_Keyword                   return X_Symbol is (16#11_005D#);
   function Model                         return X_Symbol is (16#11_005E#);
   function Model_Emin                    return X_Symbol is (16#11_005F#);
   function Model_Epsilon                 return X_Symbol is (16#11_0060#);
   function Model_Mantissa                return X_Symbol is (16#11_0061#);
   function Model_Small                   return X_Symbol is (16#11_0062#);
   function Modulus                       return X_Symbol is (16#11_0063#);
   function Output                        return X_Symbol is (16#11_0064#);
   function Partition_ID                  return X_Symbol is (16#11_0065#);
   function Pos                           return X_Symbol is (16#11_0066#);
   function Position                      return X_Symbol is (16#11_0067#);
   function Pred                          return X_Symbol is (16#11_0068#);
   function Priority                      return X_Symbol is (16#11_0069#);
   function Range_Keyword                 return X_Symbol is (16#11_006A#);
   function Read                          return X_Symbol is (16#11_006B#);
   function Remainder                     return X_Symbol is (16#11_006C#);
   function Round                         return X_Symbol is (16#11_006D#);
   function Rounding                      return X_Symbol is (16#11_006E#);
   function Safe_First                    return X_Symbol is (16#11_006F#);
   function Safe_Last                     return X_Symbol is (16#11_0070#);
   function Scale                         return X_Symbol is (16#11_0071#);
   function Scaling                       return X_Symbol is (16#11_0072#);
   function Signed_Zeros                  return X_Symbol is (16#11_0073#);
   function Size                          return X_Symbol is (16#11_0074#);
   function Small                         return X_Symbol is (16#11_0075#);
   function Storage_Pool                  return X_Symbol is (16#11_0076#);
   function Storage_Size                  return X_Symbol is (16#11_0077#);
   function Stream_Size                   return X_Symbol is (16#11_0078#);
   function Succ                          return X_Symbol is (16#11_0079#);
   function Tag                           return X_Symbol is (16#11_007A#);
   function Terminated                    return X_Symbol is (16#11_007B#);
   function Truncation                    return X_Symbol is (16#11_007C#);
   function Unbiased_Rounding             return X_Symbol is (16#11_007D#);
   function Unchecked_Access              return X_Symbol is (16#11_007E#);
   function Val                           return X_Symbol is (16#11_007F#);
   function Valid                         return X_Symbol is (16#11_0080#);
   function Value                         return X_Symbol is (16#11_0081#);
   function Version                       return X_Symbol is (16#11_0082#);
   function Wide_Image                    return X_Symbol is (16#11_0083#);
   function Wide_Value                    return X_Symbol is (16#11_0084#);
   function Wide_Wide_Image               return X_Symbol is (16#11_0085#);
   function Wide_Wide_Value               return X_Symbol is (16#11_0086#);
   function Wide_Wide_Width               return X_Symbol is (16#11_0087#);
   function Wide_Width                    return X_Symbol is (16#11_0088#);
   function Width                         return X_Symbol is (16#11_0089#);
   function Write                         return X_Symbol is (16#11_008A#);

   --  Other names:

   package S renames Standard;

   function Standard                      return X_Symbol is (16#11_008B#);
   function Boolean                       return X_Symbol is (16#11_008C#);
   function Integer                       return X_Symbol is (16#11_008D#);
   function Float                         return X_Symbol is (16#11_008E#);
   function Character                     return X_Symbol is (16#11_008F#);
   function Wide_Character                return X_Symbol is (16#11_0090#);
   function Wide_Wide_Character           return X_Symbol is (16#11_0091#);
   function String                        return X_Symbol is (16#11_0092#);
   function Wide_String                   return X_Symbol is (16#11_0093#);
   function Wide_Wide_String              return X_Symbol is (16#11_0094#);
   function Duration                      return X_Symbol is (16#11_0095#);
   function Root_Integer                  return X_Symbol is (16#11_0096#);
   function Root_Real                     return X_Symbol is (16#11_0097#);

end Program.Symbols;
