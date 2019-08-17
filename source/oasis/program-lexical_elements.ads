--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Lexical_Elements is

   pragma Pure;

   type Lexical_Element is limited interface;

   type Lexical_Element_Access is
     access all Lexical_Element'Class with Storage_Size => 0;

   function Assigned (Self : access Lexical_Element'Class) return Boolean
     is (Self /= null);

   not overriding function Image (Self  : Lexical_Element) return Text
     is abstract;
   --  Return text of the lexical element.

   type Lexical_Element_Kind is
     (Less, Equal, Greater, Hyphen, Slash,
      Star, Ampersand, Plus,
      Less_Or_Equal, Greater_Or_Equal, Inequality,
      Double_Star,

      Or_Keyword, And_Keyword, Xor_Keyword, Mod_Keyword, Rem_Keyword,
      Abs_Keyword, Not_Keyword,

      Right_Label,
      Box, Left_Label,
      Assignment, Arrow,
      Double_Dot,
      Apostrophe, Left_Parenthesis,
      Right_Parenthesis,
      Comma, Dot,
      Colon, Semicolon,
      Vertical_Line,

      Abort_Keyword,
      Abstract_Keyword, Accept_Keyword,
      Access_Keyword, Aliased_Keyword, All_Keyword,
      Array_Keyword, At_Keyword,
      Begin_Keyword, Body_Keyword, Case_Keyword,
      Constant_Keyword, Declare_Keyword, Delay_Keyword,
      Delta_Keyword, Digits_Keyword, Do_Keyword,
      Else_Keyword, Elsif_Keyword, End_Keyword,
      Entry_Keyword, Exception_Keyword, Exit_Keyword,
      For_Keyword, Function_Keyword, Generic_Keyword,
      Goto_Keyword, If_Keyword, In_Keyword,
      Interface_Keyword, Is_Keyword, Limited_Keyword,
      Loop_Keyword, New_Keyword,
      Null_Keyword, Of_Keyword,
      Others_Keyword, Out_Keyword,
      Overriding_Keyword, Package_Keyword, Pragma_Keyword,
      Private_Keyword, Procedure_Keyword, Protected_Keyword,
      Raise_Keyword, Range_Keyword, Record_Keyword,
      Renames_Keyword, Requeue_Keyword,
      Return_Keyword, Reverse_Keyword, Select_Keyword,
      Separate_Keyword, Some_Keyword, Subtype_Keyword, Synchronized_Keyword,
      Tagged_Keyword, Task_Keyword, Terminate_Keyword,
      Then_Keyword, Type_Keyword, Until_Keyword,
      Use_Keyword, When_Keyword, While_Keyword,
      With_Keyword,
      Comment, Identifier, Numeric_Literal,
      Character_Literal, String_Literal,
      Error, End_Of_Input);

   subtype Operator_Kind is Lexical_Element_Kind range Less .. Double_Star;

   subtype Keyword_Operator_Kind is Lexical_Element_Kind
     range Or_Keyword .. Not_Keyword;

   subtype Keyword_Kind is Lexical_Element_Kind
     range Abort_Keyword .. With_Keyword;

   not overriding function Kind (Self  : Lexical_Element)
     return Lexical_Element_Kind is abstract;

   type Lexical_Element_Vector is limited interface;
   --  Vector of lexical elements.

   type Lexical_Element_Vector_Access is
     access all Lexical_Element_Vector'Class
       with Storage_Size => 0;

   not overriding function First_Index (Self : Lexical_Element_Vector)
     return Positive is abstract;

   not overriding function Last_Index (Self : Lexical_Element_Vector)
     return Positive is abstract;
   --  The vector always has at least one element.

   function Length (Self : Lexical_Element_Vector'Class)
     return Positive is (Self.Last_Index - Self.First_Index + 1);
   --  Return number of elements in the vector

   not overriding function Element
     (Self  : Lexical_Element_Vector;
      Index : Positive)
      return not null Lexical_Element_Access
        is abstract
     with Pre'Class => Index in Self.First_Index .. Self.Last_Index;
   --  Return an element of the vector

   function First
     (Self  : Lexical_Element_Vector'Class)
      return not null Program.Lexical_Elements.Lexical_Element_Access
        with Inline;
   --  Get the first element of the vector

   function Last
     (Self  : Lexical_Element_Vector'Class)
      return not null Program.Lexical_Elements.Lexical_Element_Access
        with Inline;
   --  Get the last element of the vector

end Program.Lexical_Elements;
