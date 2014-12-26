with League.Characters;
with League.String_Vectors;

package body Gela.Plain_Symbol_Sets is

   use Gela.Lexical_Types;

   function To_Operator_Symbol
     (Text : League.Strings.Universal_String) return First_Symbols;
   --  Return symbol corresponding to an operator (rem mod not xor or abs and)
   --  or '0' otherwise

   --  Cast List_Index into SYmbol/Symbol_List and backward
   function To_Symbol
     (Value : List_Index) return Gela.Lexical_Types.Symbol;
   function To_Symbol_List (List : List_Index) return List_Symbol;
   function From_Symbol
     (Value : Gela.Lexical_Types.Symbol) return List_Index;
   function From_Symbol_List (List : List_Symbol) return List_Index;

   Map_1 : constant array (Wide_Wide_Character range '&' .. '>') of
     First_Symbols :=
     ('<' => Token_Kind'Pos (Less_Token),
      '=' => Token_Kind'Pos (Equal_Token),
      '>' => Token_Kind'Pos (Greater_Token),
      '-' => Token_Kind'Pos (Hyphen_Token),
      '/' => Token_Kind'Pos (Slash_Token),
      '*' => Token_Kind'Pos (Star_Token),
      '&' => Token_Kind'Pos (Ampersand_Token),
      '+' => Token_Kind'Pos (Plus_Token),
      others => 0);
   --  Single char operators

   Map_2 : constant array (Wide_Wide_Character range '/' .. '>') of
     First_Symbols :=
     ('<' => Token_Kind'Pos (Less_Or_Equal_Token),
      '/' => Token_Kind'Pos (Inequality_Token),
      '>' => Token_Kind'Pos (Greater_Or_Equal_Token),
      others => 0);
   --  First char of two chars operator (<=, /=, >=)

   Power_Value : constant Operator_Symbol :=
     Token_Kind'Pos (Double_Star_Token);
   --  ** operator symbol

   --  Images of predefined operators
   A1 : constant array (Symbol range 1 .. 8) of Wide_Wide_String (1 .. 3) :=
     ("""<""", """=""", """>""", """-""",
      """/""", """*""", """&""", """+""");
   A2 : constant array (Symbol range 9 .. 13) of Wide_Wide_String (1 .. 4)
     := ("""<=""", """>=""", """/=""", """**""", """or""");
   A3 : constant array (Symbol range 14 .. 19) of Wide_Wide_String (1 .. 5)
     := ("""and""", """xor""", """mod""", """rem""", """abs""", """not""");

   -----------------
   -- Create_List --
   -----------------

   overriding procedure Create_List
     (Self  : in out Symbol_Set;
      Head  : Gela.Lexical_Types.Symbol;
      Tail  : Gela.Lexical_Types.Symbol_List :=
        Gela.Lexical_Types.Empty_Symbol_List;
      Value : out Gela.Lexical_Types.Symbol_List)
   is
      Node   : constant List_Node := (Head, Gela.Lexical_Types.Symbol (Tail));
      Cursor : List_Maps.Cursor;
   begin
      if Tail = Gela.Lexical_Types.Empty_Symbol_List then
         --  Symbol and Symbol_List have the same encoding, just cast.
         Value := Gela.Lexical_Types.Symbol_List (Head);
         return;
      end if;

      Cursor := Self.List_Map.Find (Node);

      if List_Maps.Has_Element (Cursor) then
         Value := To_Symbol_List (List_Maps.Element (Cursor));
      else
         Self.Lists.Append (Node);
         Self.List_Map.Insert (Node, Self.Lists.Last_Index);
         Value := To_Symbol_List (Self.Lists.Last_Index);
      end if;
   end Create_List;

   -----------------
   -- Create_List --
   -----------------

   overriding procedure Create_List
     (Self  : in out Symbol_Set;
      Head  : Gela.Lexical_Types.Symbol_List;
      Tail  : Gela.Lexical_Types.Symbol_List;
      Value : out Gela.Lexical_Types.Symbol_List) is
   begin
      if Head = Empty_Symbol_List then
         Value := Tail;
      else
         Self.Create_List (Head  => Self.Tail (Head),
                           Tail  => Tail,
                           Value => Value);

         Self.Create_List (Head  => Self.Head (Head),
                           Tail  => Value,
                           Value => Value);
      end if;
   end Create_List;

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch
     (Self  : in out Symbol_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Lexical_Types.Symbol) is
   begin
      if Image.Element (1).To_Wide_Wide_Character in ''' | '"' | '%' then
         Value := Self.Get (Image);
         return;
      elsif Image.Index ('.') > 0 then
         declare
            List   : League.String_Vectors.Universal_String_Vector;
            Suffix : Gela.Lexical_Types.Symbol;
         begin
            List := Image.Split ('.');
            Self.Fetch (List.Element (1), Value);

            for J in 2 .. List.Length loop
               Self.Fetch (List.Element (J), Suffix);
               Self.Join (Value, Suffix, Value);
            end loop;

            return;
         end;
      end if;

      declare
         Cursor : String_Maps.Cursor := Self.Original.Find (Image);
         Folded : League.Strings.Universal_String;
      begin
         if String_Maps.Has_Element (Cursor) then
            Value := String_Maps.Element (Cursor);
            return;
         end if;

         Folded := Image.To_Simple_Casefold;
         Cursor := Self.Folded.Find (Folded);

         if String_Maps.Has_Element (Cursor) then
            Value := String_Maps.Element (Cursor);
            return;
         end if;

         Self.Values.Append ((Original => Image, Folded => Folded));
         Value := Gela.Lexical_Types.Symbol (Self.Values.Last_Index);
         Self.Original.Insert (Image, Value);
         Self.Folded.Insert (Folded, Value);
      end;
   end Fetch;

   ------------
   -- Folded --
   ------------

   overriding function Folded
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
   begin
      if Value not in Simple_Symbol then
         return Folded (Self, Self.Prefix (Value)) & '.' &
           Folded (Self, Self.Selector (Value));
      elsif Value < 16#11_0000# then
         return Self.Image (Value);
      else
         return Self.Values.Element (Symbol_Index (Value)).Folded;
      end if;
   end Folded;

   -----------------
   -- From_Symbol --
   -----------------

   function From_Symbol
     (Value : Gela.Lexical_Types.Symbol) return List_Index is
   begin
      return List_Index (Value - Compound_Symbol'First);
   end From_Symbol;

   ----------------------
   -- From_Symbol_List --
   ----------------------

   function From_Symbol_List (List : List_Symbol) return List_Index is
   begin
      return List_Index (List - List_Symbol'First);
   end From_Symbol_List;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self  : Symbol_Set;
      Image : League.Strings.Universal_String)
      return Gela.Lexical_Types.Symbol
   is
      use type League.Characters.Universal_Character;

      Length : constant Natural := Image.Length;
      First  : constant Wide_Wide_Character :=
        Image.Element (1).To_Wide_Wide_Character;
      Char   : Wide_Wide_Character;
   begin
      if First = ''' then  --  Character literal
         return Wide_Wide_Character'Pos
           (Image.Element (2).To_Wide_Wide_Character);
      elsif First /= '"' then  --  Identifier
         if Image.Index ('.') > 0 then  --  Compound name
            declare
               List   : League.String_Vectors.Universal_String_Vector;
               Suffix : Gela.Lexical_Types.Symbol;
               Value  : Gela.Lexical_Types.Symbol;
               Node   : List_Node;
               Cursor : List_Maps.Cursor;
            begin
               List := Image.Split ('.');
               Value := Self.Get (List.Element (1));

               if Value = Gela.Lexical_Types.No_Symbol then
                  return Gela.Lexical_Types.No_Symbol;
               end if;

               for J in 2 .. List.Length loop
                  Suffix := Self.Get (List.Element (J));
                  Node := (Value, Suffix);
                  Cursor := Self.List_Map.Find (Node);

                  if List_Maps.Has_Element (Cursor) then
                     Value := To_Symbol (List_Maps.Element (Cursor));
                  else
                     return Gela.Lexical_Types.No_Symbol;
                  end if;
               end loop;

               return Value;
            end;
         end if;

         declare
            Cursor : String_Maps.Cursor := Self.Original.Find (Image);
         begin
            if String_Maps.Has_Element (Cursor) then
               return String_Maps.Element (Cursor);
            end if;

            Cursor := Self.Folded.Find (Image.To_Simple_Casefold);

            if String_Maps.Has_Element (Cursor) then
               return String_Maps.Element (Cursor);
            end if;

            return Gela.Lexical_Types.No_Symbol;
         end;
      elsif Length = 3 then  --  String literal (one charater) "#"
         Char := Image.Element (2).To_Wide_Wide_Character;

         if Char in Map_1'Range then
            return Map_1 (Char);
         end if;
      elsif Length = 4 then  --  String literal (two charaters) "##"
         if Image.Element (3) = '=' then
            Char := Image.Element (2).To_Wide_Wide_Character;

            if Char in Map_2'Range then
               return Map_2 (Char);
            end if;
         elsif Image.Element (2) = '*' and Image.Element (3) = '*' then
            return Power_Value;
         else  --  This could be "or"
            return To_Operator_Symbol (Image);
         end if;
      elsif Length = 5 then  --  String literal (three charaters) "###"
         return To_Operator_Symbol (Image);
      end if;

      return Gela.Lexical_Types.No_Symbol;
   end Get;

   ----------
   -- Hash --
   ----------

   function Hash (Item : List_Node) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Ada.Containers.Hash_Type (Item.Right) * 3571 +
        Ada.Containers.Hash_Type (Item.Left);
   end Hash;

   ----------
   -- Head --
   ----------

   overriding function Head
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol_List)
      return Gela.Lexical_Types.Symbol is
   begin
      if Value in List_Symbol then
         declare
            Index : constant List_Index := From_Symbol_List (Value);
            Node  : constant List_Node := Self.Lists.Element (Index);
         begin
            return Node.Left;
         end;
      else
         --  Symbol and Symbol_List have the same encoding, just cast.
         return Gela.Lexical_Types.Symbol (Value);
      end if;
   end Head;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
   begin
      case Value is
         when 0 =>
            return League.Strings.Empty_Universal_String;
         when Operator_Symbol =>
            return Self.Operator (Value);
         when 20 .. 16#10_FFFF# =>
            declare
               Result : Wide_Wide_String := "'_'";
            begin
               Result (2) := Wide_Wide_Character'Val (Value);
               return League.Strings.To_Universal_String (Result);
            end;
         when 16#11_0000# .. Simple_Symbol'Last =>
            return Self.Values.Element (Symbol_Index (Value)).Original;
         when Compound_Symbol =>
            return Self.Image (Self.Prefix (Value)) & '.' &
              Self.Image (Self.Selector (Value));
         when others =>
            raise Constraint_Error;
      end case;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize (Self  : in out Symbol_Set) is
      pragma Warnings (Off);  --  Disable unreferenced warning for literals
      type Predefined_Symbol_Name is
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
         Priority_Symbol,
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
         Storage_Size_Symbol,
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
         Write_Symbol,
         Standard_Symbol,
         Boolean_Symbol,
         Integer_Symbol,
         Float_Symbol,
         Character_Symbol,
         Wide_Character_Symbol,
         Wide_Wide_Character_Symbol,
         String_Symbol,
         Wide_String_Symbol,
         Wide_Wide_String_Symbol,
         Duration_Symbol);
      pragma Warnings (On);

      function To_Mixed_Case
        (Name : Wide_Wide_String) return League.Strings.Universal_String;

      -------------------
      -- To_Mixed_Case --
      -------------------

      function To_Mixed_Case
        (Name : Wide_Wide_String) return League.Strings.Universal_String
      is
         List  : League.String_Vectors.Universal_String_Vector;
         Image : League.Strings.Universal_String;
         Upper : constant League.Strings.Universal_String :=
           League.Strings.To_Universal_String (Name);
      begin
         List := Upper.Split ('_');

         for K in 1 .. List.Length loop
            Image := List.Element (K).To_Lowercase;
            Image.Replace (1, 1, List.Element (K).Slice (1, 1));
            List.Replace (K, Image);
         end loop;

         Image := List.Join ('_');

         return Image;
      end To_Mixed_Case;

      Ignore : Gela.Lexical_Types.Symbol;
   begin
      for J in A1'Range loop
         Self.Operator (J) := League.Strings.To_Universal_String (A1 (J));
      end loop;

      for J in A2'Range loop
         Self.Operator (J) := League.Strings.To_Universal_String (A2 (J));
      end loop;

      for J in A3'Range loop
         Self.Operator (J) := League.Strings.To_Universal_String (A3 (J));
      end loop;

      for J in Self.Operator'Range loop
         Self.Fetch (Self.Operator (J), Ignore);
      end loop;

      for J in Predefined_Symbol_Name'Range loop
         declare
            Name  : constant Wide_Wide_String :=
              Predefined_Symbol_Name'Wide_Wide_Image (J);
            Image : constant League.Strings.Universal_String :=
              To_Mixed_Case (Name (1 .. Name'Last - 7));
         begin
            Self.Fetch (Image, Ignore);

            if Ignore /= 16#11_0000# + Predefined_Symbol_Name'Pos (J) then
               raise Constraint_Error;
            end if;
         end;
      end loop;
   end Initialize;

   ----------
   -- Join --
   ----------

   overriding procedure Join
     (Self  : in out Symbol_Set;
      Left  : Gela.Lexical_Types.Symbol;
      Right : Gela.Lexical_Types.Symbol;
      Value : out Gela.Lexical_Types.Symbol)
   is
      Node   : constant List_Node := (Left, Right);
      Cursor : constant List_Maps.Cursor := Self.List_Map.Find (Node);
   begin
      if List_Maps.Has_Element (Cursor) then
         Value := To_Symbol (List_Maps.Element (Cursor));
      else
         Self.Lists.Append (Node);
         Self.List_Map.Insert (Node, Self.Lists.Last_Index);
         Value := To_Symbol (Self.Lists.Last_Index);
      end if;
   end Join;

   ------------
   -- Prefix --
   ------------

   overriding function Prefix
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return Gela.Lexical_Types.Symbol is
   begin
      if Value in Simple_Symbol then
         return Gela.Lexical_Types.No_Symbol;
      else
         return Self.Lists.Element (From_Symbol (Value)).Left;
      end if;
   end Prefix;

   --------------
   -- Selector --
   --------------

   overriding function Selector
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return Gela.Lexical_Types.Symbol is
   begin
      if Value in Simple_Symbol then
         return Value;
      else
         return Self.Lists.Element (From_Symbol (Value)).Right;
      end if;
   end Selector;

   ----------
   -- Tail --
   ----------

   overriding function Tail
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol_List)
      return Gela.Lexical_Types.Symbol_List is
   begin
      if Value in List_Symbol then
         declare
            Index : constant List_Index := From_Symbol_List (Value);
            Node  : constant List_Node := Self.Lists.Element (Index);
         begin
            --  Symbol and Symbol_List have the same encoding, just cast.
            return Gela.Lexical_Types.Symbol_List (Node.Right);
         end;
      else
         return Gela.Lexical_Types.Empty_Symbol_List;
      end if;
   end Tail;

   ------------------------
   -- To_Operator_Symbol --
   ------------------------

   function To_Operator_Symbol
     (Text : League.Strings.Universal_String) return First_Symbols
   is
      X : constant Wide_Wide_String :=
        Text.Slice (2, Text.Length - 1).To_Simple_Casefold.To_Wide_Wide_String;
   begin
      if X = "abs" then
         return Token_Kind'Pos (Abs_Token);
      elsif X = "and" then
         return Token_Kind'Pos (And_Token);
      elsif X = "mod" then
         return Token_Kind'Pos (Mod_Token);
      elsif X = "not" then
         return Token_Kind'Pos (Not_Token);
      elsif X = "or" then
         return Token_Kind'Pos (Or_Token);
      elsif X = "rem" then
         return Token_Kind'Pos (Rem_Token);
      elsif X = "xor" then
         return Token_Kind'Pos (Xor_Token);
      else
         return 0;
      end if;
   end To_Operator_Symbol;

   ---------------
   -- To_Symbol --
   ---------------

   function To_Symbol
     (Value : List_Index)
      return Gela.Lexical_Types.Symbol is
   begin
      return Gela.Lexical_Types.Symbol (Value) + Compound_Symbol'First;
   end To_Symbol;

   --------------------
   -- To_Symbol_List --
   --------------------

   function To_Symbol_List (List : List_Index) return List_Symbol is
   begin
      return Gela.Lexical_Types.Symbol_List (List) + List_Symbol'First;
   end To_Symbol_List;

end Gela.Plain_Symbol_Sets;
