--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Strings.Wide_Wide_Hash;
with Ada.Wide_Wide_Characters.Handling;

package body Program.Symbols.Tables is

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Symbol_Reference) return S.Boolean is
      Left_Text  : constant Program.Text := Left.Buffer.Text (Left.Span);
      Right_Text : constant Program.Text := Right.Buffer.Text (Right.Span);
   begin
      if Left_Text (Left_Text'First) = ''' then
         return Left_Text = Right_Text;
      else
         return Ada.Wide_Wide_Characters.Handling.To_Lower (Left_Text)
           = Ada.Wide_Wide_Characters.Handling.To_Lower (Right_Text);
      end if;
   end Equal;

   ----------
   -- Find --
   ----------

   function Find
     (Self : Symbol_Table'Class; Value : Program.Text) return Symbol
   is
      type Dummy_Source_Buffer is new Program.Source_Buffers.Source_Buffer
      with null record;

      overriding function Text
        (Self   : Dummy_Source_Buffer;
         Unused : Program.Source_Buffers.Span)
            return Program.Text is (Value);

      overriding procedure Read
        (Self : in out Dummy_Source_Buffer;
         Data : out Program.Source_Buffers.Character_Info_Array;
         Last : out Natural) is null;

      overriding procedure Rewind
        (Self : in out Dummy_Source_Buffer) is null;

      Dummy  : aliased Dummy_Source_Buffer;
      Ref    : constant Symbol_Reference := (Dummy'Unchecked_Access, (1, 0));
      Cursor : constant Symbol_Maps.Cursor := Self.Map.Find (Ref);
   begin
      if Symbol_Maps.Has_Element (Cursor) then
         return Symbol_Maps.Element (Cursor);
      elsif Value (Value'First) = '''
        and Value (Value'Last) = '''
        and Value'Length = 3
      then
         --  Character literal
         return S.Wide_Wide_Character'Pos (Value (Value'First + 1));
      else
         return Program.Symbols.No_Symbol;
      end if;
   end Find;

   --------------------
   -- Find_Or_Create --
   --------------------

   procedure Find_Or_Create
     (Self   : in out Symbol_Table'Class;
      Buffer : not null Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
      Result : out Symbol)
   is
      Ref    : constant Symbol_Reference := (Buffer, Span);
      Cursor : constant Symbol_Maps.Cursor := Self.Map.Find (Ref);
   begin
      if Symbol_Maps.Has_Element (Cursor) then
         Result := Symbol_Maps.Element (Cursor);
         return;
      end if;

      pragma Assert (Buffer.Text (Span) (1) not in ''' | '"');

      Self.Last_Symbol := Self.Last_Symbol + 1;
      Result := Self.Last_Symbol;
      Self.Map.Insert (Ref, Result);
   end Find_Or_Create;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Symbol_Reference) return Ada.Containers.Hash_Type is
      Value_Text  : constant Program.Text := Value.Buffer.Text (Value.Span);
   begin
      if Value_Text (Value_Text'First) = ''' then
         return Ada.Strings.Wide_Wide_Hash (Value_Text);
      else
         return Ada.Strings.Wide_Wide_Hash
           (Ada.Wide_Wide_Characters.Handling.To_Lower (Value_Text));
      end if;
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Symbol_Table) is
      B : constant Program.Source_Buffers.Source_Buffer_Access :=
        Self.Buffer'Unchecked_Access;
   begin
      Self.Map.Insert ((B, (1, 0)), Less_Symbol);
      Self.Map.Insert ((B, (2, 0)), Equal_Symbol);
      Self.Map.Insert ((B, (3, 0)), Greater_Symbol);
      Self.Map.Insert ((B, (4, 0)), Hyphen_Symbol);
      Self.Map.Insert ((B, (5, 0)), Slash_Symbol);
      Self.Map.Insert ((B, (6, 0)), Star_Symbol);
      Self.Map.Insert ((B, (7, 0)), Ampersand_Symbol);
      Self.Map.Insert ((B, (8, 0)), Plus_Symbol);
      Self.Map.Insert ((B, (9, 0)), Less_Or_Equal_Symbol);
      Self.Map.Insert ((B, (10, 0)), Greater_Or_Equal_Symbol);
      Self.Map.Insert ((B, (11, 0)), Inequality_Symbol);
      Self.Map.Insert ((B, (12, 0)), Double_Star_Symbol);
      Self.Map.Insert ((B, (13, 0)), Or_Symbol);
      Self.Map.Insert ((B, (14, 0)), And_Symbol);
      Self.Map.Insert ((B, (15, 0)), Xor_Symbol);
      Self.Map.Insert ((B, (16, 0)), Mod_Symbol);
      Self.Map.Insert ((B, (17, 0)), Rem_Symbol);
      Self.Map.Insert ((B, (18, 0)), Abs_Symbol);
      Self.Map.Insert ((B, (19, 0)), Not_Symbol);

      Self.Map.Insert ((B, (20, 0)), All_Calls_Remote);
      Self.Map.Insert ((B, (21, 0)), Assert);
      Self.Map.Insert ((B, (22, 0)), Assertion_Policy);
      Self.Map.Insert ((B, (23, 0)), Asynchronous);
      Self.Map.Insert ((B, (24, 0)), Atomic);
      Self.Map.Insert ((B, (25, 0)), Atomic_Components);
      Self.Map.Insert ((B, (26, 0)), Attach_Handler);
      Self.Map.Insert ((B, (27, 0)), Controlled);
      Self.Map.Insert ((B, (28, 0)), Convention);
      Self.Map.Insert ((B, (29, 0)), Detect_Blocking);
      Self.Map.Insert ((B, (30, 0)), Discard_Names);
      Self.Map.Insert ((B, (31, 0)), Elaborate);
      Self.Map.Insert ((B, (32, 0)), Elaborate_All);
      Self.Map.Insert ((B, (33, 0)), Elaborate_Body);
      Self.Map.Insert ((B, (34, 0)), Export);
      Self.Map.Insert ((B, (35, 0)), Import);
      Self.Map.Insert ((B, (36, 0)), Inline);
      Self.Map.Insert ((B, (37, 0)), Inspection_Point);
      Self.Map.Insert ((B, (38, 0)), Interrupt_Handler);
      Self.Map.Insert ((B, (39, 0)), Interrupt_Priority);
      Self.Map.Insert ((B, (40, 0)), Linker_Options);
      Self.Map.Insert ((B, (41, 0)), List);
      Self.Map.Insert ((B, (42, 0)), Locking_Policy);
      Self.Map.Insert ((B, (43, 0)), No_Return);
      Self.Map.Insert ((B, (44, 0)), Normalize_Scalars);
      Self.Map.Insert ((B, (45, 0)), Optimize);
      Self.Map.Insert ((B, (46, 0)), Pack);
      Self.Map.Insert ((B, (47, 0)), Page);
      Self.Map.Insert ((B, (48, 0)), Partition_Elaboration_Policy);
      Self.Map.Insert ((B, (49, 0)), Preelaborable_Initialization);
      Self.Map.Insert ((B, (50, 0)), Preelaborate);
      Self.Map.Insert ((B, (51, 0)), Priority_Specific_Dispatching);
      Self.Map.Insert ((B, (52, 0)), Profile);
      Self.Map.Insert ((B, (53, 0)), Pure);
      Self.Map.Insert ((B, (54, 0)), Queuing_Policy);
      Self.Map.Insert ((B, (55, 0)), Relative_Deadline);
      Self.Map.Insert ((B, (56, 0)), Remote_Call_Interface);
      Self.Map.Insert ((B, (57, 0)), Remote_Types);
      Self.Map.Insert ((B, (58, 0)), Restrictions);
      Self.Map.Insert ((B, (59, 0)), Reviewable);
      Self.Map.Insert ((B, (60, 0)), Shared_Passive);
      Self.Map.Insert ((B, (61, 0)), Suppress);
      Self.Map.Insert ((B, (62, 0)), Task_Dispatching_Policy);
      Self.Map.Insert ((B, (63, 0)), Unchecked_Union);
      Self.Map.Insert ((B, (64, 0)), Unsuppress);
      Self.Map.Insert ((B, (65, 0)), Volatile);
      Self.Map.Insert ((B, (66, 0)), Volatile_Components);
   --  Attributes:
      Self.Map.Insert ((B, (67, 0)), Access_Symbol);
      Self.Map.Insert ((B, (68, 0)), Address);
      Self.Map.Insert ((B, (69, 0)), Adjacent);
      Self.Map.Insert ((B, (70, 0)), Aft);
      Self.Map.Insert ((B, (71, 0)), Alignment);
      Self.Map.Insert ((B, (72, 0)), Base);
      Self.Map.Insert ((B, (73, 0)), Bit_Order);
      Self.Map.Insert ((B, (74, 0)), Body_Version);
      Self.Map.Insert ((B, (75, 0)), Callable);
      Self.Map.Insert ((B, (76, 0)), Caller);
      Self.Map.Insert ((B, (77, 0)), Ceiling);
      Self.Map.Insert ((B, (78, 0)), Class);
      Self.Map.Insert ((B, (79, 0)), Component_Size);
      Self.Map.Insert ((B, (80, 0)), Compose);
      Self.Map.Insert ((B, (81, 0)), Constrained);
      Self.Map.Insert ((B, (82, 0)), Copy_Sign);
      Self.Map.Insert ((B, (83, 0)), Count);
      Self.Map.Insert ((B, (84, 0)), Definite);
      Self.Map.Insert ((B, (85, 0)), Delta_Symbol);
      Self.Map.Insert ((B, (86, 0)), Denorm);
      Self.Map.Insert ((B, (87, 0)), Digits_Symbol);
      Self.Map.Insert ((B, (88, 0)), Exponent);
      Self.Map.Insert ((B, (89, 0)), External_Tag);
      Self.Map.Insert ((B, (90, 0)), First);
      Self.Map.Insert ((B, (91, 0)), First_Bit);
      Self.Map.Insert ((B, (92, 0)), Floor);
      Self.Map.Insert ((B, (93, 0)), Fore);
      Self.Map.Insert ((B, (94, 0)), Fraction);
      Self.Map.Insert ((B, (95, 0)), Identity);
      Self.Map.Insert ((B, (96, 0)), Image);
      Self.Map.Insert ((B, (97, 0)), Input);
      Self.Map.Insert ((B, (98, 0)), Last);
      Self.Map.Insert ((B, (99, 0)), Last_Bit);
      Self.Map.Insert ((B, (100, 0)), Leading_Part);
      Self.Map.Insert ((B, (101, 0)), Length);
      Self.Map.Insert ((B, (102, 0)), Machine);
      Self.Map.Insert ((B, (103, 0)), Machine_Emax);
      Self.Map.Insert ((B, (104, 0)), Machine_Emin);
      Self.Map.Insert ((B, (105, 0)), Machine_Mantissa);
      Self.Map.Insert ((B, (106, 0)), Machine_Overflows);
      Self.Map.Insert ((B, (107, 0)), Machine_Radix);
      Self.Map.Insert ((B, (108, 0)), Machine_Rounding);
      Self.Map.Insert ((B, (109, 0)), Machine_Rounds);
      Self.Map.Insert ((B, (110, 0)), Max);
      Self.Map.Insert ((B, (111, 0)), Max_Size_In_Storage_Elements);
      Self.Map.Insert ((B, (112, 0)), Min);
      Self.Map.Insert ((B, (113, 0)), Mod_Keyword);
      Self.Map.Insert ((B, (114, 0)), Model);
      Self.Map.Insert ((B, (115, 0)), Model_Emin);
      Self.Map.Insert ((B, (116, 0)), Model_Epsilon);
      Self.Map.Insert ((B, (117, 0)), Model_Mantissa);
      Self.Map.Insert ((B, (118, 0)), Model_Small);
      Self.Map.Insert ((B, (119, 0)), Modulus);
      Self.Map.Insert ((B, (120, 0)), Output);
      Self.Map.Insert ((B, (121, 0)), Partition_ID);
      Self.Map.Insert ((B, (122, 0)), Pos);
      Self.Map.Insert ((B, (123, 0)), Position);
      Self.Map.Insert ((B, (124, 0)), Pred);
      Self.Map.Insert ((B, (125, 0)), Priority);
      Self.Map.Insert ((B, (126, 0)), Range_Keyword);
      Self.Map.Insert ((B, (127, 0)), Read);
      Self.Map.Insert ((B, (128, 0)), Remainder);
      Self.Map.Insert ((B, (129, 0)), Round);
      Self.Map.Insert ((B, (130, 0)), Rounding);
      Self.Map.Insert ((B, (131, 0)), Safe_First);
      Self.Map.Insert ((B, (132, 0)), Safe_Last);
      Self.Map.Insert ((B, (133, 0)), Scale);
      Self.Map.Insert ((B, (134, 0)), Scaling);
      Self.Map.Insert ((B, (135, 0)), Signed_Zeros);
      Self.Map.Insert ((B, (136, 0)), Size);
      Self.Map.Insert ((B, (137, 0)), Small);
      Self.Map.Insert ((B, (138, 0)), Storage_Pool);
      Self.Map.Insert ((B, (139, 0)), Storage_Size);
      Self.Map.Insert ((B, (140, 0)), Stream_Size);
      Self.Map.Insert ((B, (141, 0)), Succ);
      Self.Map.Insert ((B, (142, 0)), Tag);
      Self.Map.Insert ((B, (143, 0)), Terminated);
      Self.Map.Insert ((B, (144, 0)), Truncation);
      Self.Map.Insert ((B, (145, 0)), Unbiased_Rounding);
      Self.Map.Insert ((B, (146, 0)), Unchecked_Access);
      Self.Map.Insert ((B, (147, 0)), Val);
      Self.Map.Insert ((B, (148, 0)), Valid);
      Self.Map.Insert ((B, (149, 0)), Value);
      Self.Map.Insert ((B, (150, 0)), Version);
      Self.Map.Insert ((B, (151, 0)), Wide_Image);
      Self.Map.Insert ((B, (152, 0)), Wide_Value);
      Self.Map.Insert ((B, (153, 0)), Wide_Wide_Image);
      Self.Map.Insert ((B, (154, 0)), Wide_Wide_Value);
      Self.Map.Insert ((B, (155, 0)), Wide_Wide_Width);
      Self.Map.Insert ((B, (156, 0)), Wide_Width);
      Self.Map.Insert ((B, (157, 0)), Width);
      Self.Map.Insert ((B, (158, 0)), Write);
   --  Other names:
      Self.Map.Insert ((B, (159, 0)), Standard);
      Self.Map.Insert ((B, (160, 0)), Boolean);
      Self.Map.Insert ((B, (161, 0)), Integer);
      Self.Map.Insert ((B, (162, 0)), Float);
      Self.Map.Insert ((B, (163, 0)), Character);
      Self.Map.Insert ((B, (164, 0)), Wide_Character);
      Self.Map.Insert ((B, (165, 0)), Wide_Wide_Character);
      Self.Map.Insert ((B, (166, 0)), String);
      Self.Map.Insert ((B, (167, 0)), Wide_String);
      Self.Map.Insert ((B, (168, 0)), Wide_Wide_String);
      Self.Map.Insert ((B, (169, 0)), Duration);
      Self.Map.Insert ((B, (170, 0)), Root_Integer);
      Self.Map.Insert ((B, (171, 0)), Root_Real);
   end Initialize;

   ----------
   -- Text --
   ----------

   overriding function Text
     (Self : Predefined_Source_Buffer;
      Span : Program.Source_Buffers.Span) return Program.Text
   is
      pragma Unreferenced (Self);
   begin
      case Symbol (Span.From) is
         when 1 => return """<""";
         when 2 => return """=""";
         when 3 => return """>""";
         when 4 => return """-""";
         when 5 => return """/""";
         when 6 => return """*""";
         when 7 => return """&""";
         when 8 => return """+""";
         when 9 => return """<=""";
         when 10 => return """>=""";
         when 11 => return """/=""";
         when 12 => return """**""";
         when 13 => return """or""";
         when 14 => return """and""";
         when 15 => return """xor""";
         when 16 => return """mod""";
         when 17 => return """rem""";
         when 18 => return """abd""";
         when 19 => return """not""";
         when 20 => return "All_Calls_Remote";
         when 21 => return "Assert";
         when 22 => return "Assertion_Policy";
         when 23 => return "Asynchronous";
         when 24 => return "Atomic";
         when 25 => return "Atomic_Components";
         when 26 => return "Attach_Handler";
         when 27 => return "Controlled";
         when 28 => return "Convention";
         when 29 => return "Detect_Blocking";
         when 30 => return "Discard_Names";
         when 31 => return "Elaborate";
         when 32 => return "Elaborate_All";
         when 33 => return "Elaborate_Body";
         when 34 => return "Export";
         when 35 => return "Import";
         when 36 => return "Inline";
         when 37 => return "Inspection_Point";
         when 38 => return "Interrupt_Handler";
         when 39 => return "Interrupt_Priority";
         when 40 => return "Linker_Options";
         when 41 => return "List";
         when 42 => return "Locking_Policy";
         when 43 => return "No_Return";
         when 44 => return "Normalize_Scalars";
         when 45 => return "Optimize";
         when 46 => return "Pack";
         when 47 => return "Page";
         when 48 => return "Partition_Elaboration_Policy";
         when 49 => return "Preelaborable_Initialization";
         when 50 => return "Preelaborate";
         when 51 => return "Priority_Specific_Dispatching";
         when 52 => return "Profile";
         when 53 => return "Pure";
         when 54 => return "Queuing_Policy";
         when 55 => return "Relative_Deadline";
         when 56 => return "Remote_Call_Interface";
         when 57 => return "Remote_Types";
         when 58 => return "Restrictions";
         when 59 => return "Reviewable";
         when 60 => return "Shared_Passive";
         when 61 => return "Suppress";
         when 62 => return "Task_Dispatching_Policy";
         when 63 => return "Unchecked_Union";
         when 64 => return "Unsuppress";
         when 65 => return "Volatile";
         when 66 => return "Volatile_Components";
         when 67 => return "Access_Symbol";
         when 68 => return "Address";
         when 69 => return "Adjacent";
         when 70 => return "Aft";
         when 71 => return "Alignment";
         when 72 => return "Base";
         when 73 => return "Bit_Order";
         when 74 => return "Body_Version";
         when 75 => return "Callable";
         when 76 => return "Caller";
         when 77 => return "Ceiling";
         when 78 => return "Class";
         when 79 => return "Component_Size";
         when 80 => return "Compose";
         when 81 => return "Constrained";
         when 82 => return "Copy_Sign";
         when 83 => return "Count";
         when 84 => return "Definite";
         when 85 => return "Delta_Symbol";
         when 86 => return "Denorm";
         when 87 => return "Digits_Symbol";
         when 88 => return "Exponent";
         when 89 => return "External_Tag";
         when 90 => return "First";
         when 91 => return "First_Bit";
         when 92 => return "Floor";
         when 93 => return "Fore";
         when 94 => return "Fraction";
         when 95 => return "Identity";
         when 96 => return "Image";
         when 97 => return "Input";
         when 98 => return "Last";
         when 99 => return "Last_Bit";
         when 100 => return "Leading_Part";
         when 101 => return "Length";
         when 102 => return "Machine";
         when 103 => return "Machine_Emax";
         when 104 => return "Machine_Emin";
         when 105 => return "Machine_Mantissa";
         when 106 => return "Machine_Overflows";
         when 107 => return "Machine_Radix";
         when 108 => return "Machine_Rounding";
         when 109 => return "Machine_Rounds";
         when 110 => return "Max";
         when 111 => return "Max_Size_In_Storage_Elements";
         when 112 => return "Min";
         when 113 => return "Mod";
         when 114 => return "Model";
         when 115 => return "Model_Emin";
         when 116 => return "Model_Epsilon";
         when 117 => return "Model_Mantissa";
         when 118 => return "Model_Small";
         when 119 => return "Modulus";
         when 120 => return "Output";
         when 121 => return "Partition_ID";
         when 122 => return "Pos";
         when 123 => return "Position";
         when 124 => return "Pred";
         when 125 => return "Priority";
         when 126 => return "Range";
         when 127 => return "Read";
         when 128 => return "Remainder";
         when 129 => return "Round";
         when 130 => return "Rounding";
         when 131 => return "Safe_First";
         when 132 => return "Safe_Last";
         when 133 => return "Scale";
         when 134 => return "Scaling";
         when 135 => return "Signed_Zeros";
         when 136 => return "Size";
         when 137 => return "Small";
         when 138 => return "Storage_Pool";
         when 139 => return "Storage_Size";
         when 140 => return "Stream_Size";
         when 141 => return "Succ";
         when 142 => return "Tag";
         when 143 => return "Terminated";
         when 144 => return "Truncation";
         when 145 => return "Unbiased_Rounding";
         when 146 => return "Unchecked_Access";
         when 147 => return "Val";
         when 148 => return "Valid";
         when 149 => return "Value";
         when 150 => return "Version";
         when 151 => return "Wide_Image";
         when 152 => return "Wide_Value";
         when 153 => return "Wide_Wide_Image";
         when 154 => return "Wide_Wide_Value";
         when 155 => return "Wide_Wide_Width";
         when 156 => return "Wide_Width";
         when 157 => return "Width";
         when 158 => return "Write";
         when 159 => return "Standard";
         when 160 => return "Boolean";
         when 161 => return "Integer";
         when 162 => return "Float";
         when 163 => return "Character";
         when 164 => return "Wide_Character";
         when 165 => return "Wide_Wide_Character";
         when 166 => return "String";
         when 167 => return "Wide_String";
         when 168 => return "Wide_Wide_String";
         when 169 => return "Duration";
         when 170 => return "Root_Integer";
         when 171 => return "Root_Real";

         when others =>
            raise Constraint_Error;
      end case;
   end Text;

end Program.Symbols.Tables;
