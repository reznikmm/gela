with Gela.Grammars.LR_Parsers;
with Gela.LARL_Parsers.Data;
with Gela.LARL_Parsers_Nodes;
with Gela.LARL_Parsers.On_Reduce;
with Gela.Elements.Associations;
with Gela.Elements.Expression_Or_Boxes;
with Gela.Elements.Prefixes;
with Gela.Elements.Selector_Names;
with Gela.Elements.Identifiers;
with Gela.Elements.Defining_Expanded_Unit_Names;
with Gela.Elements.Constraints;
with Gela.Elements.Record_Aggregates;
with Gela.Elements.Composite_Constraints;

package body Gela.LARL_Parsers is

   use Gela.Lexical_Types;

   Map : constant array (Gela.Lexical_Types.Token_Kind)
     of Gela.Grammars.Terminal_Count :=
     (Error                   => 0,
      End_Of_Input            => 0,
      Abort_Token             => 1,
      Abs_Token               => 2,
      Abstract_Token          => 3,
      Accept_Token            => 4,
      Access_Token            => 5,
      Aliased_Token           => 6,
      All_Token               => 7,
      Ampersand_Token         => 8,
      And_Token               => 9,
      Apostrophe_Token        => 10,
      Array_Token             => 11,
      Arrow_Token             => 12,
      Assignment_Token        => 13,
      At_Token                => 14,
      Begin_Token             => 15,
      Body_Token              => 16,
      Box_Token               => 17,
      Case_Token              => 18,
      Character_Literal_Token => 19,
      Colon_Token             => 20,
      Comma_Token             => 21,
      Comment_Token           => 22,
      Constant_Token          => 23,
      Declare_Token           => 24,
      Delay_Token             => 25,
      Delta_Token             => 26,
      Digits_Token            => 27,
      Do_Token                => 28,
      Dot_Token               => 29,
      Double_Dot_Token        => 30,
      Double_Star_Token       => 31,
      Else_Token              => 32,
      Elsif_Token             => 33,
      End_Token               => 34,
      Entry_Token             => 35,
      Equal_Token             => 36,
      Exception_Token         => 37,
      Exit_Token              => 38,
      For_Token               => 39,
      Function_Token          => 40,
      Generic_Token           => 41,
      Goto_Token              => 42,
      Greater_Or_Equal_Token  => 43,
      Greater_Token           => 44,
      Hyphen_Token            => 45,
      Identifier_Token        => 46,
      If_Token                => 47,
      In_Token                => 48,
      Inequality_Token        => 49,
      Interface_Token         => 50,
      Is_Token                => 51,
      Left_Label_Token        => 52,
      Left_Parenthesis_Token  => 53,
      Less_Or_Equal_Token     => 54,
      Less_Token              => 55,
      Limited_Token           => 56,
      Loop_Token              => 57,
      Mod_Token               => 58,
      New_Token               => 59,
      Not_Token               => 60,
      Null_Token              => 61,
      Numeric_Literal_Token   => 62,
      Of_Token                => 63,
      Or_Token                => 64,
      Others_Token            => 65,
      Out_Token               => 66,
      Overriding_Token        => 67,
      Package_Token           => 68,
      Plus_Token              => 69,
      Pragma_Token            => 70,
      Private_Token           => 71,
      Procedure_Token         => 72,
      Protected_Token         => 73,
      Raise_Token             => 74,
      Range_Token             => 75,
      Record_Token            => 76,
      Rem_Token               => 77,
      Renames_Token           => 78,
      Requeue_Token           => 79,
      Return_Token            => 80,
      Reverse_Token           => 81,
      Right_Label_Token       => 82,
      Right_Parenthesis_Token => 83,
      Select_Token            => 84,
      Semicolon_Token         => 85,
      Separate_Token          => 86,
      Slash_Token             => 87,
      Some_Token              => 88,
      Star_Token              => 89,
      String_Literal_Token    => 90,
      Subtype_Token           => 91,
      Synchronized_Token      => 92,
      Tagged_Token            => 93,
      Task_Token              => 94,
      Terminate_Token         => 95,
      Then_Token              => 96,
      Type_Token              => 97,
      Until_Token             => 98,
      Use_Token               => 99,
      Vertical_Line_Token     => 100,
      When_Token              => 101,
      While_Token             => 102,
      With_Token              => 103,
      Xor_Token               => 104);

   type Input_Wrapper is record
      Last_Token : Gela.Lexical_Types.Token_Index;
      Input      : Gela.Parsers.Parser_Input_Access;
   end record;

   procedure Next_Token
     (Self  : access Input_Wrapper;
      Token : out Gela.Grammars.Terminal_Count;
      Value : out Gela.LARL_Parsers_Nodes.Node);

   ----------------
   -- Infix_Call --
   ----------------

   function Infix_Call
     (Self   : access Parser_Context;
      Prefix : Gela.Lexical_Types.Token_Count;
      Left   : Gela.Elements.Expressions.Expression_Access;
      Right  : Gela.Elements.Expressions.Expression_Access := null)
      return Gela.Elements.Function_Calls.Function_Call_Access
   is
      use type Gela.Elements.Expressions.Expression_Access;

      L    : constant Gela.Elements.Expression_Or_Boxes.
        Expression_Or_Box_Access :=
          Gela.Elements.Expression_Or_Boxes.Expression_Or_Box_Access (Left);
      R    : constant Gela.Elements.Expression_Or_Boxes.
        Expression_Or_Box_Access :=
          Gela.Elements.Expression_Or_Boxes.Expression_Or_Box_Access (Right);
      P    : Gela.Elements.Prefixes.Prefix_Access;
      Arg  : Gela.Elements.Associations.Association_Access :=
        Self.Fabric.Association
          (Array_Component_Choices => Self.Fabric.Discrete_Choice_Sequence,
           Arrow_Token             => 0,
           Component_Expression    => L);
      Args : constant Gela.Elements.Associations.Association_Sequence_Access :=
        Self.Fabric.Association_Sequence;
   begin
      Args.Append (Arg);

      if Right /= null then
         Arg := Self.Fabric.Association
           (Array_Component_Choices => Self.Fabric.Discrete_Choice_Sequence,
            Arrow_Token             => 0,
            Component_Expression    => R);

         Args.Append (Arg);
      end if;

      P := Gela.Elements.Prefixes.Prefix_Access
        (Self.Fabric.Operator_Symbol (Prefix));

      return Self.Fabric.Function_Call
        (Prefix                   => P,
         Function_Call_Parameters => Self.Fabric.Record_Aggregate
           (Left_Token                    => 0,
            Record_Component_Associations => Args,
            Right_Token                   => 0));
   end Infix_Call;

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Self  : access Input_Wrapper;
      Token : out Gela.Grammars.Terminal_Count;
      Value : out Gela.LARL_Parsers_Nodes.Node)
   is
      use Gela.LARL_Parsers_Nodes;
      Next : Gela.Lexical_Types.Token_Kind;
   begin
      Self.Input.Next_Token
        (Token => Next,
         Index => Self.Last_Token);

      Token := Map (Next);
      Value := +Self.Last_Token;
   end Next_Token;

   -----------
   -- Parse --
   -----------

   overriding procedure Parse
     (Self       : in out Parser;
      Input      : not null access Gela.Parsers.Parser_Input'Class;
      Fabric     : not null Gela.Element_Fabrics.Element_Fabric_Access;
      Root       : out Gela.Elements.Compilations.Compilation_Access;
      Last_Token : out Gela.Lexical_Types.Token_Index)
   is
      pragma Unreferenced (Self);

      procedure Do_It is new Gela.Grammars.LR_Parsers.Parse
        (Node        => Gela.LARL_Parsers_Nodes.Node,
         Node_Array  => Gela.LARL_Parsers_Nodes.Node_Array,
         Lexer       => Input_Wrapper,
         Parser      => Parser_Context,
         Next_Token  => Next_Token,
         Next_Action => Gela.LARL_Parsers.Data.Next_Action,
         Go_To       => Gela.LARL_Parsers.Data.Go_To,
         On_Reduce   => Gela.LARL_Parsers.On_Reduce);

      use Gela.LARL_Parsers_Nodes;
      Result  : Gela.LARL_Parsers_Nodes.Node;
      Success : Boolean;
      Wrapper : aliased Input_Wrapper := (1, Input);
      Context : aliased Parser_Context := (Fabric => Fabric);
   begin
      Do_It (Context'Access, Wrapper'Access, Result, Success);
      if Success then
         Root := -Result;
      end if;
      Last_Token := Wrapper.Last_Token;
   end Parse;

   -----------------------------------
   -- To_Defining_Program_Unit_Name --
   -----------------------------------

   function To_Defining_Program_Unit_Name
     (Self       : access Parser_Context;
      Value : Gela.Elements.Selected_Identifiers.Selected_Identifier_Access)
      return Gela.Elements.Defining_Program_Unit_Names.
               Defining_Program_Unit_Name_Access
   is
      Selector : constant Gela.Elements.Selector_Names.Selector_Name_Access :=
        Value.Selector;
      Identifier : constant Gela.Elements.Identifiers.Identifier_Access :=
        Gela.Elements.Identifiers.Identifier_Access (Selector);
      Result : Gela.Elements.Defining_Expanded_Unit_Names.
        Defining_Expanded_Unit_Name_Access;
   begin
      Result := Self.Fabric.Defining_Expanded_Unit_Name
        (Defining_Prefix   => Value.Prefix,
         Dot_Token         => Value.Dot_Token,
         Defining_Selector => Self.Fabric.Defining_Identifier
           (Identifier_Token => Identifier.Identifier_Token));

      return Gela.Elements.Defining_Program_Unit_Names.
               Defining_Program_Unit_Name_Access (Result);
   end To_Defining_Program_Unit_Name;

   ---------------------------
   -- To_Subtype_Indication --
   ---------------------------

   function To_Subtype_Indication
     (Self       : access Parser_Context;
      Not_Token  : Gela.Lexical_Types.Token_Count;
      Null_Token : Gela.Lexical_Types.Token_Count;
      Mark       : Gela.Elements.Subtype_Marks.Subtype_Mark_Access;
      Constraint : Gela.Elements.Scalar_Constraints.Scalar_Constraint_Access)
      return Gela.Elements.Subtype_Indications.Subtype_Indication_Access
   is
      use type Gela.Elements.Constraints.Constraint_Access;
      Subtype_Mark       : Gela.Elements.Subtype_Marks.Subtype_Mark_Access;
      Subtype_Constraint : Gela.Elements.Constraints.Constraint_Access;
   begin
      Subtype_Mark := Mark;
      Subtype_Constraint := Gela.Elements.Constraints.Constraint_Access
        (Constraint);

      if Subtype_Constraint = null and then
        Mark.all in Gela.Elements.Function_Calls.Function_Call'Class
      then
         declare
            Call : constant Gela.Elements.Function_Calls.Function_Call_Access
              := Gela.Elements.Function_Calls.Function_Call_Access (Mark);
            Prefix : constant Gela.Elements.Prefixes.Prefix_Access :=
              Call.Prefix;
            Args   : constant Gela.Elements.Record_Aggregates.
              Record_Aggregate_Access := Call.Function_Call_Parameters;
            Ass : constant Gela.Elements.Associations.
              Association_Sequence_Access :=
                Args.Record_Component_Associations;
            CC     : constant Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access := Self.Fabric.Composite_Constraint
                (Left_Token   => Args.Last_Token,
                 Associations => Ass,
                 Right_Token  => Args.Right_Token);
         begin
            Subtype_Mark := Gela.Elements.Subtype_Marks.Subtype_Mark_Access
              (Prefix);
            Subtype_Constraint := Gela.Elements.Constraints.Constraint_Access
              (CC);
         end;
      end if;

      return Self.Fabric.Subtype_Indication
        (Not_Token          => Not_Token,
         Null_Token         => Null_Token,
         Subtype_Mark       => Subtype_Mark,
         Subtype_Constraint => Subtype_Constraint);

   end To_Subtype_Indication;

end Gela.LARL_Parsers;
