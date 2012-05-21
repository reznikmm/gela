package body Asis.Gela.To is

   generic
      type Asis_Kinds is (<>);
      type Table_Type is array (P.Global_Kinds) of Asis_Kinds;
      Table : in Table_Type;
   function Convert (Kind : P.Global_Kinds) return Asis_Kinds;

   -------------
   -- Convert --
   -------------

   function Convert (Kind : P.Global_Kinds) return Asis_Kinds is
   begin
      return Table (Kind);
   end Convert;

   --------------------------------------------------------------------------
   -- Tables types:
   --------------------------------------------------------------------------

   type Element_Kinds_Table is array (P.Global_Kinds) of Asis.Element_Kinds;
   type Defining_Name_Kinds_Table is array (P.Global_Kinds) of
     Asis.Defining_Name_Kinds;
   type Declaration_Kinds_Table is array (P.Global_Kinds) of
     Asis.Declaration_Kinds;
   type Definition_Kinds_Table is array (P.Global_Kinds) of
     Asis.Definition_Kinds;
   type Type_Kinds_Table is array (P.Global_Kinds) of Asis.Type_Kinds;
   type Formal_Type_Kinds_Table is array (P.Global_Kinds) of
     Asis.Formal_Type_Kinds;
   type Access_Type_Kinds_Table is array (P.Global_Kinds) of
     Asis.Access_Type_Kinds;
   type Access_Definition_Kinds_Table is array (P.Global_Kinds) of
     Asis.Access_Definition_Kinds;
   type Constraint_Kinds_Table is array (P.Global_Kinds) of
     Asis.Constraint_Kinds;
   type Discrete_Range_Kinds_Table is array (P.Global_Kinds) of
     Asis.Discrete_Range_Kinds;
   type Association_Kinds_Table is array (P.Global_Kinds) of
     Asis.Association_Kinds;
   type Expression_Kinds_Table is array (P.Global_Kinds) of
     Asis.Expression_Kinds;
   type Statement_Kinds_Table is array (P.Global_Kinds) of
     Asis.Statement_Kinds;
   type Path_Kinds_Table is array (P.Global_Kinds) of Asis.Path_Kinds;
   type Clause_Kinds_Table is array (P.Global_Kinds) of Asis.Clause_Kinds;
   type Representation_Clause_Kinds_Table is array (P.Global_Kinds) of
     Asis.Representation_Clause_Kinds;

   --------------------------------------------------------------------------
   -- Tables :
   --------------------------------------------------------------------------

   To_Element_Kinds : constant Element_Kinds_Table :=
     (P.A_Pragma             => Asis.A_Pragma,
      P.A_Defining_Name      => Asis.A_Defining_Name,
      P.A_Declaration        => Asis.A_Declaration,
      P.A_Definition         => Asis.A_Definition,
      P.An_Expression        => Asis.An_Expression,
      P.An_Association       => Asis.An_Association,
      P.A_Statement          => Asis.A_Statement,
      P.A_Path               => Asis.A_Path,
      P.A_Clause             => Asis.A_Clause,
      P.An_Exception_Handler => Asis.An_Exception_Handler,
      others                 => Asis.Not_An_Element);

   To_Defining_Name_Kinds : constant Defining_Name_Kinds_Table :=
     (P.A_Defining_Identifier          => Asis.A_Defining_Identifier,
      P.A_Defining_Character_Literal   => Asis.A_Defining_Character_Literal,
      P.A_Defining_Enumeration_Literal => Asis.A_Defining_Enumeration_Literal,
      P.A_Defining_Operator_Symbol     => Asis.A_Defining_Operator_Symbol,
      P.A_Defining_Expanded_Name       => Asis.A_Defining_Expanded_Name,
      others                         => Asis.Not_A_Defining_Name);

   To_Declaration_Kinds : constant Declaration_Kinds_Table :=
     (P.An_Ordinary_Type_Declaration    => Asis.An_Ordinary_Type_Declaration,
      P.A_Task_Type_Declaration         => Asis.A_Task_Type_Declaration,
      P.A_Protected_Type_Declaration    => Asis.A_Protected_Type_Declaration,
      P.An_Incomplete_Type_Declaration  => Asis.An_Incomplete_Type_Declaration,
      P.A_Private_Type_Declaration      => Asis.A_Private_Type_Declaration,
      P.A_Private_Extension_Declaration =>
        Asis.A_Private_Extension_Declaration,

      P.A_Subtype_Declaration           => Asis.A_Subtype_Declaration,
      P.A_Variable_Declaration          => Asis.A_Variable_Declaration,
      P.A_Constant_Declaration          => Asis.A_Constant_Declaration,
      P.A_Deferred_Constant_Declaration =>
        Asis.A_Deferred_Constant_Declaration,

      P.A_Single_Task_Declaration       => Asis.A_Single_Task_Declaration,
      P.A_Single_Protected_Declaration  => Asis.A_Single_Protected_Declaration,
      P.An_Integer_Number_Declaration   => Asis.An_Integer_Number_Declaration,
      P.A_Real_Number_Declaration       => Asis.A_Real_Number_Declaration,

      P.An_Enumeration_Literal_Specification =>
        Asis.An_Enumeration_Literal_Specification,

      P.A_Discriminant_Specification    => Asis.A_Discriminant_Specification,
      P.A_Component_Declaration         => Asis.A_Component_Declaration,
      P.A_Return_Object_Specification   => Asis.A_Return_Object_Specification,
      P.A_Loop_Parameter_Specification  => Asis.A_Loop_Parameter_Specification,
      P.A_Procedure_Declaration         => Asis.A_Procedure_Declaration,
      P.A_Function_Declaration          => Asis.A_Function_Declaration,
      P.A_Parameter_Specification       => Asis.A_Parameter_Specification,
      P.A_Procedure_Body_Declaration    => Asis.A_Procedure_Body_Declaration,
      P.A_Function_Body_Declaration     => Asis.A_Function_Body_Declaration,
      P.A_Package_Declaration           => Asis.A_Package_Declaration,
      P.A_Package_Body_Declaration      => Asis.A_Package_Body_Declaration,
      P.An_Object_Renaming_Declaration  => Asis.An_Object_Renaming_Declaration,

      P.An_Exception_Renaming_Declaration =>
        Asis.An_Exception_Renaming_Declaration,

      P.A_Package_Renaming_Declaration  => Asis.A_Package_Renaming_Declaration,

      P.A_Procedure_Renaming_Declaration =>
        Asis.A_Procedure_Renaming_Declaration,

      P.A_Function_Renaming_Declaration =>
        Asis.A_Function_Renaming_Declaration,

      P.A_Generic_Package_Renaming_Declaration =>
        Asis.A_Generic_Package_Renaming_Declaration,

      P.A_Generic_Procedure_Renaming_Declaration =>
        Asis.A_Generic_Procedure_Renaming_Declaration,

      P.A_Generic_Function_Renaming_Declaration =>
        Asis.A_Generic_Function_Renaming_Declaration,

      P.A_Task_Body_Declaration         => Asis.A_Task_Body_Declaration,
      P.A_Protected_Body_Declaration    => Asis.A_Protected_Body_Declaration,
      P.An_Entry_Declaration            => Asis.An_Entry_Declaration,
      P.An_Entry_Body_Declaration       => Asis.An_Entry_Body_Declaration,
      P.An_Entry_Index_Specification    => Asis.An_Entry_Index_Specification,
      P.A_Procedure_Body_Stub           => Asis.A_Procedure_Body_Stub,
      P.A_Function_Body_Stub            => Asis.A_Function_Body_Stub,
      P.A_Package_Body_Stub             => Asis.A_Package_Body_Stub,
      P.A_Task_Body_Stub                => Asis.A_Task_Body_Stub,
      P.A_Protected_Body_Stub           => Asis.A_Protected_Body_Stub,
      P.An_Exception_Declaration        => Asis.An_Exception_Declaration,

      P.A_Choice_Parameter_Specification =>
        Asis.A_Choice_Parameter_Specification,

      P.A_Generic_Procedure_Declaration =>
        Asis.A_Generic_Procedure_Declaration,

      P.A_Generic_Function_Declaration  => Asis.A_Generic_Function_Declaration,
      P.A_Generic_Package_Declaration   => Asis.A_Generic_Package_Declaration,
      P.A_Package_Instantiation         => Asis.A_Package_Instantiation,
      P.A_Procedure_Instantiation       => Asis.A_Procedure_Instantiation,
      P.A_Function_Instantiation        => Asis.A_Function_Instantiation,
      P.A_Formal_Object_Declaration     => Asis.A_Formal_Object_Declaration,
      P.A_Formal_Type_Declaration       => Asis.A_Formal_Type_Declaration,
      P.A_Formal_Procedure_Declaration  => Asis.A_Formal_Procedure_Declaration,
      P.A_Formal_Function_Declaration   => Asis.A_Formal_Function_Declaration,
      P.A_Formal_Package_Declaration    => Asis.A_Formal_Package_Declaration,

      P.A_Formal_Package_Declaration_With_Box =>
        Asis.A_Formal_Package_Declaration_With_Box,

      others                         => Asis.Not_A_Declaration);

   To_Definition_Kinds : constant Definition_Kinds_Table :=
     (P.A_Type_Definition              => Asis.A_Type_Definition,
      P.A_Subtype_Indication           => Asis.A_Subtype_Indication,
      P.A_Constraint                   => Asis.A_Constraint,
      P.A_Component_Definition         => Asis.A_Component_Definition,
      P.A_Discrete_Subtype_Definition  => Asis.A_Discrete_Subtype_Definition,
      P.A_Discrete_Range               => Asis.A_Discrete_Range,
      P.An_Unknown_Discriminant_Part   => Asis.An_Unknown_Discriminant_Part,
      P.A_Known_Discriminant_Part      => Asis.A_Known_Discriminant_Part,
      P.A_Record_Definition            => Asis.A_Record_Definition,
      P.A_Null_Record_Definition       => Asis.A_Null_Record_Definition,
      P.A_Null_Component               => Asis.A_Null_Component,
      P.A_Variant_Part                 => Asis.A_Variant_Part,
      P.A_Variant                      => Asis.A_Variant,
      P.An_Others_Choice               => Asis.An_Others_Choice,
      P.An_Access_Definition           => Asis.An_Access_Definition,
      P.An_Incomplete_Type_Definition  => Asis.An_Incomplete_Type_Definition,

      P.A_Tagged_Incomplete_Type_Definition =>
        Asis.A_Tagged_Incomplete_Type_Definition,

      P.A_Private_Type_Definition     => Asis.A_Private_Type_Definition,

      P.A_Tagged_Private_Type_Definition =>
        Asis.A_Tagged_Private_Type_Definition,

      P.A_Private_Extension_Definition => Asis.A_Private_Extension_Definition,
      P.A_Task_Definition              => Asis.A_Task_Definition,
      P.A_Protected_Definition         => Asis.A_Protected_Definition,
      P.A_Formal_Type_Definition       => Asis.A_Formal_Type_Definition,
      others                         => Asis.Not_A_Definition);


   To_Type_Kinds : constant Type_Kinds_Table :=
     (P.A_Derived_Type_Definition      => Asis.A_Derived_Type_Definition,

      P.A_Derived_Record_Extension_Definition =>
        Asis.A_Derived_Record_Extension_Definition,

      P.An_Enumeration_Type_Definition => Asis.An_Enumeration_Type_Definition,

      P.A_Signed_Integer_Type_Definition =>
        Asis.A_Signed_Integer_Type_Definition,

      P.A_Modular_Type_Definition      => Asis.A_Modular_Type_Definition,
      P.A_Root_Type_Definition         => Asis.A_Root_Type_Definition,
      P.A_Floating_Point_Definition    => Asis.A_Floating_Point_Definition,

      P.An_Ordinary_Fixed_Point_Definition =>
        Asis.An_Ordinary_Fixed_Point_Definition,

      P.A_Decimal_Fixed_Point_Definition =>
        Asis.A_Decimal_Fixed_Point_Definition,

      P.An_Unconstrained_Array_Definition =>
        Asis.An_Unconstrained_Array_Definition,

      P.A_Constrained_Array_Definition  => Asis.A_Constrained_Array_Definition,
      P.A_Record_Type_Definition        => Asis.A_Record_Type_Definition,

      P.A_Tagged_Record_Type_Definition =>
        Asis.A_Tagged_Record_Type_Definition,

      P.An_Interface_Type_Definition    => Asis.An_Interface_Type_Definition,
      P.An_Access_Type_Definition       => Asis.An_Access_Type_Definition,
      others                          => Asis.Not_A_Type_Definition);

   To_Formal_Type_Kinds : constant Formal_Type_Kinds_Table :=
     (P.A_Formal_Private_Type_Definition =>
        Asis.A_Formal_Private_Type_Definition,
      P.A_Formal_Tagged_Private_Type_Definition =>
        Asis.A_Formal_Tagged_Private_Type_Definition,
      P.A_Formal_Derived_Type_Definition =>
        Asis.A_Formal_Derived_Type_Definition,
      P.A_Formal_Discrete_Type_Definition =>
        Asis.A_Formal_Discrete_Type_Definition,
      P.A_Formal_Signed_Integer_Type_Definition =>
        Asis.A_Formal_Signed_Integer_Type_Definition,
      P.A_Formal_Modular_Type_Definition =>
        Asis.A_Formal_Modular_Type_Definition,
      P.A_Formal_Floating_Point_Definition =>
        Asis.A_Formal_Floating_Point_Definition,
      P.A_Formal_Ordinary_Fixed_Point_Definition =>
        Asis.A_Formal_Ordinary_Fixed_Point_Definition,
      P.A_Formal_Decimal_Fixed_Point_Definition =>
        Asis.A_Formal_Decimal_Fixed_Point_Definition,
      P.A_Formal_Unconstrained_Array_Definition =>
        Asis.A_Formal_Unconstrained_Array_Definition,
      P.A_Formal_Constrained_Array_Definition =>
        Asis.A_Formal_Constrained_Array_Definition,
      P.A_Formal_Access_Type_Definition =>
        Asis.A_Formal_Access_Type_Definition,
      P.A_Formal_Interface_Type_Definition =>
        Asis.A_Formal_Interface_Type_Definition,
      others                          => Asis.Not_A_Formal_Type_Definition);

   To_Access_Type_Kinds : constant Access_Type_Kinds_Table :=
     (P.A_Pool_Specific_Access_To_Variable =>
        Asis.A_Pool_Specific_Access_To_Variable,

      P.An_Access_To_Variable  => Asis.An_Access_To_Variable,
      P.An_Access_To_Constant  => Asis.An_Access_To_Constant,
      P.An_Access_To_Procedure => Asis.An_Access_To_Procedure,

      P.An_Access_To_Protected_Procedure =>
        Asis.An_Access_To_Protected_Procedure,

      P.An_Access_To_Function  => Asis.An_Access_To_Function,

      P.An_Access_To_Protected_Function =>
        Asis.An_Access_To_Protected_Function,

      P.A_F_Pool_Specific_Access_To_Variable =>
        Asis.A_Pool_Specific_Access_To_Variable,

      P.An_F_Access_To_Variable  => Asis.An_Access_To_Variable,
      P.An_F_Access_To_Constant  => Asis.An_Access_To_Constant,
      P.An_F_Access_To_Procedure => Asis.An_Access_To_Procedure,

      P.An_F_Access_To_Protected_Procedure =>
        Asis.An_Access_To_Protected_Procedure,

      P.An_F_Access_To_Function  => Asis.An_Access_To_Function,

      P.An_F_Access_To_Protected_Function =>
        Asis.An_Access_To_Protected_Function,

      others                          => Asis.Not_An_Access_Type_Definition);

   To_Access_Definition_Kinds : constant Access_Definition_Kinds_Table :=
     (P.An_Anonymous_Access_To_Variable =>
        Asis.An_Anonymous_Access_To_Variable,

      P.An_Anonymous_Access_To_Constant =>
        Asis.An_Anonymous_Access_To_Constant,

      P.An_Anonymous_Access_To_Procedure =>
        Asis.An_Anonymous_Access_To_Procedure,

      P.An_Anonymous_Access_To_Protected_Procedure =>
        Asis.An_Anonymous_Access_To_Protected_Procedure,

      P.An_Anonymous_Access_To_Function =>
        Asis.An_Anonymous_Access_To_Function,

      P.An_Anonymous_Access_To_Protected_Function =>
        Asis.An_Anonymous_Access_To_Protected_Function,

      others => Asis.Not_An_Access_Definition);

   To_Constraint_Kinds : constant Constraint_Kinds_Table :=
     (P.A_Range_Attribute_Reference => Asis.A_Range_Attribute_Reference,
      P.A_Simple_Expression_Range   => Asis.A_Simple_Expression_Range,
      P.A_Digits_Constraint         => Asis.A_Digits_Constraint,
      P.A_Delta_Constraint          => Asis.A_Delta_Constraint,
      P.An_Index_Constraint         => Asis.An_Index_Constraint,
      P.A_Discriminant_Constraint   => Asis.A_Discriminant_Constraint,
      others                      => Asis.Not_A_Constraint);

   To_Discrete_Range_Kinds : constant Discrete_Range_Kinds_Table :=
     (P.A_Discrete_Subtype_Indication => Asis.A_Discrete_Subtype_Indication,

      P.A_Discrete_Range_Attribute_Reference =>
        Asis.A_Discrete_Range_Attribute_Reference,

      P.A_Discrete_Simple_Expression_Range =>
        Asis.A_Discrete_Simple_Expression_Range,

      P.A_S_Discrete_Subtype_Indication => Asis.A_Discrete_Subtype_Indication,

      P.A_S_Discrete_Range_Attribute_Reference =>
        Asis.A_Discrete_Range_Attribute_Reference,

      P.A_S_Discrete_Simple_Expression_Range =>
        Asis.A_Discrete_Simple_Expression_Range,

      others => Asis.Not_A_Discrete_Range);

   To_Association_Kinds : constant Association_Kinds_Table :=
     (P.A_Pragma_Argument_Association  => Asis.A_Pragma_Argument_Association,
      P.A_Discriminant_Association     => Asis.A_Discriminant_Association,
      P.A_Record_Component_Association => Asis.A_Record_Component_Association,
      P.An_Array_Component_Association => Asis.An_Array_Component_Association,
      P.A_Parameter_Association        => Asis.A_Parameter_Association,
      P.A_Generic_Association          => Asis.A_Generic_Association,
      others                         => Asis.Not_An_Association);

   To_Expression_Kinds : constant Expression_Kinds_Table :=
     (P.A_Box_Expression               => Asis.A_Box_Expression,
      P.An_Integer_Literal             => Asis.An_Integer_Literal,
      P.A_Real_Literal                 => Asis.A_Real_Literal,
      P.A_String_Literal               => Asis.A_String_Literal,
      P.An_Identifier                  => Asis.An_Identifier,
      P.An_Operator_Symbol             => Asis.An_Operator_Symbol,
      P.A_Character_Literal            => Asis.A_Character_Literal,
      P.An_Enumeration_Literal         => Asis.An_Enumeration_Literal,
      P.An_Explicit_Dereference        => Asis.An_Explicit_Dereference,
      P.A_Function_Call                => Asis.A_Function_Call,
      P.An_Indexed_Component           => Asis.An_Indexed_Component,
      P.A_Slice                        => Asis.A_Slice,
      P.A_Selected_Component           => Asis.A_Selected_Component,
      P.An_Attribute_Reference         => Asis.An_Attribute_Reference,
      P.A_Record_Aggregate             => Asis.A_Record_Aggregate,
      P.An_Extension_Aggregate         => Asis.An_Extension_Aggregate,
      P.A_Positional_Array_Aggregate   => Asis.A_Positional_Array_Aggregate,
      P.A_Named_Array_Aggregate        => Asis.A_Named_Array_Aggregate,
      P.An_And_Then_Short_Circuit      => Asis.An_And_Then_Short_Circuit,
      P.An_Or_Else_Short_Circuit       => Asis.An_Or_Else_Short_Circuit,
      P.An_In_Range_Membership_Test    => Asis.An_In_Range_Membership_Test,
      P.A_Not_In_Range_Membership_Test => Asis.A_Not_In_Range_Membership_Test,
      P.An_In_Type_Membership_Test     => Asis.An_In_Type_Membership_Test,
      P.A_Not_In_Type_Membership_Test  => Asis.A_Not_In_Type_Membership_Test,
      P.A_Null_Literal                 => Asis.A_Null_Literal,
      P.A_Parenthesized_Expression     => Asis.A_Parenthesized_Expression,
      P.A_Type_Conversion              => Asis.A_Type_Conversion,
      P.A_Qualified_Expression         => Asis.A_Qualified_Expression,
      P.An_Allocation_From_Subtype     => Asis.An_Allocation_From_Subtype,

      P.An_Allocation_From_Qualified_Expression =>
        Asis.An_Allocation_From_Qualified_Expression,

      others                         => Asis.Not_An_Expression);

   To_Statement_Kinds : constant Statement_Kinds_Table :=
     (P.A_Null_Statement               => Asis.A_Null_Statement,
      P.An_Assignment_Statement        => Asis.An_Assignment_Statement,
      P.An_If_Statement                => Asis.An_If_Statement,
      P.A_Case_Statement               => Asis.A_Case_Statement,
      P.A_Loop_Statement               => Asis.A_Loop_Statement,
      P.A_While_Loop_Statement         => Asis.A_While_Loop_Statement,
      P.A_For_Loop_Statement           => Asis.A_For_Loop_Statement,
      P.A_Block_Statement              => Asis.A_Block_Statement,
      P.An_Exit_Statement              => Asis.An_Exit_Statement,
      P.A_Goto_Statement               => Asis.A_Goto_Statement,
      P.A_Procedure_Call_Statement     => Asis.A_Procedure_Call_Statement,
      P.A_Simple_Return_Statement      => Asis.A_Simple_Return_Statement,
      P.An_Extended_Return_Statement   => Asis.An_Extended_Return_Statement,
      P.An_Accept_Statement            => Asis.An_Accept_Statement,
      P.An_Entry_Call_Statement        => Asis.An_Entry_Call_Statement,
      P.A_Requeue_Statement            => Asis.A_Requeue_Statement,
      P.A_Requeue_Statement_With_Abort => Asis.A_Requeue_Statement_With_Abort,
      P.A_Delay_Until_Statement        => Asis.A_Delay_Until_Statement,
      P.A_Delay_Relative_Statement     => Asis.A_Delay_Relative_Statement,

      P.A_Terminate_Alternative_Statement =>
        Asis.A_Terminate_Alternative_Statement,

      P.A_Selective_Accept_Statement   => Asis.A_Selective_Accept_Statement,
      P.A_Timed_Entry_Call_Statement   => Asis.A_Timed_Entry_Call_Statement,

      P.A_Conditional_Entry_Call_Statement =>
        Asis.A_Conditional_Entry_Call_Statement,

      P.An_Asynchronous_Select_Statement =>
        Asis.An_Asynchronous_Select_Statement,

      P.An_Abort_Statement => Asis.An_Abort_Statement,
      P.A_Raise_Statement  => Asis.A_Raise_Statement,
      P.A_Code_Statement   => Asis.A_Code_Statement,
      others             => Asis.Not_A_Statement);

   To_Path_Kinds : constant Path_Kinds_Table :=
     (P.An_If_Path        => Asis.An_If_Path,
      P.An_Elsif_Path     => Asis.An_Elsif_Path,
      P.An_Else_Path      => Asis.An_Else_Path,
      P.A_Case_Path       => Asis.A_Case_Path,
      P.A_Select_Path     => Asis.A_Select_Path,
      P.An_Or_Path        => Asis.An_Or_Path,
      P.A_Then_Abort_Path => Asis.A_Then_Abort_Path,
      others            => Asis.Not_A_Path);

   To_Clause_Kinds : constant Clause_Kinds_Table :=
     (P.A_Use_Package_Clause    => Asis.A_Use_Package_Clause,
      P.A_Use_Type_Clause       => Asis.A_Use_Type_Clause,
      P.A_With_Clause           => Asis.A_With_Clause,
      P.A_Representation_Clause => Asis.A_Representation_Clause,
      P.A_Component_Clause      => Asis.A_Component_Clause,
      others                  => Asis.Not_A_Clause);

   To_Representation_Clause_Kinds : constant Representation_Clause_Kinds_Table
     :=
     (P.An_Attribute_Definition_Clause => Asis.An_Attribute_Definition_Clause,

      P.An_Enumeration_Representation_Clause =>
        Asis.An_Enumeration_Representation_Clause,

      P.A_Record_Representation_Clause => Asis.A_Record_Representation_Clause,
      P.An_At_Clause                   => Asis.An_At_Clause,
      others                         => Asis.Not_A_Representation_Clause);

   --------------------------------------------------------------------------
   -- Instances:
   --------------------------------------------------------------------------

   function Element_Kinds_Inst is
      new Convert (Asis.Element_Kinds, Element_Kinds_Table, To_Element_Kinds);

   function Defining_Name_Kinds_Inst is
      new Convert (Asis.Defining_Name_Kinds,
                   Defining_Name_Kinds_Table,
                   To_Defining_Name_Kinds);

   function Declaration_Kinds_Inst is
      new Convert (Asis.Declaration_Kinds,
                   Declaration_Kinds_Table,
                   To_Declaration_Kinds);

   function Definition_Kinds_Inst is
      new Convert (Asis.Definition_Kinds,
                   Definition_Kinds_Table,
                   To_Definition_Kinds);

   function Type_Kinds_Inst is
      new Convert (Asis.Type_Kinds,
                   Type_Kinds_Table,
                   To_Type_Kinds);

   function Formal_Type_Kinds_Inst is
      new Convert (Asis.Formal_Type_Kinds,
                   Formal_Type_Kinds_Table,
                   To_Formal_Type_Kinds);

   function Access_Type_Kinds_Inst is
      new Convert (Asis.Access_Type_Kinds,
                   Access_Type_Kinds_Table,
                   To_Access_Type_Kinds);

   function Access_Definition_Kinds_Inst is
      new Convert (Asis.Access_Definition_Kinds,
                   Access_Definition_Kinds_Table,
                   To_Access_Definition_Kinds);

   function Constraint_Kinds_Inst is
      new Convert (Asis.Constraint_Kinds,
                   Constraint_Kinds_Table,
                   To_Constraint_Kinds);

   function Discrete_Range_Kinds_Inst is
      new Convert (Asis.Discrete_Range_Kinds,
                   Discrete_Range_Kinds_Table,
                   To_Discrete_Range_Kinds);

   function Association_Kinds_Inst is
      new Convert (Asis.Association_Kinds,
                   Association_Kinds_Table,
                   To_Association_Kinds);

   function Expression_Kinds_Inst is
      new Convert (Asis.Expression_Kinds,
                   Expression_Kinds_Table,
                   To_Expression_Kinds);

   function Statement_Kinds_Inst is
      new Convert (Asis.Statement_Kinds,
                   Statement_Kinds_Table,
                   To_Statement_Kinds);

   function Path_Kinds_Inst is new
     Convert (Asis.Path_Kinds, Path_Kinds_Table, To_Path_Kinds);

   function Clause_Kinds_Inst is new
     Convert (Asis.Clause_Kinds, Clause_Kinds_Table, To_Clause_Kinds);

   function Representation_Clause_Kinds_Inst is
      new Convert (Asis.Representation_Clause_Kinds,
                   Representation_Clause_Kinds_Table,
                   To_Representation_Clause_Kinds);

   --------------------------------------------------------------------------
   -- Renaming:
   --------------------------------------------------------------------------

   function Element_Kinds
     (Kind : P.Global_Kinds) return Asis.Element_Kinds renames
     Element_Kinds_Inst;

   function Defining_Name_Kinds
     (Kind : P.Global_Kinds) return Asis.Defining_Name_Kinds renames
     Defining_Name_Kinds_Inst;

   function Declaration_Kinds
     (Kind : P.Global_Kinds) return Asis.Declaration_Kinds renames
     Declaration_Kinds_Inst;

   function Definition_Kinds
     (Kind : P.Global_Kinds) return Asis.Definition_Kinds renames
     Definition_Kinds_Inst;

   function Type_Kinds
     (Kind : P.Global_Kinds) return Asis.Type_Kinds renames
     Type_Kinds_Inst;

   function Formal_Type_Kinds
     (Kind : P.Global_Kinds) return Asis.Formal_Type_Kinds renames
     Formal_Type_Kinds_Inst;

   function Access_Type_Kinds
     (Kind : P.Global_Kinds) return Asis.Access_Type_Kinds renames
     Access_Type_Kinds_Inst;

   function Access_Definition_Kinds
     (Kind : P.Global_Kinds) return Asis.Access_Definition_Kinds renames
     Access_Definition_Kinds_Inst;

   function Constraint_Kinds
     (Kind : P.Global_Kinds) return Asis.Constraint_Kinds renames
     Constraint_Kinds_Inst;

   function Discrete_Range_Kinds
     (Kind : P.Global_Kinds) return Asis.Discrete_Range_Kinds renames
     Discrete_Range_Kinds_Inst;

   function Association_Kinds
     (Kind : P.Global_Kinds) return Asis.Association_Kinds renames
     Association_Kinds_Inst;

   function Expression_Kinds
     (Kind : P.Global_Kinds) return Asis.Expression_Kinds renames
     Expression_Kinds_Inst;

   function Statement_Kinds
     (Kind : P.Global_Kinds) return Asis.Statement_Kinds renames
     Statement_Kinds_Inst;

   function Path_Kinds
     (Kind : P.Global_Kinds) return Asis.Path_Kinds renames
     Path_Kinds_Inst;

   function Clause_Kinds
     (Kind : P.Global_Kinds) return Asis.Clause_Kinds renames
     Clause_Kinds_Inst;

   function Representation_Clause_Kinds
     (Kind : P.Global_Kinds) return Asis.Representation_Clause_Kinds renames
     Representation_Clause_Kinds_Inst;

end Asis.Gela.To;


------------------------------------------------------------------------------
--  Copyright (c) 2009, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
