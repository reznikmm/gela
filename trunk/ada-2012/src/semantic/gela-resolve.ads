with Gela.Compilations;
with Gela.Interpretations;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Elements.Defining_Names;
with Gela.Elements.Generic_Associations;

package Gela.Resolve is
   pragma Preelaborate;

   procedure Direct_Name
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Set    : out Gela.Interpretations.Interpretation_Set_Index);
   --  Resolve Symbol as direct_name and populate interpretation set with
   --  defining name interpretations

   procedure Selected_Component
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Set    : out Gela.Interpretations.Interpretation_Set_Index);
   --  Resolve Symbol as selected_component and populate interpretation set
   --  with defining name interpretations

   procedure Attribute_Reference
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Set    : out Gela.Interpretations.Interpretation_Set_Index);
   --  Resolve Symbol as attr. reference designator and populate interpretation
   --  set with interpretations

   procedure Simple_Expression_Range
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Left   : Gela.Interpretations.Interpretation_Set_Index;
      Right  : Gela.Interpretations.Interpretation_Set_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index);

   procedure Membership_Test
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Left   : Gela.Interpretations.Interpretation_Set_Index;
      Right  : Gela.Interpretations.Interpretation_Set_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index);

   procedure Discrete_Range
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Tipe       : out Gela.Semantic_Types.Type_Index);

   procedure Discrete_Range_Lower
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Result     : out Gela.Interpretations.Interpretation_Index);

   procedure Discrete_Range_Upper
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Result     : out Gela.Interpretations.Interpretation_Index);

   procedure Function_Call
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Args   : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index);

   procedure Qualified_Expression
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Arg    : Gela.Interpretations.Interpretation_Set_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index);

   procedure Shall_Be_Subtype
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index);
   --  Set of interpretation shall resolve to denote a subtype. 3.2.2 (8)

   procedure To_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);
   --  Resolve Type_Up to be type, then resolve Expr_Up have this type

   procedure To_The_Same_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);
   --  Resolve Type_Up to be an expression of some type, then resolve Expr_Up
   --  to have this type.

   procedure Case_Statement
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Tuple   : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);
   --  Resolve Type_Up to be an expression of some type, then resolve each item
   --  of Tuple to have this type.

   procedure To_Type_Or_The_Same_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);
   --  Resolve Type_Up to be a type or an expression of some type,
   --  then resolve Expr_Up to have this type.

   procedure Character_Literal
     (Comp   : Gela.Compilations.Compilation_Access;
      Result : out Gela.Interpretations.Interpretation_Set_Index);

   procedure Numeric_Literal
     (Comp   : Gela.Compilations.Compilation_Access;
      Token  : Gela.Lexical_Types.Token_Count;
      Result : out Gela.Interpretations.Interpretation_Set_Index);

   procedure String_Literal
     (Comp   : Gela.Compilations.Compilation_Access;
      Token  : Gela.Lexical_Types.Token_Count;
      Result : out Gela.Interpretations.Interpretation_Set_Index);

   procedure Interpretation
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index);
   --  Get any interpretation from the Set excluding Symbol

   function Placeholder
     (Comp : Gela.Compilations.Compilation_Access)
      return Gela.Interpretations.Interpretation_Set_Index;

   procedure Constraint
     (Constraint : access Gela.Elements.Element'Class;
      Env        : Gela.Semantic_Types.Env_Index;
      Type_Up    : Gela.Interpretations.Interpretation_Set_Index;
      Constr     : Gela.Interpretations.Interpretation_Set_Index;
      Result     : out Gela.Interpretations.Interpretation_Index);

   procedure Variant_Part
     (Comp     : Gela.Compilations.Compilation_Access;
      Env      : Gela.Semantic_Types.Env_Index;
      Name_Up  : Gela.Interpretations.Interpretation_Set_Index;
      Variants : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result   : out Gela.Interpretations.Interpretation_Index);
   --  Resolve variant_part using Name_Up as interpretations of discriminant,
   --  Variants is putle of tuples of discrete_choice interpretations

   procedure Assignment_Right
     (Comp     : Gela.Compilations.Compilation_Access;
      Env      : Gela.Semantic_Types.Env_Index;
      Left     : Gela.Interpretations.Interpretation_Set_Index;
      Right    : Gela.Interpretations.Interpretation_Set_Index;
      Result   : out Gela.Interpretations.Interpretation_Index);

   procedure Signed_Integer_Type
     (Comp     : Gela.Compilations.Compilation_Access;
      Up       : Gela.Interpretations.Interpretation_Set_Index;
      Result   : out Gela.Interpretations.Interpretation_Index);

   procedure Real_Type
     (Comp     : Gela.Compilations.Compilation_Access;
      Up       : Gela.Interpretations.Interpretation_Set_Index;
      Result   : out Gela.Interpretations.Interpretation_Index);

   function Record_Matcher
     return not null Gela.Interpretations.Type_Matcher_Access;

   function Array_Matcher
     return not null Gela.Interpretations.Type_Matcher_Access;

   procedure Record_Aggregate
     (Comp     : Gela.Compilations.Compilation_Access;
      Env      : Gela.Semantic_Types.Env_Index;
      Up       : Gela.Interpretations.Interpretation_Index;
      Tuple    : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result   : out Gela.Interpretations.Interpretation_Index);

   procedure Generic_Association
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
--      Actual_Part  : Gela.Elements.Generic_Associations.
--                       Generic_Association_Sequence_Access;
      Up      : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);

   procedure Generic_Association_List
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      Instance     : Gela.Elements.Element_Access;
      Generic_Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Actual_Part  : Gela.Elements.Generic_Associations.
                       Generic_Association_Sequence_Access;
      Associations : Gela.Interpretations.Interpretation_Tuple_Index;
      Result       : out Gela.Interpretations.Interpretation_Index);

end Gela.Resolve;
