with Gela.Elements.Defining_Names;
with Gela.Elements;
with Gela.Interpretations;
with Gela.Lexical_Types;
with Gela.Property_Setters;
with Gela.Semantic_Types;

package Gela.Property_Resets is
   pragma Preelaborate;

   type Property_Reset is limited new Gela.Property_Setters.Property_Setter
     with record
      Defining_Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Down          : Gela.Interpretations.Interpretation_Index := 0;
      Env_In        : Gela.Semantic_Types.Env_Index := 0;
      Env_Out       : Gela.Semantic_Types.Env_Index := 0;
      Errors        : Gela.Semantic_Types.Error_Set_Index := 0;
      Expanded      : Gela.Elements.Element_Access;
      Full_Name     : Gela.Lexical_Types.Symbol := 0;
      Index         : Gela.Lexical_Types.Token_Count := 0;
      Static_Value  : Gela.Semantic_Types.Value_Index := 0;
      Type_Index    : Gela.Semantic_Types.Type_Index := 0;
      Up_Set        : Gela.Interpretations.Interpretation_Set_Index := 0;
      Up_Tuple      : Gela.Interpretations.Interpretation_Tuple_Index := 0;

      Name_List     : Gela.Lexical_Types.Symbol_List :=
        Gela.Lexical_Types.Empty_Symbol_List;

      Up_Tuple_List : Gela.Interpretations.Interpretation_Tuple_List_Index
        := 0;

      With_List     : Gela.Lexical_Types.Symbol_List :=
        Gela.Lexical_Types.Empty_Symbol_List;

      Limited_With_List : Gela.Lexical_Types.Symbol_List :=
        Gela.Lexical_Types.Empty_Symbol_List;

      Chosen_Interpretation : Gela.Interpretations.Interpretation_Kinds :=
        Gela.Interpretations.Unknown;

      Declarative_Region : Gela.Semantic_Types.Env_Index := 0;

      Corresponding_Generic_Element : Gela.Elements.Element_Access;
      Corresponding_View : Gela.Elements.Element_Access;
      Corresponding_Type : Gela.Elements.Element_Access;
   end record;

private

   overriding procedure On_Index
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Token_Count);

   overriding procedure On_Env_In
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Env_Index);

   overriding procedure On_Env_Out
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Env_Index);

   overriding procedure On_Down
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.Interpretation_Index);

   overriding procedure On_Errors
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Error_Set_Index);

   overriding procedure On_Up
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.
        Interpretation_Tuple_Index);

   overriding procedure On_Up
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.
        Interpretation_Tuple_List_Index);

   overriding procedure On_Name_List
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Symbol_List);

   overriding procedure On_Limited_With_List
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Symbol_List);

   overriding procedure On_With_List
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Symbol_List);

   overriding procedure On_Up
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.
        Interpretation_Set_Index);

   overriding procedure On_Static_Value
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Value_Index);

   overriding procedure On_Defining_Name
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Defining_Names.
        Defining_Name_Access);

   overriding procedure On_Full_Name
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Symbol);

   overriding procedure On_Declarative_Region
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Env_Index);

   overriding procedure On_Type_Index
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Type_Index);

   overriding procedure On_Corresponding_Type
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Element_Access);

   overriding procedure On_Corresponding_Generic_Element
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Element_Access);

   overriding procedure On_Corresponding_View
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Element_Access);

   overriding procedure On_Expanded
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Element_Access);

   overriding procedure On_Chosen_Interpretation
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.Interpretation_Kinds);

end Gela.Property_Resets;
