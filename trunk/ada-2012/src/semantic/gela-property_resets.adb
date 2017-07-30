package body Gela.Property_Resets is

   ------------------------------
   -- On_Chosen_Interpretation --
   ------------------------------

   overriding procedure On_Chosen_Interpretation
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.Interpretation_Kinds)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Chosen_Interpretation;
   end On_Chosen_Interpretation;

   --------------------------------------
   -- On_Corresponding_Generic_Element --
   --------------------------------------

   overriding procedure On_Corresponding_Generic_Element
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Corresponding_Generic_Element;
   end On_Corresponding_Generic_Element;

   ---------------------------
   -- On_Corresponding_Type --
   ---------------------------

   overriding procedure On_Corresponding_Type
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Corresponding_Type;
   end On_Corresponding_Type;

   ---------------------------
   -- On_Corresponding_View --
   ---------------------------

   overriding procedure On_Corresponding_View
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Corresponding_View;
   end On_Corresponding_View;

   ---------------------------
   -- On_Declarative_Region --
   ---------------------------

   overriding procedure On_Declarative_Region
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Env_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Declarative_Region;
   end On_Declarative_Region;

   ----------------------
   -- On_Defining_Name --
   ----------------------

   overriding procedure On_Defining_Name
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Defining_Names.
        Defining_Name_Access)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Defining_Name;
   end On_Defining_Name;

   -------------
   -- On_Down --
   -------------

   overriding procedure On_Down
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Down;
   end On_Down;

   ---------------
   -- On_Env_In --
   ---------------

   overriding procedure On_Env_In
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Env_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Env_In;
   end On_Env_In;

   ----------------
   -- On_Env_Out --
   ----------------

   overriding procedure On_Env_Out
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Env_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Env_Out;
   end On_Env_Out;

   ---------------
   -- On_Errors --
   ---------------

   overriding procedure On_Errors
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Error_Set_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Errors;
   end On_Errors;

   -----------------
   -- On_Expanded --
   -----------------

   overriding procedure On_Expanded
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Expanded;
   end On_Expanded;

   ------------------
   -- On_Full_Name --
   ------------------

   overriding procedure On_Full_Name
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Symbol)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Full_Name;
   end On_Full_Name;

   --------------
   -- On_Index --
   --------------

   overriding procedure On_Index
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Token_Count)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Index;
   end On_Index;

   --------------------------
   -- On_Limited_With_List --
   --------------------------

   overriding procedure On_Limited_With_List
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Symbol_List)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Limited_With_List;
   end On_Limited_With_List;

   ------------------
   -- On_Name_List --
   ------------------

   overriding procedure On_Name_List
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Symbol_List)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Name_List;
   end On_Name_List;

   ---------------------
   -- On_Static_Value --
   ---------------------

   overriding procedure On_Static_Value
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Value_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Static_Value;
   end On_Static_Value;

   -------------------
   -- On_Type_Index --
   -------------------

   overriding procedure On_Type_Index
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Semantic_Types.Type_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Type_Index;
   end On_Type_Index;

   -----------
   -- On_Up --
   -----------

   overriding procedure On_Up
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.Interpretation_Tuple_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Up_Tuple;
   end On_Up;

   -----------
   -- On_Up --
   -----------

   overriding procedure On_Up
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.Interpretation_Tuple_List_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Up_Tuple_List;
   end On_Up;

   -----------
   -- On_Up --
   -----------

   overriding procedure On_Up
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Interpretations.Interpretation_Set_Index)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.Up_Set;
   end On_Up;

   ------------------
   -- On_With_List --
   ------------------

   overriding procedure On_With_List
     (Self    : in out Property_Reset;
      Element : Gela.Elements.Element_Access;
      Value   : out Gela.Lexical_Types.Symbol_List)
   is
      pragma Unreferenced (Element);
   begin
      Value := Self.With_List;
   end On_With_List;

end Gela.Property_Resets;
