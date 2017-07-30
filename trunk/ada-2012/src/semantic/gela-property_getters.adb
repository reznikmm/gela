package body Gela.Property_Getters is

   --------------
   -- On_Index --
   --------------

   overriding procedure On_Index
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Lexical_Types.Token_Count)
   is
      pragma Unreferenced (Element);
   begin
      Self.Index := Value;
   end On_Index;

   ---------------
   -- On_Env_In --
   ---------------

   overriding procedure On_Env_In
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Semantic_Types.Env_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Env_In := Value;
   end On_Env_In;

   ----------------
   -- On_Env_Out --
   ----------------

   overriding procedure On_Env_Out
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Semantic_Types.Env_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Env_Out := Value;
   end On_Env_Out;

   -------------
   -- On_Down --
   -------------

   overriding procedure On_Down
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Down := Value;
   end On_Down;

   ---------------
   -- On_Errors --
   ---------------

   overriding procedure On_Errors
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Semantic_Types.Error_Set_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Errors := Value;
   end On_Errors;

   -----------
   -- On_Up --
   -----------

   overriding procedure On_Up
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Interpretations.Interpretation_Tuple_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Up_Tuple := Value;
   end On_Up;

   -----------
   -- On_Up --
   -----------

   overriding procedure On_Up
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Interpretations.Interpretation_Tuple_List_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Up_Tuple_List := Value;
   end On_Up;

   ------------------
   -- On_Name_List --
   ------------------

   overriding procedure On_Name_List
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Lexical_Types.Symbol_List)
   is
      pragma Unreferenced (Element);
   begin
      Self.Name_List := Value;
   end On_Name_List;

   --------------------------
   -- On_Limited_With_List --
   --------------------------

   overriding procedure On_Limited_With_List
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Lexical_Types.Symbol_List)
   is
      pragma Unreferenced (Element);
   begin
      Self.Limited_With_List := Value;
   end On_Limited_With_List;

   ------------------
   -- On_With_List --
   ------------------

   overriding procedure On_With_List
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Lexical_Types.Symbol_List)
   is
      pragma Unreferenced (Element);
   begin
      Self.With_List := Value;
   end On_With_List;

   -----------
   -- On_Up --
   -----------

   overriding procedure On_Up
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Interpretations.Interpretation_Set_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Up_Set := Value;
   end On_Up;

   ---------------------
   -- On_Static_Value --
   ---------------------

   overriding procedure On_Static_Value
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Semantic_Types.Value_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Static_Value := Value;
   end On_Static_Value;

   ------------------------------
   -- On_Chosen_Interpretation --
   ------------------------------

   overriding procedure On_Chosen_Interpretation
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Interpretations.Interpretation_Kinds)
   is
      pragma Unreferenced (Element);
   begin
      Self.Chosen_Interpretation := Value;
   end On_Chosen_Interpretation;

   ----------------------
   -- On_Defining_Name --
   ----------------------

   overriding procedure On_Defining_Name
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Elements.Defining_Names.Defining_Name_Access)
   is
      pragma Unreferenced (Element);
   begin
      Self.Defining_Name := Value;
   end On_Defining_Name;

   ------------------
   -- On_Full_Name --
   ------------------

   overriding procedure On_Full_Name
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Lexical_Types.Symbol)
   is
      pragma Unreferenced (Element);
   begin
      Self.Full_Name := Value;
   end On_Full_Name;

   ---------------------------
   -- On_Declarative_Region --
   ---------------------------

   overriding procedure On_Declarative_Region
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Semantic_Types.Env_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Declarative_Region := Value;
   end On_Declarative_Region;

   --------------------------------------
   -- On_Corresponding_Generic_Element --
   --------------------------------------

   overriding procedure On_Corresponding_Generic_Element
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Element);
   begin
      Self.Corresponding_Generic_Element := Value;
   end On_Corresponding_Generic_Element;

   ---------------------------
   -- On_Corresponding_View --
   ---------------------------

   overriding procedure On_Corresponding_View
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Element);
   begin
      Self.Corresponding_View := Value;
   end On_Corresponding_View;

   -------------------
   -- On_Type_Index --
   -------------------

   overriding procedure On_Type_Index
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Semantic_Types.Type_Index)
   is
      pragma Unreferenced (Element);
   begin
      Self.Type_Index := Value;
   end On_Type_Index;

   ---------------------------
   -- On_Corresponding_Type --
   ---------------------------

   overriding procedure On_Corresponding_Type
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Element);
   begin
      Self.Corresponding_Type := Value;
   end On_Corresponding_Type;

   -----------------
   -- On_Expanded --
   -----------------

   overriding procedure On_Expanded
     (Self    : in out Getter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Element);
   begin
      Self.Expanded := Value;
   end On_Expanded;

end Gela.Property_Getters;
