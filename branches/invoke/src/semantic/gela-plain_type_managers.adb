package body Gela.Plain_Type_Managers is

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self  : access Type_Manager;
      Index : Gela.Semantic_Types.Type_Index)
      return Gela.Type_Views.Type_View_Access
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Index);
   begin
      return null;
   end Get;

   ------------------
   -- Type_By_Name --
   ------------------

   overriding function Type_By_Name
     (Self  : access Type_Manager;
      Node  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Type_Index is
      pragma Unreferenced (Node);
      pragma Unreferenced (Self);
   begin
      return 0;
   end Type_By_Name;

   ----------------------------
   -- Type_From_Subtype_Mark --
   ----------------------------

   overriding function Type_From_Subtype_Mark
     (Self  : access Type_Manager;
      Node  : Gela.Elements.Subtype_Marks.Subtype_Mark_Access)
      return Gela.Semantic_Types.Type_Index
   is
      pragma Unreferenced (Node);
      pragma Unreferenced (Self);
   begin
      return 0;
   end Type_From_Subtype_Mark;

end Gela.Plain_Type_Managers;
