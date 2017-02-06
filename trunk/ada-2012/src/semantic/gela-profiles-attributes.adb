package body Gela.Profiles.Attributes is

   -------------------------------
   -- Allow_Empty_Argument_List --
   -------------------------------

   overriding function Allow_Empty_Argument_List
     (Self : Profile) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Allow_Empty_Argument_List;

   ------------
   -- Create --
   ------------

   function Create
     (Params : Gela.Semantic_Types.Type_Index_Array;
      Result : Gela.Semantic_Types.Type_Index)
     return Gela.Profiles.Profile'Class is
   begin
      return Profile'(Params'Length, Params, Result);
   end Create;

   ---------------
   -- Get_Index --
   ---------------

   overriding function Get_Index
     (Self   : Profile;
      Symbol : Gela.Lexical_Types.Symbol)
      return Natural
   is
      pragma Unreferenced (Self, Symbol);
   begin
      return 0;
   end Get_Index;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Self  : Profile;
      Index : Positive)
      return Gela.Elements.Defining_Names.Defining_Name_Access
   is
      pragma Unreferenced (Self, Index);
   begin
      return null;
   end Get_Name;

   --------------
   -- Get_Type --
   --------------

   overriding function Get_Type
     (Self  : Profile; Index : Positive)
      return Gela.Semantic_Types.Type_Index is
   begin
      return Self.Types (Index);
   end Get_Type;

   -----------------
   -- Is_Function --
   -----------------

   overriding function Is_Function (Self : Profile) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Function;

   ------------
   -- Length --
   ------------

   overriding function Length (Self : Profile) return Natural is
   begin
      return Self.Length;
   end Length;

   -----------------
   -- Return_Type --
   -----------------

   overriding function Return_Type
     (Self  : Profile) return Gela.Semantic_Types.Type_Index is
   begin
      return Self.Result;
   end Return_Type;

end Gela.Profiles.Attributes;
