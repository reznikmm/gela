package Gela.Profiles.Names is
   pragma Preelaborate;

   type Profile (<>) is limited new Gela.Profiles.Profile with private;
   type Profile_Access is access all Profile'Class;

   function Create
     (Env  : Gela.Semantic_Types.Env_Index;
      Name : Gela.Elements.Defining_Names.Defining_Name_Access)
     return Gela.Profiles.Profile'Class;

   overriding function Is_Function
     (Self : Profile) return Boolean;

   overriding function Allow_Empty_Argument_List
     (Self : Profile) return Boolean;

   overriding function Length
     (Self : Profile) return Natural;

   overriding function Return_Type
     (Self  : Profile) return Gela.Semantic_Types.Type_Index;

   overriding function Get_Type
     (Self  : Profile;
      Index : Positive) return Gela.Semantic_Types.Type_Index;

   overriding function Get_Name
     (Self  : Profile;
      Index : Positive)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

   overriding function Get_Index
     (Self   : Profile;
      Symbol : Gela.Lexical_Types.Symbol) return Natural;

private

   type Param is record
      Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Tipe : Gela.Semantic_Types.Type_Index;
   end record;

   type Param_Array is array (Positive range <>) of Param;

   type Profile (Length : Natural) is limited new Gela.Profiles.Profile
   with record
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Empty  : Boolean := False;
      Funct  : Boolean := False;
      Result : Gela.Semantic_Types.Type_Index := 0;
      Params : Param_Array (1 .. Length);
   end record;

end Gela.Profiles.Names;
