with Gela.Types;

package Gela.Profiles.Attributes is
   pragma Preelaborate;

   type Profile (<>) is limited new Gela.Profiles.Profile with private;
   type Profile_Access is access all Profile'Class;

   function Create
     (Params : Gela.Types.Type_View_Array;
      Result : Gela.Semantic_Types.Type_View_Index)
     return Gela.Profiles.Profile'Class;

private

   type Profile (Length : Natural) is limited new Gela.Profiles.Profile
   with record
      Types  : Gela.Types.Type_View_Array (1 .. Length);
      Result : Gela.Semantic_Types.Type_View_Index;
   end record;

   overriding function Is_Function
     (Self : Profile) return Boolean;

   overriding function Allow_Empty_Argument_List
     (Self : Profile) return Boolean;

   overriding function Length
     (Self : Profile) return Natural;

   overriding function Return_Type
     (Self  : Profile) return Gela.Semantic_Types.Type_View_Index;

   overriding function Get_Type
     (Self  : Profile;
      Index : Positive) return Gela.Semantic_Types.Type_View_Index;

   overriding function Get_Name
     (Self  : Profile;
      Index : Positive)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

   overriding function Get_Index
     (Self   : Profile;
      Symbol : Gela.Lexical_Types.Symbol) return Natural;

end Gela.Profiles.Attributes;
