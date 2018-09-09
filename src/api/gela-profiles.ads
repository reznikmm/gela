--  This package provides representation of types and their categories.

with Gela.Elements.Defining_Names;
with Gela.Lexical_Types;
with Gela.Types;

package Gela.Profiles is
   pragma Preelaborate;

   type Profile is limited interface;
   type Profile_Access is access all Profile'Class;
   for Profile_Access'Storage_Size use 0;

   function Assigned (Self : access Profile'Class) return Boolean
     is (Self /= null);

   not overriding function Is_Function
     (Self : Profile) return Boolean is abstract;

   not overriding function Allow_Empty_Argument_List
     (Self : Profile) return Boolean is abstract;

   not overriding function Length
     (Self : Profile) return Natural is abstract;

   not overriding function Return_Type
     (Self  : Profile) return Gela.Types.Type_View_Access is abstract;

   not overriding function Get_Type
     (Self  : Profile;
      Index : Positive) return Gela.Types.Type_View_Access is abstract;

   not overriding function Get_Name
     (Self  : Profile;
      Index : Positive)
      return Gela.Elements.Defining_Names.Defining_Name_Access is abstract;
   --  Could be null when prefix is attribute_reference, like 'Pos (X).

   not overriding function Get_Index
     (Self   : Profile;
      Symbol : Gela.Lexical_Types.Symbol) return Natural is abstract;

end Gela.Profiles;
