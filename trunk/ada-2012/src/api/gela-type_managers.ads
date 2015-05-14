--  Type manager keeps types found in all compilation units.

with Gela.Type_Views;
with Gela.Semantic_Types;
with Gela.Elements.Defining_Names;
with Gela.Elements.Subtype_Mark_Or_Access_Definitions;
with Gela.Profiles;

package Gela.Type_Managers is

   pragma Preelaborate;

   type Type_Manager is limited interface;
   type Type_Manager_Access is access all Type_Manager'Class;
   for Type_Manager_Access'Storage_Size use 0;

   not overriding function Get
     (Self  : access Type_Manager;
      Index : Gela.Semantic_Types.Type_Index)
      return Gela.Type_Views.Type_View_Access is abstract;
   --  Get type view by given Index

   not overriding function Type_From_Declaration
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_Index is abstract;
   --  Return type corresponding to Node of type declaration

   not overriding function Type_Of_Object_Declaration
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_Index is abstract;
   --  Return type corresponding to Node of object declaration

   not overriding function Type_From_Subtype_Mark
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Subtype_Mark_Or_Access_Definitions.
                Subtype_Mark_Or_Access_Definition'Class)
      return Gela.Semantic_Types.Type_Index is abstract;
   --  Get type view from given subtype mark

   not overriding function Type_By_Name
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Type_Index is abstract;
   --  Get type view from given type's defining name

   not overriding function Universal_Integer
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_Index is abstract;
   --  Get type view of predefined universal_integer

   not overriding function Universal_Real
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_Index is abstract;
   --  Get type view of predefined universal_real

   not overriding function Universal_Access
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_Index is abstract;
   --  Get type view of predefined universal_access

   not overriding function Get_Profile
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Profiles.Profile_Access is abstract;
   --  If Name if callable entity return corresponding profile

end Gela.Type_Managers;
