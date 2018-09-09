--  Type manager keeps types found in all compilation units.

with Gela.Types;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Elements.Defining_Names;
with Gela.Elements.Discrete_Subtype_Definitions;
with Gela.Elements.Object_Definitions;
with Gela.Elements.Subtype_Mark_Or_Access_Definitions;
with Gela.Profiles;

package Gela.Type_Managers is

   pragma Preelaborate;

   type Type_Manager is limited interface;
   type Type_Manager_Access is access all Type_Manager'Class;
   for Type_Manager_Access'Storage_Size use 0;

   not overriding function Get
     (Self  : access Type_Manager;
      Index : Gela.Semantic_Types.Type_View_Index)
      return Gela.Types.Type_View_Access is abstract;
   --  Get type view by given Index

   not overriding function Type_From_Declaration
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Return type corresponding to Node of type declaration

   not overriding function Type_Of_Object_Declaration
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Element_Access)  --  FIXME access Element'Class?
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Return type corresponding to Node of object declaration

   not overriding function Type_From_Subtype_Mark
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Subtype_Mark_Or_Access_Definitions.
                Subtype_Mark_Or_Access_Definition'Class)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view from given subtype mark

   not overriding function Type_From_Subtype_Indication
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Object_Definitions.Object_Definition'Class)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view from given subtype indication

   not overriding function Type_From_Discrete_Subtype
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Discrete_Subtype_Definitions.
                Discrete_Subtype_Definition'Class)
        return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view from given discrete subtype definition


   not overriding function Type_By_Name
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view from given type's defining name

   not overriding function Universal_Integer
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view of predefined universal_integer

   not overriding function Universal_Real
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view of predefined universal_real

   not overriding function Universal_Access
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view of predefined universal_access

   not overriding function Boolean
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view of predefined universal_access

   not overriding function Root_Integer
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view of predefined root_integer

   not overriding function Root_Real
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_View_Index is abstract;
   --  Get type view of predefined root_real

   not overriding function Get_Profile
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Profiles.Profile_Access is abstract;
   --  If Name if callable entity return corresponding profile

   not overriding function Get_Profile
     (Self      : access Type_Manager;
      Tipe      : Gela.Semantic_Types.Type_View_Index;
      Attribute : Gela.Lexical_Types.Symbol)
      return Gela.Profiles.Profile_Access is abstract;
   --  If given attribute if callable entity return corresponding profile

end Gela.Type_Managers;
