--  This package provides Environment_Set.
with Gela.Contexts;
with Gela.Defining_Name_Cursors;
with Gela.Elements.Defining_Names;
with Gela.Environments;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Library_Environments;

with Ada.Containers.Vectors;

package Gela.Plain_Environments is
   pragma Preelaborate;

   type Environment_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Environments.Environment_Set with private;
   type Plain_Environment_Set_Access is access all Environment_Set;

private

   type Direct_Visible_Item_Count is range 0 .. Natural'Last;
   subtype Direct_Visible_Item_Index is
     Direct_Visible_Item_Count range 1 .. Direct_Visible_Item_Count'Last;

   type Direct_Visible_Item is record
      Prev   : Direct_Visible_Item_Count;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
   end record;

   package Direct_Visible_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Direct_Visible_Item_Index,
      Element_Type => Direct_Visible_Item);

   subtype Region_Item_Index is Gela.Semantic_Types.Env_Index
     range 2 .. Gela.Semantic_Types.Env_Index'Last;

   type Region_Item is record
      Prev    : Gela.Semantic_Types.Env_Index;
      Region  : Gela.Elements.Defining_Names.Defining_Name_Access;
      Local   : Direct_Visible_Item_Count;
--        Visible : Direct_Visible_Item_Count;
      --        Visible        : Visible_Item_Index;
   end record;

   package Region_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Region_Item_Index,
      Element_Type => Region_Item);

   type Environment_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Environments.Environment_Set with
   record
      Lib : aliased Gela.Library_Environments.Environment_Set (Context);
      Region         : Region_Vectors.Vector;
      Direct_Visible : Direct_Visible_Vectors.Vector;
--        Visible        : Visible_Vectors.Vector;
   end record;

   overriding function Add_Defining_Name
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Direct_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class;

   overriding function Enter_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Leave_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index)
      return Gela.Semantic_Types.Env_Index;

   overriding procedure Library_Level_Environment
     (Self  : in out Environment_Set;
      Value : out Gela.Semantic_Types.Env_Index);

   overriding function Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : access Boolean)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class;

end Gela.Plain_Environments;
