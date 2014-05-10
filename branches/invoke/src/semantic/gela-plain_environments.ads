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

   type Change_Kinds is (Direct_Visible);

   type Change (Kind : Change_Kinds := Direct_Visible) is record
      Prev : Gela.Semantic_Types.Env_Index;

      case Kind is
         when Direct_Visible =>
            DV_Symbol : Gela.Lexical_Types.Symbol;
            DV_Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      end case;
   end record;

   subtype Change_Index is Gela.Semantic_Types.Env_Index
     range 2 .. Gela.Semantic_Types.Env_Index'Last;

   package Change_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Change_Index,
      Element_Type => Change);

   type Environment_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Environments.Environment_Set with
   record
      Lib : aliased Gela.Library_Environments.Environment_Set (Context);
      Set : Change_Vectors.Vector;
   end record;

   overriding function Add_Direct_Visible
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
