--  This package provides Value_Set implementation.
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with League.Strings;

with Gela.Elements.Defining_Names;
with Gela.Semantic_Types;
with Gela.Value_Sets;

package Gela.Plain_Value_Sets is
   pragma Preelaborate;

   type Value_Set is limited new Gela.Value_Sets.Value_Set with private;
   type Value_Set_Access is access all Value_Set'Class;

private
   subtype Positive_Value_Index is Gela.Semantic_Types.Value_Index
     range 1 .. Gela.Semantic_Types.Value_Index'Last;

   type Value_Kinds is (String_Value, List_Value, Denote_Function);

   type Value (Kind : Value_Kinds := String_Value) is record
      case Kind is
         when Denote_Function =>
            Op : Gela.Semantic_Types.Static_Operator;
         when String_Value =>
            String : League.Strings.Universal_String;
         when List_Value =>
            Head : Positive_Value_Index;
            Tail : Positive_Value_Index;
      end case;
   end record;

   function Hash (X : Value) return Ada.Containers.Hash_Type;

   package Hash_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Value,
      Element_Type    => Positive_Value_Index,
      Hash            => Hash,
      Equivalent_Keys => "=",
     "="              => Gela.Semantic_Types."=");

   package Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive_Value_Index,
      Element_Type => Value,
      "="          => "=");

   type Value_Set is limited new Gela.Value_Sets.Value_Set with record
      Map    : Hash_Maps.Map;
      Vector : Vectors.Vector;
   end record;

   not overriding procedure Put_Value
     (Self  : in out Value_Set;
      Item  : Value;
      Value : out Gela.Semantic_Types.Value_Index);

   overriding procedure String_Literal
     (Self  : in out Value_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Semantic_Types.Value_Index);

   overriding procedure Name
     (Self  : in out Value_Set;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access;
      Value : out Gela.Semantic_Types.Value_Index);

   overriding procedure List
     (Self  : in out Value_Set;
      Head  : Gela.Semantic_Types.Value_Index;
      Tail  : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index);

   overriding procedure Apply
     (Self  : in out Value_Set;
      Name  : Gela.Semantic_Types.Value_Index;
      Args  : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index);

   overriding function Image
     (Self  : Value_Set;
      Value : Gela.Semantic_Types.Value_Index)
      return League.Strings.Universal_String;

end Gela.Plain_Value_Sets;
