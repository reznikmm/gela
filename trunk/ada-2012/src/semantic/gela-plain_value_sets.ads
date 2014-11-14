--  This package provides Value_Set implementation.
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with League.Strings;

with Gela.Semantic_Types;
with Gela.Value_Sets;

package Gela.Plain_Value_Sets is
   pragma Preelaborate;

   type Value_Set is limited new Gela.Value_Sets.Value_Set with private;
   type Value_Set_Access is access all Value_Set'Class;

   overriding procedure String_Literal
     (Self  : in out Value_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Semantic_Types.Value_Index);
   --  Get string value for given literal.

   overriding procedure Concat
     (Self  : in out Value_Set;
      Left  : Gela.Semantic_Types.Value_Index;
      Right : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index);
   --  Return "Left & Right"

   overriding function Image
     (Self  : Value_Set;
      Value : Gela.Semantic_Types.Value_Index)
      return League.Strings.Universal_String;
   --  Get image of given value

private
   subtype Positive_Value_Index is Gela.Semantic_Types.Value_Index
     range 1 .. Gela.Semantic_Types.Value_Index'Last;

   type Value_Kinds is (String_Value);

   type Value (Kind : Value_Kinds := String_Value) is record
      String : League.Strings.Universal_String;
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

end Gela.Plain_Value_Sets;
