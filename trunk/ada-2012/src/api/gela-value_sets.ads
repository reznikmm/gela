--  This package provides Value_Set interface and its methods.
with League.Strings;

with Gela.Semantic_Types;
with Gela.Elements.Defining_Names;

package Gela.Value_Sets is
   pragma Preelaborate;

   type Value_Set is limited interface;
   --  Set of all values encountered in compilation units of some context
   type Value_Set_Access is access all Value_Set'Class;
   for Value_Set_Access'Storage_Size use 0;

   not overriding procedure String_Literal
     (Self  : in out Value_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Semantic_Types.Value_Index) is abstract;
   --  Get string value for given literal.

   not overriding procedure Name
     (Self  : in out Value_Set;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access;
      Value : out Gela.Semantic_Types.Value_Index) is abstract;
   --  Get value corresponding to given defining name

   not overriding procedure List
     (Self  : in out Value_Set;
      Head  : Gela.Semantic_Types.Value_Index;
      Tail  : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index) is abstract;
   --  Get list as (Head, Tail)

   not overriding procedure Apply
     (Self  : in out Value_Set;
      Name  : Gela.Semantic_Types.Value_Index;
      Args  : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index) is abstract;
   --  Return Name (Args)

   not overriding function Image
     (Self  : Value_Set;
      Value : Gela.Semantic_Types.Value_Index)
      return League.Strings.Universal_String is abstract;
   --  Get image of given value

end Gela.Value_Sets;
