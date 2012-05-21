with Asis.Gela.Pools;
with Gela.Containers.Vectors; use Gela;

package Asis.Gela.Strings is

   package Wide_Character_Vectors is new Containers.Vectors
     (Item_Type  => Wide_Character,
      Index_Type => Positive,
      Pool       => Pools.Pool);

   subtype Text_Buffer is Wide_Character_Vectors.Vector;

   procedure Add
     (Object : in out Wide_Character_Vectors.Vector;
      Text   : in     Wide_String;
      Index  :    out Positive);

   procedure Get
     (Object : in     Wide_Character_Vectors.Vector;
      Text   :    out Wide_String;
      Index  : in     Positive);

   function Get
     (Object : Wide_Character_Vectors.Vector;
      Index  : Positive) return Wide_String;

   function Get_Length
     (Object : Wide_Character_Vectors.Vector;
      Index  : Positive) return Natural;

end Asis.Gela.Strings;
