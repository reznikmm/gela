with Gela.Interpretations;
with Gela.Int;

package Gela.Int_Sets is
   pragma Preelaborate;

   type Interpretation_Set is abstract tagged null record;

   type Interpretation_Set_Access is access all Interpretation_Set'Class;

   not overriding function Element
     (Self  : Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Index)
     return Gela.Int.Interpretation_Access is abstract;

   not overriding function Get_Cursor
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
     return Gela.Interpretations.Cursor'Class is abstract;

   type Index_Provider is limited interface;

   not overriding procedure Reserve_Indexes
     (Self : in out Index_Provider;
      Set  : Interpretation_Set_Access;
      From : out Gela.Interpretations.Interpretation_Set_Index;
      To   : out Gela.Interpretations.Interpretation_Set_Index) is abstract;

   not overriding procedure Reserve_Indexes
     (Self : in out Index_Provider;
      Set  : Interpretation_Set_Access;
      From : out Gela.Interpretations.Interpretation_Index;
      To   : out Gela.Interpretations.Interpretation_Index) is abstract;

end Gela.Int_Sets;