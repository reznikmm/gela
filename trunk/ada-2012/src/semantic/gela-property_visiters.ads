with Gela.Element_Visiters;
with Gela.Elements.Associations;
with Gela.Elements.Auxiliary_Applies;
with Gela.Elements.Composite_Constraints;
with Gela.Elements.Numeric_Literals;
with Gela.Elements.Simple_Expression_Range_Drs;
with Gela.Elements;
with Gela.Interpretations;
with Gela.Semantic_Types;

package Gela.Property_Visiters is
   pragma Preelaborate;

   type Property_Visiter is limited interface;

   not overriding procedure On_Down
     (Self    : in out Property_Visiter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Interpretations.Interpretation_Index) is null;

   not overriding procedure On_Up
     (Self    : in out Property_Visiter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Interpretations.Interpretation_Set_Index) is null;

   not overriding procedure On_Env_In
     (Self    : in out Property_Visiter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Semantic_Types.Env_Index) is null;

   not overriding procedure On_Env_Out
     (Self    : in out Property_Visiter;
      Element : Gela.Elements.Element_Access;
      Value   : Gela.Semantic_Types.Env_Index) is null;

   type Visiter (V : not null access Property_Visiter'Class) is
     new Gela.Element_Visiters.Visiter with null record;

private

   overriding procedure Association
     (Self : in out Visiter;
      Node : not null Gela.Elements.Associations.Association_Access);

   overriding procedure Auxiliary_Apply
     (Self : in out Visiter;
      Node : not null Gela.Elements.Auxiliary_Applies.Auxiliary_Apply_Access);

   overriding procedure Composite_Constraint
     (Self : in out Visiter;
      Node : not null Gela.Elements.Composite_Constraints.
        Composite_Constraint_Access);

   overriding procedure Numeric_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Numeric_Literals.Numeric_Literal_Access);

   overriding procedure Simple_Expression_Range_Dr
     (Self : in out Visiter;
      Node : not null Gela.Elements.Simple_Expression_Range_Drs.
        Simple_Expression_Range_Dr_Access);

end Gela.Property_Visiters;
