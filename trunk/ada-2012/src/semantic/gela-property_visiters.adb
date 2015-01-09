package body Gela.Property_Visiters is

   overriding procedure Association
     (Self : in out Visiter;
      Node : not null Gela.Elements.Associations.Association_Access) is
   begin
      Self.V.On_Down (Gela.Elements.Element_Access (Node), Node.Down);
      Self.V.On_Up (Gela.Elements.Element_Access (Node), Node.Up);
   end Association;

   overriding procedure Auxiliary_Apply
     (Self : in out Visiter;
      Node : not null Gela.Elements.Auxiliary_Applies.Auxiliary_Apply_Access)
   is
   begin
      Self.V.On_Down (Gela.Elements.Element_Access (Node), Node.Down);
      Self.V.On_Up (Gela.Elements.Element_Access (Node), Node.Up);
   end Auxiliary_Apply;

   overriding procedure Composite_Constraint
     (Self : in out Visiter;
      Node : not null Gela.Elements.Composite_Constraints.
        Composite_Constraint_Access) is
   begin
      Self.V.On_Down (Gela.Elements.Element_Access (Node), Node.Down);
      Self.V.On_Up (Gela.Elements.Element_Access (Node), Node.Up);
   end Composite_Constraint;

   overriding procedure Numeric_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Numeric_Literals.Numeric_Literal_Access) is
   begin
      Self.V.On_Down (Gela.Elements.Element_Access (Node), Node.Down);
      Self.V.On_Up (Gela.Elements.Element_Access (Node), Node.Up);
   end Numeric_Literal;

   overriding procedure Simple_Expression_Range_Dr
     (Self : in out Visiter;
      Node : not null Gela.Elements.Simple_Expression_Range_Drs.
        Simple_Expression_Range_Dr_Access) is
   begin
      Self.V.On_Down (Gela.Elements.Element_Access (Node), Node.Down);
      Self.V.On_Up (Gela.Elements.Element_Access (Node), Node.Up);
   end Simple_Expression_Range_Dr;

end Gela.Property_Visiters;
