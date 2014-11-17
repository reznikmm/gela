with Asis.Extensions.Flat_Kinds;

with Gela.Properties;
with Gela.Rule.Declarations.Defining_Identifier;
with Gela.Rule.Declarations.Subprogram;
with Gela.Rule.Declarations.Subprogram_Body;
with Gela.Rule.Expressions.Identifier;
with Gela.Rule.Statements.Procedure_Call;

with Gela.Rule.Empty;
with Gela.Rule.Join_Nested;

procedure Gela.Rule.Register_All
  (Engine : in out Gela.Engines.Engine)
is
   package F renames Asis.Extensions.Flat_Kinds;
   package N renames Gela.Properties;
begin
   Engine.Register_Rule
     (Kind     => F.A_Procedure_Body_Declaration,
      Property => N.Code,
      Action   => Declarations.Subprogram_Body.Code'Access);
   Engine.Register_Rule
     (Kind     => F.A_Procedure_Call_Statement,
      Property => N.Code,
      Action   => Statements.Procedure_Call.Code'Access);

   Engine.Register_Rule
     (Kind     => F.A_Defining_Identifier,
      Property => N.Global,
      Action   => Gela.Rule.Empty.Text'Access);
   Engine.Register_Rule
     (Kind     => F.A_Parameter_Association,
      Property => N.Global,
      Action   => Gela.Rule.Join_Nested.Text'Access);
   Engine.Register_Rule
     (Kind     => F.A_Procedure_Declaration,
      Property => N.Global,
      Action   => Gela.Rule.Declarations.Subprogram.Global'Access);
   Engine.Register_Rule
     (Kind     => F.A_Procedure_Body_Declaration,
      Property => N.Global,
      Action   => Gela.Rule.Join_Nested.Text'Access);
   Engine.Register_Rule
     (Kind     => F.A_Procedure_Call_Statement,
      Property => N.Global,
      Action   => Gela.Rule.Join_Nested.Text'Access);
   Engine.Register_Rule
     (From     => Asis.Extensions.Flat_Kinds.An_Expression'First,
      To       => Asis.Extensions.Flat_Kinds.An_Expression'Last,
      Property => N.Global,
      Action   => Gela.Rule.Expressions.Global'Access);
   Engine.Register_Rule
     (Kind     => F.An_Identifier,
      Property => N.Global,
      Action   => Gela.Rule.Expressions.Identifier.Global'Access,
      Redefine => True);

   Engine.Register_Rule
     (Kind     => F.A_Defining_Identifier,
      Property => N.Non_Static_Value,
      Action   => Gela.Rule.Declarations.Defining_Identifier.Value'Access);
   Engine.Register_Rule
     (Kind     => F.An_Identifier,
      Property => N.Non_Static_Value,
      Action   => Gela.Rule.Expressions.Identifier.Non_Static_Value'Access);

   Engine.Register_Rule
     (From     => Asis.Extensions.Flat_Kinds.An_Expression'First,
      To       => Asis.Extensions.Flat_Kinds.An_Expression'Last,
      Property => N.Value,
      Action   => Gela.Rule.Expressions.Value'Access);

end Gela.Rule.Register_All;

