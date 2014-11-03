with Asis.Extensions.Flat_Kinds;

with Gela.Properties;
with Gela.Rule.Declarations.Subprogram_Body;
with Gela.Rule.Statements.Procedure_Call;

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
end Gela.Rule.Register_All;

