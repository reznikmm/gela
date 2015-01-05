with Asis.Definitions;
with Asis.Elements;

package body Gela.Rule.Definitions.Subtype_Definition is

   function Length
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Subtype_Indication;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      Result : Gela.Properties.Text.Text;

      Constr : constant Asis.Constraint :=
        Asis.Definitions.Subtype_Constraint (Element);
   begin
      if Asis.Elements.Is_Nil (Constr) then
         raise Constraint_Error with "Unimplemented";
      end if;

      Result := Engine.Get (Constr, Property);

      return Result;
   end Length;

end Gela.Rule.Definitions.Subtype_Definition;
