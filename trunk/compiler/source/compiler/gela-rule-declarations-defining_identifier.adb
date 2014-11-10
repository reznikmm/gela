with Asis.Declarations;

package body Gela.Rule.Declarations.Defining_Identifier is

   -----------
   -- Value --
   -----------

   function Value
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      pragma Unreferenced (Property);

      Image  : constant Wide_String :=
        Asis.Declarations.Defining_Name_Image (Element);
      Txt    : String (Image'Range);
      Result : Gela.Properties.Text.Text;
   begin
      for J in Image'Range loop
         Txt (J) := Character'Val (Wide_Character'Pos (Image (J)));
      end loop;

      Result := Engine.Text_Container.Literal ("@");
      Result := Engine.Text_Container.Join (Result, Txt);

      return Result;
   end Value;

end Gela.Rule.Declarations.Defining_Identifier;
