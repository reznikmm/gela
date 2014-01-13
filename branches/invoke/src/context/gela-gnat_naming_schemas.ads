with Gela.Lexical_Types;
with Gela.Naming_Schemas;
with Gela.Contexts;

package Gela.GNAT_Naming_Schemas is
   pragma Preelaborate;

   type Naming_Schema (Context : access Gela.Contexts.Context'Class) is
     limited new Gela.Naming_Schemas.Naming_Schema with null record;

   overriding function Declaration_Name
     (Self   : Naming_Schema;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Naming_Schemas.File_Base_Name;

   overriding function Body_Name
     (Self   : Naming_Schema;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Naming_Schemas.File_Base_Name;

end Gela.GNAT_Naming_Schemas;
