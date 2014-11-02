--  This package provides Naming_Schema interface and its methods.

with League.Strings;

with Gela.Lexical_Types;

package Gela.Naming_Schemas is
   pragma Preelaborate;

   subtype File_Base_Name is League.Strings.Universal_String;

   type Naming_Schema is limited interface;
   --  Interface to get file name for given compilation unit.

   type Naming_Schema_Access is access all Naming_Schema'Class;
   for Naming_Schema_Access'Storage_Size use 0;

   not overriding function Declaration_Name
     (Self   : Naming_Schema;
      Symbol : Gela.Lexical_Types.Symbol)
      return File_Base_Name is abstract;
   --  Get file name for given library declaration unit.

   not overriding function Body_Name
     (Self   : Naming_Schema;
      Symbol : Gela.Lexical_Types.Symbol)
      return File_Base_Name is abstract;
   --  Get file name for given body or subunit.

end Gela.Naming_Schemas;
