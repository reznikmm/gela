with Gela.Elements.Defining_Names;
with Gela.Lexical_Types;

package Gela.Types.Discriminated is
   pragma Preelaborate;

   type Discriminated_Type is limited interface;
   type Discriminated_Type_Access is access all Discriminated_Type'Class;
   for Discriminated_Type_Access'Storage_Size use 0;

   not overriding function Get_Discriminant
     (Self   : Discriminated_Type;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access is abstract;

end Gela.Types.Discriminated;
