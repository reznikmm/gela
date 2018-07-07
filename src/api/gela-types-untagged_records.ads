with Gela.Elements.Defining_Names;
with Gela.Lexical_Types;
with Gela.Types.Discriminated;

package Gela.Types.Untagged_Records is
   pragma Preelaborate;

   type Untagged_Record_Type is limited interface
     and Type_View
     and Gela.Types.Discriminated.Discriminated_Type;

   type Untagged_Record_Type_Access is access all Untagged_Record_Type'Class;
   for Untagged_Record_Type_Access'Storage_Size use 0;

   not overriding function Get_Component
     (Self   : Untagged_Record_Type;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access is abstract;

end Gela.Types.Untagged_Records;
