with Gela.Types.Discriminated;

package Gela.Types.Untagged_Records is
   pragma Preelaborate;

   type Untagged_Record_Type is limited interface
     and Type_View
     and Gela.Types.Discriminated.Discriminated_Type;

   type Untagged_Record_Type_Access is access all Untagged_Record_Type'Class;
   for Untagged_Record_Type_Access'Storage_Size use 0;

end Gela.Types.Untagged_Records;
