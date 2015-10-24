package Gela.Types.Discriminated is
   pragma Preelaborate;

   type Discriminated_Type is limited interface;
   type Discriminated_Type_Access is access all Discriminated_Type'Class;
   for Discriminated_Type_Access'Storage_Size use 0;

end Gela.Types.Discriminated;
