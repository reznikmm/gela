package Gela.Types.Arrays is
   pragma Preelaborate;

   type Array_Type is limited interface and Type_View;
   type Array_Type_Access is access all Array_Type'Class;
   for Array_Type_Access'Storage_Size use 0;

end Gela.Types.Arrays;
