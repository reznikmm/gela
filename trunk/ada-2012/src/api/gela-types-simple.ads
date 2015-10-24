package Gela.Types.Simple is
   pragma Preelaborate;

   type Discrete_Type is limited interface;
   type Discrete_Type_Access is access all Discrete_Type'Class;
   for Discrete_Type_Access'Storage_Size use 0;

   type Discrete_Type_Array is
     array (Positive range <>) of Discrete_Type_Access;

   -------------------------
   -- Signed_Integer_Type --
   -------------------------

   type Signed_Integer_Type is limited interface
     and Type_View
     and Discrete_Type;

   type Signed_Integer_Type_Access is access all Signed_Integer_Type'Class;
   for Signed_Integer_Type_Access'Storage_Size use 0;

   -------------------------
   -- Floating_Point_Type --
   -------------------------

   type Floating_Point_Type is limited interface
     and Type_View
     and Discrete_Type;

   type Floating_Point_Type_Access is access all Floating_Point_Type'Class;
   for Floating_Point_Type_Access'Storage_Size use 0;

end Gela.Types.Simple;
