limited with Gela.Elements.Subtype_Indications;

package Gela.Types.Simple is
   pragma Preelaborate;

   type Discrete_Type is limited interface;
   type Discrete_Type_Access is access all Discrete_Type'Class;
   for Discrete_Type_Access'Storage_Size use 0;

   type Discrete_Type_Array is
     array (Positive range <>) of Discrete_Type_Access;

   ----------------------
   -- Enumeration_Type --
   ----------------------

   type Enumeration_Type is limited interface
     and Type_View
     and Discrete_Type;

   type Enumeration_Type_Access is access all Enumeration_Type'Class;
   for Enumeration_Type_Access'Storage_Size use 0;

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

   ------------------------
   -- Object_Access_Type --
   ------------------------

   type Object_Access_Type is limited interface
     and Type_View;

   type Object_Access_Type_Access is access all Object_Access_Type'Class;
   for Object_Access_Type_Access'Storage_Size use 0;

   not overriding function Get_Designated
     (Self   : Object_Access_Type)
      return Gela.Elements.Subtype_Indications.Subtype_Indication_Access
        is abstract;

   ----------------------------
   -- Subprogram_Access_Type --
   ----------------------------

   type Subprogram_Access_Type is limited interface
     and Type_View;

   type Subprogram_Access_Type_Access is
     access all Subprogram_Access_Type'Class;
   for Subprogram_Access_Type_Access'Storage_Size use 0;

end Gela.Types.Simple;