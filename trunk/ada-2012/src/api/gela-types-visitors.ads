with Gela.Types.Simple;
with Gela.Types.Arrays;

package Gela.Types.Visitors is
   pragma Preelaborate;

   type Type_Visitor is limited interface;

   not overriding procedure Signed_Integer_Type
     (Self  : in out Type_Visitor;
      Value : not null Gela.Types.Simple.Signed_Integer_Type_Access)
        is null;

   not overriding procedure Floating_Point_Type
     (Self  : in out Type_Visitor;
      Value : not null Gela.Types.Simple.Floating_Point_Type_Access)
        is null;

   not overriding procedure Array_Type
     (Self  : in out Type_Visitor;
      Value : not null Gela.Types.Arrays.Array_Type_Access)
        is null;

end Gela.Types.Visitors;
