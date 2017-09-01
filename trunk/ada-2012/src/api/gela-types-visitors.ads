with Gela.Types.Simple;
with Gela.Types.Arrays;
with Gela.Types.Untagged_Records;

package Gela.Types.Visitors is
   pragma Preelaborate;

   type Type_Visitor is limited interface;

   not overriding procedure Enumeration_Type
     (Self  : in out Type_Visitor;
      Value : not null Gela.Types.Simple.Enumeration_Type_Access)
        is null;

   not overriding procedure Character_Type
     (Self  : in out Type_Visitor;
      Value : not null Gela.Types.Simple.Character_Type_Access)
        is null;

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

   not overriding procedure Untagged_Record
     (Self  : in out Type_Visitor;
      Value : not null Gela.Types.Untagged_Records.Untagged_Record_Type_Access)
        is null;

   not overriding procedure Object_Access_Type
     (Self  : in out Type_Visitor;
      Value : not null Gela.Types.Simple.Object_Access_Type_Access)
        is null;

   not overriding procedure Subprogram_Access_Type
     (Self  : in out Type_Visitor;
      Value : not null Gela.Types.Simple.Subprogram_Access_Type_Access)
        is null;

end Gela.Types.Visitors;
