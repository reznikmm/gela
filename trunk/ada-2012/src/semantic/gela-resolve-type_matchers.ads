with Gela.Types.Arrays;
with Gela.Types.Simple;
with Gela.Types.Untagged_Records;

private package Gela.Resolve.Type_Matchers is
   pragma Preelaborate;

   type Type_Matcher_Access is
     not null access all Gela.Interpretations.Type_Matcher'Class;

   type Array_Type_Matcher is
     new Gela.Interpretations.Type_Matcher with private;

   type Float_Type_Matcher is
     new Gela.Interpretations.Type_Matcher with private;

   type Integer_Type_Matcher is
     new Gela.Interpretations.Type_Matcher with private;

   type Record_Type_Matcher is
     new Gela.Interpretations.Type_Matcher with private;

   type String_Type_Matcher is
     new Gela.Interpretations.Type_Matcher with private;

private

   type Base_Type_Matcher is abstract new Gela.Interpretations.Type_Matcher
     with record
       Match : Boolean := False;
   end record;

   overriding function Is_Matched (Self : Base_Type_Matcher) return Boolean;

   type Array_Type_Matcher is new Base_Type_Matcher with null record;

   overriding procedure Array_Type
     (Self  : in out Array_Type_Matcher;
      Value : not null Gela.Types.Arrays.Array_Type_Access);

   type Float_Type_Matcher is new Base_Type_Matcher with null record;

   overriding procedure Floating_Point_Type
     (Self  : in out Float_Type_Matcher;
      Value : not null Gela.Types.Simple.Floating_Point_Type_Access);

   type Integer_Type_Matcher is new Base_Type_Matcher with null record;

   overriding procedure Signed_Integer_Type
     (Self  : in out Integer_Type_Matcher;
      Value : not null Gela.Types.Simple.Signed_Integer_Type_Access);

   type Record_Type_Matcher is new Base_Type_Matcher with null record;

   overriding procedure Untagged_Record
     (Self  : in out Record_Type_Matcher;
      Value : not null Gela.Types.Untagged_Records.
        Untagged_Record_Type_Access);

   type String_Type_Matcher is new Base_Type_Matcher with null record;

   overriding procedure Array_Type
     (Self  : in out String_Type_Matcher;
      Value : not null Gela.Types.Arrays.Array_Type_Access);

end Gela.Resolve.Type_Matchers;
