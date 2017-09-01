package body Gela.Resolve.Type_Matchers is

   ----------------
   -- Array_Type --
   ----------------

   overriding procedure Array_Type
     (Self  : in out Array_Type_Matcher;
      Value : not null Gela.Types.Arrays.Array_Type_Access)
   is
      pragma Unreferenced (Value);
   begin
      Self.Match := True;
   end Array_Type;

   ----------------
   -- Array_Type --
   ----------------

   overriding procedure Array_Type
     (Self  : in out String_Type_Matcher;
      Value : not null Gela.Types.Arrays.Array_Type_Access)
   is
      pragma Unreferenced (Value);
   begin
      Self.Match := True;  --  Value.Is_String;  FIXME
   end Array_Type;

   --------------------
   -- Character_Type --
   --------------------

   overriding procedure Character_Type
     (Self  : in out Character_Type_Matcher;
      Value : not null Gela.Types.Simple.Character_Type_Access)
   is
      pragma Unreferenced (Value);
   begin
      Self.Match := True;
   end Character_Type;

   -------------------------
   -- Floating_Point_Type --
   -------------------------

   overriding procedure Floating_Point_Type
     (Self  : in out Float_Type_Matcher;
      Value : not null Gela.Types.Simple.Floating_Point_Type_Access)
   is
      pragma Unreferenced (Value);
   begin
      Self.Match := True;
   end Floating_Point_Type;

   ----------------
   -- Is_Matched --
   ----------------

   overriding function Is_Matched (Self : Base_Type_Matcher) return Boolean is
   begin
      return Self.Match;
   end Is_Matched;

   -------------------------
   -- Signed_Integer_Type --
   -------------------------

   overriding procedure Signed_Integer_Type
     (Self  : in out Integer_Type_Matcher;
      Value : not null Gela.Types.Simple.Signed_Integer_Type_Access)
   is
      pragma Unreferenced (Value);
   begin
      Self.Match := True;
   end Signed_Integer_Type;

   ---------------------
   -- Untagged_Record --
   ---------------------

   overriding procedure Untagged_Record
     (Self  : in out Record_Type_Matcher;
      Value : not null Gela.Types.Untagged_Records.Untagged_Record_Type_Access)
   is
      pragma Unreferenced (Value);
   begin
      Self.Match := True;
   end Untagged_Record;

end Gela.Resolve.Type_Matchers;
