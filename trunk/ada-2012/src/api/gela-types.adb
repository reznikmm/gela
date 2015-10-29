package body Gela.Types is

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Self : Type_View'Class) return Boolean is
   begin
      return Self.Is_Signed_Integer or else Self.Is_Modular_Integer;
   end Is_Integer;

   ----------------
   -- Is_Numeric --
   ----------------

   function Is_Numeric (Self : Type_View'Class) return Boolean is
   begin
      return Self.Is_Integer or else Self.Is_Real;
   end Is_Numeric;

   -------------
   -- Is_Real --
   -------------

   function Is_Real (Self : Type_View'Class) return Boolean is
   begin
      return Self.Is_Floating_Point; --  or Self.Is_Fixed_Point
   end Is_Real;

end Gela.Types;
