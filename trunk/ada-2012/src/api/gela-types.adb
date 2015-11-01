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

   -----------------------
   -- Visit_If_Assigned --
   -----------------------

   procedure Visit_If_Assigned
     (Self    : access Type_View'Class;
      Visiter : in out Gela.Types.Visitors.Type_Visitor'Class) is
   begin
      if Self.Assigned then
         Self.Visit (Visiter);
      end if;
   end Visit_If_Assigned;

end Gela.Types;
