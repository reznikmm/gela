package body Gela.Compilation_Unit_Sets is

   --------------
   -- Assigned --
   --------------

   function Assigned
     (Self : Gela.Compilation_Units.Compilation_Unit_Access)
      return Boolean is
   begin
      return Self.Assigned;
   end Assigned;

   -----------------------
   -- Constant_Indexing --
   -----------------------

   function Constant_Indexing
     (Self   : Compilation_Unit_Set'Class;
      Cursor : not null Gela.Compilation_Units.Compilation_Unit_Access)
      return Reference_Type
   is
      pragma Unreferenced (Self);
   begin
      return (Unit => Cursor);
   end Constant_Indexing;

   -----------------------
   -- Constant_Indexing --
   -----------------------

   function Constant_Indexing
     (Self  : Compilation_Unit_Set'Class;
      Value : Wide_Wide_String) return Reference_Type is
   begin
      return Self.Constant_Indexing
        (League.Strings.To_Universal_String (Value));
   end Constant_Indexing;

   -----------------------
   -- Constant_Indexing --
   -----------------------

   function Constant_Indexing
     (Self  : Compilation_Unit_Set'Class;
      Value : League.Strings.Universal_String) return Reference_Type is
   begin
      return Self.Constant_Indexing (Self.Find (Self.Context.Symbol (Value)));
   end Constant_Indexing;

end Gela.Compilation_Unit_Sets;
