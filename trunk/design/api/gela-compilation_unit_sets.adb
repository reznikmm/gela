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
   -- Variable_Indexing --
   -----------------------

   function Variable_Indexing
     (Self   : Compilation_Unit_Set'Class;
      Cursor : not null Gela.Compilation_Units.Compilation_Unit_Access)
      return Reference_Type
   is
      pragma Unreferenced (Self);
   begin
      return (Unit => Cursor);
   end Variable_Indexing;

end Gela.Compilation_Unit_Sets;
