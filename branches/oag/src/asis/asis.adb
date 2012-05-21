package body Asis is

   --------------
   -- Assigned --
   --------------

   function Assigned (Unit : Compilation_Unit) return Boolean is
   begin
      return Unit.Index /= 0;
   end Assigned;

   --------------
   -- Assigned --
   --------------

   function Assigned (Item : Element) return Boolean is
   begin
      return Assigned (Item.Unit);
   end Assigned;

   --------------
   -- Assigned --
   --------------

   function Assigned (Element : Element_Index) return Boolean is
   begin
      return Element /= 0;
   end Assigned;

end Asis;
