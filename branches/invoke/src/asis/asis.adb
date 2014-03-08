with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

package body Asis is

   --------------
   -- Assigned --
   --------------

   function Assigned (Unit : in Asis.Compilation_Unit) return Boolean is
      use type Gela.Compilation_Units.Compilation_Unit_Access;
   begin
      return Unit.Data /= null;
   end Assigned;

   --------------
   -- Assigned --
   --------------

   function Assigned (Element : in Asis.Element) return Boolean is
      use type Gela.Elements.Element_Access;
   begin
      return Element.Data /= null;
   end Assigned;

   --------------------
   -- Check_Nil_Unit --
   --------------------

   procedure Check_Nil_Unit
     (Unit : Asis.Compilation_Unit;
      From : Wide_String) is
   begin
      if not Assigned (Unit) then
         Asis.Implementation.Set_Status
           (Asis.Errors.Value_Error, "Null compilation unit in " & From);
         raise Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit;
      end if;
   end Check_Nil_Unit;

   -----------------------
   -- Check_Nil_Element --
   -----------------------

   procedure Check_Nil_Element
     (Element : Asis.Element;
      From    : Wide_String) is
   begin
      if not Assigned (Element) then
         Asis.Implementation.Set_Status
           (Asis.Errors.Value_Error, "Null element in " & From);
         raise Asis.Exceptions.ASIS_Inappropriate_Element;
      end if;
   end Check_Nil_Element;

   ---------------------------
   -- Raise_Not_Implemented --
   ---------------------------

   procedure Raise_Not_Implemented (From : Wide_String) is
   begin
      Asis.Implementation.Set_Status
        (Asis.Errors.Not_Implemented_Error, From);
      raise Asis.Exceptions.ASIS_Failed;
   end Raise_Not_Implemented;

end Asis;
