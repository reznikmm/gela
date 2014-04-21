with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

package body Asis is

   ----------------------------------
   -- Assert_Inappropriate_Element --
   ----------------------------------

   procedure Assert_Inappropriate_Element (Ok : Boolean; From : Wide_String) is
   begin
      if not Ok then
         Asis.Implementation.Set_Status
           (Asis.Errors.Value_Error, "Inappropriate element in " & From);
         raise Asis.Exceptions.ASIS_Inappropriate_Element;
      end if;
   end Assert_Inappropriate_Element;

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

   -------------
   -- To_List --
   -------------

   function To_List
     (X : Gela.Elements.Element_Sequence_Access) return Asis.Element_List
   is
      Result : Asis.Element_List (1 .. ASIS_Natural (X.Length));
      Cursor : Gela.Elements.Element_Sequence_Cursor := X.First;
   begin
      for J in Result'Range loop
         Result (J) := (Data => Cursor.Element);
         Cursor.Next;
      end loop;

      return Result;
   end To_List;

end Asis;
