with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Ada_Environments;

package body Asis.Gela is

   -------------------
   -- Check_Context --
   -------------------

   procedure Check_Context (The_Context : Asis.Context) is
   begin
      if not Ada_Environments.Is_Open (The_Context) then
         Implementation.Set_Status
           (Errors.Value_Error, "Null or unopen context");
         raise Exceptions.ASIS_Inappropriate_Context;
      end if;
   end Check_Context;

   -----------------------
   -- Check_Nil_Element --
   -----------------------

   procedure Check_Nil_Element
     (Element : Asis.Element;
      Raiser  : Wide_String := "")
   is
   begin
      if not Assigned (Element) then
         Raise_Inappropriate_Element (Raiser);
      end if;
   end Check_Nil_Element;

   --------------------
   -- Check_Nil_Unit --
   --------------------

   procedure Check_Nil_Unit
     (Unit    : Asis.Compilation_Unit;
      Raiser  : Wide_String := "")
   is
   begin
      if not Assigned (Unit) then
         Implementation.Set_Status
           (Errors.Value_Error, "Null compilation unit " & Raiser);
         raise Exceptions.ASIS_Inappropriate_Compilation_Unit;
      end if;
   end Check_Nil_Unit;

   ---------------------------------
   -- Raise_Inappropriate_Element --
   ---------------------------------

   procedure Raise_Inappropriate_Element (Raiser : Wide_String := "") is
      Text : constant Wide_String := "Inappropriate element";
   begin
      if Raiser /= "" then
         Implementation.Set_Status
           (Errors.Value_Error, Text & " in " & Raiser);
      else
         Implementation.Set_Status
           (Errors.Value_Error, Text);
      end if;

      raise Exceptions.ASIS_Inappropriate_Element;
   end Raise_Inappropriate_Element;

end Asis.Gela;
