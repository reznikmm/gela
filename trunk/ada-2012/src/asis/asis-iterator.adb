with Gela.Elements;
with Asis.Extensions.Flat_Kinds;

package body Asis.Iterator is

   --  We need to skip some children of element while iterating, because
   --  ASIS doesn't need all of them. For instance we keep "end_name" of
   --  procedure body, but ASIS never iterate it.
   Skip : constant array (Asis.Extensions.Flat_Kinds.Element_Flat_Kind) of
     Natural :=
       (Asis.Extensions.Flat_Kinds.A_Procedure_Body_Declaration => 16,
        others => 0);

   ----------------------
   -- Traverse_Element --
   ----------------------

   procedure Traverse_Element
     (Element : in     Asis.Element;
      Control : in out Traverse_Control;
      State   : in out State_Information) is
   begin
      Check_Nil_Element (Element, "Traverse_Element");

      if Control /= Continue then
         return;
      end if;

      declare
         use type Gela.Elements.Element_Sequence_Access;

         Kind     : Asis.Extensions.Flat_Kinds.Element_Flat_Kind;
         Children : constant Gela.Elements.Nested_Array :=
           Element.Data.Nested_Items;
      begin
         if not Auxilary (Element) then
            Pre_Operation (Element, Control, State);
         end if;

         if Control = Continue then
            Kind := Asis.Extensions.Flat_Kinds.Flat_Kind (Element);

            for J in Children'Range loop
               case Children (J).Kind is
                  when Gela.Elements.Nested_Token =>
                     null;

                  when Gela.Elements.Nested_Element =>
                     declare
                        Next : constant Asis.Element :=
                          (Data => Children (J).Nested_Element);
                     begin
                        if Assigned (Next) and Skip (Kind) /= J then
                           Traverse_Element (Next, Control, State);
                        end if;
                     end;

                  when Gela.Elements.Nested_Sequence =>
                     if Children (J).Nested_Sequence /= null then
                        declare
                           Next : Gela.Elements.Element_Sequence_Cursor :=
                             Children (J).Nested_Sequence.First;
                        begin
                           while Next.Has_Element loop
                              Traverse_Element
                                ((Data => Next.Element), Control, State);

                              exit when Control /= Continue;
                              Next.Next;
                           end loop;
                        end;
                     end if;

               end case;

               exit when Control /= Continue;
            end loop;

            if Control = Abandon_Siblings then
               Control := Continue;
            end if;
         end if;

         if Control = Continue and then not Auxilary (Element) then
            Post_Operation (Element, Control, State);
         end if;

         if Control = Abandon_Children then
            Control := Continue;
         end if;
      end;
   end Traverse_Element;

end Asis.Iterator;
