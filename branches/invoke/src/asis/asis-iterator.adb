with Gela.Elements;

package body Asis.Iterator is

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
         Children : constant Gela.Elements.Nested_Array :=
           Element.Data.Nested_Items;
      begin
         Pre_Operation (Element, Control, State);

         if Control = Continue then
            for J in Children'Range loop
               case Children (J).Kind is
                  when Gela.Elements.Nested_Token =>
                     null;

                  when Gela.Elements.Nested_Element =>
                     declare
                        Next : constant Asis.Element :=
                          (Data => Children (J).Nested_Element);
                     begin
                        if Assigned (Next) then
                           Traverse_Element (Next, Control, State);
                        end if;
                     end;

                  when Gela.Elements.Nested_Sequence =>
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

               end case;

               exit when Control /= Continue;
            end loop;

            if Control = Abandon_Siblings then
               Control := Continue;
            end if;
         end if;

         if Control = Continue then
            Post_Operation (Element, Control, State);
         end if;

         if Control = Abandon_Children then
            Control := Continue;
         end if;
      end;
   end Traverse_Element;

end Asis.Iterator;
