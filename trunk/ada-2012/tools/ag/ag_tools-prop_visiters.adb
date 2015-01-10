with Ada.Text_IO;

with League.String_Vectors;

with AG_Tools.Input;
with AG_Tools.Contexts;
with AG_Tools.Writers; use AG_Tools.Writers;

package body AG_Tools.Prop_Visiters is

   --------------
   -- Generate --
   --------------

   procedure Generate (G : Gela.Grammars.Grammar_Access) is
      use type League.Strings.Universal_String;
      use type Gela.Grammars.Attribute_Declaration_Index;

      Context : aliased AG_Tools.Contexts.Context;
      Done    : League.String_Vectors.Universal_String_Vector;
      Name    : League.Strings.Universal_String;
      Spec    : Writer renames Context.Spec;
      Impl    : Writer renames Context.Impl;
   begin
      Context.Add_With ("Gela.Element_Visiters", AG_Tools.Contexts.Spec_Unit);

      Spec.P;
      Spec.P ("package Gela.Property_Visiters is");
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Property_Visiter is limited interface;");
      Spec.P;

      for J of G.Declaration loop
         if Done.Index (J.Name) = 0 then
            Context.Add_With
              (Package_Name (J.Type_Name), AG_Tools.Contexts.Spec_Unit);
            Spec.N ("   not overriding procedure On_");
            Spec.P (To_Ada (J.Name));
            Spec.P ("     (Self    : in out Property_Visiter;");
            Spec.P ("      Element : Gela.Elements.Element_Access;");
            Spec.N ("      Value   : ");
            Spec.N (J.Type_Name);
            Spec.P (") is null;");
            Spec.P;

            Done.Append (J.Name);
         end if;
      end loop;

      Spec.P
        ("   type Visiter (V : not null access Property_Visiter'Class) is");
      Spec.P ("     new Gela.Element_Visiters.Visiter with null record;");
      Spec.P;
      Spec.P ("private");

      Impl.P ("package body Gela.Property_Visiters is");

      for NT of G.Non_Terminal loop
         for Prod of G.Production (NT.First .. NT.Last) loop
            if AG_Tools.Input.Is_Concrete (NT.Index) and not NT.Is_List then
               Name := To_Ada (NT.Name);
               Context.Add_With
                 ("Gela.Elements." & Plural (Name),
                  AG_Tools.Contexts.Spec_Unit);
               Spec.N ("   overriding procedure ", Impl);
               Spec.P (To_Ada (Name), Impl);
               Spec.P ("     (Self : in out Visiter;", Impl);
               Spec.N ("      Node : not null Gela.Elements.", Impl);
               Spec.N (Plural (Name), Impl);
               Spec.N (".", Impl);
               Spec.N (To_Ada (Name), Impl);
               Spec.N ("_Access)", Impl);
               Spec.P (";");
               Impl.P (" is");
               Impl.P ("   begin");
               for Decl of
                 G.Declaration (NT.First_Attribute .. NT.Last_Attribute)
               loop
                  Impl.N ("      Self.V.On_");
                  Impl.N (To_Ada (Decl.Name));
                  Impl.N (" (Gela.Elements.Element_Access (Node), Node.");
                  Impl.N (To_Ada (Decl.Name));
                  Impl.P (");");
               end loop;

               if NT.First_Attribute > NT.Last_Attribute then
                  Impl.P ("      null;");
               end if;

               Impl.N ("   end ");
               Impl.N (To_Ada (Name));
               Impl.P (";");
               Impl.P;
               Spec.P;
            end if;
         end loop;
      end loop;

      Spec.P;
      Spec.P ("end Gela.Property_Visiters;", Impl);
      Context.Print_Withes (AG_Tools.Contexts.Spec_Unit);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
   end Generate;

end AG_Tools.Prop_Visiters;
