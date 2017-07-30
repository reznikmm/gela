------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Text_IO;

with AG_Tools.Writers; use AG_Tools.Writers;
with AG_Tools.Input; use AG_Tools.Input;

package body AG_Tools.Clone_Generator is

   ---------
   -- Run --
   ---------

   procedure Run (G : Gela.Grammars.Grammar_Access) is
      procedure Each_Part
        (Part   : Gela.Grammars.Part;
         Decl   : in out Writer;
         Assign : in out Writer;
         List   : in out Writer);

      Spec_Withes : Writer;
      Body_Withes : Writer;
      Withed : array (G.Non_Terminal'Range) of Boolean := (others => False);

      ---------------
      -- Each_Part --
      ---------------

      procedure Each_Part
        (Part   : Gela.Grammars.Part;
         Decl   : in out Writer;
         Assign : in out Writer;
         List   : in out Writer)
      is
         use type League.Strings.Universal_String;

         Item : League.Strings.Universal_String;
         Name : constant League.Strings.Universal_String := To_Ada (Part.Name);
         Mark : constant League.Strings.Universal_String :=
           Return_Type (G.all, Part);
      begin
         if not List.Text.Is_Empty then
            List.N (", ");
         end if;

         if Part.Is_Terminal_Reference then
            List.N ("Node.");
            List.N (Name);
            return;
         end if;

         Decl.N ("      ");
         Decl.N (Name);
         Decl.N (" : ");
         Decl.N (Mark);
         Decl.P ("_Access;");

         List.N (Name);

         if Part.Is_List_Reference then
            declare
               NT : constant Gela.Grammars.Non_Terminal_Index :=
                 List_Item (G.all, G.Non_Terminal (Part.Denote));
            begin
               if not Is_Concrete (NT) and not Withed (NT) then
                  Withed (NT) := True;
                  Body_Withes.N ("with Gela.Elements.");
                  Body_Withes.N (Plural (G.Non_Terminal (NT).Name));
                  Body_Withes.P (";");
               end if;
               --  Item := Mark.Head (Mark.Length - 9);
               Item := Return_Type (G.all, G.Non_Terminal (NT));
            end;

            Assign.N ("      if Node.");
            Assign.N (Name);
            Assign.P (" not in null then");
            Assign.P ("         declare");
            Assign.N ("            Item   : ");
            Assign.N (Item);
            Assign.P ("_Access;");
            Assign.N ("            Cursor : ");
            Assign.N (Mark);
            Assign.P ("_Cursor :=");
            Assign.N ("              Node.");
            Assign.N (Name);
            Assign.P (".First;");
            Assign.P ("         begin");
            Assign.N ("            ");
            Assign.N (Name);
            Assign.N (" := Self.Factory.");
            Assign.N (Mark.Tail_From (Mark.Last_Index ('.') + 1));
            Assign.P (";");
            Assign.P;
            Assign.P ("            while Cursor.Has_Element loop");
            Assign.N ("               Item := ");
            Assign.N (Item);
            Assign.P ("_Access");
            Assign.P ("                  " &
                        "(Cloner'Class (Self).Clone (Cursor.Element));");
            Assign.N ("               ");
            Assign.N (Name);
            Assign.P (".Append (Item);");
            Assign.P ("               Cursor.Next;");
            Assign.P ("            end loop;");
            Assign.P ("         end;");
            Assign.P ("      end if;");
            Assign.P;
         else
            declare
               NT : constant Gela.Grammars.Non_Terminal_Index := Part.Denote;
            begin
               if not Is_Concrete (NT) and not Withed (NT) then
                  Withed (NT) := True;
                  Body_Withes.N ("with Gela.Elements.");
                  Body_Withes.N (Plural (G.Non_Terminal (NT).Name));
                  Body_Withes.P (";");
               end if;
            end;

            Assign.N ("      ");
            Assign.N (Name);
            Assign.N (" := ");
            Assign.N (Mark);
            Assign.P ("_Access");
            Assign.N ("        (Cloner'Class (Self).Clone (Node.");
            Assign.N (Name);
            Assign.P ("));");
            Assign.P;
         end if;
      end Each_Part;

      Spec   : Writer;
      Impl   : Writer;
      Name   : League.Strings.Universal_String;
      Mark   : League.Strings.Universal_String;
   begin
      Spec_Withes.P ("with Gela.Element_Factories;");
      Spec_Withes.P ("with Gela.Element_Visiters;");
      Spec.P ("package Gela.Element_Cloners is");
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Cloner (Factory : not null");
      Spec.P ("                  " &
                "Gela.Element_Factories.Element_Factory_Access) is limited");
      Spec.P ("   new Gela.Element_Visiters.Visiter with record");
      Spec.P ("      Result  : Gela.Elements.Element_Access;");
      Spec.P ("   end record;");
      Spec.P;
      Impl.P ("pragma Style_Checks (""N"");");
      Impl.P;
      Impl.P ("package body Gela.Element_Cloners is");
      Impl.P;
      Spec.P ("   not overriding function Clone", Impl);
      Spec.P ("     (Self    : in out Cloner;", Impl);
      Spec.P ("      Element : access Gela.Elements.Element'Class)", Impl);
      Spec.N ("     return Gela.Elements.Element_Access", Impl);
      Spec.P (";");

      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      if Element.Assigned then");
      Impl.P ("         Self.Result := null;");
      Impl.P ("         Element.Visit (Self);");
      Impl.P ("         return Self.Result;");
      Impl.P ("      else");
      Impl.P ("         return null;");
      Impl.P ("      end if;");
      Impl.P ("   end Clone;");

      for NT of G.Non_Terminal loop
         for Prod of G.Production (NT.First .. NT.Last) loop
            if Is_Concrete (NT.Index) and not NT.Is_List then
               Name := To_Ada (NT.Name);
               Mark := Return_Type (G.all, NT);
               Spec_Withes.N ("with Gela.Elements.");
               Spec_Withes.N (Plural (Name));
               Spec_Withes.P (";");
               Spec.P;
               Impl.P;
               Spec.N ("   overriding procedure ", Impl);
               Spec.P (Name, Impl);
               Spec.P ("     (Self : in out Cloner;", Impl);
               Spec.N ("      Node : not null ", Impl);
               Spec.N (Mark, Impl);
               Spec.N ("_Access)", Impl);
               Spec.P (";");

               declare
                  Decl   : Writer;
                  Assign : Writer;
                  List   : Writer;
               begin
                  for Part of G.Part (Prod.First .. Prod.Last) loop
                     Each_Part (Part, Decl, Assign, List);
                  end loop;

                  Impl.P;
                  Impl.P ("   is");

                  Impl.P (Decl.Text);
                  Impl.N ("      Result : ");
                  Impl.N (Mark);
                  Impl.P ("_Access;");

                  Impl.P ("   begin");
                  Impl.P (Assign.Text);
                  Impl.N ("      Result := Self.Factory.");
                  Impl.P (To_Ada (Name));
                  Impl.N ("        (");
                  Impl.N (List.Text);
                  Impl.P (");");
                  Impl.P;
                  Impl.P
                    ("      Self.Result := "
                     & "Gela.Elements.Element_Access (Result);");
                  Impl.N ("   end ");
                  Impl.N (To_Ada (Name));
                  Impl.P (";");
               end;
            end if;
         end loop;
      end loop;

      Spec.P;
      Spec.P ("end Gela.Element_Cloners;", Impl);
      Ada.Text_IO.Put_Line (Spec_Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Body_Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
   end Run;

end AG_Tools.Clone_Generator;
