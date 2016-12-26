------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

with League.Strings;

with Gela.Grammars;

with AG_Tools; use AG_Tools;
with AG_Tools.Writers; use AG_Tools.Writers;
--  with Gela.Grammars_Debug;
with AG_Tools.Input;
with AG_Tools.Element_Generators;
with AG_Tools.Prop_Visiters;
with AG_Tools.Prop_Setter;
with AG_Tools.Clone_Generator;

procedure AG_Driver is
   use AG_Tools.Input;

   use type Gela.Grammars.Production_Index;
   use type Gela.Grammars.Part_Count;
   use type Gela.Grammars.Non_Terminal_Count;
   use type League.Strings.Universal_String;

   procedure Generate_Visiter;
   procedure Generate_2;

   Name  : constant String := Ada.Command_Line.Argument (1);
   G     : Gela.Grammars.Grammar_Access;

   procedure Generate_2 is
      Fab_With    : Writer;
      Fab_Kind    : Writer;
      Fab_When    : Writer;
      Factories   : Writer;
      Impl        : Writer;
      Conv        : Writer;
   begin
      Factories.P ("with Gela.Lexical_Types;");
      Factories.P ("with Gela.Elements;");
      Factories.P;
      Factories.P ("package Gela.LARL_Parsers_Nodes is");
      Impl.P ("package body Gela.LARL_Parsers_Nodes is");
      Factories.P ("   pragma Preelaborate;");
      Factories.P;
      Factories.P ("   type Node is private;");
      Factories.P ("   type Node_Array is array (Positive range <>) of Node;");
      Factories.P;
      Factories.P ("   None     : constant Node;");
      Factories.P ("   No_Token : constant Node;");
      Factories.P;
      Factories.P ("   No_Token_Index : constant " &
                   "Gela.Lexical_Types.Token_Count := 0;");
      Factories.P;

      Factories.N ("   function ""-"" (X : Node) return " &
                     "Gela.Elements.Element_Sequence_Access", Conv);
      Factories.P (";");
      Factories.P;
      Conv.P (" is");
      Conv.P ("   begin");
      Conv.P ("      case X.Kind is");

      Factories.N ("   function ""-"" (X : Node)" &
                   " return Gela.Lexical_Types.Token_Count", Impl);
      Factories.P (";");
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      return X.Token;");
      Impl.P ("   end ""-"";");
      Impl.P;

      Factories.N ("   function ""-"" (X : Node)" &
                   " return access Gela.Elements.Element'Class", Impl);
      Factories.P (";");
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      return X.Element;");
      Impl.P ("   end ""-"";");
      Impl.P;

      Factories.N ("   function ""+"" (X : Gela.Lexical_Types.Token_Count)" &
                   " return Node", Impl);
      Factories.P (";");
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      return (Token, X);");
      Impl.P ("   end ""+"";");
      Impl.P;

      Factories.N
        ("   function ""+"" (X : access Gela.Elements.Element'Class)" &
           " return Node", Impl);
      Factories.P (";");
      Impl.P (" is");
      Impl.P ("   begin");
      Impl.P ("      return (Element, X);");
      Impl.P ("   end ""+"";");
      Impl.P;

      Fab_Kind.P ("   type Node_Kinds is");
      Fab_Kind.P ("     (Token,");
      Fab_Kind.N ("      Element");

      for NT of G.Non_Terminal loop
         if not NT.Is_List then
            if Has_List (NT.Index) then
               Fab_With.N ("with Gela.Elements.");
               Fab_With.N (To_Ada (Plural (NT.Name)));
               Fab_With.P (";");

               Fab_Kind.P (",");
               Fab_Kind.N ("      ");
               Fab_Kind.N (To_Ada (NT.Name));
               Fab_Kind.N ("_Sequence");

               Factories.P ("   function ""-"" (X : Node) return", Impl);
               Factories.N ("     Gela.Elements.", Impl);
               Factories.N (To_Ada (Plural (NT.Name)), Impl);
               Factories.N (".", Impl);
               Factories.N (To_Ada (NT.Name), Impl);
               Factories.N ("_Sequence_Access", Impl);
               Factories.P (";");
               Impl.P (" is");
               Impl.P ("   begin");
               Impl.N ("      return X.");
               Impl.N (To_Ada (NT.Name));
               Impl.P ("_Sequence;");
               Impl.P ("   end ""-"";");
               Impl.P;

               Factories.P ("   function ""+""", Impl);
               Factories.N ("     (X : Gela.Elements.", Impl);
               Factories.N (To_Ada (Plural (NT.Name)), Impl);
               Factories.N (".", Impl);
               Factories.N (To_Ada (NT.Name), Impl);
               Factories.P ("_Sequence_Access)", Impl);
               Factories.N ("     return Node", Impl);
               Factories.P (";");
               Impl.P (" is");
               Impl.P ("   begin");
               Impl.N ("      return (");
               Impl.N (To_Ada (NT.Name));
               Impl.P ("_Sequence, X);");
               Impl.P ("   end ""+"";");
               Impl.P;

               Fab_When.N ("         when ");
               Fab_When.N (To_Ada (NT.Name));
               Fab_When.P ("_Sequence =>");
               Fab_When.N ("            ");
               Fab_When.N (To_Ada (NT.Name));
               Fab_When.P ("_Sequence :");
               Fab_When.N ("              Gela.Elements.");
               Fab_When.N (To_Ada (Plural (NT.Name)));
               Fab_When.N (".");
               Fab_When.N (To_Ada (NT.Name));
               Fab_When.P ("_Sequence_Access;");

               Conv.N ("         when ");
               Conv.N (To_Ada (NT.Name));
               Conv.P ("_Sequence =>");
               Conv.P
                 ("            return Gela.Elements.Element_Sequence_Access");
               Conv.N ("              (X.");
               Conv.N (To_Ada (NT.Name));
               Conv.P ("_Sequence);");
            end if;
         end if;
      end loop;

      Conv.P ("         when others =>");
      Conv.P ("            raise Constraint_Error;");
      Conv.P ("      end case;");
      Conv.P ("   end ""-"";");

      Factories.P;
      Factories.P ("private");
      Factories.P;
      Factories.N (Fab_Kind.Text);
      Factories.P (");");
      Factories.P;
      Factories.P ("   type Node (Kind : Node_Kinds := Token) is record");
      Factories.P ("      case Kind is");
      Factories.P ("         when Token =>");
      Factories.P ("            Token : Gela.Lexical_Types.Token_Count;");
      Factories.P ("         when Element =>");
      Factories.P ("            Element : Gela.Elements.Element_Access;");

      Ada.Text_IO.Put_Line (Fab_With.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Factories.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Fab_When.Text.To_UTF_8_String);
      Factories.Clear;

      Factories.P ("      end case;");
      Factories.P ("   end record;");
      Factories.P;
      Factories.P ("   None     : constant Node := (Element, null);");
      Factories.P ("   No_Token : constant Node := (Token, 0);");
      Factories.P;
      Factories.N ("end Gela.LARL_Parsers_Nodes;", Conv);

      Ada.Text_IO.Put_Line (Factories.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Conv.Text.To_UTF_8_String);
   end Generate_2;

   ----------------------
   -- Generate_Visiter --
   ----------------------

   procedure Generate_Visiter is
      Withes : Writer;
      Spec   : Writer;
      Name   : League.Strings.Universal_String;
   begin
      Spec.P ("package Gela.Element_Visiters is");
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Visiter is limited interface;");
      Spec.P ("   type Visiter_Access is access all Visiter'Class;");

      for NT of G.Non_Terminal loop
         for Prod of G.Production (NT.First .. NT.Last) loop
            if Is_Concrete (NT.Index) and not NT.Is_List then
               Name := To_Ada (NT.Name);
               Withes.N ("with Gela.Elements.");
               Withes.N (Plural (Name));
               Withes.P (";");
               Spec.P;
               Spec.N ("   not overriding procedure ");
               Spec.P (To_Ada (Name));
               Spec.P ("     (Self : in out Visiter;");
               Spec.N ("      Node : not null Gela.Elements.");
               Spec.N (Plural (Name));
               Spec.N (".");
               Spec.N (To_Ada (Name));
               Spec.P ("_Access)");
               Spec.P ("        is null;");
            end if;
         end loop;
      end loop;

      Spec.P;
      Spec.P ("end Gela.Element_Visiters;");
      Ada.Text_IO.Put_Line (Withes.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
   end Generate_Visiter;

begin
   AG_Tools.Input.Initialize (Name);
   G := AG_Tools.Input.Grammar;

--     Gela.Grammars_Debug.Print (G);

--   Generate_Factory;
   Generate_2;
   Generate_Visiter;
   AG_Tools.Prop_Visiters.Generate (G);
   AG_Tools.Prop_Setter.Generate (G);
   AG_Tools.Element_Generators.Generate_Elements (G);
   AG_Tools.Element_Generators.Generate_Factory (G);
   AG_Tools.Clone_Generator.Run (G);
end AG_Driver;
