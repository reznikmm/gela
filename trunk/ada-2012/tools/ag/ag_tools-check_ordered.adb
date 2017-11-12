------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Text_IO;

with Anagram.Grammars_Checks;
with Anagram.Grammars_Convertors;
with Anagram.Grammars.Ordered;
--  with Anagram.Grammars_Debug;

with League.Strings;

with AG_Tools.Contexts;
with AG_Tools.Generator_Factories;
with AG_Tools.Writers; use AG_Tools.Writers;
with AG_Tools.Input;   use AG_Tools.Input;

package body AG_Tools.Check_Ordered is

   procedure Generate2
     (G           : Anagram.Grammars.Grammar_Access;
      Order       : Anagram.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Anagram.Grammars.Ordered.Partition_Array);

   -----------
   -- Check --
   -----------

   procedure Check is
      Ok     : Boolean;
      Result : constant Anagram.Grammars.Grammar_Access :=
        new Anagram.Grammars.Grammar'
          (Anagram.Grammars_Convertors.Convert_With_Empty
               (AG_Tools.Input.Grammar.all));

      Order  : Anagram.Grammars.Ordered.Order_Maps.Map;
      Partitions : Anagram.Grammars.Ordered.Partition_Array
        (Result.Declaration'Range);
   begin
      --           Anagram.Grammars_Debug.Print (G);
      --           Anagram.Grammars_Debug.Print (Result.all);

      Ok := Anagram.Grammars_Checks.Is_Well_Formed (Result.all, True);

      if not Ok then
         raise Constraint_Error with "Not well formed";
      end if;

      Anagram.Grammars.Ordered.Find_Order (Result.all, Ok, Partitions, Order);

      if not Ok then
         raise Constraint_Error with "Not ordered";
      end if;

      Generate2 (Result, Order, Partitions);

      return;
      pragma Warnings (Off);
      for J in Order.Iterate loop
         declare
            Key : constant Anagram.Grammars.Ordered.Key :=
              Anagram.Grammars.Ordered.Order_Maps.Key (J);
            Item : constant Anagram.Grammars.Ordered.Action :=
              Anagram.Grammars.Ordered.Order_Maps.Element (J);
         begin
            Ada.Text_IO.Put
              (Result.Non_Terminal
                 (Result.Production (Key.Prod).Parent).Name.
                   To_UTF_8_String);
            Ada.Text_IO.Put (".");
            Ada.Text_IO.Put
              (Result.Production (Key.Prod).Name.
                   To_UTF_8_String);
            Ada.Text_IO.Put_Line (Key.Pass'Img);

            Ada.Text_IO.Put (" ");
            Ada.Text_IO.Put (Item.Kind'Img);
            Ada.Text_IO.Put (" ");

            case Item.Kind is
               when Anagram.Grammars.Ordered.Evaluate_Rule =>
                  Ada.Text_IO.Put_Line (Item.Rule'Img);
                  Ada.Text_IO.Put_Line
                    (Result.Rule (Item.Rule).Text.To_UTF_8_String);
               when Anagram.Grammars.Ordered.Descent =>
                  Ada.Text_IO.Put
                    (Result.Part (Item.Part).Name.
                         To_UTF_8_String);
                  Ada.Text_IO.Put_Line (Item.Pass'Img);
            end case;
         end;
      end loop;
      pragma Warnings (On);
   end Check;

   ---------------
   -- Generate2 --
   ---------------

   procedure Generate2
     (G           : Anagram.Grammars.Grammar_Access;
      Order       : Anagram.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Anagram.Grammars.Ordered.Partition_Array)
   is
      use Anagram.Grammars;
      use type League.Strings.Universal_String;

      Context : constant AG_Tools.Contexts.Context_Access :=
        new AG_Tools.Contexts.Context;
      Spec    : AG_Tools.Writers.Writer renames Context.Spec;
      Impl    : AG_Tools.Writers.Writer renames Context.Impl;
      Head    : AG_Tools.Writers.Writer;
      Pass    : Positive := 1;
      Found   : Boolean := True;
   begin
      Context.Factory := new AG_Tools.Generator_Factories.Factory (Context);
      Context.Grammar := G;
      Context.Partition :=
        new Anagram.Grammars.Ordered.Partition_Array'(Partitions);
      Context.Part_Map :=
        new AG_Tools.Contexts.Part_Map'(G.Part'Range => False);

      Context.Add_With ("Gela.Compilations", Contexts.Spec_Unit);
      Context.Add_With ("Gela.Element_Visiters", Contexts.Spec_Unit);
      Context.Add_With ("Gela.Pass_Utils");

      Impl.P ("pragma Warnings (""FUM"");");
      Impl.P ("pragma Style_Checks (""N"");");
      Impl.P;
      Impl.P ("package body Gela.Pass_List is");
      Impl.P;
      Impl.P ("   function Assigned");
      Impl.P ("     (Item : access Gela.Elements.Element'Class)" &
                " return Boolean is");
      Impl.P ("   begin");
      Impl.P ("      return Item /= null;");
      Impl.P ("   end Assigned;");

      Head.P ("package Gela.Pass_List is");
      Head.P ("   pragma Preelaborate;");
      Head.P;
      Head.P ("   type Visiter");
      Head.P ("     (Compilation : not null" &
                " Gela.Compilations.Compilation_Access) is tagged");
      Head.P ("   record");
      Head.P ("      null;");
      Head.P ("   end record;");
      Head.P;
      Head.P ("   type Visiter_Access is access all Visiter;");

      while Found loop
         Found := False;

         for NT of G.Non_Terminal loop
            declare
               use Anagram.Grammars.Ordered.Order_Maps;
               Pos : constant Cursor :=
                 Order.Ceiling ((NT.First, Pass, Step => 1));
            begin
               if Has_Element (Pos) and then
                 Key (Pos).Prod = NT.First and then
                 Key (Pos).Pass = Pass
               then
                  Context.Factory.Get (NT).Make_Procedure
                    (Order, NT, Pass);
                  Found := True;
               end if;
            end;
         end loop;

         Pass := Pass + 1;
         Context.Part_Map.all := (others => False);
         Context.Attr_Map.Clear;
      end loop;
      Spec.P ("end Gela.Pass_List;", Impl);

      Context.Print_Withes (AG_Tools.Contexts.Spec_Unit);
      Ada.Text_IO.Put_Line (Head.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
      Context.Print_Withes (AG_Tools.Contexts.Body_Unit);
      Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
   end Generate2;

end AG_Tools.Check_Ordered;
