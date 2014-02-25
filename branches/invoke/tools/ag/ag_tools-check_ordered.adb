------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Text_IO;

with Gela.Grammars_Checks;
with Gela.Grammars_Convertors;
with Gela.Grammars.Ordered;
--  with Gela.Grammars_Debug;

with League.Strings;

with AG_Tools.Contexts;
with AG_Tools.Generator_Factories;
with AG_Tools.Writers; use AG_Tools.Writers;
with AG_Tools.Input;   use AG_Tools.Input;

package body AG_Tools.Check_Ordered is

   procedure Generate2
     (G           : Gela.Grammars.Grammar_Access;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array);

   procedure Generate_Pass
     (G           : Gela.Grammars.Grammar_Access;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array;
      Pass        : Positive;
      Found       : out Boolean);

   -----------
   -- Check --
   -----------

   procedure Check is
      Ok     : Boolean;
      Result : constant Gela.Grammars.Grammar_Access :=
        new Gela.Grammars.Grammar'
          (Gela.Grammars_Convertors.Convert_With_Empty
               (AG_Tools.Input.Grammar.all));

      Order  : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions : Gela.Grammars.Ordered.Partition_Array
        (Result.Declaration'Range);
   begin
      --           Gela.Grammars_Debug.Print (G);
      --           Gela.Grammars_Debug.Print (Result.all);

      Ok := Gela.Grammars_Checks.Is_Well_Formed (Result.all, True);

      if not Ok then
         raise Constraint_Error with "Not well formed";
      end if;

      Gela.Grammars.Ordered.Find_Order (Result.all, Ok, Partitions, Order);

      if not Ok then
         raise Constraint_Error with "Not ordered";
      end if;

      Generate2 (Result, Order, Partitions);

      return;
      pragma Warnings (Off);
      for J in Order.Iterate loop
         declare
            Key : constant Gela.Grammars.Ordered.Key :=
              Gela.Grammars.Ordered.Order_Maps.Key (J);
            Item : constant Gela.Grammars.Ordered.Action :=
              Gela.Grammars.Ordered.Order_Maps.Element (J);
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
               when Gela.Grammars.Ordered.Evaluate_Rule =>
                  Ada.Text_IO.Put_Line (Item.Rule'Img);
               when Gela.Grammars.Ordered.Descent =>
                  Ada.Text_IO.Put
                    (Result.Part (Item.Part).Name.
                         To_UTF_8_String);
                  Ada.Text_IO.Put_Line (Item.Pass'Img);
            end case;
         end;
      end loop;
      pragma Warnings (On);
   end Check;

   procedure Generate_Pass
     (G           : Gela.Grammars.Grammar_Access;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array;
      Pass        : Positive;
      Found       : out Boolean)
   is
      use Gela.Grammars;
      use type Ordered.Partition_Count;
      use type League.Strings.Universal_String;

      Context : constant AG_Tools.Contexts.Context_Access :=
        new AG_Tools.Contexts.Context;
      Spec    : AG_Tools.Writers.Writer renames Context.Spec;
      Impl    : AG_Tools.Writers.Writer renames Context.Impl;
   begin
      Context.Factory := new AG_Tools.Generator_Factories.Factory (Context);
      Context.Grammar := G;
      Context.Partition :=
        new Gela.Grammars.Ordered.Partition_Array'(Partitions);
      Context.Part_Map :=
        new AG_Tools.Contexts.Part_Map'(G.Part'Range => False);

      Impl.P ("pragma Warnings (""FUM"");");
      Impl.P ("pragma Style_Checks (""N"");");

      --      Context.Add_With ("Gela.Compilations");
      Context.Add_With ("Gela.Symbol_Sets");
      Context.Add_With ("Gela.Environments");
      Context.Add_With ("Gela.Pass_Utils");

      Spec.P ("with Gela.Compilations;");
      Spec.P ("with Gela.Element_Visiters;");
      Spec.P ("with Gela.Pass_List;");
      Spec.P;
      Spec.N ("package Gela.Pass_");
      Impl.N ("package body Gela.Pass_");
      Spec.N (Natural (Pass));
      Impl.N (Natural (Pass));
      Spec.P (" is", Impl);
      Impl.P ("   function Assigned");
      Impl.P ("     (Item : access Gela.Elements.Element'Class)" &
                " return Boolean is");
      Impl.P ("   begin");
      Impl.P ("      return Item /= null;");
      Impl.P ("   end Assigned;");
      Impl.P;
      Spec.P ("   pragma Preelaborate;");
      Spec.P;
      Spec.P ("   type Visiter");
      Spec.P ("     (Compilation : not null" &
                " Gela.Compilations.Compilation_Access;");
      Spec.P ("      Parent : not null Gela.Pass_List.Visiter_Access)");
      Spec.P
        ("     is new Gela.Element_Visiters.Visiter with null record;");
      Spec.P ("   type Visiter_Access is access all Visiter;");

      for NT of G.Non_Terminal loop
         if Is_Concrete (NT.Index) and not Is_Converted_List (G.all, NT) then
            declare
               use Gela.Grammars.Ordered.Order_Maps;
               Pos : constant Cursor :=
                 Order.Ceiling ((NT.First, Pass, Step => 1));
            begin
               if Has_Element (Pos) and then Key (Pos).Prod = NT.First then
                  Context.Factory.Get (NT).Make_Procedure (Order, NT, Pass);
                  Found := True;
               end if;
            end;
         end if;
      end loop;

      Spec.P ("", Impl);
      Spec.N ("end Gela.Pass_", Impl);
      Spec.N (Natural (Pass));
      Impl.N (Natural (Pass));
      Spec.N (";", Impl);

      if Found then
         Context.Print_Withes (AG_Tools.Contexts.Spec_Unit);
         Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
         Context.Print_Withes (AG_Tools.Contexts.Body_Unit);
         Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
      end if;
   end Generate_Pass;

   ---------------
   -- Generate2 --
   ---------------

   procedure Generate2
     (G           : Gela.Grammars.Grammar_Access;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array)
   is
      use Gela.Grammars;
      use type Ordered.Partition_Count;
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
        new Gela.Grammars.Ordered.Partition_Array'(Partitions);
      Context.Part_Map :=
        new AG_Tools.Contexts.Part_Map'(G.Part'Range => False);

      Context.Add_With ("Gela.Compilations", Contexts.Spec_Unit);
      Context.Add_With ("Gela.Element_Visiters", Contexts.Spec_Unit);
      Context.Add_With ("Gela.Pass_Utils");

      Impl.P ("pragma Warnings (""FUM"");");
      Impl.P ("pragma Style_Checks (""N"");");
      Impl.P;
      Impl.P ("package body Gela.Pass_List is");
      Head.P ("package Gela.Pass_List is");
      Head.P ("   pragma Preelaborate;");
      Head.P;
      Head.P ("   type Visiter;");
      Head.P ("   type Visiter_Access is access all Visiter;");
      Head.P;
      Head.P ("   type Visiter");
      Head.P ("     (Compilation : not null" &
                " Gela.Compilations.Compilation_Access) is tagged");
      Head.P ("   record");
      Head.P ("      Parent : Gela.Pass_List.Visiter_Access;");

      while Found loop
         Generate_Pass (G, Order, Partitions, Pass, Found);
         if Found then
            Head.N ("      P");
            Head.N (Pass);
            Head.P (" : Gela.Element_Visiters.Visiter_Access;");

            for NT of G.Non_Terminal loop
               if Is_Converted_List (G.all, NT) then
                  declare
                     use Gela.Grammars.Ordered.Order_Maps;
                     Pos : constant Cursor :=
                       Order.Ceiling ((NT.First, Pass, Step => 1));
                  begin
                     if Has_Element (Pos) and then
                       Key (Pos).Prod = NT.First
                     then
                        Context.Factory.Get (NT).Make_Procedure
                          (Order, NT, Pass);
                        Found := True;
                     end if;
                  end;
               end if;
            end loop;

            Pass := Pass + 1;
            Context.Part_Map.all := (others => False);
            Context.Attr_Map.Clear;
         end if;
      end loop;
      Head.P ("   end record;");
      Spec.P ("end Gela.Pass_List;", Impl);

      Context.Print_Withes (AG_Tools.Contexts.Spec_Unit);
      Ada.Text_IO.Put_Line (Head.Text.To_UTF_8_String);
      Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
      Context.Print_Withes (AG_Tools.Contexts.Body_Unit);
      Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
   end Generate2;

end AG_Tools.Check_Ordered;
