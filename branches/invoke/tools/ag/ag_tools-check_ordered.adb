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
with AG_Tools.Generator_Fabrics;
with AG_Tools.Writers; use AG_Tools.Writers;
with AG_Tools.Input;   use AG_Tools.Input;

package body AG_Tools.Check_Ordered is

   procedure Generate2
     (G           : Gela.Grammars.Grammar_Access;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array;
      Pass        : in out Positive;
      Found       : out Boolean);

   -----------
   -- Check --
   -----------

   procedure Check is
      Ok     : Boolean;
      Pass   : Positive := 1;
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

      while Ok loop
         Generate2
           (Result, Order, Partitions, Pass, Ok);
      end loop;
   end Check;

   ---------------
   -- Generate2 --
   ---------------

   procedure Generate2
     (G           : Gela.Grammars.Grammar_Access;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array;
      Pass        : in out Positive;
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
      Context.Fabric := new AG_Tools.Generator_Fabrics.Fabric (Context);
      Context.Grammar := G;
      Context.Partition :=
        new Gela.Grammars.Ordered.Partition_Array'(Partitions);
      Context.Part_Map :=
        new AG_Tools.Contexts.Part_Map'(G.Part'Range => False);

      Found := False;
      Impl.P ("pragma Warnings (""FUM"");");
      Impl.P ("pragma Style_Checks (""N"");");

      Context.Add_With ("Gela.Mutables.Compilations");
      Context.Add_With ("Gela.Symbol_Sets");
      Context.Add_With ("Gela.Pass_Utils");

      Spec.P ("with Gela.Mutables;");
      Spec.P ("with Gela.Nodes.Visiters;");
      Spec.P;
      Spec.N ("package Gela.Pass_");
      Impl.N ("package body Gela.Pass_");
      Spec.N (Natural (Pass));
      Impl.N (Natural (Pass));
      Spec.P (" is", Impl);
      Spec.P ("   pragma Preelaborate;");
      Impl.P ("   use Gela.Nodes;");
      Spec.P;
      Spec.P ("   type Visiter");
      Spec.P ("     (Compilation : Gela.Mutables.Mutable_Compilation_Access)");
      Spec.P ("     is new Gela.Nodes.Visiters.Visiter with null record;");

      for NT of G.Non_Terminal loop
         if Is_Concrete (NT.Index) or Is_List (NT) then
            declare
               use Gela.Grammars.Ordered.Order_Maps;
               Pos : Cursor := Order.Ceiling ((NT.First, Pass, Step => 1));
            begin
               if Has_Element (Pos) and then Key (Pos).Prod = NT.First then
                  Context.Fabric.Get (NT).Make_Procedure (Pos);
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
         Pass := Pass + 1;
         Context.Print_Withes (AG_Tools.Contexts.Spec_Unit);
         Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
         Context.Print_Withes (AG_Tools.Contexts.Body_Unit);
         Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
      end if;
   end Generate2;

end AG_Tools.Check_Ordered;
