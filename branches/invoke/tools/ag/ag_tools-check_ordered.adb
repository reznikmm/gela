with Ada.Text_IO;

with Gela.Grammars.Constructors;
--  with Gela.Grammars_Debug;
with Gela.Grammars_Checks;
with Gela.Grammars_Convertors;
with Gela.Grammars.Ordered;
with Gela.Grammars.Rule_Templates;

with League.Strings;
with League.String_Vectors;

with AG_Tools.Writers; use AG_Tools.Writers;

package body AG_Tools.Check_Ordered is

   procedure Copy
     (G : Gela.Grammars.Grammar;
      V : in out Gela.Grammars.Constructors.Constructor;
      Is_Concrete : NT_List);

   procedure Copy_Attr
     (G      : Gela.Grammars.Grammar;
      V      : in out Gela.Grammars.Constructors.Constructor;
      Child  : Gela.Grammars.Non_Terminal_Index;
      Parent : Gela.Grammars.Non_Terminal_Index;
      Done   : in out League.String_Vectors.Universal_String_Vector);

   procedure Copy_Productions
     (G : Gela.Grammars.Grammar;
      V : in out Gela.Grammars.Constructors.Constructor;
      From : Gela.Grammars.Production_Index;
      To   : Gela.Grammars.Production_Count;
      PL : in out Gela.Grammars.Constructors.Production_List);

   procedure Generate
     (G           : Gela.Grammars.Grammar;
      Implement   : NT_Map;
      Is_Concrete : NT_List;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array;
      Pass        : in out Positive;
      Found       : out Boolean);

   This : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("This");

   -----------
   -- Check --
   -----------

   procedure Check
     (G           : Gela.Grammars.Grammar;
      Implement   : NT_Map;
      Is_Concrete : NT_List)
   is
      V : Gela.Grammars.Constructors.Constructor;
   begin
      Copy (G, V, Is_Concrete);

      for Parent in Implement'Range (2) loop
         for Child in Implement'Range (1) loop
            declare
               Done : League.String_Vectors.Universal_String_Vector;
            begin
               if Implement (Child, Parent) then
                  Copy_Attr (G, V, Child, Parent, Done);
               end if;
            end;
         end loop;
      end loop;

      declare
         Ok     : Boolean;
         Pass   : Positive := 1;
         Temp   : constant Gela.Grammars.Grammar := V.Complete;
         Result : constant Gela.Grammars.Grammar :=
           Gela.Grammars_Convertors.Convert_With_Empty (Temp);
         Order  : Gela.Grammars.Ordered.Order_Maps.Map;
         Partitions : Gela.Grammars.Ordered.Partition_Array
           (Result.Declaration'Range);
      begin
--           Gela.Grammars_Debug.Print (G);
--           Gela.Grammars_Debug.Print (Result);

         Ok := Gela.Grammars_Checks.Is_Well_Formed (Result, True);

         if not Ok then
            raise Constraint_Error with "Not well formed";
         end if;

         Gela.Grammars.Ordered.Find_Order (Result, Ok, Partitions, Order);

         if not Ok then
            raise Constraint_Error with "Not ordered";
         end if;

         while Ok loop
            Generate
              (Result, Implement, Is_Concrete, Order, Partitions, Pass, Ok);
         end loop;
      end;
   end Check;

   ----------------------
   -- Copy_Productions --
   ----------------------

   procedure Copy_Productions
     (G : Gela.Grammars.Grammar;
      V : in out Gela.Grammars.Constructors.Constructor;
      From : Gela.Grammars.Production_Index;
      To   : Gela.Grammars.Production_Count;
      PL : in out Gela.Grammars.Constructors.Production_List)
   is
   begin
      for K in From .. To loop
         declare
            S : Gela.Grammars.Production renames G.Production (K);
            P : Gela.Grammars.Constructors.Production :=
              V.Create_Production (S.Name, S.Precedence);
         begin
            for X in S.First .. S.Last loop
               declare
                  R : Gela.Grammars.Part renames G.Part (X);
               begin
                  if R.Is_Terminal_Reference then
                     P.Add (V.Create_Terminal_Reference
                            (R.Name,
                               G.Terminal (R.Denote).Image));
                  elsif R.Is_List_Reference then
                     P.Add (V.Create_List_Reference
                            (R.Name,
                               G.Non_Terminal (R.Denote).Name));
                  elsif R.Is_Non_Terminal_Reference then
                     P.Add (V.Create_Non_Terminal_Reference
                            (R.Name,
                               G.Non_Terminal (R.Denote).Name));
                  else
                     declare
                        Next : Gela.Grammars.Constructors.Production_List :=
                          V.Create_Production_List;
                     begin
                        Copy_Productions (G, V, R.First, R.Last, Next);
                        P.Add (V.Create_Option (R.Name, Next));
                     end;
                  end if;
               end;
            end loop;

            PL.Add (P);
         end;
      end loop;
   end Copy_Productions;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (G : Gela.Grammars.Grammar;
      V : in out Gela.Grammars.Constructors.Constructor;
      Is_Concrete : NT_List) is
   begin
      for J in G.Terminal'Range loop
         declare
            T : Gela.Grammars.Terminal renames G.Terminal (J);
         begin
            V.Create_Terminal (T.Image, T.Precedence);

            for A in T.First_Attribute .. T.Last_Attribute loop
               V.Create_Attribute_Declaration
                 (T.Image,
                  G.Declaration (A).Name,
                  G.Declaration (A).Type_Name);
            end loop;
         end;
      end loop;

      for J in G.Non_Terminal'Range loop
         declare
            N  : Gela.Grammars.Non_Terminal renames G.Non_Terminal (J);
            PL : Gela.Grammars.Constructors.Production_List :=
              V.Create_Production_List;
         begin
            Copy_Productions (G, V, N.First, N.Last, PL);

            if N.Is_List then
               V.Create_List (N.Name, PL);
            else
               V.Create_Non_Terminal (N.Name, PL);
            end if;

            for A in N.First_Attribute .. N.Last_Attribute loop
               V.Create_Attribute_Declaration
                 (N.Name,
                  G.Declaration (A).Name,
                  G.Declaration (A).Is_Inherited,
                  G.Declaration (A).Type_Name);
            end loop;

            for K in N.First .. N.Last loop
               declare
                  S : Gela.Grammars.Production renames G.Production (K);
               begin
                  for Y in S.First_Rule .. S.Last_Rule loop
                     declare
                        R : Gela.Grammars.Rule renames G.Rule (Y);
                     begin
                        V.Create_Rule (N.Name, S.Name, R.Text);

                        if not Is_Concrete (N.Index) then
                           raise Constraint_Error;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end;
      end loop;
   end Copy;

   ---------------
   -- Copy_Attr --
   ---------------

   procedure Copy_Attr
     (G      : Gela.Grammars.Grammar;
      V      : in out Gela.Grammars.Constructors.Constructor;
      Child  : Gela.Grammars.Non_Terminal_Index;
      Parent : Gela.Grammars.Non_Terminal_Index;
      Done   : in out League.String_Vectors.Universal_String_Vector)
   is
      X : Gela.Grammars.Non_Terminal renames G.Non_Terminal (Child);
      Y : Gela.Grammars.Non_Terminal renames G.Non_Terminal (Parent);
   begin
      for K in Y.First_Attribute .. Y.Last_Attribute loop
         declare
            YA : Gela.Grammars.Attribute_Declaration renames
              G.Declaration (K);
         begin
            if Done.Index (YA.Name) > 0 then
               goto Continue;
            else
               Done.Append (YA.Name);
            end if;

            for J in X.First_Attribute .. X.Last_Attribute loop
               declare
                  use type League.Strings.Universal_String;

                  XA : Gela.Grammars.Attribute_Declaration renames
                    G.Declaration (J);
                  Text : League.Strings.Universal_String;
               begin
                  if XA.Name = YA.Name then
                     Text.Append ("${");
                     Text.Append (X.Name);
                     Text.Append (".");
                     Text.Append (XA.Name);
                     Text.Append ("} = ${");
                     Text.Append (Y.Name);
                     Text.Append (".");
                     Text.Append (YA.Name);
                     Text.Append ("}");
                     V.Create_Rule (Y.Name, X.Name, Text);
                  end if;
               end;
            end loop;

            <<Continue>>
         end;
      end loop;
   end Copy_Attr;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (G           : Gela.Grammars.Grammar;
      Implement   : NT_Map;
      Is_Concrete : NT_List;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array;
      Pass        : in out Positive;
      Found       : out Boolean)
   is
      pragma Unreferenced (Implement, Partitions);
      use Gela.Grammars;
      use type Ordered.Partition_Count;
      use type League.Strings.Universal_String;

      procedure Generate_Proc
        (NT  : Non_Terminal;
         Pos : in out Gela.Grammars.Ordered.Order_Maps.Cursor);

      procedure Generate_List_Proc
        (NT  : Non_Terminal;
         Pos : in out Gela.Grammars.Ordered.Order_Maps.Cursor);

      procedure Add_With (Name : League.Strings.Universal_String);
      procedure Add_With_Stores (Name : League.Strings.Universal_String);
      procedure Add_With_Nodes (Name : League.Strings.Universal_String);
      function Is_List (NT : Non_Terminal) return Boolean;
      --  Detect if NT was a list before converting to plain AG

      Spec    : Writer;
      Impl    : Writer;
      Withs   : Writer;
      Withed  : League.String_Vectors.Universal_String_Vector;

      --------------
      -- Add_With --
      --------------

      procedure Add_With (Name : League.Strings.Universal_String) is
      begin
         if Withed.Index (Name) > 0 then
            return;
         end if;

         Withed.Append (Name);
         Withs.N ("with ");
         Withs.N (Name);
         Withs.P (";");
      end Add_With;

      ---------------------
      -- Add_With_Stores --
      ---------------------

      procedure Add_With_Stores (Name : League.Strings.Universal_String) is
      begin
         Add_With ("Gela.Stores." & Name);
      end Add_With_Stores;

      --------------------
      -- Add_With_Nodes --
      --------------------

      procedure Add_With_Nodes (Name : League.Strings.Universal_String) is
      begin
         Add_With ("Gela.Nodes." & Name);
--         Withs.N ("pragma Unreferenced (Gela.Nodes.");
--           Withs.N (Name);
--           Withs.P (");");
      end Add_With_Nodes;

      ------------------------
      -- Generate_List_Proc --
      ------------------------

      procedure Generate_List_Proc
        (NT  : Non_Terminal;
         Pos : in out Gela.Grammars.Ordered.Order_Maps.Cursor) is
      begin
         null;
      end Generate_List_Proc;
      -------------------
      -- Generate_Proc --
      -------------------

      procedure Generate_Proc
        (NT  : Non_Terminal;
         Pos : in out Gela.Grammars.Ordered.Order_Maps.Cursor)
      is
         procedure Generate_Local (X : Attribute);
         procedure Generate_Local (P : Part);
         procedure Generate_Get (X : Attribute);
         procedure Generate_Rule (R : Rule);
         procedure Generate_Set (X : Attribute);
         procedure Generate_Call (Item : Gela.Grammars.Ordered.Action);

         Prod : Production renames G.Production (NT.First);
         Part_Map : array (Prod.First .. Prod.Last) of Boolean :=
           (others => False);

         -------------------
         -- Generate_Call --
         -------------------

         procedure Generate_Call (Item : Gela.Grammars.Ordered.Action) is
            P : Part renames G.Part (Item.Part);
         begin
            Impl.N ("      ");
            Impl.N (To_Ada (P.Name));
            Impl.P (".its.Visit");
            Impl.N ("        (");
            Impl.N (To_Ada (P.Name));
            Impl.P (".Payload, Self);");
         end Generate_Call;

         ------------------
         -- Generate_Get --
         ------------------

         procedure Generate_Get (X : Attribute) is
            Origin : League.Strings.Universal_String := This;
            D      : Attribute_Declaration renames
              G.Declaration (X.Declaration);
         begin
            if not X.Is_Left_Hand_Side then
               Origin := To_Ada (G.Part (X.Origin).Name);
            end if;
            Impl.N ("      ");
            Impl.N (Origin);
            Impl.N ("_");
            Impl.N (To_Ada (D.Name));
            Impl.P (" :=");
            Impl.N ("        ");
            Impl.N (Origin);
            Impl.N (".its.");
            Impl.N (To_Ada (D.Name));
            Impl.N (" (");
            Impl.N (Origin);
            Impl.P (".Payload);");
         end Generate_Get;

         --------------------
         -- Generate_Local --
         --------------------

         procedure Generate_Local (P : Part) is
         begin
            if not Part_Map (P.Index) then
               Part_Map (P.Index) := True;

               Impl.N ("      ");
               Impl.N (To_Ada (P.Name));
               Impl.N (" : constant Gela.Nodes.");
               Impl.N (Return_Type (G, P));
               Impl.P (" :=");
               Impl.N ("        ");
               Impl.N (This);
               Impl.N (".");
               Impl.N (To_Ada (P.Name));
               Impl.P (" (Node.Payload);");
               Add_With_Nodes (Plural (Return_Type (G, P)));
            end if;
         end Generate_Local;

         --------------------
         -- Generate_Local --
         --------------------

         procedure Generate_Local (X : Attribute) is
            D : Attribute_Declaration renames G.Declaration (X.Declaration);
         begin
            if X.Is_Left_Hand_Side then
               Impl.N ("      ");
               Impl.N (This);
               Impl.N ("_");
               Impl.N (To_Ada (D.Name));
               Impl.N (" : ");
               Impl.N (To_Ada (D.Type_Name));
               Impl.P (";");
               Add_With (Package_Name (To_Ada (D.Type_Name)));
            else
               Generate_Local (G.Part (X.Origin));

               Impl.N ("      ");
               Impl.N (To_Ada (G.Part (X.Origin).Name));
               Impl.N ("_");
               Impl.N (To_Ada (D.Name));
               Impl.N (" : ");
               Impl.N (To_Ada (D.Type_Name));
               Impl.P (";");
               Add_With (Package_Name (To_Ada (D.Type_Name)));
            end if;
         end Generate_Local;

         procedure Generate_Rule (R : Rule) is
            T      : constant Rule_Templates.Rule_Template :=
              Rule_Templates.Create (R.Text);
            Value  : League.Strings.Universal_String;
            Values : League.String_Vectors.Universal_String_Vector;
         begin
            for J in 1 .. T.Count loop
               if T.Part_Name (J) = NT.Name then
                  Value := This;
               else
                  Value := T.Part_Name (J);
               end if;

               Value.Append ("_");
               Value.Append (T.Attribute_Name (J));
               Value := To_Ada (Value);
               Values.Append (Value);
            end loop;

            Impl.P (T.Substitute (Values));
         end Generate_Rule;

         ------------------
         -- Generate_Set --
         ------------------

         procedure Generate_Set (X : Attribute) is
            Origin : League.Strings.Universal_String := This;
            D      : Attribute_Declaration renames
              G.Declaration (X.Declaration);
         begin
            Impl.N ("      ");
            if X.Is_Left_Hand_Side then
               Impl.N ("This.Set_");
               Impl.P (To_Ada (D.Name));
               Impl.N ("        (Node");
            else
               Origin := To_Ada (G.Part (X.Origin).Name);
               Impl.N (Origin);
               Impl.N (".its.Set_");
               Impl.P (To_Ada (D.Name));
               Impl.N ("        (");
               Impl.N (Origin);
            end if;
            Impl.N (".Payload, ");
            Impl.N (Origin);
            Impl.N ("_");
            Impl.N (To_Ada (D.Name));
            Impl.P (");");
         end Generate_Set;

         use type Gela.Grammars.Ordered.Action_Kinds;
         use Gela.Grammars.Ordered.Order_Maps;

         Name : constant League.Strings.Universal_String := To_Ada (NT.Name);
         Unit : constant League.Strings.Universal_String := Plural (NT.Name);
         Each : Cursor;
      begin
         Spec.P ("", Impl);
         Spec.N ("   overriding procedure ", Impl);
         Spec.P (Name, Impl);
         Spec.P ("     (Self    : in out Visiter;", Impl);
         Spec.N ("      Node    : Gela.Nodes.", Impl);
         Spec.N (Name, Impl);
         Spec.N (")", Impl);
         Spec.N (";");
         Spec.P ("", Impl);
         Impl.P ("   is");

         Add_With_Stores (Unit);
         Impl.N ("      This : constant Gela.Stores.");
         Impl.P (Unit);
         Impl.P ("        .Object_Access :=");
         Impl.N ("        Gela.Stores.");
         Impl.N (Unit);
         Impl.P (".Object_Access");
         Impl.P ("          (Node.its);");

         Each := Pos;
         while Has_Element (Each) and then Key (Each).Prod = NT.First loop
            case Element (Each).Kind is
               when Gela.Grammars.Ordered.Evaluate_Rule =>
                  declare
                     R    : Rule renames G.Rule (Element (Each).Rule);
                     List : Attribute_Array renames
                       G.Attribute (R.First_Argument .. R.Last_Argument);
                  begin
                     for X of List loop
                        Generate_Local (X);
                     end loop;
                     Generate_Local (G.Attribute (R.Result));
                  end;

               when Gela.Grammars.Ordered.Descent =>
                  declare
                     P : Part renames G.Part (Element (Each).Part);
                  begin
                     Generate_Local (P);
                  end;
            end case;

            Next (Each);
         end loop;

         Impl.P ("   begin");

         Each := Pos;
         while Has_Element (Each) and then Key (Each).Prod = NT.First loop
            case Element (Each).Kind is
               when Gela.Grammars.Ordered.Evaluate_Rule =>
                  declare
                     R    : Rule renames G.Rule (Element (Each).Rule);
                     List : Attribute_Array renames
                       G.Attribute (R.First_Argument .. R.Last_Argument);
                  begin
                     for X of List loop
                        Generate_Get (X);
                     end loop;

                     Generate_Rule (R);

                     Generate_Set (G.Attribute (R.Result));
                  end;
               when Gela.Grammars.Ordered.Descent =>
                  Generate_Call (Element (Each));
            end case;

            Next (Each);
         end loop;

         Impl.P ("      null;");
         Impl.N ("   end ");
         Impl.N (Name);
         Impl.P (";");
      end Generate_Proc;

      -------------
      -- Is_List --
      -------------

      function Is_List (NT : Non_Terminal) return Boolean is
      begin
         return NT.First /= NT.Last;
      end Is_List;

   begin
      Found := False;
      Spec.P ("--  Auto generated file. DO NOT EDIT!!!", Withs);
      Withs.P ("pragma Warnings (""FU"");");
      Add_With (League.Strings.To_Universal_String
                ("Gela.Mutables.Compilations"));
      Spec.P ("with Gela.Mutables;");
      Spec.P ("with Gela.Nodes.Visiters;");
      Spec.P;
      Spec.N ("package Gela.Pass_");
      Impl.N ("package body Gela.Pass_");
      Spec.N (Natural (Pass));
      Impl.N (Natural (Pass));
      Spec.P (" is", Impl);
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
                  if Is_List (NT) then
                     Generate_Proc (NT, Pos);
                  else
                     Generate_List_Proc (NT, Pos);
                  end if;
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
         Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
         Ada.Text_IO.Put_Line (Withs.Text.To_UTF_8_String);
         Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
      end if;
   end Generate;

end AG_Tools.Check_Ordered;
