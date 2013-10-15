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
      Is_Option : in out Option_List;
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
      PL : in out Gela.Grammars.Constructors.Production_List;
      Is_Option : in out Option_List);

   procedure Generate
     (G           : Gela.Grammars.Grammar;
      Implement   : NT_Map;
      Is_Concrete : NT_List;
      Is_Option   : Option_List;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array;
      Pass        : in out Positive;
      Found       : out Boolean);

   function List_Item
     (G : Gela.Grammars.Grammar;
      NT : Gela.Grammars.Non_Terminal)
      return Gela.Grammars.Non_Terminal_Index;

   This : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("This");

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option
     (Self : in out Option_List;
      G    : Gela.Grammars.Grammar;
      Prod : Gela.Grammars.Production;
      Part : Gela.Grammars.Part)
   is
      use type League.Strings.Universal_String;
      NT   : Gela.Grammars.Non_Terminal renames G.Non_Terminal (Prod.Parent);
      Name : constant League.Strings.Universal_String :=
        NT.Name & " " & Prod.Name & " " & Part.Name;
   begin
      Self.Set.Insert (Name);
   end Add_Option;

   -----------
   -- Check --
   -----------

   procedure Check
     (G           : Gela.Grammars.Grammar;
      Implement   : NT_Map;
      Is_Concrete : NT_List;
      Is_Option : Option_List)
   is
      Ok     : Boolean;
      Pass   : Positive := 1;
      Result : constant Gela.Grammars.Grammar :=
        Gela.Grammars_Convertors.Convert_With_Empty (G);
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
           (Result, Implement,
            Is_Concrete, Is_Option,
            Order, Partitions, Pass, Ok);
      end loop;
   end Check;

   ----------------------
   -- Copy_Productions --
   ----------------------

   procedure Copy_Productions
     (G : Gela.Grammars.Grammar;
      V : in out Gela.Grammars.Constructors.Constructor;
      From : Gela.Grammars.Production_Index;
      To   : Gela.Grammars.Production_Count;
      PL : in out Gela.Grammars.Constructors.Production_List;
      Is_Option : in out Option_List)
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
                     --  remove Option from grammar by replacing its
                     --  nested items
                     declare
                        Nested_Production : Gela.Grammars.Production renames
                          G.Production (R.First);
                        Nested_Part : Gela.Grammars.Part renames
                          G.Part (Nested_Production.First);
                     begin
                        Is_Option.Add_Option
                          (G, G.Production (R.Parent), Nested_Part);

                        if Nested_Part.Is_Terminal_Reference then
                           P.Add (V.Create_Terminal_Reference
                                  (Nested_Part.Name,
                                     G.Terminal (Nested_Part.Denote).Image));
                        elsif Nested_Part.Is_List_Reference then
                           P.Add (V.Create_List_Reference
                                  (Nested_Part.Name,
                                     G.Non_Terminal
                                       (Nested_Part.Denote).Name));
                        elsif Nested_Part.Is_Non_Terminal_Reference then
                           P.Add (V.Create_Non_Terminal_Reference
                                  (Nested_Part.Name,
                                     G.Non_Terminal
                                       (Nested_Part.Denote).Name));
                        else
                           raise Constraint_Error with "option in option";
                        end if;
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
      Is_Option : in out Option_List;
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
            Copy_Productions (G, V, N.First, N.Last, PL, Is_Option);

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
      Is_Option   : Option_List;
      Order       : Gela.Grammars.Ordered.Order_Maps.Map;
      Partitions  : Gela.Grammars.Ordered.Partition_Array;
      Pass        : in out Positive;
      Found       : out Boolean)
   is
      pragma Unreferenced (Implement);
      use Gela.Grammars;
      use type Ordered.Partition_Count;
      use type League.Strings.Universal_String;

      type Unit_Kinds is (Spec_Unit, Body_Unit);

      procedure Generate_Proc
        (NT  : Non_Terminal;
         Pos : in out Gela.Grammars.Ordered.Order_Maps.Cursor);

      procedure Generate_List_Proc
        (NT  : Non_Terminal;
         Pos : in out Gela.Grammars.Ordered.Order_Maps.Cursor);

      procedure Add_With
        (Name : League.Strings.Universal_String;
         Kind : Unit_Kinds := Body_Unit);
      procedure Add_With_Stores (Name : League.Strings.Universal_String);
      procedure Add_With_Nodes (Name : League.Strings.Universal_String);
      procedure Print_Withes (Kind : Unit_Kinds);
      pragma Unreferenced (Add_With_Nodes);
      function Is_List (NT : Non_Terminal) return Boolean;
      --  Detect if NT was a list before converting to plain AG
      procedure Generate_Local (X : Attribute; Is_List : Boolean);
      procedure Generate_Local (P : Part; Is_List : Boolean);
      procedure Generate_Local
        (Origin  : League.Strings.Universal_String;
         D       : Attribute_Declaration);
      procedure Generate_Get (X : Attribute; Impl : in out Writer);

      procedure Generate_Rule
        (R    : Rule;
         NT   : Non_Terminal;
         Impl : in out Writer);

      procedure Generate_Set (X : Attribute; Impl : in out Writer);

      procedure Generate_Call
        (Item : Gela.Grammars.Ordered.Action;
         Impl : in out Writer;
         Code : in out Writer);

      function Return_Type
        (Part : Gela.Grammars.Part) return League.Strings.Universal_String;

      type With_Records is array (Unit_Kinds) of
        League.String_Vectors.Universal_String_Vector;

      Spec  : Writer;
      Impl  : Writer;
      Withs : With_Records;

      Part_Map : array (G.Part'Range) of Boolean := (others => False);

      --------------
      -- Add_With --
      --------------

      procedure Add_With
        (Name : League.Strings.Universal_String;
         Kind : Unit_Kinds := Body_Unit)
      is
      begin
         if Name.Is_Empty or Withs (Kind).Index (Name) > 0 then
            return;
         elsif Kind = Spec_Unit then
            declare
               Body_Index : constant Natural := Withs (Body_Unit).Index (Name);
            begin
               if Body_Index > 0 then
                  Withs (Body_Unit).Replace
                    (Body_Index, League.Strings.Empty_Universal_String);
               end if;
            end;
         elsif Withs (Spec_Unit).Index (Name) > 0 then
            return;
         end if;

         Withs (Kind).Append (Name);
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
      end Add_With_Nodes;

      -------------------
      -- Generate_Call --
      -------------------

      procedure Generate_Call
        (Item : Gela.Grammars.Ordered.Action;
         Impl : in out Writer;
         Code : in out Writer)
      is
         pragma Unreferenced (Impl);
         P         : Part renames G.Part (Item.Part);
         NT        : Non_Terminal renames G.Non_Terminal (P.Denote);
         Next_Pass : constant Positive := Item.Pass;
         Parts     : Gela.Grammars.Ordered.Partition_Array renames
           Partitions (NT.First_Attribute .. NT.Last_Attribute);
      begin
         Code.N ("      ");
         if Is_List (NT) then
            Code.N ("Self.");
            Code.N (To_Ada (P.Name));
            Code.N (" (");
            Code.N (To_Ada (P.Name));

            for J in NT.First_Attribute .. NT.Last_Attribute loop
               if Gela.Grammars.Ordered.To_Pass (Parts, J) = Next_Pass then
                  Generate_Local (P.Name, G.Declaration (J));

                  Code.N (", ");
                  Code.N (To_Ada (P.Name));
                  Code.N ("_");
                  Code.N (To_Ada (G.Declaration (J).Name));
               end if;
            end loop;

            Code.P (");");
         else
            if Is_Option.Is_Option (G, P) then
               Code.N ("if ");
               Code.N (To_Ada (P.Name));
               Code.P (".its /= null then");
               Code.N ("         ");
               Code.N (To_Ada (P.Name));
               Code.P (".its.Visit");
               Code.N ("           (");
               Code.N (To_Ada (P.Name));
               Code.P (".Payload, Self);");
               Code.P ("      end if;");
            else
               Code.N (To_Ada (P.Name));
               Code.P (".its.Visit");
               Code.N ("        (");
               Code.N (To_Ada (P.Name));
               Code.P (".Payload, Self);");
            end if;
         end if;
      end Generate_Call;

      ------------------
      -- Generate_Get --
      ------------------

      procedure Generate_Get (X : Attribute; Impl : in out Writer) is
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

      procedure Generate_Rule
        (R   : Rule;
         NT   : Non_Terminal;
         Impl : in out Writer)
      is
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

      procedure Generate_Set (X : Attribute; Impl : in out Writer) is
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

      ------------------------
      -- Generate_List_Proc --
      ------------------------

      procedure Generate_List_Proc
        (NT  : Non_Terminal;
         Pos : in out Gela.Grammars.Ordered.Order_Maps.Cursor)
      is
         use type Gela.Grammars.Ordered.Action_Kinds;
         use Gela.Grammars.Ordered.Order_Maps;

         Name : constant League.Strings.Universal_String := To_Ada (NT.Name);

         Parts   : Gela.Grammars.Ordered.Partition_Array renames
           Partitions (NT.First_Attribute .. NT.Last_Attribute);
         Prod    : Gela.Grammars.Production renames G.Production (NT.First);
         List    : Gela.Grammars.Part renames G.Part (Prod.First);
         Item_NT : Gela.Grammars.Non_Terminal renames G.Non_Terminal
           (List_Item (G, NT));
         Item : constant League.Strings.Universal_String :=
           To_Ada (Item_NT.Name);
         Unit : constant League.Strings.Universal_String :=
           Plural (Item & "_Sequence");
         Each : Cursor;
         Code : Writer;
      begin
         Spec.P ("", Impl);
         Spec.N ("   not overriding procedure ", Impl);
         Spec.P (Name, Impl);
         Spec.P ("     (Self    : in out Visiter;", Impl);
         Spec.N ("      Node    : Gela.Nodes.", Impl);
         Spec.N (Return_Type (List), Impl);

         for J in NT.First_Attribute .. NT.Last_Attribute loop
            if Gela.Grammars.Ordered.To_Pass (Parts, J) = Pass then
               Spec.P (";", Impl);
               Spec.N ("      ", Impl);
               Spec.N (This, Impl);
               Spec.N ("_", Impl);
               Spec.N (To_Ada (G.Declaration (J).Name), Impl);
               Spec.N (" : ", Impl);

               if not G.Declaration (J).Is_Inherited then
                  Spec.N ("out ", Impl);
               end if;

               Spec.N (To_Ada (G.Declaration (J).Type_Name), Impl);
               Add_With
                 (Package_Name (To_Ada (G.Declaration (J).Type_Name)),
                  Spec_Unit);
            end if;
         end loop;

         Spec.N (")", Impl);
         Spec.N (";");
         Spec.P ("", Impl);
         Impl.P ("   is");

         Add_With_Stores (Unit);
--           Impl.N ("      use type Gela.Nodes.");
--           Impl.N (Item);
--           Impl.P ("_Access;");

         Impl.N ("      This : constant Gela.Stores.");
         Impl.P (Unit);
         Impl.P ("        .List_Access :=");
         Impl.N ("        Gela.Stores.");
         Impl.N (Unit);
         Impl.P (".List_Access");
         Impl.P ("          (Node.its);");

         Code.N ("      while ");
         Code.N (Item);
         Code.P (".its /= null loop");

         Each := Pos;
         while Has_Element (Each) and then Key (Each).Prod = NT.First loop
            case Element (Each).Kind is
               when Gela.Grammars.Ordered.Evaluate_Rule =>
                  declare
                     R    : Rule renames G.Rule (Element (Each).Rule);
                     List : Attribute_Array renames
                       G.Attribute (R.First_Argument .. R.Last_Argument);
                     Result : Attribute renames G.Attribute (R.Result);
                  begin
                     for X of List loop
                        if not X.Is_Left_Hand_Side then
                           Generate_Local (X, Is_List => True);
                           Generate_Get (X, Code);
                        end if;
                     end loop;

                     Generate_Rule (R, NT, Code);

                     if not Result.Is_Left_Hand_Side then
                        Generate_Local (Result, Is_List => True);
                        Generate_Set (G.Attribute (R.Result), Code);
                     end if;
                  end;

               when Gela.Grammars.Ordered.Descent =>
                  declare
                     P : Part renames G.Part (Element (Each).Part);
                  begin
                     if P.Index /= List.Index then
                        Generate_Local (P, Is_List => True);
                        Generate_Call (Element (Each), Impl, Code);
                     end if;
                  end;
            end case;

            Next (Each);
         end loop;

         Code.P ("         null;");
         Code.N ("         This.Next (Node.Payload, ");
         Code.N (Item);
         Code.P (");");
         Code.P ("      end loop;");
         Impl.P ("   begin");

         while Has_Element (Each) and then Key (Each).Prod = NT.Last loop
            case Element (Each).Kind is
               when Gela.Grammars.Ordered.Evaluate_Rule =>
                  declare
                     R    : Rule renames G.Rule (Element (Each).Rule);
                  begin
                     Generate_Rule (R, NT, Impl);
                  end;

               when Gela.Grammars.Ordered.Descent =>
                  raise Program_Error;
            end case;

            Next (Each);
         end loop;

         Impl.N (Code.Text);
         Impl.N ("   end ");
         Impl.N (Name);
         Impl.P (";");
      end Generate_List_Proc;

      --------------------
      -- Generate_Local --
      --------------------

      procedure Generate_Local (P : Part; Is_List : Boolean) is
      begin
         if Part_Map (P.Index) then
            return;
         end if;

         Part_Map (P.Index) := True;
         Impl.N ("      ");
         Impl.N (To_Ada (P.Name));

         if Is_List then
            Impl.N (" : Gela.Nodes.");
            Impl.N (Return_Type (P));
            Impl.P (" :=");
            Impl.P ("        This.Head (Node.Payload);");
         else
            Impl.N (" : constant Gela.Nodes.");
            Impl.N (Return_Type (P));
            Impl.P (" :=");
            Impl.N ("        ");
            Impl.N (This);
            Impl.N (".");
            Impl.N (To_Ada (P.Name));
            Impl.P (" (Node.Payload);");
         end if;
      end Generate_Local;

      --------------------
      -- Generate_Local --
      --------------------

      procedure Generate_Local (X : Attribute; Is_List : Boolean) is
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
            Generate_Local (G.Part (X.Origin), Is_List);
            Generate_Local (G.Part (X.Origin).Name, D);
         end if;
      end Generate_Local;

      procedure Generate_Local
        (Origin  : League.Strings.Universal_String;
         D       : Attribute_Declaration) is
      begin
         Impl.N ("      ");
         Impl.N (To_Ada (Origin));
         Impl.N ("_");
         Impl.N (To_Ada (D.Name));
         Impl.N (" : ");
         Impl.N (To_Ada (D.Type_Name));
         Impl.P (";");
         Add_With (Package_Name (To_Ada (D.Type_Name)));
      end Generate_Local;

      -------------------
      -- Generate_Proc --
      -------------------

      procedure Generate_Proc
        (NT  : Non_Terminal;
         Pos : in out Gela.Grammars.Ordered.Order_Maps.Cursor)
      is
         Prod : Production renames G.Production (NT.First);

         use type Gela.Grammars.Ordered.Action_Kinds;
         use Gela.Grammars.Ordered.Order_Maps;

         Name : constant League.Strings.Universal_String := To_Ada (NT.Name);
         Unit : constant League.Strings.Universal_String := Plural (NT.Name);
         Each : Cursor;
         Code : Writer;
      begin
         Part_Map (Prod.First .. Prod.Last) := (others => False);

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
                        Generate_Local (X, Is_List => False);
                        Generate_Get (X, Code);
                     end loop;

                     Generate_Rule (R, NT, Code);

                     Generate_Local (G.Attribute (R.Result), Is_List => False);
                     Generate_Set (G.Attribute (R.Result), Code);
                  end;
               when Gela.Grammars.Ordered.Descent =>
                  declare
                     P : Part renames G.Part (Element (Each).Part);
                  begin
                     Generate_Local (P, Is_List => False);
                     Generate_Call (Element (Each), Impl, Code);
                  end;
            end case;

            Next (Each);
         end loop;

         Impl.P ("   begin");
         Impl.P (Code.Text);
         Impl.N ("   end ");
         Impl.N (Name);
         Impl.P (";");
      end Generate_Proc;

      -------------
      -- Is_List --
      -------------

      function Is_List (NT : Non_Terminal) return Boolean is
         Prod : Production renames G.Production (NT.Last);
      begin
         return Prod.First > Prod.Last;
      end Is_List;

      ------------------
      -- Print_Withes --
      ------------------

      procedure Print_Withes (Kind : Unit_Kinds) is
      begin
         Ada.Text_IO.Put_Line ("--  Auto generated file. DO NOT EDIT!!!");
         for J in 1 .. Withs (Kind).Length loop
            declare
               Item : constant League.Strings.Universal_String :=
                 Withs (Kind).Element (J);
            begin
               if not Item.Is_Empty then
                  Ada.Text_IO.Put ("with ");
                  Ada.Text_IO.Put (Item.To_UTF_8_String);
                  Ada.Text_IO.Put_Line (";");
               end if;
            end;
         end loop;
      end Print_Withes;

      -----------------
      -- Return_Type --
      -----------------

      function Return_Type
        (Part : Gela.Grammars.Part) return League.Strings.Universal_String
      is
         Result : League.Strings.Universal_String;
      begin
         if Part.Is_Terminal_Reference then
            Result.Append ("Token");
         else
            declare
               NT   : Non_Terminal renames G.Non_Terminal (Part.Denote);
               Prod : Gela.Grammars.Production renames G.Production (NT.First);
            begin
               if Is_List (NT) then
                  Result := Return_Type (G, G.Part (Prod.Last));
                  Result.Append ("_Sequence");
               else
                  Result := To_Ada (NT.Name);
               end if;
            end;
         end if;

         return Result;
      end Return_Type;

   begin
      Found := False;
      Impl.P ("pragma Warnings (""FUM"");");
      Impl.P ("pragma Style_Checks (""N"");");

      Add_With
        (League.Strings.To_Universal_String ("Gela.Mutables.Compilations"));
      Add_With
        (League.Strings.To_Universal_String ("Gela.Symbol_Sets"));
      Add_With
        (League.Strings.To_Universal_String ("Gela.Pass_Utils"));

      Spec.P ("with Gela.Mutables;");
      Spec.P ("with Gela.Nodes.Visiters;");
      Spec.P;
      Spec.N ("package Gela.Pass_");
      Impl.N ("package body Gela.Pass_");
      Spec.N (Natural (Pass));
      Impl.N (Natural (Pass));
      Spec.P (" is", Impl);
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
                  if Is_List (NT) then
                     Generate_List_Proc (NT, Pos);
                  else
                     Generate_Proc (NT, Pos);
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
         Print_Withes (Spec_Unit);
         Ada.Text_IO.Put_Line (Spec.Text.To_UTF_8_String);
         Print_Withes (Body_Unit);
         Ada.Text_IO.Put_Line (Impl.Text.To_UTF_8_String);
      end if;
   end Generate;

   ---------------
   -- Is_Option --
   ---------------

   function Is_Option
     (Self : Option_List;
      G    : Gela.Grammars.Grammar;
      Part : Gela.Grammars.Part) return Boolean
   is
      use type League.Strings.Universal_String;
      Prod : Gela.Grammars.Production renames G.Production (Part.Parent);
      NT   : Gela.Grammars.Non_Terminal renames G.Non_Terminal (Prod.Parent);
      Name : constant League.Strings.Universal_String :=
        NT.Name & " " & Prod.Name & " " & Part.Name;
   begin
      return Self.Set.Contains (Name);
   end Is_Option;

   ---------------
   -- List_Item --
   ---------------

   function List_Item
     (G : Gela.Grammars.Grammar;
      NT : Gela.Grammars.Non_Terminal)
      return Gela.Grammars.Non_Terminal_Index
   is
      Prod : Gela.Grammars.Production renames G.Production (NT.First);
      Part : Gela.Grammars.Part renames G.Part (Prod.Last);
   begin
      return Part.Denote;
   end List_Item;

   -----------------
   -- Pre_Process --
   -----------------

   function Pre_Process
     (G : Gela.Grammars.Grammar;
      Implement : NT_Map;
      Is_Concrete : NT_List;
      Is_Option : in out Option_List) return Gela.Grammars.Grammar_Access
   is
      V : Gela.Grammars.Constructors.Constructor;
   begin
      Copy (G, V, Is_Option, Is_Concrete);

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

      return new Gela.Grammars.Grammar'(V.Complete);
   end Pre_Process;

end AG_Tools.Check_Ordered;
