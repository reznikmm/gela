------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.String_Vectors;

with AG_Tools.Input;
with AG_Tools.Writers;

package body AG_Tools.NT_Generators is
   use type League.Strings.Universal_String;

   procedure Write_Declaration
     (Context : AG_Tools.Contexts.Context_Access;
      NT      : Gela.Grammars.Non_Terminal;
      Pass    : Positive);

   procedure Generate_Rule
     (T    : Gela.Grammars.Rule_Templates.Rule_Template;
      NT   : Gela.Grammars.Non_Terminal;
      Impl : in out AG_Tools.Writers.Writer);
   --  Output text of rule to Impl

   procedure Write_Rules
     (Context : AG_Tools.Contexts.Context_Access;
      NT      : Gela.Grammars.Non_Terminal;
      Prod    : Gela.Grammars.Production_Index;
      Pass    : Positive;
      Each    : in out Gela.Grammars.Ordered.Order_Maps.Cursor);

   This : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("This");

   procedure Generate_Rule
     (T    : Gela.Grammars.Rule_Templates.Rule_Template;
      NT   : Gela.Grammars.Non_Terminal;
      Impl : in out AG_Tools.Writers.Writer)
   is
      use Gela.Grammars;

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

   --------------
   -- Make_Get --
   --------------

   overriding procedure Make_Get
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute;
      Template  : Gela.Grammars.Rule_Templates.Rule_Template)
   is
      pragma Unreferenced (Template);
      G      : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      D      : Gela.Grammars.Attribute_Declaration renames
        G.Declaration (Attribute.Declaration);
   begin
      Code.N ("      This_");
      Code.N (To_Ada (D.Name));
      Code.N (":= This.");
      Code.N (To_Ada (D.Name));
      Code.P (";");

      Self.Make_Local_Variable (This, D);
   end Make_Get;

   --------------
   -- Make_Get --
   --------------

   overriding procedure Make_Get
     (Self      : access List_Generator;
      Attribute : Gela.Grammars.Attribute;
      Template  : Gela.Grammars.Rule_Templates.Rule_Template)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Attribute);
   begin
      null;
   end Make_Get;

   -------------------------
   -- Make_Local_Variable --
   -------------------------

   overriding procedure Make_Local_Variable
     (Self      : access Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Gela.Grammars.Attribute_Declaration)
   is
      Impl : AG_Tools.Writers.Writer renames Self.Context.Impl;
      Attr : constant AG_Tools.Contexts.Attr := (Origin, Attribute.Index);
   begin
      if Self.Context.Attr_Map.Contains (Attr) then
         return;
      else
         Self.Context.Attr_Map.Insert (Attr);
      end if;

      Impl.N ("      ");
      Impl.N (To_Ada (Origin));
      Impl.N ("_");
      Impl.N (To_Ada (Attribute.Name));
      Impl.N (" : ");
      Impl.N (To_Ada (Attribute.Type_Name));
      Impl.P (";");
      Self.Context.Add_With (Package_Name (To_Ada (Attribute.Type_Name)));
   end Make_Local_Variable;

   -------------------------
   -- Make_Local_Variable --
   -------------------------

   overriding procedure Make_Local_Variable
     (Self      : access List_Generator;
      Origin    : League.Strings.Universal_String;
      Attribute : Gela.Grammars.Attribute_Declaration)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Attribute);
   begin
      null;
   end Make_Local_Variable;

   --------------------
   -- Make_Procedure --
   --------------------

   overriding procedure Make_Procedure
     (Self  : access Generator;
      Order : Gela.Grammars.Ordered.Order_Maps.Map;
      NT    : Gela.Grammars.Non_Terminal;
      Pass  : Positive)
   is
      Pos : Gela.Grammars.Ordered.Order_Maps.Cursor :=
        Order.Ceiling ((NT.First, Pass, Step => 1));

      Spec    : AG_Tools.Writers.Writer renames Self.Context.Spec;
      Impl    : AG_Tools.Writers.Writer renames Self.Context.Impl;
      Code    : AG_Tools.Writers.Writer renames Self.Context.Code;
      Name    : constant League.Strings.Universal_String := To_Ada (NT.Name);
   begin
      Code.Clear;
      Self.Context.Attr_Map.Clear;

      Spec.P ("", Impl);
      Spec.N ("   ", Impl);
      Write_Declaration (Self.Context, NT, Pass);

      Spec.N (")", Impl);
      Spec.N (";");
      Spec.P ("", Impl);
      Impl.P ("   is");

      Impl.N ("      This : ");
      Impl.N (Return_Type (Self.Context.Grammar.all, NT));
      Impl.P ("_Access renames Node;");

      Write_Rules (Self.Context, NT, NT.First, Pass, Pos);

      Impl.P ("   begin");
      Impl.N (Code);  --  <--- Make .N
      Impl.N ("   end ");
      Impl.N (Name);
      Impl.N ("_");
      Impl.N (Pass);
      Impl.P (";");
   end Make_Procedure;

   --------------------
   -- Make_Procedure --
   --------------------

   overriding procedure Make_Procedure
     (Self  : access List_Generator;
      Order : Gela.Grammars.Ordered.Order_Maps.Map;
      NT    : Gela.Grammars.Non_Terminal;
      Pass  : Positive)
   is
      Pos : Gela.Grammars.Ordered.Order_Maps.Cursor :=
        Order.Ceiling ((NT.First, Pass, Step => 1));

      Prod : Gela.Grammars.Production renames
        Self.Context.Grammar.Production (NT.First);

      G  : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      RT : constant League.Strings.Universal_String := Return_Type (G, NT);
      Parts   : Gela.Grammars.Ordered.Partition_Array renames
        Self.Context.Partition (NT.First_Attribute .. NT.Last_Attribute);
      Spec    : AG_Tools.Writers.Writer renames Self.Context.Spec;
      Impl    : AG_Tools.Writers.Writer renames Self.Context.Impl;
      Code    : AG_Tools.Writers.Writer renames Self.Context.Code;
      Piece   : AG_Tools.Writers.Writer;
      Item_NT : Gela.Grammars.Non_Terminal renames G.Non_Terminal
        (List_Item (G, NT));
      Item : constant League.Strings.Universal_String :=
        To_Ada (Item_NT.Name);
   begin
      Code.Clear;
      Self.Context.Attr_Map.Clear;

      Spec.P ("", Impl);
      Impl.N ("   --  ");
      Impl.N (Pass);
      Impl.P;
      Spec.N ("   ", Impl);
      Write_Declaration (Self.Context, NT, Pass);

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
            Self.Context.Add_With
              (Package_Name (To_Ada (G.Declaration (J).Type_Name)),
               AG_Tools.Contexts.Spec_Unit);
         end if;
      end loop;

      Spec.N (")", Impl);
      Spec.N (";");
      Spec.P ("", Impl);
      Impl.P ("   is");

      Impl.N ("      This : ");
      Impl.N (RT);
      Impl.P ("_Cursor :=");
      Impl.P ("        Node.First;");

      Self.Context.Factory.Get (G.Part (Prod.Last)).
         Make_Local_Variable (Prod.Last);

      Code.P ("      while This.Has_Element loop");
      Code.N ("      ");
      Code.N (Item);
      Code.P (" := This.Element;");

      Write_Rules (Self.Context, NT, NT.First, Pass, Pos);

      Code.P ("         This.Next;");
      Code.P ("      end loop;");
      Piece := Code;
      Code.Clear;
      Pos := Order.Ceiling ((NT.Last, Pass, Step => 1));
      Write_Rules (Self.Context, NT, NT.Last, Pass, Pos);
      Impl.P ("   begin");
      Impl.N (Code);

      Impl.P ("      if not This.Has_Element then");
      Impl.P ("         return;");
      Impl.P ("      end if;");

      Impl.N (Piece);
      Impl.N ("   end ");
      Impl.N (To_Ada (NT.Name));
      Impl.N ("_");
      Impl.N (Pass);
      Impl.P (";");
   end Make_Procedure;

   --------------------
   -- Make_Procedure --
   --------------------

   overriding procedure Make_Procedure
     (Self  : access Abstract_Generator;
      Order : Gela.Grammars.Ordered.Order_Maps.Map;
      NT    : Gela.Grammars.Non_Terminal;
      Pass  : Positive)
   is
      G : Gela.Grammars.Grammar renames Self.Context.Grammar.all;

      type Pass_Range is record
         From, To : Natural;
      end record;

      procedure Add (Item : in out Pass_Range; Pass : Positive);

      type Pass_Array is array (G.Non_Terminal'Range) of Pass_Range;

      procedure Find
        (NT     : Gela.Grammars.Non_Terminal;
         Pass   : Positive;
         Result : in out Pass_Array);

      ---------
      -- Add --
      ---------

      procedure Add (Item : in out Pass_Range; Pass : Positive) is
      begin
         if Item.From = 0 then
            Item.From := Pass;
         elsif Pass in Item.From .. Item.To then
            --  Already processed in prev production like:
            --  defining_name := defining_identifier | defining_unit_name;
            --  defining_unit_name := defining_identifier;
            return;
         elsif Item.To + 1 /= Pass then
            raise Constraint_Error;
         end if;

         Item.To := Pass;
      end Add;

      procedure Find
        (NT     : Gela.Grammars.Non_Terminal;
         Pass   : Positive;
         Result : in out Pass_Array) is
      begin
         for J in NT.First .. NT.Last loop
            declare
               use Gela.Grammars.Ordered.Order_Maps;
               use type Gela.Grammars.Production_Index;
               Pos   : Cursor := Order.Ceiling ((J, Pass, Step => 1));
               Value : Gela.Grammars.Ordered.Action;
               Go    : Gela.Grammars.Non_Terminal_Index;
            begin
               while Has_Element (Pos) and then
                 Key (Pos).Prod = J and then
                 Key (Pos).Pass = Pass
               loop
                  Value := Element (Pos);

                  case Value.Kind is
                     when Gela.Grammars.Ordered.Descent =>
                        Go := G.Part (Value.Part).Denote;

                        if Input.Is_Concrete (Go) then
                           Add (Result (Go), Value.Pass);
                        else
                           Find (G.Non_Terminal (Go), Value.Pass, Result);
                        end if;
                     when Gela.Grammars.Ordered.Evaluate_Rule =>
                        null;
                  end case;

                  Next (Pos);
               end loop;
            end;

         end loop;
      end Find;

      Found   : Pass_Array := (others => (0, 0));
      Spec    : AG_Tools.Writers.Writer renames Self.Context.Spec;
      Impl    : AG_Tools.Writers.Writer renames Self.Context.Impl;
   begin
      Find (NT, Pass, Found);
      Spec.P ("", Impl);
      Spec.N ("   ", Impl);
      Write_Declaration (Self.Context, NT, Pass);
      Spec.N (")", Impl);
      Spec.N (";");
      Spec.P ("", Impl);
      Impl.P ("   is");
      Impl.P ("      type Visiter is new Gela.Element_Visiters.Visiter" &
                " with null record;");

      for X in Found'Range loop
         if Found (X).From /= 0 then
            Impl.N ("      overriding procedure ");
            Impl.P (To_Ada (G.Non_Terminal (X).Name));
            Impl.P ("        (This    : in out Visiter;");
            Impl.N ("         Node    : not null Gela.Elements.");
            Impl.N (Plural (G.Non_Terminal (X).Name));
            Impl.N (".");
            Impl.N (To_Ada (G.Non_Terminal (X).Name));
            Impl.P ("_Access);");
            Impl.P;
         end if;
      end loop;

      for X in Found'Range loop
         if Found (X).From /= 0 then
            Impl.N ("      overriding procedure ");
            Impl.P (To_Ada (G.Non_Terminal (X).Name));
            Impl.P ("        (This    : in out Visiter;");
            Impl.N ("         Node    : not null Gela.Elements.");
            Impl.N (Plural (G.Non_Terminal (X).Name));
            Impl.N (".");
            Impl.N (To_Ada (G.Non_Terminal (X).Name));
            Impl.P ("_Access) is");
            Impl.P ("      begin");

            for J in Found (X).From .. Found (X).To loop
               Impl.N ("         ");
               Impl.N (To_Ada (G.Non_Terminal (X).Name));
               Impl.N ("_");
               Impl.N (J);
               Impl.P (" (Self, Node);");
            end loop;

            Impl.N ("      end ");
            Impl.N (To_Ada (G.Non_Terminal (X).Name));
            Impl.P (";");
            Impl.P;
         end if;
      end loop;

      Impl.P ("      V : Visiter;");
      Impl.P ("   begin");
      Impl.P ("      Node.Visit (V);");
      Impl.N ("   end ");
      Impl.N (To_Ada (NT.Name));
      Impl.N ("_");
      Impl.N (Pass);
      Impl.P (";");
   end Make_Procedure;

   --------------
   -- Make_Set --
   --------------

   overriding procedure Make_Set
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute)
   is
      G      : Gela.Grammars.Grammar renames Self.Context.Grammar.all;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      D      : Gela.Grammars.Attribute_Declaration renames
        G.Declaration (Attribute.Declaration);
   begin
      Code.N ("      ");

      Code.N ("This.Set_");
      Code.N (To_Ada (D.Name));
      Code.N ("        (This_");
      Code.N (To_Ada (D.Name));
      Code.P (");");
      Self.Make_Local_Variable (This, D);
   end Make_Set;

   --------------
   -- Make_Set --
   --------------

   overriding procedure Make_Set
     (Self      : access List_Generator;
      Attribute : Gela.Grammars.Attribute)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Attribute);
   begin
      null;
   end Make_Set;

   -----------------------
   -- Write_Declaration --
   -----------------------

   procedure Write_Declaration
     (Context : AG_Tools.Contexts.Context_Access;
      NT      : Gela.Grammars.Non_Terminal;
      Pass    : Positive)
   is
      Spec : AG_Tools.Writers.Writer renames Context.Spec;
      Impl : AG_Tools.Writers.Writer renames Context.Impl;
      RT   : constant League.Strings.Universal_String :=
        Return_Type (Context.Grammar.all, NT);
   begin
      Context.Add_With
        (Name => Package_Name (RT),
         Kind => AG_Tools.Contexts.Spec_Unit);

      Spec.N ("not overriding procedure ", Impl);
      Spec.N (To_Ada (NT.Name), Impl);
      Spec.N ("_", Impl);
      Spec.N (Pass);
      Impl.N (Pass);
      Spec.P ("", Impl);
      Spec.P ("     (Self    : in out Visiter;", Impl);
      Spec.N ("      Node    : not null ", Impl);
      Spec.N (RT, Impl);
      Spec.N ("_Access", Impl);
   end Write_Declaration;

   -----------------
   -- Write_Rules --
   -----------------

   procedure Write_Rules
     (Context : AG_Tools.Contexts.Context_Access;
      NT      : Gela.Grammars.Non_Terminal;
      Prod    : Gela.Grammars.Production_Index;
      Pass    : Positive;
      Each    : in out Gela.Grammars.Ordered.Order_Maps.Cursor)
   is
      use Gela.Grammars;
      use Gela.Grammars.Ordered.Order_Maps;
      G       : Gela.Grammars.Grammar renames Context.Grammar.all;
   begin
      while Has_Element (Each)
        and then Key (Each).Prod = Prod
        and then Key (Each).Pass = Pass
      loop

         case Element (Each).Kind is
            when Gela.Grammars.Ordered.Evaluate_Rule =>
               declare
                  Gen    : AG_Tools.Visit_Generators.Generator_Access;
                  R      : Rule renames G.Rule (Element (Each).Rule);
                  Result : Attribute renames G.Attribute (R.Result);
                  List   : Attribute_Array renames
                    G.Attribute (R.First_Argument .. R.Last_Argument);
                  T      : constant Rule_Templates.Rule_Template :=
                    Rule_Templates.Create (R.Text);
               begin
                  for X of List loop
                     Gen := Context.Factory.Get (X, NT);
                     Gen.Make_Get (X, T);
                  end loop;

                  Generate_Rule (T, NT, Context.Code);

                  Gen := Context.Factory.Get (Result, NT);
                  Gen.Make_Set (Result);
               end;
            when Gela.Grammars.Ordered.Descent =>
               declare
                  Gen : AG_Tools.Visit_Generators.Part_Generator_Access;
                  P   : Part renames G.Part (Element (Each).Part);
               begin
                  Gen := Context.Factory.Get (P);
                  Gen.Make_Descent (P.Index, Element (Each).Pass);
               end;
         end case;

         Next (Each);
      end loop;
   end Write_Rules;

end AG_Tools.NT_Generators;