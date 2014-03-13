------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.String_Vectors;

with Gela.Grammars.Rule_Templates;

with AG_Tools.Writers;

package body AG_Tools.NT_Generators is
   use type League.Strings.Universal_String;

   procedure Write_Declaration
     (Context : AG_Tools.Contexts.Context_Access;
      NT      : Gela.Grammars.Non_Terminal);

   procedure Generate_Rule
     (R    : Gela.Grammars.Rule;
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
     (R    : Gela.Grammars.Rule;
      NT   : Gela.Grammars.Non_Terminal;
      Impl : in out AG_Tools.Writers.Writer)
   is
      use Gela.Grammars;

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

   --------------
   -- Make_Get --
   --------------

   overriding procedure Make_Get
     (Self      : access Generator;
      Attribute : Gela.Grammars.Attribute)
   is
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
      Attribute : Gela.Grammars.Attribute)
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
      Write_Declaration (Self.Context, NT);

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
      Spec.N ("   not ", Impl);
      Write_Declaration (Self.Context, NT);

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
      NT      : Gela.Grammars.Non_Terminal)
   is
      Spec      : AG_Tools.Writers.Writer renames Context.Spec;
      Impl      : AG_Tools.Writers.Writer renames Context.Impl;
      RT : constant League.Strings.Universal_String :=
        Return_Type (Context.Grammar.all, NT);
   begin
      Context.Add_With
        (Name => Package_Name (RT),
         Kind => AG_Tools.Contexts.Spec_Unit);

      Spec.N ("overriding procedure ", Impl);
      Spec.P (To_Ada (NT.Name), Impl);
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
               begin
                  for X of List loop
                     Gen := Context.Factory.Get (X, NT);
                     Gen.Make_Get (X);
                  end loop;

                  Generate_Rule (R, NT, Context.Code);

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
