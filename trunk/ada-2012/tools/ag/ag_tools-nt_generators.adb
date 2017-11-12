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
      NT      : Anagram.Grammars.Non_Terminal;
      Pass    : Positive);

   procedure Generate_Rule
     (T    : Anagram.Grammars.Rule_Templates.Rule_Template;
      NT   : Anagram.Grammars.Non_Terminal;
      Impl : in out AG_Tools.Writers.Writer);
   --  Output text of rule to Impl

   procedure Write_Rules
     (Context : AG_Tools.Contexts.Context_Access;
      NT      : Anagram.Grammars.Non_Terminal;
      Prod    : Anagram.Grammars.Production_Index;
      Pass    : Positive;
      Each    : in out Anagram.Grammars.Ordered.Order_Maps.Cursor);

   This : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("This");

   procedure Generate_Rule
     (T    : Anagram.Grammars.Rule_Templates.Rule_Template;
      NT   : Anagram.Grammars.Non_Terminal;
      Impl : in out AG_Tools.Writers.Writer)
   is
      use Anagram.Grammars;

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
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template)
   is
      pragma Unreferenced (Template);
      G      : Anagram.Grammars.Grammar renames Self.Context.Grammar.all;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      D      : Anagram.Grammars.Attribute_Declaration renames
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
      Attribute : Anagram.Grammars.Attribute;
      Template  : Anagram.Grammars.Rule_Templates.Rule_Template)
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
      Attribute : Anagram.Grammars.Attribute_Declaration)
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
      Attribute : Anagram.Grammars.Attribute_Declaration)
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
      Order : Anagram.Grammars.Ordered.Order_Maps.Map;
      NT    : Anagram.Grammars.Non_Terminal;
      Pass  : Positive)
   is
      Pos : Anagram.Grammars.Ordered.Order_Maps.Cursor :=
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
      Order : Anagram.Grammars.Ordered.Order_Maps.Map;
      NT    : Anagram.Grammars.Non_Terminal;
      Pass  : Positive)
   is
      Pos : Anagram.Grammars.Ordered.Order_Maps.Cursor :=
        Order.Ceiling ((NT.First, Pass, Step => 1));

      Prod : Anagram.Grammars.Production renames
        Self.Context.Grammar.Production (NT.First);

      G  : Anagram.Grammars.Grammar renames Self.Context.Grammar.all;
      RT : constant League.Strings.Universal_String := Return_Type (G, NT);
      Parts   : Anagram.Grammars.Ordered.Partition_Array renames
        Self.Context.Partition (NT.First_Attribute .. NT.Last_Attribute);
      Spec    : AG_Tools.Writers.Writer renames Self.Context.Spec;
      Impl    : AG_Tools.Writers.Writer renames Self.Context.Impl;
      Code    : AG_Tools.Writers.Writer renames Self.Context.Code;
      Piece   : AG_Tools.Writers.Writer;
      Item_NT : Anagram.Grammars.Non_Terminal renames G.Non_Terminal
        (List_Item (G, NT));
      Item : constant League.Strings.Universal_String :=
        To_Ada (Item_NT.Name);
   begin
      Code.Clear;
      Self.Context.Attr_Map.Clear;

      Spec.P ("", Impl);
      Spec.N ("   ", Impl);
      Write_Declaration (Self.Context, NT, Pass);

      for J in NT.First_Attribute .. NT.Last_Attribute loop
         if Anagram.Grammars.Ordered.To_Pass (Parts, J) = Pass then
            Spec.P (";", Impl);
            Spec.N ("      ", Impl);
            Spec.N (This, Impl);
            Spec.N ("_", Impl);
            Spec.N (To_Ada (G.Declaration (J).Name), Impl);
            Spec.N (" : ", Impl);

            Code.P (";");
            Code.N ("      ");
            Code.N (This);
            Code.N ("_");
            Code.N (To_Ada (G.Declaration (J).Name));
            Code.N (" : ");

            if not G.Declaration (J).Is_Inherited then
               Spec.N ("out ", Impl);
               Code.N ("out ");
            end if;

            Spec.N (To_Ada (G.Declaration (J).Type_Name), Impl);
            Code.N (To_Ada (G.Declaration (J).Type_Name));

            Self.Context.Add_With
              (Package_Name (To_Ada (G.Declaration (J).Type_Name)),
               AG_Tools.Contexts.Spec_Unit);
         end if;
      end loop;

      Spec.N (")", Impl);
      Spec.N (";");
      Spec.P ("", Impl);
      Impl.P ("   is");

      Code.P (")");

      Impl.P ("      procedure Descent");
      Impl.N ("      (This : in out ");
      Impl.N (RT);
      Impl.N ("_Cursor");
      Impl.N (Code);
      Code.Clear;
      Impl.P ("      is");

      Self.Context.Factory.Get (G.Part (Prod.First)).
         Make_Local_Variable (Prod.First);

      Write_Rules (Self.Context, NT, NT.First, Pass, Pos);
      Impl.P ("      begin");
      Impl.P ("         if not This.Has_Element then");
      Piece := Code;
      Code.Clear;

      Pos := Order.Ceiling ((NT.Last, Pass, Step => 1));
      Write_Rules (Self.Context, NT, NT.Last, Pass, Pos);
      Impl.N (Code);
      Code.Clear;
      Impl.P ("            return;");
      Impl.P ("         end if;");

      Impl.N ("         ");
      Impl.N (Item);
      Impl.P (" := This.Element;");
      Impl.P ("         This.Next;");

      Impl.N (Piece);

      Impl.P ("      end Descent;");
      Impl.P;
      Impl.N ("      This : ");
      Impl.N (RT);
      Impl.P ("_Cursor :=");
      Impl.P ("        Node.First;");
      Impl.P ("   begin");
      Impl.N ("      Descent (This");

      for J in NT.First_Attribute .. NT.Last_Attribute loop
         if Anagram.Grammars.Ordered.To_Pass (Parts, J) = Pass then
            Impl.N (", ");
            Impl.N (This);
            Impl.N ("_");
            Impl.N (To_Ada (G.Declaration (J).Name));
         end if;
      end loop;

      Impl.P (");");

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
      Order : Anagram.Grammars.Ordered.Order_Maps.Map;
      NT    : Anagram.Grammars.Non_Terminal;
      Pass  : Positive)
   is
      G : Anagram.Grammars.Grammar renames Self.Context.Grammar.all;

      type Pass_Range is record
         From, To : Natural;
      end record;

      procedure Add (Item : in out Pass_Range; Pass : Positive);

      type Pass_Array is array (G.Non_Terminal'Range) of Pass_Range;

      procedure Find
        (NT     : Anagram.Grammars.Non_Terminal;
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
        (NT     : Anagram.Grammars.Non_Terminal;
         Pass   : Positive;
         Result : in out Pass_Array) is
      begin
         for J in NT.First .. NT.Last loop
            declare
               use Anagram.Grammars.Ordered.Order_Maps;
               use type Anagram.Grammars.Production_Index;
               Pos   : Cursor := Order.Ceiling ((J, Pass, Step => 1));
               Value : Anagram.Grammars.Ordered.Action;
               Go    : Anagram.Grammars.Non_Terminal_Index;
            begin
               while Has_Element (Pos) and then
                 Key (Pos).Prod = J and then
                 Key (Pos).Pass = Pass
               loop
                  Value := Element (Pos);

                  case Value.Kind is
                     when Anagram.Grammars.Ordered.Descent =>
                        Go := G.Part (Value.Part).Denote;

                        if Input.Is_Concrete (Go) then
                           Add (Result (Go), Value.Pass);
                        else
                           Find (G.Non_Terminal (Go), Value.Pass, Result);
                        end if;
                     when Anagram.Grammars.Ordered.Evaluate_Rule =>
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
      Attribute : Anagram.Grammars.Attribute)
   is
      G      : Anagram.Grammars.Grammar renames Self.Context.Grammar.all;
      Code   : AG_Tools.Writers.Writer renames Self.Context.Code;
      D      : Anagram.Grammars.Attribute_Declaration renames
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
      Attribute : Anagram.Grammars.Attribute)
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
      NT      : Anagram.Grammars.Non_Terminal;
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
      NT      : Anagram.Grammars.Non_Terminal;
      Prod    : Anagram.Grammars.Production_Index;
      Pass    : Positive;
      Each    : in out Anagram.Grammars.Ordered.Order_Maps.Cursor)
   is
      use Anagram.Grammars;
      use Anagram.Grammars.Ordered.Order_Maps;
      G       : Anagram.Grammars.Grammar renames Context.Grammar.all;
   begin
      while Has_Element (Each)
        and then Key (Each).Prod = Prod
        and then Key (Each).Pass = Pass
      loop

         case Element (Each).Kind is
            when Anagram.Grammars.Ordered.Evaluate_Rule =>
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
            when Anagram.Grammars.Ordered.Descent =>
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
