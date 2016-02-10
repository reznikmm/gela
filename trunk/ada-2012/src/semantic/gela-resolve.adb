with Gela.Defining_Name_Cursors;
with Gela.Element_Visiters;
with Gela.Elements.Composite_Constraints;
with Gela.Elements.Defining_Names;
with Gela.Elements.Range_Attribute_References;
with Gela.Elements.Simple_Expression_Ranges;
with Gela.Elements.Subtype_Indications;
with Gela.Environments;
with Gela.Profiles;
with Gela.Type_Managers;
with Gela.Types.Arrays;
with Gela.Types.Simple;
with Gela.Types.Visitors;
with Gela.Types.Untagged_Records;

package body Gela.Resolve is

   procedure To_Type_Category
     (Comp     : Gela.Compilations.Compilation_Access;
      Up       : Gela.Interpretations.Interpretation_Set_Index;
      Tipe     : Gela.Semantic_Types.Type_Index;
      Result   : out Gela.Interpretations.Interpretation_Index);
   --  Fetch Type_Category interpretation from Up that match given Tipe.

   procedure Get_Subtype
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Index  : out Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Semantic_Types.Type_Index);

   procedure To_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Semantic_Types.Type_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);

   procedure Each_Expression
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Target : in out Gela.Interpretations.Up_Visiter'Class);
   --  Resolve given interpretation set as expression. So ignore symbol and
   --  others non-expression interpretations. Translate defining name into
   --  expression. But On_Defining_Name is called when Name doen't have a type
   --  (like subprogram name)

   procedure Each_Prefix
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Target : in out Gela.Interpretations.Up_Visiter'Class);
   --  The same as Each_Expression, but add implicit dereference
   --  interpretations.

   procedure Wrap_Tuple
     (Self   : access Gela.Interpretations.Up_Visiter'Class;
      IM     : Gela.Interpretations.Interpretation_Manager_Access;
      Value  : Gela.Interpretations.Interpretation_Set_Index_Array;
      Found  : access Gela.Interpretations.Interpretation_Index;
      Chosen : out Gela.Interpretations.Interpretation_Index);
   --  For each Value (J), iterate over its interpretation set and call Self to
   --  resolve. Read resolved value from Found. Wrap each resolved value in
   --  down interpretation, then return its index as Chosen

   procedure Discrete_Range
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Down_Left  : out Gela.Interpretations.Interpretation_Index;
      Down_Right : out Gela.Interpretations.Interpretation_Index;
      Tipe       : out Gela.Semantic_Types.Type_Index);

   package String_Type_Matcher is
      type Type_Matcher is new Gela.Interpretations.Type_Matcher with record
         Match : Boolean := False;
      end record;

      type Type_Matcher_Access is access all Type_Matcher'Class;

      overriding procedure Array_Type
        (Self  : in out Type_Matcher;
         Value : not null Gela.Types.Arrays.Array_Type_Access);

      overriding function Is_Matched
        (Self : Type_Matcher) return Boolean;
   end String_Type_Matcher;

   package body String_Type_Matcher is

      overriding procedure Array_Type
        (Self  : in out Type_Matcher;
         Value : not null Gela.Types.Arrays.Array_Type_Access)
      is
         pragma Unreferenced (Value);
      begin
         Self.Match := True;  --  Value.Is_String;  FIXME
      end Array_Type;

      overriding function Is_Matched
        (Self : Type_Matcher) return Boolean is
      begin
         return Self.Match;
      end Is_Matched;

   end String_Type_Matcher;

   package Record_Type_Matcher is
      type Type_Matcher is new Gela.Interpretations.Type_Matcher with record
         Match : Boolean := False;
      end record;

      type Type_Matcher_Access is not null access all Type_Matcher'Class;

      overriding procedure Untagged_Record
        (Self  : in out Type_Matcher;
         Value : not null Gela.Types.Untagged_Records.
           Untagged_Record_Type_Access);

      overriding function Is_Matched
        (Self : Type_Matcher) return Boolean;
   end Record_Type_Matcher;

   package body Record_Type_Matcher is

      overriding procedure Untagged_Record
        (Self  : in out Type_Matcher;
         Value : not null Gela.Types.Untagged_Records.
           Untagged_Record_Type_Access)
      is
         pragma Unreferenced (Value);
      begin
         Self.Match := True;
      end Untagged_Record;

      overriding function Is_Matched
        (Self : Type_Matcher) return Boolean is
      begin
         return Self.Match;
      end Is_Matched;

   end Record_Type_Matcher;

   package Integer_Type_Matcher is
      type Type_Matcher is new Gela.Interpretations.Type_Matcher with record
         Match : Boolean := False;
      end record;

      type Type_Matcher_Access is access all Type_Matcher'Class;

      overriding procedure Signed_Integer_Type
        (Self  : in out Type_Matcher;
         Value : not null Gela.Types.Simple.Signed_Integer_Type_Access);

      overriding function Is_Matched
        (Self : Type_Matcher) return Boolean;
   end Integer_Type_Matcher;

   package body Integer_Type_Matcher is

      overriding procedure Signed_Integer_Type
        (Self  : in out Type_Matcher;
         Value : not null Gela.Types.Simple.Signed_Integer_Type_Access)
      is
         pragma Unreferenced (Value);
      begin
         Self.Match := True;
      end Signed_Integer_Type;

      overriding function Is_Matched
        (Self : Type_Matcher) return Boolean is
      begin
         return Self.Match;
      end Is_Matched;

   end Integer_Type_Matcher;

   package Float_Type_Matcher is
      type Type_Matcher is new Gela.Interpretations.Type_Matcher with record
         Match : Boolean := False;
      end record;

      type Type_Matcher_Access is access all Type_Matcher'Class;

      overriding procedure Floating_Point_Type
        (Self  : in out Type_Matcher;
         Value : not null Gela.Types.Simple.Floating_Point_Type_Access);

      overriding function Is_Matched
        (Self : Type_Matcher) return Boolean;
   end Float_Type_Matcher;

   package body Float_Type_Matcher is

      overriding procedure Floating_Point_Type
        (Self  : in out Type_Matcher;
         Value : not null Gela.Types.Simple.Floating_Point_Type_Access)
      is
         pragma Unreferenced (Value);
      begin
         Self.Match := True;
      end Floating_Point_Type;

      overriding function Is_Matched
        (Self : Type_Matcher) return Boolean is
      begin
         return Self.Match;
      end Is_Matched;

   end Float_Type_Matcher;

   ----------------------
   -- Assignment_Right --
   ----------------------

   procedure Assignment_Right
     (Comp     : Gela.Compilations.Compilation_Access;
      Env      : Gela.Semantic_Types.Env_Index;
      Left     : Gela.Interpretations.Interpretation_Set_Index;
      Right    : Gela.Interpretations.Interpretation_Set_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is
   begin
      --  ARM 5.2 (4/2)
      To_Type_Or_The_Same_Type
        (Comp    => Comp,
         Env     => Env,
         Type_Up => Left,
         Expr_Up => Right,
         Result  => Result);
   end Assignment_Right;

   -------------------------
   -- Attribute_Reference --
   -------------------------

   procedure Attribute_Reference
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Token  : Gela.Lexical_Types.Token_Count;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is
      use type Gela.Lexical_Types.Symbol;
      use type Gela.Lexical_Types.Token_Count;
      use type Gela.Semantic_Types.Type_Index;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Length : Boolean;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);
      end Each;

      Index      : Gela.Interpretations.Interpretation_Index;

      package body Each is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Cursor);
            View : constant Gela.Types.Type_View_Access := TM.Get (Tipe);
            Arr  : Gela.Types.Arrays.Array_Type_Access;
         begin
            if View.Assigned and then View.Is_Array then
               Arr  := Gela.Types.Arrays.Array_Type_Access (View);

               if Self.Length then
                  Comp.Context.Interpretation_Manager.Add_Expression
                    (Tipe   => TM.Universal_Integer,
                     Down   => (1 => Index),
                     Result => Set);
               else
                  Comp.Context.Interpretation_Manager.Add_Expression
                    (Tipe   => Arr.all.Index_Types (1),
                     Down   => (1 => Index),
                     Result => Set);
               end if;
            end if;
         end On_Expression;
      end Each;

      Arr_Visiter : Each.Visiter;
      Attr        : Gela.Lexical_Types.Predefined_Symbols.Symbol;
      Type_Index  : Gela.Semantic_Types.Type_Index;
   begin
      Set := 0;

      if Token = 0 then
         Attr := Symbol;
      else
         Attr := Gela.Lexical_Types.Predefined_Symbols.Range_Symbol;
      end if;

      Arr_Visiter.Length :=
        Attr = Gela.Lexical_Types.Predefined_Symbols.Length;

      case Attr is
         when Gela.Lexical_Types.Predefined_Symbols.Length |
              Gela.Lexical_Types.Predefined_Symbols.First |
              Gela.Lexical_Types.Predefined_Symbols.Range_Symbol |
              Gela.Lexical_Types.Predefined_Symbols.Last =>

            Get_Subtype
              (Comp,
               Env    => Env,
               Set    => Prefix,
               Index  => Index,
               Result => Type_Index);

            if Type_Index = 0 then
               Resolve.Each_Prefix (Comp, Env, Prefix, Arr_Visiter);
            elsif Arr_Visiter.Length then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => TM.Universal_Integer,
                  Down   => (1 => Index),
                  Result => Set);
            else
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => Type_Index,
                  Down   => (1 => Index),
                  Result => Set);
            end if;
         when Gela.Lexical_Types.Predefined_Symbols.Val =>
            Get_Subtype
              (Comp,
               Env    => Env,
               Set    => Prefix,
               Index  => Index,
               Result => Type_Index);

            Comp.Context.Interpretation_Manager.Add_Attr_Function
              (Kind   => Attr,
               Down   => (1 => Index),
               Result => Set);

         when Gela.Lexical_Types.Predefined_Symbols.Size =>
            Get_Subtype
              (Comp,
               Env    => Env,
               Set    => Prefix,
               Index  => Index,
               Result => Type_Index);

            Comp.Context.Interpretation_Manager.Add_Expression
              (Tipe   => TM.Universal_Integer,
               Down   => (1 => Index),
               Result => Set);

         when others =>
            null;
      end case;
   end Attribute_Reference;

   --------------------
   -- Case_Statement --
   --------------------

   procedure Case_Statement
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Tuple   : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is
      package Each_Expr is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Result : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Expr;

      IM   : constant Gela.Interpretations.Interpretation_Manager_Access
        := Comp.Context.Interpretation_Manager;

      package body Each_Expr is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Cursor, Self);
            Tuples : constant Gela.Interpretations
              .Interpretation_Tuple_Index_Array :=
                IM.Get_Tuple_List (Tuple);
            Output : Gela.Interpretations.Interpretation_Index_Array
              (Tuples'Range);
         begin
            for J in Tuples'Range loop
               declare
                  Chosen : Gela.Interpretations.Interpretation_Index := 0;
                  Value  : constant Gela.Interpretations
                    .Interpretation_Set_Index_Array :=
                      IM.Get_Tuple (Tuples (J));
                  List   : Gela.Interpretations.Interpretation_Index_Array
                    (Value'Range);
               begin
                  for K in Value'Range loop
                     To_Type
                       (Comp    => Comp,
                        Env     => Env,
                        Type_Up => Tipe,
                        Expr_Up => Value (K),
                        Result  => List (K));
                  end loop;

                  Chosen := 0;

                  for K in reverse List'Range loop
                     IM.Get_Tuple_Index (List (K), Chosen, Chosen);
                  end loop;

                  Output (J) := Chosen;
               end;
            end loop;

            for J in reverse Output'Range loop
               IM.Get_Tuple_Index (Output (J), Result, Result);
            end loop;
         end On_Expression;

      end Each_Expr;

      Expr_Visiter : aliased Each_Expr.Visiter;
   begin
      Result := 0;
      Each_Expression (Comp   => Comp,
                       Env    => Env,
                       Set    => Expr_Up,
                       Target => Expr_Visiter);
   end Case_Statement;

   ----------------
   -- Constraint --
   ----------------

   procedure Constraint
     (Constraint : Gela.Elements.Constraints.Constraint_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Type_Up    : Gela.Interpretations.Interpretation_Set_Index;
      Constr     : Gela.Interpretations.Interpretation_Set_Index;
      Result     : out Gela.Interpretations.Interpretation_Index)
   is
      package Each_Constraint is
         type Visiter is new Gela.Element_Visiters.Visiter with null record;

         overriding procedure Composite_Constraint
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access);

         overriding procedure Range_Attribute_Reference
           (Self : in out Visiter;
            Node : not null Gela.Elements.Range_Attribute_References.
              Range_Attribute_Reference_Access);

         overriding procedure Simple_Expression_Range
           (Self : in out Visiter;
            Node : not null Gela.Elements.Simple_Expression_Ranges.
              Simple_Expression_Range_Access);

      end Each_Constraint;

      Comp       : Gela.Compilations.Compilation_Access;
      IM         : Gela.Interpretations.Interpretation_Manager_Access;
      Type_Index : Gela.Semantic_Types.Type_Index;

      package body Each_Constraint is

         overriding procedure Composite_Constraint
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access)
         is
            pragma Unreferenced (Node, Self);

            X : constant Gela.Interpretations.Interpretation_Tuple_List_Index
              := Gela.Interpretations.Interpretation_Tuple_List_Index (Constr);

            Tuples : constant Gela.Interpretations
              .Interpretation_Tuple_Index_Array := IM.Get_Tuple_List (X);

            Output : Gela.Interpretations.Interpretation_Index_Array
              (Tuples'Range);

            TM : constant Gela.Type_Managers.Type_Manager_Access :=
              Comp.Context.Types;

            package Type_Visiters is
               type Type_Visitor is new Gela.Types.Visitors.Type_Visitor
                 with null record;

               overriding procedure Array_Type
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Arrays.Array_Type_Access);

               overriding procedure Untagged_Record
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Untagged_Records
                    .Untagged_Record_Type_Access);

            end Type_Visiters;

            package body Type_Visiters is

               overriding procedure Array_Type
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Arrays.Array_Type_Access)
               is
                  pragma Unreferenced (Self);

                  IT : constant Gela.Semantic_Types.Type_Index_Array :=
                    Value.Index_Types;
                  Count : Natural := 0;

                  Chosen : Gela.Interpretations.Interpretation_Index;
               begin
                  for K in Tuples'Range loop
                     declare
                        Tuple  : constant Gela.Interpretations
                          .Interpretation_Set_Index_Array :=
                            IM.Get_Tuple (Tuples (K));
                     begin
                        Count := Count + 1;

                        To_Type
                          (Comp    => Comp,
                           Env     => Env,
                           Type_Up => IT (Count),
                           Expr_Up => Tuple (Tuple'Last),
                           Result  => Chosen);

                        IM.Get_Tuple_Index (Chosen, 0, Chosen);

                        if Tuple'Length = 2 then
                           --  Put some interpretation to placeholder item
                           IM.Get_Tuple_Index (0, Chosen, Chosen);
                        end if;

                        Output (K) := Chosen;
                     end;
                  end loop;

                  Chosen := 0;

                  for J in reverse Output'Range loop
                     IM.Get_Tuple_Index (Output (J), Chosen, Chosen);
                  end loop;

                  Result := Chosen;
               end Array_Type;

               overriding procedure Untagged_Record
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Untagged_Records
                  .Untagged_Record_Type_Access)
               is
                  pragma Unreferenced (Self);

                  package Each_Choice is
                     type Visiter is new Gela.Interpretations.Up_Visiter with
                     record
                        Index    : aliased Gela.Interpretations.
                          Interpretation_Index := 0;
                        Exp_Type : Gela.Semantic_Types.Type_Index := 0;
                     end record;

                     overriding procedure On_Symbol
                       (Self   : in out Visiter;
                        Symbol : Gela.Lexical_Types.Symbol;
                        Cursor : Gela.Interpretations.Cursor'Class);

                  end Each_Choice;

                  package body Each_Choice is

                     overriding procedure On_Symbol
                       (Self   : in out Visiter;
                        Symbol : Gela.Lexical_Types.Symbol;
                        Cursor : Gela.Interpretations.Cursor'Class)
                     is
                        pragma Unreferenced (Cursor);
                        use type Gela.Semantic_Types.Type_Index;

                        Name : Gela.Elements.Defining_Names
                          .Defining_Name_Access;
                     begin
                        Name := Value.Get_Discriminant (Symbol);

                        if Name.Assigned then
                           IM.Get_Defining_Name_Index (Name, Self.Index);

                           if Self.Exp_Type = 0 then
                              Self.Exp_Type := TM.Type_Of_Object_Declaration
                                (Env, Name.Enclosing_Element);
                           end if;
                        end if;
                     end On_Symbol;

                  end Each_Choice;

                  Chosen : Gela.Interpretations.Interpretation_Index;
               begin
                  for K in Tuples'Range loop
                     declare
                        V      : aliased Each_Choice.Visiter;
                        Tuple  : constant Gela.Interpretations
                          .Interpretation_Set_Index_Array :=
                            IM.Get_Tuple (Tuples (K));
                     begin
                        --  Resolve choices of association
                        Wrap_Tuple
                          (Self   => V'Access,
                           IM     => IM,
                           Value  => Tuple (Tuple'First + 1 .. Tuple'Last),
                           Found  => V.Index'Access,
                           Chosen => Output (K));

                        --  Resolve expression of association
                        To_Type
                          (Comp    => Comp,
                           Env     => Env,
                           Type_Up => V.Exp_Type,
                           Expr_Up => Tuple (Tuple'First),
                           Result  => Chosen);

                        IM.Get_Tuple_Index (Chosen, Output (K), Output (K));
                     end;
                  end loop;

                  Chosen := 0;

                  for J in reverse Output'Range loop
                     IM.Get_Tuple_Index (Output (J), Chosen, Chosen);
                  end loop;

                  Result := Chosen;
               end Untagged_Record;

            end Type_Visiters;

            Type_View : Gela.Types.Type_View_Access;
            Visiter   : Type_Visiters.Type_Visitor;
            Ignore    : Gela.Interpretations.Interpretation_Index;
         begin
            Get_Subtype
              (Comp,
               Env    => Env,
               Set    => Type_Up,
               Index  => Ignore,
               Result => Type_Index);

            Type_View := TM.Get (Type_Index);

            Type_View.Visit_If_Assigned (Visiter);
         end Composite_Constraint;

         overriding procedure Range_Attribute_Reference
           (Self : in out Visiter;
            Node : not null Gela.Elements.Range_Attribute_References.
              Range_Attribute_Reference_Access)
         is
            pragma Unreferenced (Node, Self);
         begin
            --  3.5 (5)
            Gela.Resolve.To_Type
              (Comp    => Comp,
               Env     => Env,
               Type_Up => Type_Up,
               Expr_Up => Constr,
               Result  => Result);
         end Range_Attribute_Reference;

         overriding procedure Simple_Expression_Range
           (Self : in out Visiter;
            Node : not null Gela.Elements.Simple_Expression_Ranges.
              Simple_Expression_Range_Access)
         is
            pragma Unreferenced (Node, Self);
         begin
            --  3.5 (5)
            Gela.Resolve.To_Type
              (Comp    => Comp,
               Env     => Env,
               Type_Up => Type_Up,
               Expr_Up => Constr,
               Result  => Result);
         end Simple_Expression_Range;

      end Each_Constraint;

      V : Each_Constraint.Visiter;

   begin
      Result := 0;

      if not Constraint.Assigned then
         return;
      end if;

      Comp := Constraint.Enclosing_Compilation;
      pragma Warnings (Off);
      --  GNAT GPL 2013 gets warnings here about useless assignment
      IM := Comp.Context.Interpretation_Manager;
      pragma Warnings (On);

      Constraint.Visit (V);
   end Constraint;

   -----------------
   -- Direct_Name --
   -----------------

   procedure Direct_Name
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is
      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;
      ES : constant Gela.Environments.Environment_Set_Access :=
        Comp.Context.Environment_Set;
      DV : Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class :=
        ES.Direct_Visible (Env, Symbol);

      Have_Direct_Visible : constant Boolean := DV.Has_Element;
   begin
      Set := 0;
      IM.Add_Symbol (Symbol, Set);

      while DV.Has_Element loop
         IM.Add_Defining_Name
           (Name   => DV.Element,
            Down   => (1 .. 0 => 0),
            Result => Set);

         DV.Next;
      end loop;

      if Have_Direct_Visible then
         return;
      end if;

      declare
         UV : Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class :=
           ES.Use_Visible (Env, Symbol);
      begin
         while UV.Has_Element loop
            IM.Add_Defining_Name
              (Name   => UV.Element,
               Down   => (1 .. 0 => 0),
               Result => Set);

            UV.Next;
         end loop;
      end;
   end Direct_Name;

   procedure Each_Expression
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Target : in out Gela.Interpretations.Up_Visiter'Class)
   is
      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            null;
         end record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
            use type Gela.Semantic_Types.Type_Index;

            Decl : constant Gela.Elements.Element_Access :=
              Name.Enclosing_Element;
            Type_Index : constant Gela.Semantic_Types.Type_Index :=
              TM.Type_Of_Object_Declaration (Env, Decl);
         begin
            if Type_Index = 0 then
               Target.On_Defining_Name (Name, Cursor);
            else
               Target.On_Expression (Type_Index, Cursor);
            end if;
         end On_Defining_Name;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Expression (Tipe, Cursor);
         end On_Expression;

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      Visiter : aliased Each.Visiter;
      Cursor  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Set);
   begin
      while Cursor.Has_Element loop
         Cursor.Visit (Visiter'Access);
         Cursor.Next;
      end loop;
   end Each_Expression;

   -----------------
   -- Each_Prefix --
   -----------------

   procedure Each_Prefix
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Target : in out Gela.Interpretations.Up_Visiter'Class)
   is
      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            null;
         end record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Defining_Name (Name, Cursor);
         end On_Defining_Name;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);

            package Type_Visiters is
               type Type_Visitor is new Gela.Types.Visitors.Type_Visitor
                 with null record;

               overriding procedure Object_Access_Type
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Simple
                    .Object_Access_Type_Access);

            end Type_Visiters;

            package body Type_Visiters is

               overriding procedure Object_Access_Type
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Simple
                    .Object_Access_Type_Access)
               is
                  pragma Unreferenced (Self);
                  SI : constant Gela.Elements.Subtype_Indications
                    .Subtype_Indication_Access := Value.Get_Designated;
                  Index : constant Gela.Semantic_Types.Type_Index :=
                    TM.Type_From_Subtype_Mark (Env, SI.Subtype_Mark);
               begin
                  Target.On_Expression (Index, Cursor);
               end Object_Access_Type;

            end Type_Visiters;

            View : constant Gela.Types.Type_View_Access :=
              TM.Get (Tipe);
            Visiter : Type_Visiters.Type_Visitor;
         begin
            Target.On_Expression (Tipe, Cursor);
            View.Visit_If_Assigned (Visiter);
         end On_Expression;

      end Each;

      Visiter : aliased Each.Visiter;
   begin
      Each_Expression (Comp   => Comp,
                       Env    => Env,
                       Set    => Set,
                       Target => Visiter);
   end Each_Prefix;

   -------------------
   -- Function_Call --
   -------------------

   procedure Function_Call
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Args   : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is

      use type Gela.Interpretations.Interpretation_Index;
      use type Gela.Interpretations.Interpretation_Index_Array;

      package Each_Prefix is
         type Visiter is new Gela.Interpretations.Up_Visiter with null record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);
      end Each_Prefix;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Tuples : constant Gela.Interpretations.Interpretation_Tuple_Index_Array
        := IM.Get_Tuple_List (Args);

      package body Each_Prefix is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
            Chosen : Gela.Interpretations.Interpretation_Index := 0;
            Count  : Natural := 0;
            Output : Gela.Interpretations.Interpretation_Index_Array
              (Tuples'Range);

            Profile : constant Gela.Profiles.Profile_Access :=
              TM.Get_Profile (Env, Name);

         begin
            if not Profile.Assigned then
               return;
            end if;

            for J in Tuples'Range loop
               declare
                  Tuple : constant Gela.Interpretations
                    .Interpretation_Set_Index_Array
                      := IM.Get_Tuple (Tuples (J));

                  Tipe   : Gela.Semantic_Types.Type_Index;
                  List   : Gela.Interpretations.Interpretation_Index_Array
                    (Tuple'Range);
               begin
                  --  Check if this is positional association
                  if Tuple'Length = 1 then
                     if Count < Profile.Length then
                        Count := Count + 1;
                        Tipe := Profile.Get_Type (Count);
                        To_Type (Comp, Env, Tipe, Tuple (Tuple'First), Chosen);

                        if Chosen = 0 then
                           return;
                        else
                           List (List'First) := Chosen;
                        end if;
                     else
                        return;
                     end if;
                  else
                     for J in Tuple'Range loop
                        Interpretation
                          (Comp   => Comp,
                           Env    => Env,
                           Set    => Tuple (J),
                           Result => List (J));
                     end loop;
                  end if;

                  Chosen := 0;

                  for K in reverse List'Range loop
                     IM.Get_Tuple_Index (List (K), Chosen, Chosen);
                  end loop;

                  Output (J) := Chosen;
               end;
            end loop;

            Chosen := 0;

            for K in reverse Output'Range loop
               IM.Get_Tuple_Index (Output (K), Chosen, Chosen);
            end loop;

            if Chosen /= 0 then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => Profile.Return_Type,
                  Down   => Cursor.Get_Index & Chosen,
                  Result => Set);

            elsif Tuples'Length = 0
              and then Profile.Allow_Empty_Argument_List
            then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => Profile.Return_Type,
                  Down   => Cursor.Get_Index & 0,
                  Result => Set);

            end if;
         end On_Defining_Name;

         -------------------
         -- On_Expression --
         -------------------

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
            View   : constant Gela.Types.Type_View_Access := TM.Get (Tipe);
            Arr    : Gela.Types.Arrays.Array_Type_Access;
            Chosen : Gela.Interpretations.Interpretation_Index := 0;
            Count  : Natural := 0;
            Output : Gela.Interpretations.Interpretation_Index_Array
              (Tuples'Range);
         begin
            if not View.Assigned or else not View.Is_Array then
               return;
            end if;

            Arr := Gela.Types.Arrays.Array_Type_Access (View);

            for J in Tuples'Range loop
               declare
                  Tuple : constant Gela.Interpretations
                    .Interpretation_Set_Index_Array
                      := IM.Get_Tuple (Tuples (J));

                  Index_Type : Gela.Semantic_Types.Type_Index := 0;
               begin
                  --  Check if this is positional association
                  if Tuple'Length = 1 then
                     Count := Count + 1;
                     Index_Type := Arr.all.Index_Types (Count);
                     To_Type
                       (Comp, Env, Index_Type, Tuple (Tuple'First), Chosen);

                     if Chosen = 0 then
                        return;
                     else
                        IM.Get_Tuple_Index (Chosen, 0, Chosen);
                     end if;
                  else
                     return;
                  end if;

                  Output (J) := Chosen;
               end;
            end loop;

            Chosen := 0;

            for K in reverse Output'Range loop
               IM.Get_Tuple_Index (Output (K), Chosen, Chosen);
            end loop;

            if Chosen /= 0 then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => Arr.Component_Type,
                  Down   => Cursor.Get_Index & Chosen,
                  Result => Set);
            end if;
         end On_Expression;
      end Each_Prefix;

      Visiter : aliased Each_Prefix.Visiter;
   begin
      Set := 0;
      Resolve.Each_Prefix (Comp, Env, Prefix, Visiter);
   end Function_Call;

   -----------------
   -- Get_Subtype --
   -----------------

   procedure Get_Subtype
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Index  : out Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Semantic_Types.Type_Index)
   is

      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index  : Gela.Interpretations.Interpretation_Index := 0;
            Result : Gela.Semantic_Types.Type_Index := 0;
         end record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
         begin
            Self.Result := TM.Type_By_Name (Env, Name);
            Self.Index := Cursor.Get_Index;
         end On_Defining_Name;

      end Each;

      Cursor  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Set);
      Visiter : aliased Each.Visiter;
   begin
      while Cursor.Has_Element loop
         Cursor.Visit (Visiter'Access);
         Cursor.Next;
      end loop;

      Index := Visiter.Index;
      Result := Visiter.Result;
   end Get_Subtype;

   procedure Interpretation
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Env);

      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Prev   : Gela.Interpretations.Interpretation_Index := 0;
            Result : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      package body Each is

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Symbol, Cursor);
         begin
            --  Skip symbols
            Self.Result := Self.Prev;
         end On_Symbol;

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      Cursor : Gela.Interpretations.Cursor'Class :=
        IM.Get_Cursor (Set);

      Visiter : aliased Each.Visiter;
   begin
      while Cursor.Has_Element loop
         Visiter.Result := Cursor.Get_Index;
         Cursor.Visit (Visiter'Access);
         Visiter.Prev := Visiter.Result;
         Cursor.Next;
      end loop;

      Result := Visiter.Result;
   end Interpretation;

   ---------------------
   -- Membership_Test --
   ---------------------

   procedure Membership_Test
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Left   : Gela.Interpretations.Interpretation_Set_Index;
      Right  : Gela.Interpretations.Interpretation_Set_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is
      package Each_Right is
         type Visiter is new Gela.Interpretations.Up_Visiter with null record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Right;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package body Each_Right is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            package Each_Left is
               type Visiter is new Gela.Interpretations.Up_Visiter with record
                  Tipe   : Gela.Semantic_Types.Type_Index;
               end record;

               overriding procedure On_Expression
                 (Self   : in out Visiter;
                  Tipe   : Gela.Semantic_Types.Type_Index;
                  Cursor : Gela.Interpretations.Cursor'Class);

            end Each_Left;

            package body Each_Left is

               overriding procedure On_Expression
                 (Self   : in out Visiter;
                  Tipe   : Gela.Semantic_Types.Type_Index;
                  Cursor : Gela.Interpretations.Cursor'Class)
               is
                  use type Gela.Interpretations.Interpretation_Index_Array;

                  Left_Type : constant Gela.Types.Type_View_Access :=
                    TM.Get (Tipe);
                  Right_Type : constant Gela.Types.Type_View_Access :=
                    TM.Get (Self.Tipe);
               begin
                  if Left_Type.Is_Expected_Type (Expected => Right_Type) then
                     Comp.Context.Interpretation_Manager.Add_Expression
                       (Tipe   => TM.Boolean,
                        Down   => Cursor.Get_Index &
                          Each_Right.On_Expression.Cursor.Get_Index,
                        Result => Set);
                  end if;
               end On_Expression;

            end Each_Left;

            pragma Unreferenced (Cursor, Self);
            ELV : Each_Left.Visiter := (Tipe => Tipe);
         begin
            Each_Expression (Comp   => Comp,
                             Env    => Env,
                             Set    => Left,
                             Target => ELV);
         end On_Expression;

      end Each_Right;

      Visiter : aliased Each_Right.Visiter;
   begin
      Set := 0;
      Each_Expression (Comp   => Comp,
                       Env    => Env,
                       Set    => Right,
                       Target => Visiter);
   end Membership_Test;

   ---------------------
   -- Numeric_Literal --
   ---------------------

   procedure Numeric_Literal
     (Comp   : Gela.Compilations.Compilation_Access;
      Token  : Gela.Lexical_Types.Token_Count;
      Result : out Gela.Interpretations.Interpretation_Set_Index)
   is
      Value : constant Gela.Lexical_Types.Token := Comp.Get_Token (Token);
      Type_Index : Gela.Semantic_Types.Type_Index;
   begin
      Result := 0;

      if Comp.Source.Index (Value.First, Value.Last, '.') = 0 then
         Type_Index := Comp.Context.Types.Universal_Integer;
      else
         Type_Index := Comp.Context.Types.Universal_Real;
      end if;

      Comp.Context.Interpretation_Manager.Add_Expression
        (Tipe   => Type_Index,
         Down   => (1 .. 0 => 0),
         Result => Result);
   end Numeric_Literal;

   -----------------
   -- Placeholder --
   -----------------

   function Placeholder
     (Comp : Gela.Compilations.Compilation_Access)
      return Gela.Interpretations.Interpretation_Set_Index
   is
      Result : Gela.Interpretations.Interpretation_Set_Index := 0;
   begin
      Comp.Context.Interpretation_Manager.Add_Placeholder
        (Kind   => Gela.Interpretations.Absent,
         Result => Result);

      return Result;
   end Placeholder;

   ---------------
   -- Real_Type --
   ---------------

   procedure Real_Type
     (Comp     : Gela.Compilations.Compilation_Access;
      Up       : Gela.Interpretations.Interpretation_Set_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is
      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;
   begin
      To_Type_Category (Comp, Up, TM.Universal_Real, Result);
   end Real_Type;

   ----------------------
   -- Record_Aggregate --
   ----------------------

   procedure Record_Aggregate
     (Comp     : Gela.Compilations.Compilation_Access;
      Env      : Gela.Semantic_Types.Env_Index;
      Up       : Gela.Interpretations.Interpretation_Index;
      Tuple    : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is

      package Each is
         type Visiter is new Gela.Interpretations.Down_Visiter with record
            Result : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Expression
           (Self : in out Visiter;
            Tipe : Gela.Semantic_Types.Type_Index;
            Down : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      package Each_Choice is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Exp    : Gela.Semantic_Types.Type_Index := 0;
            Name   : Gela.Interpretations.Interpretation_Index := 0;
            View   : Gela.Types.Type_View_Access;
         end record;

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Choice;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package body Each is

         overriding procedure On_Expression
           (Self : in out Visiter;
            Tipe : Gela.Semantic_Types.Type_Index;
            Down : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down);

            View   : constant Gela.Types.Type_View_Access := TM.Get (Tipe);
            Tuples : constant Gela.Interpretations
              .Interpretation_Tuple_Index_Array :=
                IM.Get_Tuple_List (Tuple);
            Output : Gela.Interpretations.Interpretation_Index_Array
              (Tuples'Range);
         begin
            if not View.Is_Record then
               return;
            end if;

            for J in Tuples'Range loop
               declare
                  Chosen : Gela.Interpretations.Interpretation_Index := 0;
                  Value  : constant Gela.Interpretations
                    .Interpretation_Set_Index_Array :=
                      IM.Get_Tuple (Tuples (J));
                  List   : Gela.Interpretations.Interpretation_Index_Array
                    (Value'Range);
                  V : aliased Each_Choice.Visiter := (0, 0, View);
               begin
                  for K in 2 .. Value'Last loop
                     declare
                        Cursor : Gela.Interpretations.Cursor'Class :=
                          IM.Get_Cursor (Value (K));
                     begin
                        while Cursor.Has_Element loop
                           Cursor.Visit (V'Access);
                           Cursor.Next;
                        end loop;

                        List (K) := V.Name;
                     end;
                  end loop;

                  To_Type
                    (Comp    => Comp,
                     Env     => Env,
                     Type_Up => V.Exp,
                     Expr_Up => Value (Value'First),
                     Result  => List (List'First));

                  Chosen := 0;

                  for K in reverse List'Range loop
                     IM.Get_Tuple_Index (List (K), Chosen, Chosen);
                  end loop;

                  Output (J) := Chosen;
               end;
            end loop;

            for J in reverse Output'Range loop
               IM.Get_Tuple_Index (Output (J), Self.Result, Self.Result);
            end loop;
         end On_Expression;

      end Each;

      package body Each_Choice is

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Cursor);

            Name : constant Gela.Elements.Defining_Names.Defining_Name_Access
              := Gela.Types.Untagged_Records.Untagged_Record_Type_Access
                   (Self.View).Get_Component (Symbol);
         begin
            if Name.Assigned then
               IM.Get_Defining_Name_Index (Name, Self.Name);

               Self.Exp :=
                 TM.Type_Of_Object_Declaration (Env, Name.Enclosing_Element);
            end if;
         end On_Symbol;

      end Each_Choice;

      V : Each.Visiter;
   begin
      IM.Visit (Up, V);
      Result := V.Result;
   end Record_Aggregate;

   --------------------
   -- Record_Matcher --
   --------------------

   function Record_Matcher
     return not null Gela.Interpretations.Type_Matcher_Access
   is
      Result : constant Record_Type_Matcher.Type_Matcher_Access :=
        new Record_Type_Matcher.Type_Matcher;
   begin
      return Gela.Interpretations.Type_Matcher_Access (Result);
   end Record_Matcher;

   ------------------------
   -- Selected_Component --
   ------------------------

   procedure Selected_Component
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is
      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with null record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      ES : constant Gela.Environments.Environment_Set_Access :=
        Comp.Context.Environment_Set;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Is_Expanded_Name : Boolean := False;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
            Found : aliased Boolean := False;
            NC : Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class :=
              ES.Visible (Env, Name, Symbol, Found'Access);
         begin
            if Found then
               --  ARM 4.1.3(4)
               Is_Expanded_Name := True;

               while NC.Has_Element loop
                  IM.Add_Defining_Name
                    (Name   => NC.Element,
                     Down   => (1 => Cursor.Get_Index),
                     Result => Set);

                  NC.Next;
               end loop;
            end if;
         end On_Defining_Name;

      end Each;

      package Each_Expr is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Result : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Expr;

      package body Each_Expr is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);

            package Type_Visiters is
               type Type_Visitor is new Gela.Types.Visitors.Type_Visitor
                 with null record;

               overriding procedure Untagged_Record
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Untagged_Records
                    .Untagged_Record_Type_Access);

            end Type_Visiters;

            package body Type_Visiters is

               overriding procedure Untagged_Record
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Untagged_Records
                    .Untagged_Record_Type_Access)
               is
                  pragma Unreferenced (Self);
                  Name : Gela.Elements.Defining_Names.Defining_Name_Access;
               begin
                  Name := Value.Get_Component (Symbol);

                  if Name.Assigned then
                     IM.Add_Defining_Name
                       (Name   => Name,
                        Down   => (1 => Cursor.Get_Index),
                        Result => Set);
                  end if;
               end Untagged_Record;

            end Type_Visiters;

            Type_View : constant Gela.Types.Type_View_Access :=
              TM.Get (Tipe);
            Visiter : Type_Visiters.Type_Visitor;
         begin
            Type_View.Visit_If_Assigned (Visiter);
         end On_Expression;

      end Each_Expr;

      Cursor  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Prefix);
      Visiter : aliased Each.Visiter;
   begin
      Set := 0;
      while Cursor.Has_Element loop
         Cursor.Visit (Visiter'Access);
         Cursor.Next;
      end loop;

      if not Is_Expanded_Name then
         declare
            Expr : Each_Expr.Visiter;
         begin
            Each_Prefix (Comp, Env, Prefix, Expr);
         end;
      end if;
   end Selected_Component;

   ----------------------
   -- Shall_Be_Subtype --
   ----------------------

   procedure Shall_Be_Subtype
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      Type_Index : Gela.Semantic_Types.Type_Index;
   begin
      Get_Subtype
        (Comp,
         Env    => Env,
         Set    => Set,
         Index  => Result,
         Result => Type_Index);
   end Shall_Be_Subtype;

   -------------------------
   -- Signed_Integer_Type --
   -------------------------

   procedure Signed_Integer_Type
     (Comp     : Gela.Compilations.Compilation_Access;
      Up       : Gela.Interpretations.Interpretation_Set_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is
      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;
   begin
      To_Type_Category (Comp, Up, TM.Universal_Integer, Result);
   end Signed_Integer_Type;

   -----------------------------
   -- Simple_Expression_Range --
   -----------------------------

   procedure Simple_Expression_Range
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Left   : Gela.Interpretations.Interpretation_Set_Index;
      Right  : Gela.Interpretations.Interpretation_Set_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is

      type Counter is record
         Count : Natural := 0;
         Index : Gela.Interpretations.Interpretation_Index;
      end record;

      type Type_Kind is (Integer, Float);
      type Counter_By_Type is array (Type_Kind) of Counter;
      type Side is (Left_Side, Right_Side);
      type Counter_Array is array (Side) of Counter_By_Type;

      procedure Increment
        (Value    : in out Counter_By_Type;
         Index    : Gela.Interpretations.Interpretation_Index;
         Tipe     : Type_Kind);

      procedure Increment
        (Value    : in out Counter_Array;
         Index    : Gela.Interpretations.Interpretation_Index;
         Count    : Counter_By_Type;
         Tipe     : Type_Kind);

      package Each_Left is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Counters : Counter_Array;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Left;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package body Each_Left is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is

            Left_Cursor : Gela.Interpretations.Cursor'Class renames Cursor;

            package Each_Right is
               type Visiter is new Gela.Interpretations.Up_Visiter with record
                  Tipe      : Gela.Semantic_Types.Type_Index;
                  Type_View : Gela.Types.Type_View_Access;
                  Counters  : Counter_By_Type;
               end record;

               overriding procedure On_Expression
                 (Self   : in out Visiter;
                  Tipe   : Gela.Semantic_Types.Type_Index;
                  Cursor : Gela.Interpretations.Cursor'Class);

            end Each_Right;

            package body Each_Right is

               overriding procedure On_Expression
                 (Self   : in out Visiter;
                  Tipe   : Gela.Semantic_Types.Type_Index;
                  Cursor : Gela.Interpretations.Cursor'Class)
               is
                  Chosen : Gela.Semantic_Types.Type_Index;
                  Type_View : constant Gela.Types.Type_View_Access :=
                    TM.Get (Tipe);
               begin
                  if not Type_View.Assigned then
                     return;
                  elsif Type_View.Is_Integer then
                     Increment (Self.Counters, Cursor.Get_Index, Integer);
                  elsif Type_View.Is_Real then
                     Increment (Self.Counters, Cursor.Get_Index, Float);
                  else  --  FIXME Return after implementation of types
                     null;
                  end if;

                  if Type_View.Is_Expected_Type (Self.Type_View) then
                     if Type_View.Is_Universal
                       and then Type_View.Is_Numeric
                     then
                        Chosen := Self.Tipe;
                     else
                        Chosen := Tipe;
                     end if;

                     Comp.Context.Interpretation_Manager.Add_Expression
                       (Tipe   => Chosen,
                        Down   => (Left_Cursor.Get_Index, Cursor.Get_Index),
                        Result => Set);
                  end if;
               end On_Expression;

            end Each_Right;

            Visiter_Right : aliased Each_Right.Visiter :=
              (Tipe => Tipe, Type_View => TM.Get (Tipe), others => <>);
         begin
            if not Visiter_Right.Type_View.Assigned then
               return;
            end if;

            Each_Expression
              (Comp   => Comp,
               Env    => Env,
               Set    => Right,
               Target => Visiter_Right);

            if Visiter_Right.Type_View.Is_Integer then
               Increment
                 (Self.Counters,
                  Cursor.Get_Index,
                  Visiter_Right.Counters,
                  Integer);
            elsif Visiter_Right.Type_View.Is_Real then
               Increment
                 (Self.Counters,
                  Cursor.Get_Index,
                  Visiter_Right.Counters,
                  Float);
            else  --  FIXME Drop after implementation of types
               null;
            end if;
         end On_Expression;

      end Each_Left;

      ---------------
      -- Increment --
      ---------------

      procedure Increment
        (Value    : in out Counter_By_Type;
         Index    : Gela.Interpretations.Interpretation_Index;
         Tipe     : Type_Kind) is
      begin
         Value (Tipe).Count := Value (Tipe).Count + 1;
         Value (Tipe).Index := Index;
      end Increment;

      ---------------
      -- Increment --
      ---------------

      procedure Increment
        (Value    : in out Counter_Array;
         Index    : Gela.Interpretations.Interpretation_Index;
         Count    : Counter_By_Type;
         Tipe     : Type_Kind)
      is
         L_Val : Counter_By_Type renames Value (Left_Side);
         R_Val : Counter_By_Type renames Value (Right_Side);
      begin
         Increment (L_Val, Index, Tipe);
         R_Val (Tipe) := Count (Tipe);
      end Increment;

      Visiter : aliased Each_Left.Visiter;
      L_Val : Counter_By_Type renames Visiter.Counters (Left_Side);
      R_Val : Counter_By_Type renames Visiter.Counters (Right_Side);

      Int_Matcher   : Integer_Type_Matcher.Type_Matcher_Access;
      Float_Matcher : Float_Type_Matcher.Type_Matcher_Access;
   begin
      Set := 0;
      Each_Expression
        (Comp   => Comp,
         Env    => Env,
         Set    => Left,
         Target => Visiter);

      if L_Val (Integer).Count = 1 and R_Val (Integer).Count = 1 then
         Int_Matcher := new Integer_Type_Matcher.Type_Matcher;

         Comp.Context.Interpretation_Manager.Add_Expression_Category
           (Match  => Gela.Interpretations.Type_Matcher_Access (Int_Matcher),
            Down   => (L_Val (Integer).Index, R_Val (Integer).Index),
            Result => Set);
      end if;

      if L_Val (Float).Count = 1 and R_Val (Float).Count = 1 then
         Float_Matcher := new Float_Type_Matcher.Type_Matcher;

         Comp.Context.Interpretation_Manager.Add_Expression_Category
           (Match  => Gela.Interpretations.Type_Matcher_Access (Float_Matcher),
            Down   => (L_Val (Float).Index, R_Val (Float).Index),
            Result => Set);
      end if;
   end Simple_Expression_Range;

   --------------------
   -- Discrete_Range --
   --------------------

   procedure Discrete_Range
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Down_Left  : out Gela.Interpretations.Interpretation_Index;
      Down_Right : out Gela.Interpretations.Interpretation_Index;
      Tipe       : out Gela.Semantic_Types.Type_Index)
   is

      package Each_Left is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Result    : Gela.Semantic_Types.Type_Index := 0;
            Count     : Natural := 0;
            Left      : Gela.Interpretations.Interpretation_Index;
            Right     : Gela.Interpretations.Interpretation_Index;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Left;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package body Each_Left is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is

            package Each_Right is
               type Visiter is new Gela.Interpretations.Up_Visiter with record
                  Tipe      : Gela.Semantic_Types.Type_Index;
                  Type_View : Gela.Types.Type_View_Access;
                  Result    : Gela.Semantic_Types.Type_Index;
                  Count     : Natural;
                  Right     : Gela.Interpretations.Interpretation_Index;
               end record;

               overriding procedure On_Expression
                 (Self   : in out Visiter;
                  Tipe   : Gela.Semantic_Types.Type_Index;
                  Cursor : Gela.Interpretations.Cursor'Class);

            end Each_Right;

            package body Each_Right is

               overriding procedure On_Expression
                 (Self   : in out Visiter;
                  Tipe   : Gela.Semantic_Types.Type_Index;
                  Cursor : Gela.Interpretations.Cursor'Class)
               is
                  use type Gela.Semantic_Types.Type_Index;

                  Type_View : constant Gela.Types.Type_View_Access :=
                    TM.Get (Tipe);
               begin
                  if not Type_View.Assigned or else
                    not Type_View.Is_Integer
                  then
                     null;
                  elsif not Type_View.Is_Universal then
                     if Self.Type_View.Is_Expected_Type (Type_View) then
                        Self.Result := Tipe;
                        Self.Count := Self.Count + 1;
                        Self.Right := Cursor.Get_Index;
                     end if;
                  elsif Self.Type_View.Is_Universal then
                     if Type_View.Is_Expected_Type (Self.Type_View) then
                        Self.Result := Self.Tipe;
                        Self.Count := Self.Count + 1;
                        Self.Right := Cursor.Get_Index;
                     end if;
                  elsif Tipe = Self.Tipe then
                     Self.Result := TM.Universal_Integer; --  FIXME Root_Int
                     Self.Count := Self.Count + 1;
                     Self.Right := Cursor.Get_Index;
                  end if;
               end On_Expression;

            end Each_Right;

            Type_View : constant Gela.Types.Type_View_Access := TM.Get (Tipe);
            Visiter_Right : aliased Each_Right.Visiter :=
              (Tipe => Tipe, Type_View => Type_View,
               Result => 0, Count => 0, Right => 0);
         begin
            if Type_View.Assigned and then Type_View.Is_Integer then
               --  FIXME .Is_Discrete
               Each_Expression
                 (Comp   => Comp,
                  Env    => Env,
                  Set    => Right,
                  Target => Visiter_Right);

               if Visiter_Right.Count > 0 then
                  Self.Count := Self.Count + Visiter_Right.Count;
                  Self.Result := Visiter_Right.Result;
                  Self.Right := Visiter_Right.Right;
                  Self.Left := Cursor.Get_Index;
               end if;
            end if;
         end On_Expression;

      end Each_Left;

      Visiter : Each_Left.Visiter;
   begin
      Each_Expression
        (Comp   => Comp,
         Env    => Env,
         Set    => Left,
         Target => Visiter);

      if Visiter.Count = 1 then
         Down_Left := Visiter.Left;
         Down_Right := Visiter.Right;
         Tipe := Visiter.Result;
      else
         Down_Left := 0;
         Down_Right := 0;
         Tipe := 0;
      end if;
   end Discrete_Range;

   --------------------
   -- Discrete_Range --
   --------------------

   procedure Discrete_Range
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Tipe       : out Gela.Semantic_Types.Type_Index)
   is
      Ignore_Left  : Gela.Interpretations.Interpretation_Index;
      Ignore_Right : Gela.Interpretations.Interpretation_Index;
   begin
      Discrete_Range
        (Comp       => Comp,
         Env        => Env,
         Left       => Left,
         Right      => Right,
         Tipe       => Tipe,
         Down_Left  => Ignore_Left,
         Down_Right => Ignore_Right);
   end Discrete_Range;

   --------------------------
   -- Discrete_Range_Lower --
   --------------------------

   procedure Discrete_Range_Lower
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Result     : out Gela.Interpretations.Interpretation_Index)
   is
      Ignore_Tipe  : Gela.Semantic_Types.Type_Index;
      Ignore_Right : Gela.Interpretations.Interpretation_Index;
   begin
      Discrete_Range
        (Comp       => Comp,
         Env        => Env,
         Left       => Left,
         Right      => Right,
         Tipe       => Ignore_Tipe,
         Down_Left  => Result,
         Down_Right => Ignore_Right);
   end Discrete_Range_Lower;

   --------------------------
   -- Discrete_Range_Upper --
   --------------------------

   procedure Discrete_Range_Upper
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Result     : out Gela.Interpretations.Interpretation_Index)
   is
      Ignore_Tipe  : Gela.Semantic_Types.Type_Index;
      Ignore_Left  : Gela.Interpretations.Interpretation_Index;
   begin
      Discrete_Range
        (Comp       => Comp,
         Env        => Env,
         Left       => Left,
         Right      => Right,
         Tipe       => Ignore_Tipe,
         Down_Left  => Ignore_Left,
         Down_Right => Result);
   end Discrete_Range_Upper;

   --------------------
   -- String_Literal --
   --------------------

   procedure String_Literal
     (Comp   : Gela.Compilations.Compilation_Access;
      Token  : Gela.Lexical_Types.Token_Count;
      Result : out Gela.Interpretations.Interpretation_Set_Index)
   is
      pragma Unreferenced (Token);
      Matcher : constant String_Type_Matcher.Type_Matcher_Access :=
        new String_Type_Matcher.Type_Matcher;
   begin
      Result := 0;

      Comp.Context.Interpretation_Manager.Add_Expression_Category
        (Match => Gela.Interpretations.Type_Matcher_Access (Matcher),
         Down   => (1 .. 0 => 0),
         Result => Result);

   end String_Literal;

   ----------------------
   -- To_The_Same_Type --
   ----------------------

   procedure To_The_Same_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is

      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            null;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self, Cursor);
         begin
            To_Type
              (Comp    => Comp,
               Env     => Env,
               Type_Up => Tipe,
               Expr_Up => Expr_Up,
               Result  => Result);
         end On_Expression;

      end Each;

      Visiter    : Each.Visiter;
   begin
      Result := 0;

      Each_Expression (Comp   => Comp,
                       Env    => Env,
                       Set    => Type_Up,
                       Target => Visiter);
   end To_The_Same_Type;

   -------------
   -- To_Type --
   -------------

   procedure To_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is
      Index      : Gela.Interpretations.Interpretation_Index;
      Type_Index : Gela.Semantic_Types.Type_Index;
   begin
      Get_Subtype
        (Comp,
         Env    => Env,
         Set    => Type_Up,
         Index  => Index,
         Result => Type_Index);

      To_Type
        (Comp    => Comp,
         Env     => Env,
         Type_Up => Type_Index,
         Expr_Up => Expr_Up,
         Result  => Result);
   end To_Type;

   -------------
   -- To_Type --
   -------------

   procedure To_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Semantic_Types.Type_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Env);

      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Type_Index : Gela.Semantic_Types.Type_Index;
            Index      : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

         overriding procedure On_Expression_Category
           (Self   : in out Visiter;
            Match  : not null Gela.Interpretations.Type_Matcher_Access;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      View : constant Gela.Types.Type_View_Access := TM.Get (Type_Up);

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Name);
         begin
            Self.Index := Cursor.Get_Index;
         end On_Defining_Name;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            This_Type : constant Gela.Types.Type_View_Access :=
              TM.Get (Tipe);
         begin
            if This_Type.Assigned and then
              This_Type.Is_Expected_Type (View)
            then
               Self.Index := Cursor.Get_Index;
            end if;
         end On_Expression;

         overriding procedure On_Expression_Category
           (Self   : in out Visiter;
            Match  : not null Gela.Interpretations.Type_Matcher_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Cursor);
            use type Gela.Interpretations.Interpretation_Index;
         begin
            View.Visit (Match.all);

            if Match.Is_Matched and Self.Index = 0 then
               IM.Get_Expression_Index
                 (Tipe   => Self.Type_Index,
                  Result => Self.Index);
            end if;
         end On_Expression_Category;

      end Each;

      Cursor  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Expr_Up);
      Visiter : aliased Each.Visiter;
   begin
      Visiter.Type_Index := Type_Up;

      if View.Assigned then
         while Cursor.Has_Element loop
            Cursor.Visit (Visiter'Access);
            Cursor.Next;
         end loop;
      end if;

      Result := Visiter.Index;
   end To_Type;

   ----------------------
   -- To_Type_Category --
   ----------------------

   procedure To_Type_Category
     (Comp     : Gela.Compilations.Compilation_Access;
      Up       : Gela.Interpretations.Interpretation_Set_Index;
      Tipe     : Gela.Semantic_Types.Type_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is

      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with null record;

         overriding procedure On_Expression_Category
           (Self   : in out Visiter;
            Match  : not null Gela.Interpretations.Type_Matcher_Access;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      View : constant Gela.Types.Type_View_Access := TM.Get (Tipe);

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Expression_Category
           (Self   : in out Visiter;
            Match  : not null Gela.Interpretations.Type_Matcher_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
         begin
            View.Visit (Match.all);

            if Match.Is_Matched then
               Result := Cursor.Get_Index;
            end if;
         end On_Expression_Category;

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      Cursor  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Up);
      Visiter : aliased Each.Visiter;
   begin
      Result := 0;

      while Cursor.Has_Element loop
         Cursor.Visit (Visiter'Access);
         Cursor.Next;
      end loop;
   end To_Type_Category;

   ------------------------------
   -- To_Type_Or_The_Same_Type --
   ------------------------------

   procedure To_Type_Or_The_Same_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is
      use type Gela.Semantic_Types.Type_Index;

      Index      : Gela.Interpretations.Interpretation_Index;
      Type_Index : Gela.Semantic_Types.Type_Index;
   begin
      Get_Subtype
        (Comp,
         Env    => Env,
         Set    => Type_Up,
         Index  => Index,
         Result => Type_Index);

      if Type_Index = 0 then
         To_The_Same_Type
           (Comp    => Comp,
            Env     => Env,
            Type_Up => Type_Up,
            Expr_Up => Expr_Up,
            Result  => Result);
      else
         To_Type
           (Comp    => Comp,
            Env     => Env,
            Type_Up => Type_Index,
            Expr_Up => Expr_Up,
            Result  => Result);
      end if;
   end To_Type_Or_The_Same_Type;

   procedure Variant_Part
     (Comp     : Gela.Compilations.Compilation_Access;
      Env      : Gela.Semantic_Types.Env_Index;
      Name_Up  : Gela.Interpretations.Interpretation_Set_Index;
      Variants : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      Tuples : constant Gela.Interpretations.Interpretation_Tuple_Index_Array
        := IM.Get_Tuple_List (Variants);

      package Each_Name is
         type Visiter is new Gela.Interpretations.Up_Visiter with null record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Name;

      package body Each_Name is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self, Cursor);
            Chosen  : Gela.Interpretations.Interpretation_Index := 0;
            Output  : Gela.Interpretations.Interpretation_Index_Array
              (Tuples'Range);
         begin
            for J in Tuples'Range loop
               declare
                  Tuple : constant Gela.Interpretations
                    .Interpretation_Set_Index_Array
                      := IM.Get_Tuple (Tuples (J));
                  List   : Gela.Interpretations.Interpretation_Index_Array
                    (Tuple'Range);
               begin
                  for K in Tuple'Range loop
                     To_Type
                       (Comp    => Comp,
                        Env     => Env,
                        Type_Up => Tipe,
                        Expr_Up => Tuple (K),
                        Result  => List (K));
                  end loop;

                  Chosen := 0;

                  for K in reverse List'Range loop
                     IM.Get_Tuple_Index (List (K), Chosen, Chosen);
                  end loop;

                  Output (J) := Chosen;
               end;
            end loop;

            Chosen := 0;

            for J in reverse Output'Range loop
               IM.Get_Tuple_Index (Output (J), Chosen, Chosen);
            end loop;

            Result := Chosen;
         end On_Expression;

      end Each_Name;

      Visiter : aliased Each_Name.Visiter;
   begin
      Result := 0;

      Each_Expression
        (Comp   => Comp,
         Env    => Env,
         Set    => Name_Up,
         Target => Visiter);

   end Variant_Part;

   ----------------
   -- Wrap_Tuple --
   ----------------

   procedure Wrap_Tuple
     (Self   : access Gela.Interpretations.Up_Visiter'Class;
      IM     : Gela.Interpretations.Interpretation_Manager_Access;
      Value  : Gela.Interpretations.Interpretation_Set_Index_Array;
      Found  : access Gela.Interpretations.Interpretation_Index;
      Chosen : out Gela.Interpretations.Interpretation_Index)
   is
      List   : Gela.Interpretations.Interpretation_Index_Array (Value'Range) :=
        (others => 0);
   begin
      Chosen := 0;

      for J in Value'Range loop
         declare
            use type Gela.Interpretations.Interpretation_Index;

            Cursor : Gela.Interpretations.Cursor'Class :=
              IM.Get_Cursor (Value (J));
         begin
            while Cursor.Has_Element loop
               Cursor.Visit (Self);

               if Found.all /= 0 then
                  List (J) := Found.all;
                  Found.all := 0;
               end if;

               Cursor.Next;
            end loop;

            if List (J) = 0 then
               return;
            end if;
         end;
      end loop;

      for J in reverse List'Range loop
         IM.Get_Tuple_Index (List (J), Chosen, Chosen);
      end loop;
   end Wrap_Tuple;

end Gela.Resolve;
