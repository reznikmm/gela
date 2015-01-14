with Gela.Defining_Name_Cursors;
with Gela.Element_Visiters;
with Gela.Elements.Composite_Constraints;
with Gela.Elements.Defining_Names;
with Gela.Elements.Range_Attribute_References;
with Gela.Elements.Simple_Expression_Ranges;
with Gela.Environments;
with Gela.Type_Managers;
with Gela.Type_Views;

package body Gela.Resolve is

   procedure Get_Subtype
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Index  : out Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Semantic_Types.Type_Index);

   procedure To_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Type_Up : Gela.Semantic_Types.Type_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);

   procedure Each_Expression
     (Comp   : Gela.Compilations.Compilation_Access;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Target : in out Gela.Interpretations.Visiter'Class);

   Use_This_Interretation : constant Gela.Interpretations.Interpretation_Index
     := Gela.Interpretations.Interpretation_Index'Last;

   procedure Wrap_Tuple
     (Self   : access Gela.Interpretations.Visiter'Class;
      IM     : Gela.Interpretations.Interpretation_Manager_Access;
      Value  : Gela.Interpretations.Interpretation_Set_Index_Array;
      Found  : access Gela.Interpretations.Interpretation_Index;
      Chosen : out Gela.Interpretations.Interpretation_Index);
   --  For each Value (J), iterate over its interpretation set and call Self to
   --  resolve. Read resolved value from Found. Wrap each resolved value in
   --  down interpretation, then return its index as Chosen
   --  When Found = Use_This_Interretation use index of current interpretation.

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

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Attr       : Gela.Lexical_Types.Predefined_Symbols.Symbol;
      Type_Index : Gela.Semantic_Types.Type_Index;
      Index      : Gela.Interpretations.Interpretation_Index;
   begin
      Set := 0;

      if Token = 0 then
         Attr := Symbol;
      else
         Attr := Gela.Lexical_Types.Predefined_Symbols.Range_Symbol;
      end if;

      case Attr is
         when Gela.Lexical_Types.Predefined_Symbols.Last =>
            Get_Subtype
              (Comp,
               Env    => Env,
               Set    => Prefix,
               Index  => Index,
               Result => Type_Index);

            Comp.Context.Interpretation_Manager.Add_Expression
              (Tipe   => Type_Index,
               Down   => (1 => Index),
               Result => Set);
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
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Tuple   : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is
      package Each_Tuple is
         type Visiter is new Gela.Interpretations.Visiter with null record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Tuple;

      package Each_Choice is
         type Visiter is new Gela.Interpretations.Visiter with record
            Index : aliased Gela.Interpretations.Interpretation_Index;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Choice;

      IM   : constant Gela.Interpretations.Interpretation_Manager_Access
        := Comp.Context.Interpretation_Manager;

      Type_Index : Gela.Semantic_Types.Type_Index;

      package body Each_Choice is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down);

            Chosen : Gela.Interpretations.Interpretation_Index;
            List   : Gela.Interpretations.Interpretation_Index_Array
              (Value'Range);
         begin
            for J in Value'Range loop
               To_Type
                 (Comp    => Comp,
                  Type_Up => Type_Index,
                  Expr_Up => Value (J),
                  Result  => List (J));
            end loop;

            Chosen := 0;

            for J in reverse List'Range loop
               IM.Get_Tuple_Index (List (J), Chosen, Chosen);
            end loop;

            Self.Index := Chosen;
         end On_Tuple;

      end Each_Choice;

      package body Each_Tuple is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down, Self);
            V      : aliased Each_Choice.Visiter;
            Chosen : Gela.Interpretations.Interpretation_Index;
         begin
            Wrap_Tuple
              (Self   => V'Access,
               IM     => IM,
               Value  => Value,
               Found  => V.Index'Access,
               Chosen => Chosen);

            Result := Chosen;
         end On_Tuple;

      end Each_Tuple;

      Index      : Gela.Interpretations.Interpretation_Index;
      Tuple_Visiter : aliased Each_Tuple.Visiter;
      Cursor        : Gela.Interpretations.Cursor'Class :=
        IM.Get_Cursor (Tuple);
   begin
      Get_Subtype
        (Comp,
         Env    => 0,
         Set    => Type_Up,
         Index  => Index,
         Result => Type_Index);

      while Cursor.Has_Element loop
         Cursor.Visit (Tuple_Visiter'Access);
         Cursor.Next;
      end loop;
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

      package Each_Choice is
         type Visiter is new Gela.Interpretations.Visiter with record
            Index : aliased Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Choice;

      package Each_Tuple is
         type Visiter is new Gela.Interpretations.Visiter with null record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Tuple;

      Comp       : Gela.Compilations.Compilation_Access;
      IM         : Gela.Interpretations.Interpretation_Manager_Access;
      Type_Index : Gela.Semantic_Types.Type_Index;

      package body Each_Choice is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Tipe, Down);
         begin
            Self.Index := Use_This_Interretation;
         end On_Expression;

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down);
            TM : constant Gela.Type_Managers.Type_Manager_Access :=
              Comp.Context.Types;
            Type_View : constant Gela.Type_Views.Type_View_Access :=
              TM.Get (Type_Index);
            Name : constant Gela.Elements.Defining_Names.Defining_Name_Access
              := Type_View.Get_Discriminant (Symbol);
         begin
            if Name.Assigned then
               IM.Get_Defining_Name_Index (Name, Self.Index);
            else
               Self.Index := 0;
            end if;
         end On_Symbol;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down);
            use type Gela.Semantic_Types.Type_Index;

            Chosen : Gela.Interpretations.Interpretation_Index := 0;
            List   : Gela.Interpretations.Interpretation_Index_Array
              (Value'Range);
         begin
            --  Resolve expression of association
            Interpretation
              (Comp   => Comp,
               Env    => Env,
               Set    => Value (Value'First),
               Result => List (Value'First));

--            if Type_Index /= 0 then
            Wrap_Tuple
              (Self   => Self'Access,
               IM     => IM,
               Value  => Value (Value'First + 1 .. Value'Last),
               Found  => Self.Index'Access,
               Chosen => Chosen);

            IM.Get_Tuple_Index (List (Value'First), Chosen, Chosen);

            Self.Index := Chosen;
         end On_Tuple;

      end Each_Choice;

      package body Each_Constraint is

         overriding procedure Composite_Constraint
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access)
         is
            pragma Unreferenced (Node, Self);

            Index         : Gela.Interpretations.Interpretation_Index;
            Tuple_Visiter : aliased Each_Tuple.Visiter;
            Cursor        : Gela.Interpretations.Cursor'Class :=
              IM.Get_Cursor (Constr);
         begin
            Get_Subtype
              (Comp,
               Env    => Env,
               Set    => Type_Up,
               Index  => Index,
               Result => Type_Index);

            while Cursor.Has_Element loop
               Cursor.Visit (Tuple_Visiter'Access);
               Cursor.Next;
            end loop;
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

      package body Each_Tuple is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down, Self);
            Chosen : Gela.Interpretations.Interpretation_Index;
            V      : aliased Each_Choice.Visiter;
         begin
            Wrap_Tuple
              (Self   => V'Access,
               IM     => IM,
               Value  => Value,
               Found  => V.Index'Access,
               Chosen => Chosen);

            Result := Chosen;
         end On_Tuple;

      end Each_Tuple;

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
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Target : in out Gela.Interpretations.Visiter'Class)
   is
      package Each is
         type Visiter is new Gela.Interpretations.Visiter with record
            null;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Expression (Tipe, Down);
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

   -------------------
   -- Function_Call --
   -------------------

   procedure Function_Call
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Args   : Gela.Interpretations.Interpretation_Set_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is

      use type Gela.Interpretations.Interpretation_Index_Array;

      No_Args_Allowed : constant Boolean := True;
      --  FIXME Replace with actual check

      package Each_Prefix is
         type Visiter is new Gela.Interpretations.Visiter with record
            Index  : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Prefix;

      package Each_Arg is
         type Visiter is new Gela.Interpretations.Visiter with record
            Index  : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Arg;

      package Each_Association is
         type Visiter is new Gela.Interpretations.Visiter with record
            Index  : aliased Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Association;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      package body Each_Arg is
         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down);

            V      : aliased Each_Association.Visiter;
            Chosen : Gela.Interpretations.Interpretation_Index;
         begin
            Wrap_Tuple
              (Self   => V'Access,
               IM     => IM,
               Value  => Value,
               Found  => V.Index'Access,
               Chosen => Chosen);

            Comp.Context.Interpretation_Manager.Add_Expression
              (Tipe   => Comp.Context.Types.Universal_Integer,
               Down   => Self.Index & Chosen,
               Result => Set);
         end On_Tuple;
      end Each_Arg;

      package body Each_Association is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down);

            Chosen : Gela.Interpretations.Interpretation_Index;
            List   : Gela.Interpretations.Interpretation_Index_Array
              (Value'Range);
         begin
            for J in Value'Range loop
               Interpretation
                 (Comp   => Comp,
                  Env    => Env,
                  Set    => Value (J),
                  Result => List (J));
            end loop;

            Chosen := 0;

            for J in reverse List'Range loop
               IM.Get_Tuple_Index (List (J), Chosen, Chosen);
            end loop;

            Self.Index := Chosen;
         end On_Tuple;

      end Each_Association;

      package body Each_Prefix is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Name, Down);
            Visiter : aliased Each_Arg.Visiter := (Index => Self.Index);
            Arg     : Gela.Interpretations.Cursor'Class :=
              IM.Get_Cursor (Args);
         begin
            if Arg.Has_Element then
               while Arg.Has_Element loop
                  Arg.Visit (Visiter'Access);
                  Arg.Next;
               end loop;
            elsif No_Args_Allowed then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => Comp.Context.Types.Universal_Integer,
                  Down   => Visiter.Index & 0,
                  Result => Set);
            end if;
         end On_Defining_Name;

      end Each_Prefix;

      Cursor  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Prefix);
   begin
      Set := 0;

      while Cursor.Has_Element loop
         declare
            Visiter : aliased Each_Prefix.Visiter :=
              (Index => Cursor.Get_Index);
         begin
            Cursor.Visit (Visiter'Access);
            Cursor.Next;
         end;
      end loop;
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
      pragma Unreferenced (Env);

      package Each is
         type Visiter is new Gela.Interpretations.Visiter with record
            Index  : Gela.Interpretations.Interpretation_Index := 0;
            Result : Gela.Semantic_Types.Type_Index := 0;
         end record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Cursor : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Set);

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Self);
            pragma Unreferenced (Down);
         begin
            Self.Result := TM.Type_By_Name (Name);
            Self.Index := Cursor.Get_Index;
         end On_Defining_Name;

      end Each;

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
         type Visiter is new Gela.Interpretations.Visiter with record
            Prev   : Gela.Interpretations.Interpretation_Index := 0;
            Result : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      package body Each is

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Symbol, Down);
         begin
            --  Skip symbos
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
         type Visiter is new Gela.Interpretations.Visiter with null record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      ES : constant Gela.Environments.Environment_Set_Access :=
        Comp.Context.Environment_Set;

      Cursor : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Prefix);

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Self);
            pragma Unreferenced (Down);
            Found : aliased Boolean;
            NC : Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
              := ES.Visible (Env, Name, Symbol, Found'Access);
         begin
            while NC.Has_Element loop
               IM.Add_Defining_Name
                 (Name   => NC.Element,
                  Down   => (1 => Cursor.Get_Index),
                  Result => Set);

               NC.Next;
            end loop;
         end On_Defining_Name;

      end Each;

      Visiter : aliased Each.Visiter;
   begin
      Set := 0;
      while Cursor.Has_Element loop
         Cursor.Visit (Visiter'Access);
         Cursor.Next;
      end loop;
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
      pragma Unreferenced (Env);
      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      Cursor_Left  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Left);
      Cursor_Right : Gela.Interpretations.Cursor'Class :=
        IM.Get_Cursor (Right);

   begin
      Set := 0;
      while Cursor_Left.Has_Element loop
         while Cursor_Right.Has_Element loop
            --  FIX ME: compare types of left and right interpretation
            Comp.Context.Interpretation_Manager.Add_Expression
              (Tipe   => 0,
               Down   => (Cursor_Left.Get_Index,
                          Cursor_Right.Get_Index),
               Result => Set);

            Cursor_Right.Next;
         end loop;

         Cursor_Left.Next;
      end loop;
   end Simple_Expression_Range;

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
      pragma Unreferenced (Env);

      package Each is
         type Visiter is new Gela.Interpretations.Visiter with record
            null;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Self);
            pragma Unreferenced (Down);
         begin
            To_Type
              (Comp    => Comp,
               Type_Up => Tipe,
               Expr_Up => Expr_Up,
               Result  => Result);
         end On_Expression;

      end Each;

      Visiter    : Each.Visiter;
   begin
      Result := 0;

      Each_Expression (Comp   => Comp,
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
         Type_Up => Type_Index,
         Expr_Up => Expr_Up,
         Result  => Result);
   end To_Type;

   -------------
   -- To_Type --
   -------------

   procedure To_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Type_Up : Gela.Semantic_Types.Type_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is

      package Each is
         type Visiter is new Gela.Interpretations.Visiter with record
            Type_Index : Gela.Semantic_Types.Type_Index;
            Index      : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      Cursor : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Expr_Up);

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Name);
            pragma Unreferenced (Down);
         begin
            Self.Index := Cursor.Get_Index;
         end On_Defining_Name;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Tipe);
            pragma Unreferenced (Down);
         begin
            Self.Index := Cursor.Get_Index;
         end On_Expression;

      end Each;

      Visiter : aliased Each.Visiter;
   begin
      Visiter.Type_Index := Type_Up;

      while Cursor.Has_Element loop
         Cursor.Visit (Visiter'Access);
         Cursor.Next;
      end loop;

      Result := Visiter.Index;
   end To_Type;

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
            Type_Up => Type_Index,
            Expr_Up => Expr_Up,
            Result  => Result);
      end if;
   end To_Type_Or_The_Same_Type;

   procedure Variant_Part
     (Comp     : Gela.Compilations.Compilation_Access;
      Name_Up  : Gela.Interpretations.Interpretation_Set_Index;
      Variants : Gela.Interpretations.Interpretation_Set_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Name_Up);

      package Each_Variant is
         type Visiter is new Gela.Interpretations.Visiter with record
            null;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Variant;

      package Each_Choice is
         type Visiter is new Gela.Interpretations.Visiter with record
            Index  : aliased Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array);

      end Each_Choice;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      package body Each_Variant is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down, Self);

            V      : aliased Each_Choice.Visiter;
            Chosen : Gela.Interpretations.Interpretation_Index;
         begin
            Wrap_Tuple
              (Self   => V'Access,
               IM     => IM,
               Value  => Value,
               Found  => V.Index'Access,
               Chosen => Chosen);

            Result := Chosen;
         end On_Tuple;

      end Each_Variant;

      package body Each_Choice is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array;
            Down  : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down);

            Chosen : Gela.Interpretations.Interpretation_Index;
            List   : Gela.Interpretations.Interpretation_Index_Array
              (Value'Range);
         begin
            for J in Value'Range loop
               declare
                  Cursor : Gela.Interpretations.Cursor'Class :=
                    IM.Get_Cursor (Value (J));
               begin
                  List (J) := 0;
                  while Cursor.Has_Element loop
                     List (J) := Cursor.Get_Index;
                     Cursor.Next;
                  end loop;
               end;
            end loop;

            Chosen := 0;

            for J in reverse List'Range loop
               IM.Get_Tuple_Index (List (J), Chosen, Chosen);
            end loop;

            Self.Index := Chosen;
         end On_Tuple;

      end Each_Choice;

      Visiter : aliased Each_Variant.Visiter;
      Cursor  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Variants);
   begin
      Result := 0;

      while Cursor.Has_Element loop
         Cursor.Visit (Visiter'Access);
         Cursor.Next;
      end loop;
   end Variant_Part;

   ----------------
   -- Wrap_Tuple --
   ----------------

   procedure Wrap_Tuple
     (Self   : access Gela.Interpretations.Visiter'Class;
      IM     : Gela.Interpretations.Interpretation_Manager_Access;
      Value  : Gela.Interpretations.Interpretation_Set_Index_Array;
      Found  : access Gela.Interpretations.Interpretation_Index;
      Chosen : out Gela.Interpretations.Interpretation_Index)
   is
      List   : Gela.Interpretations.Interpretation_Index_Array (Value'Range) :=
        (others => 0);
   begin
      for J in Value'Range loop
         declare
            use type Gela.Interpretations.Interpretation_Index;

            Cursor : Gela.Interpretations.Cursor'Class :=
              IM.Get_Cursor (Value (J));
         begin
            while Cursor.Has_Element loop
               Cursor.Visit (Self);

               if Found.all = Use_This_Interretation then
                  List (J) := Cursor.Get_Index;
                  Found.all := 0;
               elsif Found.all /= 0 then
                  List (J) := Found.all;
                  Found.all := 0;
               end if;

               Cursor.Next;
            end loop;

         end;
      end loop;

      Chosen := 0;

      for J in reverse List'Range loop
         IM.Get_Tuple_Index (List (J), Chosen, Chosen);
      end loop;
   end Wrap_Tuple;

end Gela.Resolve;
