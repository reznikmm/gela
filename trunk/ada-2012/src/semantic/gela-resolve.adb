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
   --  Resolve given interpretation set as expression. So ingnore symbol and
   --  others non-expression interpretations. Translate defining name into
   --  expression. On_Defining_Name called when Name doen't have a type
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

      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Result : Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each;

      package body Each is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Cursor);
         begin
            To_Type (Comp    => Comp,
                     Env     => Env,
                     Type_Up => Tipe,
                     Expr_Up => Right,
                     Result  => Self.Result);
         end On_Expression;

      end Each;

      Visiter : aliased Each.Visiter;
   begin
      --  ARM 5.2 (4/2)
      Each_Expression (Comp   => Comp,
                       Env    => Env,
                       Set    => Left,
                       Target => Visiter);

      Result := Visiter.Result;
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
         when Gela.Lexical_Types.Predefined_Symbols.First |
              Gela.Lexical_Types.Predefined_Symbols.Last =>

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
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Tuple   : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is
      package Each_Tuple is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Type_Index : Gela.Semantic_Types.Type_Index;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Tuple;

      package Each_Choice is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Type_Index : Gela.Semantic_Types.Type_Index;
            Index      : aliased Gela.Interpretations.Interpretation_Index;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Choice;

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

      package body Each_Choice is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is

            Chosen : Gela.Interpretations.Interpretation_Index;
            List   : Gela.Interpretations.Interpretation_Index_Array
              (Value'Range);
         begin
            for J in Value'Range loop
               To_Type
                 (Comp    => Comp,
                  Env     => Env,
                  Type_Up => Self.Type_Index,
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
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            V  : aliased Each_Choice.Visiter :=
              (Type_Index => Self.Type_Index, Index => 0);
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

      package body Each_Expr is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Cursor, Self);
            Tuple_Visiter : aliased Each_Tuple.Visiter := (Type_Index => Tipe);
            Pos  : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Tuple);
         begin
            while Pos.Has_Element loop
               Pos.Visit (Tuple_Visiter'Access);
               Pos.Next;
            end loop;
         end On_Expression;

      end Each_Expr;

      Expr_Visiter : aliased Each_Expr.Visiter;
   begin
      Result := 0;
      Each_Expression (Comp   => Comp,
                       Env    => Env,
                       Set    => Type_Up,
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

      package Each_Choice is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index : aliased Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class);

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Choice;

      package Each_Tuple is
         type Visiter is new Gela.Interpretations.Up_Visiter with null record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Tuple;

      Comp       : Gela.Compilations.Compilation_Access;
      IM         : Gela.Interpretations.Interpretation_Manager_Access;
      Type_Index : Gela.Semantic_Types.Type_Index;

      package body Each_Choice is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Tipe);
         begin
            Self.Index := Cursor.Get_Index;
         end On_Expression;

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Cursor);
            use type Gela.Types.Type_View_Access;

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
                  Name := Value.Get_Discriminant (Symbol);

                  if Name.Assigned then
                     IM.Get_Defining_Name_Index (Name, On_Symbol.Self.Index);
                  end if;
               end Untagged_Record;

            end Type_Visiters;

            TM : constant Gela.Type_Managers.Type_Manager_Access :=
              Comp.Context.Types;
            Type_View : constant Gela.Types.Type_View_Access :=
              TM.Get (Type_Index);
            Visiter : Type_Visiters.Type_Visitor;
         begin
            Self.Index := 0;
            Type_View.Visit_If_Assigned (Visiter);
         end On_Symbol;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
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
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            pragma Unreferenced (Self);
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
      Args   : Gela.Interpretations.Interpretation_Set_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is

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

      package Each_Arg is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index   : Gela.Interpretations.Interpretation_Index := 0;
            Profile : Gela.Profiles.Profile_Access;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Arg;

      package Each_Array_Arg is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index      : Gela.Interpretations.Interpretation_Index := 0;
            Tipe       : Gela.Semantic_Types.Type_Index;
            View       : Gela.Types.Arrays.Array_Type_Access;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Array_Arg;

      package Each_Association is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index   : aliased Gela.Interpretations.Interpretation_Index := 0;
            Profile : Gela.Profiles.Profile_Access;
            Count   : Natural := 0;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Association;

      package Each_Array_Association is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index : aliased Gela.Interpretations.Interpretation_Index := 0;
            View       : Gela.Types.Arrays.Array_Type_Access;
            Count      : Natural := 0;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Array_Association;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package body Each_Arg is
         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            use type Gela.Interpretations.Interpretation_Index;

            V      : aliased Each_Association.Visiter :=
              (Index   => 0,
               Profile => Self.Profile,
               Count   => 0);
            Chosen : Gela.Interpretations.Interpretation_Index;
         begin
            Wrap_Tuple
              (Self   => V'Access,
               IM     => IM,
               Value  => Value,
               Found  => V.Index'Access,
               Chosen => Chosen);

            if Chosen /= 0 then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => V.Profile.Return_Type,
                  Down   => Self.Index & Chosen,
                  Result => Set);
            end if;
         end On_Tuple;
      end Each_Arg;

      package body Each_Array_Arg is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            use type Gela.Interpretations.Interpretation_Index;

            V      : aliased Each_Array_Association.Visiter :=
              (Index => 0,
               View  => Self.View,
               Count => 0);
            Chosen : Gela.Interpretations.Interpretation_Index := 0;
         begin
            if V.View.Dimension = Value'Length then
               Wrap_Tuple
                 (Self   => V'Access,
                  IM     => IM,
                  Value  => Value,
                  Found  => V.Index'Access,
                  Chosen => Chosen);
            end if;

            if Chosen /= 0 then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => Self.Tipe,
                  Down   => Self.Index & Chosen,
                  Result => Set);
            end if;
         end On_Tuple;

      end Each_Array_Arg;


      package body Each_Association is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            use type Gela.Interpretations.Interpretation_Index;

            Tipe   : Gela.Semantic_Types.Type_Index;
            Chosen : Gela.Interpretations.Interpretation_Index;
            List   : Gela.Interpretations.Interpretation_Index_Array
              (Value'Range);
         begin
            if Value'Length = 1 then
               if Self.Count < Self.Profile.Length then
                  Self.Count := Self.Count + 1;
                  Tipe := Self.Profile.Get_Type (Self.Count);
                  To_Type (Comp, Env, Tipe, Value (Value'First), Chosen);

                  if Chosen = 0 then
                     Self.Index := 0;
                     return;
                  else
                     List (List'First) := Chosen;
                  end if;
               else
                  Self.Index := 0;
                  return;
               end if;
            else
               for J in Value'Range loop
                  Interpretation
                    (Comp   => Comp,
                     Env    => Env,
                     Set    => Value (J),
                     Result => List (J));
               end loop;
            end if;

            Chosen := 0;

            for J in reverse List'Range loop
               IM.Get_Tuple_Index (List (J), Chosen, Chosen);
            end loop;

            Self.Index := Chosen;
         end On_Tuple;

      end Each_Association;

      package body Each_Array_Association is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            use type Gela.Interpretations.Interpretation_Index;
            Chosen : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index := 0;
         begin
            --  Check if this is positional association
            if Value'Length = 1 then
               Self.Count := Self.Count + 1;
               Tipe := Self.View.all.Index_Types (Self.Count);
               To_Type
                 (Comp, Env, Tipe, Value (Value'First), Chosen);

               if Chosen = 0 then
                  Self.Index := 0;
               else
                  IM.Get_Tuple_Index (Chosen, 0, Self.Index);
               end if;
            else
               Self.Index := 0;
            end if;
         end On_Tuple;

      end Each_Array_Association;

      package body Each_Prefix is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Self);
            Visiter : aliased Each_Arg.Visiter :=
              (Index   => Cursor.Get_Index,
               Profile => TM.Get_Profile (Env, Name));
            Arg : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Args);
         begin
            if not Visiter.Profile.Assigned then
               return;
            end if;

            if Arg.Has_Element then
               while Arg.Has_Element loop
                  Arg.Visit (Visiter'Access);
                  Arg.Next;
               end loop;
            elsif Visiter.Profile.Allow_Empty_Argument_List then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => Visiter.Profile.Return_Type,
                  Down   => Visiter.Index & 0,
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
            View : constant Gela.Types.Type_View_Access := TM.Get (Tipe);
         begin
            if View.Assigned and then View.Is_Array then
               declare
                  Visiter : aliased Each_Array_Arg.Visiter :=
                    (Index => Cursor.Get_Index,
                     View  => Gela.Types.Arrays.Array_Type_Access (View),
                     Tipe  => Tipe);
                  Arg : Gela.Interpretations.Cursor'Class :=
                    IM.Get_Cursor (Args);
               begin
                  while Arg.Has_Element loop
                     Arg.Visit (Visiter'Access);
                     Arg.Next;
                  end loop;
               end;
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
         Env    => Env,
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

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);
         --  Tuple is interpretation of aggregate

      end Each;

      package Each_Association is
         --  Visiter for interpretation of an association of aggregate

         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index : aliased Gela.Interpretations.Interpretation_Index;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Association;

      package Each_Symbol is
         --  Visiter for interpretation of an symbol of association

         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index          : aliased Gela.Interpretations.Interpretation_Index;
            Component_Type : Gela.Semantic_Types.Type_Index;
         end record;

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Cursor : Gela.Interpretations.Cursor'Class);

      end Each_Symbol;

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
            Cursor : Gela.Interpretations.Cursor'Class) is
         begin
            View.Visit (Match.all);

            if Match.Is_Matched then
               Self.Index := Cursor.Get_Index;
            end if;
         end On_Expression_Category;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            pragma Unreferenced (Self);
            use type Gela.Types.Type_View_Access;
            V      : aliased Each_Association.Visiter;
            Chosen : Gela.Interpretations.Interpretation_Index;
         begin
            if View /= null and then View.Is_Record then
               Wrap_Tuple
                 (Self   => V'Access,
                  IM     => IM,
                  Value  => Value,
                  Found  => V.Index'Access,
                  Chosen => Chosen);

               Self.Index := Chosen;
            end if;
         end On_Tuple;
      end Each;

      package body Each_Association is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is

            V      : aliased Each_Symbol.Visiter := (0, 0);
            Expr   : Gela.Interpretations.Interpretation_Index;
            Chosen : Gela.Interpretations.Interpretation_Index;
         begin
            Wrap_Tuple
              (Self   => V'Access,
               IM     => IM,
               Value  => Value (Value'First + 1 .. Value'Last),
               Found  => V.Index'Access,
               Chosen => Chosen);

            --  Resolve expression of association
            To_Type
              (Comp    => Comp,
               Env     => Env,
               Type_Up => V.Component_Type,
               Expr_Up => Value (Value'First),
               Result  => Expr);

            IM.Get_Tuple_Index (Expr, Chosen, Chosen);

            Self.Index := Chosen;
         end On_Tuple;

      end Each_Association;

      package body Each_Symbol is

         overriding procedure On_Symbol
           (Self   : in out Visiter;
            Symbol : Gela.Lexical_Types.Symbol;
            Cursor : Gela.Interpretations.Cursor'Class)
         is
            pragma Unreferenced (Cursor);

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
                     IM.Get_Defining_Name_Index
                       (Name   => Name,
                        Result => On_Symbol.Self.Index);

                     On_Symbol.Self.Component_Type :=
                       TM.Type_Of_Object_Declaration
                         (Env, Name.Enclosing_Element);
                  end if;
               end Untagged_Record;

            end Type_Visiters;

            Visiter : Type_Visiters.Type_Visitor;
         begin
            View.Visit (Visiter);
         end On_Symbol;

      end Each_Symbol;

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
      Name_Up  : Gela.Interpretations.Interpretation_Set_Index;
      Variants : Gela.Interpretations.Interpretation_Set_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Name_Up);

      package Each_Variant is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            null;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Variant;

      package Each_Choice is
         type Visiter is new Gela.Interpretations.Up_Visiter with record
            Index  : aliased Gela.Interpretations.Interpretation_Index := 0;
         end record;

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each_Choice;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      package body Each_Variant is

         overriding procedure On_Tuple
           (Self  : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            pragma Unreferenced (Self);

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
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
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
