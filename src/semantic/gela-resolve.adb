with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with Gela.Defining_Name_Cursors;
with Gela.Element_Visiters;
with Gela.Elements.Composite_Constraints;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Formal_Object_Declarations;
with Gela.Elements.Formal_Type_Declarations;
with Gela.Elements.Generic_Formals;
with Gela.Elements.Generic_Package_Declarations;
with Gela.Elements.Range_Attribute_References;
with Gela.Elements.Simple_Expression_Ranges;
with Gela.Environments;
with Gela.Profiles;
with Gela.Resolve.Type_Matchers;
with Gela.Type_Managers;
with Gela.Types.Arrays;
with Gela.Types.Simple;
with Gela.Types.Untagged_Records;
with Gela.Types.Visitors;

with Gela.Resolve.Each;

package body Gela.Resolve is

   procedure To_Type_Category
     (Comp     : Gela.Compilations.Compilation_Access;
      Up       : Gela.Interpretations.Interpretation_Set_Index;
      Tipe     : Gela.Semantic_Types.Type_View_Index;
      Result   : out Gela.Interpretations.Interpretation_Index);
   --  Fetch Type_Category interpretation from Up that match given Tipe.

   procedure Get_Subtype
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Index  : out Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Semantic_Types.Type_View_Index);

   procedure To_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : access Gela.Types.Type_View'Class;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);

   procedure Discrete_Range
     (Comp       : Gela.Compilations.Compilation_Access;
      Env        : Gela.Semantic_Types.Env_Index;
      Left       : Gela.Interpretations.Interpretation_Set_Index;
      Right      : Gela.Interpretations.Interpretation_Set_Index;
      Down_Left  : out Gela.Interpretations.Interpretation_Index;
      Down_Right : out Gela.Interpretations.Interpretation_Index;
      Tipe       : out Gela.Semantic_Types.Type_View_Index);

   function Array_Matcher
     return not null Gela.Interpretations.Type_Matcher_Access
   is
      Result : constant Type_Matchers.Type_Matcher_Access :=
        new Type_Matchers.Array_Type_Matcher;
   begin
      return Gela.Interpretations.Type_Matcher_Access (Result);
   end Array_Matcher;

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
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is
      use type Gela.Lexical_Types.Symbol;
      use type Gela.Semantic_Types.Type_View_Index;

      IM   : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Index       : Gela.Interpretations.Interpretation_Index;
      Is_Length   : constant Boolean :=
        Symbol = Gela.Lexical_Types.Predefined_Symbols.Length;
      Type_Index  : Gela.Semantic_Types.Type_View_Index;
   begin
      Set := 0;

      case Symbol is
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
               for J in Each.Prefix (IM, TM, Env, Prefix) loop
                  if Is_Length then
                     IM.Add_Expression
                       (Tipe   => TM.Universal_Integer,
                        Down   => (1 => Index),
                        Result => Set);
                  else
                     declare
                        View : constant Gela.Types.Type_View_Access :=
                          TM.Get (J.Expression_Type);
                     begin
                        if View.Assigned and then View.Is_Array then
                           declare
                              Index_Types : constant
                                Gela.Types.Simple.Discrete_Type_Array :=
                                  Gela.Types.Arrays.Array_Type_Access (View)
                                    .all.Index_Types;
                           begin
                              if Index_Types (1).Assigned then
                                 IM.Add_Expression
                                   (Tipe   => Index_Types (1).Type_View_Index,
                                    Down   => (1 => Index),
                                    Result => Set);
                              end if;
                           end;
                        end if;
                     end;
                  end if;
               end loop;
            elsif Is_Length then
               IM.Add_Expression
                 (Tipe   => TM.Universal_Integer,
                  Down   => (1 => Index),
                  Result => Set);
            else
               IM.Add_Expression
                 (Tipe   => Type_Index,
                  Down   => (1 => Index),
                  Result => Set);
            end if;
         when
--                Gela.Lexical_Types.Predefined_Symbols.Adjacent |
              Gela.Lexical_Types.Predefined_Symbols.Ceiling |
--                Gela.Lexical_Types.Predefined_Symbols.Compose |
--                Gela.Lexical_Types.Predefined_Symbols.Copy_Sign |
--                Gela.Lexical_Types.Predefined_Symbols.Exponent |
              Gela.Lexical_Types.Predefined_Symbols.Floor |
              Gela.Lexical_Types.Predefined_Symbols.Fraction |
--                Gela.Lexical_Types.Predefined_Symbols.Image |
--                Gela.Lexical_Types.Predefined_Symbols.Input |
--                Gela.Lexical_Types.Predefined_Symbols.Leading_Part |
              Gela.Lexical_Types.Predefined_Symbols.Machine |
              Gela.Lexical_Types.Predefined_Symbols.Machine_Rounding |
--                Gela.Lexical_Types.Predefined_Symbols.Max |
--                Gela.Lexical_Types.Predefined_Symbols.Min |
              Gela.Lexical_Types.Predefined_Symbols.Mod_Symbol |
              Gela.Lexical_Types.Predefined_Symbols.Model |
              Gela.Lexical_Types.Predefined_Symbols.Pos |
              Gela.Lexical_Types.Predefined_Symbols.Pred |
--                Gela.Lexical_Types.Predefined_Symbols.Remainder |
--                Gela.Lexical_Types.Predefined_Symbols.Round |
              Gela.Lexical_Types.Predefined_Symbols.Rounding |
--                Gela.Lexical_Types.Predefined_Symbols.Scaling |
              Gela.Lexical_Types.Predefined_Symbols.Succ |
              Gela.Lexical_Types.Predefined_Symbols.Truncation |
              Gela.Lexical_Types.Predefined_Symbols.Unbiased_Rounding |
              Gela.Lexical_Types.Predefined_Symbols.Val =>
--                Gela.Lexical_Types.Predefined_Symbols.Value |
--                Gela.Lexical_Types.Predefined_Symbols.Wide_Image |
--                Gela.Lexical_Types.Predefined_Symbols.Wide_Value |
--                Gela.Lexical_Types.Predefined_Symbols.Wide_Wide_Image |
--                Gela.Lexical_Types.Predefined_Symbols.Wide_Wide_Value =>

            Get_Subtype
              (Comp,
               Env    => Env,
               Set    => Prefix,
               Index  => Index,
               Result => Type_Index);

            IM.Add_Attr_Function
              (Kind   => Symbol,
               Tipe   => TM.Get (Type_Index),
               Down   => (1 => Index),
               Result => Set);

         when Gela.Lexical_Types.Predefined_Symbols.Size =>
            Get_Subtype
              (Comp,
               Env    => Env,
               Set    => Prefix,
               Index  => Index,
               Result => Type_Index);

            IM.Add_Expression
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
      IM   : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Tuples : constant Gela.Interpretations.Interpretation_Tuple_Index_Array
        := IM.Get_Tuple_List (Tuple);

      Output : Gela.Interpretations.Interpretation_Index_Array (Tuples'Range);
      Chosen : Gela.Interpretations.Interpretation_Index := 0;
   begin
      Result := 0;

      for X in Each.Expression (IM, TM, Env, Expr_Up) loop
         for J in Tuples'Range loop
            declare
               Value : constant Gela.Interpretations
                 .Interpretation_Set_Index_Array := IM.Get_Tuple (Tuples (J));
               List  : Gela.Interpretations.Interpretation_Index_Array
                 (Value'Range);
            begin
               for K in Value'Range loop
                  To_Type
                    (Comp    => Comp,
                     Env     => Env,
                     Type_Up => TM.Get (X.Expression_Type),
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

         exit;
      end loop;
   end Case_Statement;

   -----------------------
   -- Character_Literal --
   -----------------------

   procedure Character_Literal
     (Comp   : Gela.Compilations.Compilation_Access;
      Result : out Gela.Interpretations.Interpretation_Set_Index)
   is
      Type_Matcher : constant Type_Matchers.Type_Matcher_Access :=
        new Type_Matchers.Character_Type_Matcher;
   begin
      Result := 0;

      Comp.Context.Interpretation_Manager.Add_Expression_Category
        (Match => Gela.Interpretations.Type_Matcher_Access (Type_Matcher),
         Down => (1 .. 0 => 0),
         Result => Result);
   end Character_Literal;

   ----------------
   -- Constraint --
   ----------------

   procedure Constraint
     (Comp       : Gela.Compilations.Compilation_Access;
      Constraint : access Gela.Elements.Element'Class;
      Env        : Gela.Semantic_Types.Env_Index;
      Type_Up    : Gela.Interpretations.Interpretation_Set_Index;
      Constr     : Gela.Interpretations.Interpretation_Set_Index;
      Result     : out Gela.Interpretations.Interpretation_Index)
   is
      package Each_Constraint is
         type Visiter is new Gela.Element_Visiters.Visiter with null record;

         overriding procedure Range_Attribute_Reference
           (Self : in out Visiter;
            Node : not null Gela.Elements.Range_Attribute_References.
              Range_Attribute_Reference_Access);

         overriding procedure Simple_Expression_Range
           (Self : in out Visiter;
            Node : not null Gela.Elements.Simple_Expression_Ranges.
              Simple_Expression_Range_Access);

      end Each_Constraint;

      package body Each_Constraint is

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

      Constraint.Visit (V);
   end Constraint;

   procedure Constraint
     (Comp       : Gela.Compilations.Compilation_Access;
      Constraint : access Gela.Elements.Element'Class;
      Env        : Gela.Semantic_Types.Env_Index;
      Type_Up    : Gela.Interpretations.Interpretation_Set_Index;
      Constr     : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result     : out Gela.Interpretations.Interpretation_Index)
   is
      package Each_Constraint is
         type Visiter is new Gela.Element_Visiters.Visiter with null record;

         overriding procedure Composite_Constraint
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access);

      end Each_Constraint;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;
      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Type_Index : Gela.Semantic_Types.Type_View_Index;

      package body Each_Constraint is

         overriding procedure Composite_Constraint
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access)
         is
            pragma Unreferenced (Node, Self);

            Tuples : constant Gela.Interpretations
              .Interpretation_Tuple_Index_Array := IM.Get_Tuple_List (Constr);

            Output : Gela.Interpretations.Interpretation_Index_Array
              (Tuples'Range);

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

               overriding procedure Object_Access_Type
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Simple
                    .Object_Access_Type_Access);

            end Type_Visiters;

            package body Type_Visiters is

               overriding procedure Array_Type
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Arrays.Array_Type_Access)
               is
                  pragma Unreferenced (Self);

                  IT : constant Gela.Types.Simple.Discrete_Type_Array :=
                    Value.Index_Types;
                  Count : Natural := 0;

                  Chosen : Gela.Interpretations.Interpretation_Index;
               begin
                  if not (for all X of IT => X.Assigned) then
                     Result := 0;
                     return;
                  end if;

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

               overriding procedure Object_Access_Type
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Simple
                    .Object_Access_Type_Access)
               is
                  Des_Index  : constant Gela.Semantic_Types.Type_View_Index :=
                    TM.Type_From_Subtype_Mark (Env, Value.Get_Designated);
                  Des_View   : constant Gela.Types.Type_View_Access :=
                    TM.Get (Des_Index);
               begin
                  Des_View.Visit_If_Assigned (Self);
               end Object_Access_Type;

               overriding procedure Untagged_Record
                 (Self  : in out Type_Visitor;
                  Value : not null Gela.Types.Untagged_Records
                    .Untagged_Record_Type_Access)
               is
                  pragma Unreferenced (Self);

                  Chosen : Gela.Interpretations.Interpretation_Index;
               begin
                  for K in Tuples'Range loop
                     declare
                        use type Gela.Semantic_Types.Type_View_Index;
                        Tuple : constant Gela.Interpretations.
                                  Interpretation_Set_Index_Array :=
                                    IM.Get_Tuple (Tuples (K));
                        Exp   : Gela.Semantic_Types.Type_View_Index := 0;
                        List  : Gela.Interpretations.Interpretation_Index_Array
                                  (Tuple'Range) := (others => 0);
                        Name  : Gela.Elements.Defining_Names.
                                  Defining_Name_Access;
                     begin
                        --  Resolve choices of association
                        Output (K) := 0;

                        for J in List'First + 1 .. List'Last loop
                           for S in IM.Symbols (Tuple (J)) loop
                              Name := Value.Get_Discriminant (S.Symbol);

                              if Name.Assigned then
                                 IM.Get_Defining_Name_Index (Name, List (J));

                                 if Exp = 0 then
                                    Exp := TM.Type_Of_Object_Declaration
                                      (Env, Name.Enclosing_Element);
                                 end if;
                              end if;
                           end loop;
                        end loop;

                        --  Resolve expression of association
                        To_Type
                          (Comp    => Comp,
                           Env     => Env,
                           Type_Up => TM.Get (Exp),
                           Expr_Up => Tuple (Tuple'First),
                           Result  => List (List'First));

                        for J in reverse List'Range loop
                           IM.Get_Tuple_Index
                             (List (J), Output (K), Output (K));
                        end loop;
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

      end Each_Constraint;

      V : Each_Constraint.Visiter;

   begin
      Result := 0;

      if not Constraint.Assigned then
         return;
      end if;

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
      procedure Add_Function
        (Name : Gela.Elements.Defining_Names.Defining_Name_Access);

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;
      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;
      ES : constant Gela.Environments.Environment_Set_Access :=
        Comp.Context.Environment_Set;

      ------------------
      -- Add_Function --
      ------------------

      procedure Add_Function
        (Name : Gela.Elements.Defining_Names.Defining_Name_Access)
      is
         Index   : Gela.Interpretations.Interpretation_Index;
         Tipe    : Gela.Semantic_Types.Type_View_Index;
         Profile : constant Gela.Profiles.Profile_Access :=
            TM.Get_Profile (Env, Name);
      begin
         if Profile not in null and then
           Profile.Is_Function and then
           Profile.Allow_Empty_Argument_List and then
           Profile.Return_Type.Assigned
         then
            Tipe := Profile.Return_Type.Type_View_Index;

            if Tipe not in 0 then
               IM.Get_Defining_Name_Index (Name, Index);
               IM.Add_Expression
                 (Tipe   => Tipe,
                  Kind   => Gela.Interpretations.Function_Call,
                  Down   => (1 => Index),
                  Result => Set);
            end if;
         end if;
      end Add_Function;

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

         Add_Function (DV.Element);

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

            Add_Function (UV.Element);

            UV.Next;
         end loop;
      end;
   end Direct_Name;

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

      procedure On_Call
        (Profile : Gela.Profiles.Profile_Access;
         Cursor  : Gela.Interpretations.Abstract_Cursor'Class);

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Tuples : constant Gela.Interpretations.Interpretation_Tuple_Index_Array
        := IM.Get_Tuple_List (Args);

      -------------
      -- On_Call --
      -------------

      procedure On_Call
        (Profile : Gela.Profiles.Profile_Access;
         Cursor  : Gela.Interpretations.Abstract_Cursor'Class)
      is
         Chosen : Gela.Interpretations.Interpretation_Index := 0;
         Count  : Natural := 0;
         Output : Gela.Interpretations.Interpretation_Index_Array
           (Tuples'Range);

         Return_Type : Gela.Semantic_Types.Type_View_Index := 0;
      begin
         if not Profile.Assigned then
            return;
         elsif Profile.Return_Type.Assigned then
            Return_Type := Profile.Return_Type.Type_View_Index;
         end if;

         for J in Tuples'Range loop
            declare
               Tuple : constant Gela.Interpretations
                 .Interpretation_Set_Index_Array
                   := IM.Get_Tuple (Tuples (J));

               Tipe   : Gela.Types.Type_View_Access;
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
              (Tipe   => Return_Type,
               Kind   => Gela.Interpretations.Function_Call,
               Down   => Cursor.Get_Index & Chosen,
               Result => Set);

         elsif Tuples'Length = 0
           and then Profile.Allow_Empty_Argument_List
         then
            Comp.Context.Interpretation_Manager.Add_Expression
              (Tipe   => Return_Type,
               Kind   => Gela.Interpretations.Function_Call,
               Down   => Cursor.Get_Index & 0,
               Result => Set);

         end if;
      end On_Call;

      Profile : Gela.Profiles.Profile_Access;
   begin
      Set := 0;

      for J in IM.Profiles (Prefix) loop
         if J.Corresponding_Type.Assigned then
            Profile := TM.Get_Profile
              (J.Corresponding_Type.Type_View_Index, J.Attribute_Kind);
            On_Call (Profile, J);
         end if;
      end loop;

      for J in IM.Defining_Names (Prefix) loop
         Profile := TM.Get_Profile (Env, J.Defining_Name);
         On_Call (Profile, J);
      end loop;

      for J in Each.Prefix (IM, TM, Env, Prefix) loop
         declare
            View   : constant Gela.Types.Type_View_Access :=
              TM.Get (J.Expression_Type);
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
                  Index_Types : constant Gela.Types.Simple.Discrete_Type_Array
                    := Arr.all.Index_Types;
               begin
                  --  Check if this is positional association
                  --  Check agains Constraint_Error in case of slice FIXME
                  if Tuple'Length = 1 and Count + 1 <= Index_Types'Last then
                     Count := Count + 1;

                     if not Index_Types (Count).Assigned then
                        return;
                     end if;

                     To_Type
                       (Comp, Env,
                        Index_Types (Count), Tuple (Tuple'First), Chosen);

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

            if Chosen /= 0 and Arr.Component_Type.Assigned then
               Comp.Context.Interpretation_Manager.Add_Expression
                 (Tipe   => Arr.Component_Type.Type_View_Index,
                  Kind   => Gela.Interpretations.Indexed_Component,
                  Down   => J.Get_Index & Chosen,
                  Result => Set);
            end if;
         end;
      end loop;

      --  Type Convertion
      if Tuples'Length = 1 then
         declare
            Tipe        : Gela.Interpretations.Interpretation_Index;
            Chosen      : Gela.Interpretations.Interpretation_Index;
            Type_Index  : Gela.Semantic_Types.Type_View_Index;
            Tuple       : constant Gela.Interpretations
              .Interpretation_Set_Index_Array
                := IM.Get_Tuple (Tuples (1));
         begin
            if Tuple'Length = 1 then  --  Single expression without choices
               Get_Subtype
                 (Comp   => Comp,
                  Env    => Env,
                  Set    => Prefix,
                  Index  => Tipe,
                  Result => Type_Index);

               if Type_Index not in 0 then
                  Interpretation
                    (Comp,
                     Env,
                     Set    => Tuple (1),
                     Result => Chosen);

                  IM.Get_Tuple_Index (Chosen, 0, Chosen);
                  IM.Get_Tuple_Index (Chosen, 0, Chosen);

                  Comp.Context.Interpretation_Manager.Add_Expression
                    (Tipe   => Type_Index,
                     Kind   => Gela.Interpretations.Type_Convertion,
                     Down   => Tipe & Chosen,
                     Result => Set);
               end if;
            end if;
         end;
      end if;
   end Function_Call;

   --------------------------
   -- Qualified_Expression --
   --------------------------

   procedure Qualified_Expression
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Arg    : Gela.Interpretations.Interpretation_Set_Index;
      Set    : out Gela.Interpretations.Interpretation_Set_Index)
   is
      pragma Unreferenced (Arg);
      Tipe        : Gela.Interpretations.Interpretation_Index;
      Type_Index  : Gela.Semantic_Types.Type_View_Index;
   begin
      Set := 0;
      Get_Subtype
        (Comp   => Comp,
         Env    => Env,
         Set    => Prefix,
         Index  => Tipe,
         Result => Type_Index);

      if Type_Index not in 0 then
         Comp.Context.Interpretation_Manager.Add_Expression
           (Tipe   => Type_Index,
            Kind   => Gela.Interpretations.Type_Convertion,
            Down   => (1 .. 0 => 0),
            Result => Set);
      end if;
   end Qualified_Expression;


   -------------------------
   -- Generic_Association --
   -------------------------

   procedure Generic_Association
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
--      Actual_Part  : Gela.Elements.Generic_Associations.
--                       Generic_Association_Sequence_Access;
      Up      : Gela.Interpretations.Interpretation_Set_Index;
      Result       : out Gela.Interpretations.Interpretation_Index) is
   begin
      Interpretation
        (Comp,
         Env,
         Up,
         Result);
   end Generic_Association;

   ------------------------------
   -- Generic_Association_List --
   ------------------------------

   procedure Generic_Association_List
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      Instance     : Gela.Elements.Element_Access;
      Generic_Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Actual_Part  : Gela.Elements.Generic_Associations.
                       Generic_Association_Sequence_Access;
      Associations : Gela.Interpretations.Interpretation_Tuple_Index;
      Result       : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Env);

      function Hash
        (Value : Gela.Lexical_Types.Symbol)
         return Ada.Containers.Hash_Type is
          (Ada.Containers.Hash_Type (Value));

      type Name_Index is new Positive;

      package Symbol_To_Name_Index_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Gela.Lexical_Types.Symbol,
         Element_Type    => Name_Index,
         Hash            => Hash,
         Equivalent_Keys => Gela.Lexical_Types."=");

      package Name_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Name_Index,
         Element_Type => Gela.Elements.Defining_Names.Defining_Name_Access,
         "="          => Gela.Elements.Defining_Names."=");

      type Formal_Defining_Names is record
         Names : Name_Vectors.Vector;
         Map   : Symbol_To_Name_Index_Maps.Map;
      end record;

      procedure Resolve_Formal
        (Formal : Formal_Defining_Names;
         Value  : Gela.Interpretations.Interpretation_Set_Index;
         Name   : out Gela.Elements.Defining_Names.Defining_Name_Access);


      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Formal : Formal_Defining_Names;
         end record;

         overriding procedure Formal_Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Object_Declarations.
              Formal_Object_Declaration_Access);

         overriding procedure Formal_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Type_Declarations.
              Formal_Type_Declaration_Access);

         overriding procedure Generic_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Declarations.
              Generic_Package_Declaration_Access);

      end Visiters;

      package body Visiters is

         overriding procedure Formal_Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Object_Declarations.
              Formal_Object_Declaration_Access)
         is
            List   : constant Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Sequence_Access := Node.Names;
            Item   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Cursor : Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Sequence_Cursor := List.First;
         begin
            while Cursor.Has_Element loop
               Item := Gela.Elements.Defining_Names.Defining_Name_Access
                 (Cursor.Element);
               Self.Formal.Names.Append (Item);
               Self.Formal.Map.Include
                 (Item.Full_Name, Self.Formal.Names.Last_Index);
               Cursor.Next;
            end loop;
         end Formal_Object_Declaration;

         -----------------------------
         -- Formal_Type_Declaration --
         -----------------------------

         overriding procedure Formal_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Type_Declarations.
              Formal_Type_Declaration_Access)
         is
            Name : constant Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access := Node.Names;
            Item : constant Gela.Elements.Defining_Names.Defining_Name_Access
              := Gela.Elements.Defining_Names.Defining_Name_Access (Name);
         begin
            Self.Formal.Names.Append (Item);
            Self.Formal.Map.Include
              (Item.Full_Name, Self.Formal.Names.Last_Index);
         end Formal_Type_Declaration;

         ---------------------------------
         -- Generic_Package_Declaration --
         ---------------------------------

         overriding procedure Generic_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Declarations.
              Generic_Package_Declaration_Access)
         is
            Formal_Part : constant Gela.Elements.Generic_Formals.
              Generic_Formal_Sequence_Access := Node.Generic_Formal_Part;
            Cursor : Gela.Elements.Generic_Formals.
              Generic_Formal_Sequence_Cursor := Formal_Part.First;
            Element : Gela.Elements.Generic_Formals.Generic_Formal_Access;
         begin
            while Cursor.Has_Element loop
               Element := Cursor.Element;
               Element.Visit (Self);
               Cursor.Next;
            end loop;
         end Generic_Package_Declaration;

      end Visiters;


      --------------------
      -- Resolve_Formal --
      --------------------

      procedure Resolve_Formal
        (Formal : Formal_Defining_Names;
         Value  : Gela.Interpretations.Interpretation_Set_Index;
         Name   : out Gela.Elements.Defining_Names.Defining_Name_Access) is
      begin
         Result := 0;

         for J in IM.Symbols (Value) loop
            declare
               Found : constant Symbol_To_Name_Index_Maps.Cursor :=
                 Formal.Map.Find (J.Symbol);
            begin
               if Symbol_To_Name_Index_Maps.Has_Element (Found) then
                  Name := Formal.Names
                    (Symbol_To_Name_Index_Maps.Element (Found));
               end if;
            end;
         end loop;
      end Resolve_Formal;

      Visitor  : Visiters.Visiter;
      Tuples   : constant Gela.Interpretations.Interpretation_Set_Index_Array
        := IM.Get_Tuple (Associations);

      Name    : Gela.Elements.Defining_Names.Defining_Name_Access;
      Formal  : Gela.Interpretations.Interpretation_Index_Array (Tuples'Range)
        := (others => 0);
      Chosen  : Gela.Interpretations.Interpretation_Index;
      Element : Gela.Elements.Defining_Names.Defining_Name_Access;
      Cursor  : Gela.Elements.Generic_Associations
        .Generic_Association_Sequence_Cursor := Actual_Part.First;
   begin
      if not Generic_Name.Assigned or not Instance.Assigned then
         Result := 0;
         return;
      end if;
      --  Collect defining names of formal declarations in the instance
      Instance.Visit (Visitor);

      for J in Tuples'Range loop
         Resolve_Formal (Visitor.Formal, Tuples (J), Name);

         if Name.Assigned then
            Name.Set_Corresponding_View
              (Gela.Elements.Element_Access
                 (Cursor.Element.Actual_Parameter));

            Element := Gela.Elements.Defining_Names.Defining_Name_Access
              (Name.Corresponding_Generic_Element);

            IM.Get_Defining_Name_Index (Element, Formal (J));
         end if;

         Cursor.Next;
      end loop;

      Chosen := 0;

      for K in reverse Formal'Range loop
         IM.Get_Tuple_Index (Formal (K), Chosen, Chosen);
      end loop;

      Result := Chosen;
   end Generic_Association_List;

   -----------------
   -- Get_Subtype --
   -----------------

   procedure Get_Subtype
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Index  : out Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Semantic_Types.Type_View_Index)
   is

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

   begin
      Index := 0;
      Result := 0;

      for J in IM.Defining_Names (Set) loop
         Result := TM.Type_By_Name (Env, J.Defining_Name);
         Index := J.Get_Index;
      end loop;
   end Get_Subtype;

   --------------------
   -- Interpretation --
   --------------------

   procedure Interpretation
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Env);

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;
   begin
      Result := 0;

      for J in IM.Each (Set) loop
         if not J.Is_Symbol then
            Result := J.Get_Index;
         end if;
      end loop;
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
      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      use type Gela.Interpretations.Interpretation_Index_Array;
   begin
      Set := 0;

      for R in Each.Expression (IM, TM, Env, Right) loop
         declare
            Right_Type : constant Gela.Types.Type_View_Access :=
              TM.Get (R.Expression_Type);
         begin
            for L in Each.Expression (IM, TM, Env, Left) loop
               declare
                  Left_Type : constant Gela.Types.Type_View_Access :=
                    TM.Get (L.Expression_Type);
               begin
                  if Left_Type.Is_Expected_Type (Expected => Right_Type) then
                     Comp.Context.Interpretation_Manager.Add_Expression
                       (Tipe   => TM.Boolean,
                        Down   => L.Get_Index & R.Get_Index,
                        Result => Set);
                  end if;
               end;
            end loop;
         end;
      end loop;
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
      Type_Index : Gela.Semantic_Types.Type_View_Index;
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
            Tipe : Gela.Semantic_Types.Type_View_Index;
            Kind : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds;
            Down : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package body Each is

         overriding procedure On_Expression
           (Self : in out Visiter;
            Tipe : Gela.Semantic_Types.Type_View_Index;
            Kind : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds;
            Down : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down, Kind);

            View   : constant Gela.Types.Type_View_Access := TM.Get (Tipe);
            Tuples : constant Gela.Interpretations
              .Interpretation_Tuple_Index_Array :=
                IM.Get_Tuple_List (Tuple);
            Output : Gela.Interpretations.Interpretation_Index_Array
              (Tuples'Range);

            Comp_Type : Gela.Types.Type_View_Access;
         begin
            if View.Assigned and then View.Is_Array then
               declare
                  Arr : constant Gela.Types.Arrays.Array_Type_Access :=
                    Gela.Types.Arrays.Array_Type_Access (View);
               begin
                  Comp_Type := Arr.Component_Type;
               end;
            elsif not View.Is_Record then
               return;
            end if;

            for J in Tuples'Range loop
               declare
                  Exp    : Gela.Types.Type_View_Access;
                  Chosen : Gela.Interpretations.Interpretation_Index := 0;
                  Value  : constant Gela.Interpretations
                    .Interpretation_Set_Index_Array :=
                      IM.Get_Tuple (Tuples (J));
                  List   : Gela.Interpretations.Interpretation_Index_Array
                    (Value'Range);
               begin
                  for K in 2 .. Value'Last loop
                     declare
                        Name : Gela.Interpretations.Interpretation_Index := 0;

                        Component : Gela.Elements.Defining_Names.
                          Defining_Name_Access;
                     begin
                        for S in IM.Symbols (Value (K)) loop
                           if View.Is_Record then
                              Component := Gela.Types.Untagged_Records.
                                Untagged_Record_Type_Access (View).
                                  Get_Component (S.Symbol);
                           end if;

                           if Component.Assigned then
                              IM.Get_Defining_Name_Index (Component, Name);

                              Exp := TM.Get
                                (TM.Type_Of_Object_Declaration
                                   (Env, Component.Enclosing_Element));
                           end if;
                        end loop;

                        List (K) := Name;
                     end;
                  end loop;

                  if View.Is_Record then
                     Comp_Type := Exp;
                  end if;

                  To_Type
                    (Comp    => Comp,
                     Env     => Env,
                     Type_Up => Comp_Type,
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
      Result : constant Type_Matchers.Type_Matcher_Access :=
        new Type_Matchers.Record_Type_Matcher;
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
      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      ES : constant Gela.Environments.Environment_Set_Access :=
        Comp.Context.Environment_Set;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Is_Expanded_Name : Boolean := False;

   begin
      Set := 0;
      for Cursor in IM.Defining_Names (Prefix) loop
         declare
            Found : aliased Boolean := False;
            NC    : Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class :=
              ES.Visible (Env, Cursor.Defining_Name, Symbol, Found'Access);
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
         end;
      end loop;

      if not Is_Expanded_Name then
         for J in Each.Prefix (IM, TM, Env, Prefix) loop
            declare
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
                           Down   => (1 => J.Get_Index),
                           Result => Set);
                     end if;
                  end Untagged_Record;

               end Type_Visiters;

               Type_View : constant Gela.Types.Type_View_Access :=
                 TM.Get (J.Expression_Type);
               Visiter : Type_Visiters.Type_Visitor;
            begin
               Type_View.Visit_If_Assigned (Visiter);
            end;
         end loop;
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
      Type_Index : Gela.Semantic_Types.Type_View_Index;
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

      procedure Increment
        (Value    : in out Counter_By_Type;
         Index    : Gela.Interpretations.Interpretation_Index;
         Tipe     : Type_Kind);

      procedure Increment
        (L_Val    : in out Counter_By_Type;
         R_Val    : in out Counter_By_Type;
         Index    : Gela.Interpretations.Interpretation_Index;
         Count    : Counter_By_Type;
         Tipe     : Type_Kind);

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

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
        (L_Val    : in out Counter_By_Type;
         R_Val    : in out Counter_By_Type;
         Index    : Gela.Interpretations.Interpretation_Index;
         Count    : Counter_By_Type;
         Tipe     : Type_Kind) is
      begin
         Increment (L_Val, Index, Tipe);
         R_Val (Tipe) := Count (Tipe);
      end Increment;

      L_Val : Counter_By_Type;
      R_Val : Counter_By_Type;

   begin
      Set := 0;
      for L in Each.Expression (IM, TM, Env, Left) loop
         declare

            L_Tipe      : constant Gela.Semantic_Types.Type_View_Index :=
              L.Expression_Type;
            L_Type_View : constant Gela.Types.Type_View_Access :=
              TM.Get (L_Tipe);

         begin
            if not L_Type_View.Assigned then
               return;
            end if;

            for R in Each.Expression (IM, TM, Env, Right) loop
               declare
                  Chosen : Gela.Semantic_Types.Type_View_Index;
                  Type_View : constant Gela.Types.Type_View_Access :=
                    TM.Get (R.Expression_Type);
               begin
                  if not Type_View.Assigned then
                     return;
                  else  --  FIXME Return after implementation of types
                     null;
                  end if;

                  if Type_View.Is_Expected_Type (L_Type_View) then
                     if Type_View.Is_Universal
                       and then Type_View.Is_Numeric
                     then
                        Chosen := L_Tipe;
                     else
                        Chosen := R.Expression_Type;
                     end if;

                     Comp.Context.Interpretation_Manager.Add_Expression
                       (Tipe   => Chosen,
                        Down   => (L.Get_Index, R.Get_Index),
                        Result => Set);
                  end if;
               end;
            end loop;

            for R in IM.Categories (Right) loop
               declare
                  Match : constant Gela.Interpretations.Type_Matcher_Access :=
                    R.Matcher;
               begin
                  L_Type_View.Visit (Match.all);

                  if Match.Is_Matched then
                     Comp.Context.Interpretation_Manager.Add_Expression
                       (Tipe   => L_Tipe,
                        Down   => (L.Get_Index, R.Get_Index),
                        Result => Set);
                  end if;
               end;
            end loop;
         end;
      end loop;

      for L in IM.Categories (Left) loop
         for R in Each.Expression (IM, TM, Env, Right) loop
            declare
               Match : constant Gela.Interpretations.Type_Matcher_Access :=
                 L.Matcher;
               Type_View : constant Gela.Types.Type_View_Access :=
                 TM.Get (R.Expression_Type);
            begin
               Type_View.Visit (Match.all);

               if Match.Is_Matched then
                  Comp.Context.Interpretation_Manager.Add_Expression
                    (Tipe   => R.Expression_Type,
                     Down   => (L.Get_Index, R.Get_Index),
                     Result => Set);
               end if;
            end;
         end loop;
      end loop;

      for L in Each.Prefer_Root (IM, TM, Env, Left) loop
         declare

            R_Counters  : Counter_By_Type;
            L_Type_View : constant Gela.Types.Type_View_Access :=
              TM.Get (L.Expression_Type);

         begin
            for R in Each.Prefer_Root (IM, TM, Env, Right) loop
               declare
                  Type_View : constant Gela.Types.Type_View_Access :=
                    TM.Get (R.Expression_Type);
               begin
                  if Type_View.Is_Integer then
                     Increment (R_Counters, R.Get_Index, Integer);
                  elsif Type_View.Is_Real then
                     Increment (R_Counters, R.Get_Index, Float);
                  else  --  FIXME Return after implementation of types
                     null;
                  end if;
               end;
            end loop;

            if L_Type_View.Is_Integer then
               Increment
                 (L_Val,
                  R_Val,
                  L.Get_Index,
                  R_Counters,
                  Integer);
            elsif L_Type_View.Is_Real then
               Increment
                 (L_Val,
                  R_Val,
                  L.Get_Index,
                  R_Counters,
                  Float);
            else  --  FIXME Drop after implementation of types
               null;
            end if;
         end;
      end loop;

      if L_Val (Integer).Count = 1 and R_Val (Integer).Count = 1 then
         declare
            Matcher : constant Type_Matchers.Type_Matcher_Access :=
              new Type_Matchers.Integer_Type_Matcher;
         begin
            Comp.Context.Interpretation_Manager.Add_Expression_Category
              (Match  => Gela.Interpretations.Type_Matcher_Access (Matcher),
               Down   => (L_Val (Integer).Index, R_Val (Integer).Index),
               Result => Set);
         end;
      end if;

      if L_Val (Float).Count = 1 and R_Val (Float).Count = 1 then
         declare
            Matcher : constant Type_Matchers.Type_Matcher_Access :=
              new Type_Matchers.Float_Type_Matcher;
         begin
            Comp.Context.Interpretation_Manager.Add_Expression_Category
              (Match  => Gela.Interpretations.Type_Matcher_Access (Matcher),
               Down   => (L_Val (Float).Index, R_Val (Float).Index),
               Result => Set);
         end;
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
      Tipe       : out Gela.Semantic_Types.Type_View_Index)
   is

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      L_Count : Natural := 0;
   begin
      for L in Each.Prefer_Root (IM, TM, Env, Left) loop
         declare

            L_Tipe      : constant Gela.Semantic_Types.Type_View_Index :=
              L.Expression_Type;
            L_Type_View : constant Gela.Types.Type_View_Access :=
              TM.Get (L_Tipe);
            R_Count     : Natural := 0;
         begin
            if L_Type_View.Assigned and then L_Type_View.Is_Discrete then
               for R in Each.Prefer_Root (IM, TM, Env, Right) loop
                  declare
                     use type Gela.Semantic_Types.Type_View_Index;

                     R_Tipe : constant Gela.Semantic_Types.Type_View_Index :=
                       R.Expression_Type;
                     Type_View : constant Gela.Types.Type_View_Access :=
                       TM.Get (R_Tipe);
                  begin
                     if not Type_View.Assigned or else
                       not Type_View.Is_Discrete
                     then
                        null;
                     elsif not Type_View.Is_Universal then
                        if L_Type_View.Is_Expected_Type (Type_View) then
                           Tipe := R_Tipe;
                           R_Count := R_Count + 1;
                           Down_Right := R.Get_Index;
                        end if;
                     elsif L_Type_View.Is_Universal then
                        if Type_View.Is_Expected_Type (L_Type_View) then
                           Tipe := L_Tipe;
                           R_Count := R_Count + 1;
                           Down_Right := R.Get_Index;
                        end if;
                     elsif R_Tipe = L_Tipe and Type_View.Is_Integer then
                        Tipe := TM.Universal_Integer; --  FIXME Root_Int
                        R_Count := R_Count + 1;
                        Down_Right := R.Get_Index;
                     else
                        null;
                     end if;
                  end;
               end loop;
               --  FIXME .Is_Discrete

               if R_Count > 0 then
                  L_Count := L_Count + R_Count;
                  Down_Left := L.Get_Index;
               end if;
            end if;
         end;
      end loop;

      if L_Count /= 1 then
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
      Tipe       : out Gela.Semantic_Types.Type_View_Index)
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
      Ignore_Tipe  : Gela.Semantic_Types.Type_View_Index;
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
      Ignore_Tipe  : Gela.Semantic_Types.Type_View_Index;
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
      Matcher : constant Type_Matchers.Type_Matcher_Access :=
        new Type_Matchers.String_Type_Matcher;
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

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

   begin
      Result := 0;

      for J in Each.Expression (IM, TM, Env, Type_Up) loop
         To_Type
           (Comp    => Comp,
            Env     => Env,
            Type_Up => TM.Get (J.Expression_Type),
            Expr_Up => Expr_Up,
            Result  => Result);

         exit;
      end loop;
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
      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;
      Index      : Gela.Interpretations.Interpretation_Index;
      Type_Index : Gela.Semantic_Types.Type_View_Index;
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
         Type_Up => TM.Get (Type_Index),
         Expr_Up => Expr_Up,
         Result  => Result);
   end To_Type;

   -------------
   -- To_Type --
   -------------

   procedure To_Type
     (Comp    : Gela.Compilations.Compilation_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : access Gela.Types.Type_View'Class;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Env);

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

   begin
      Result := 0;

      if not Type_Up.Assigned then
         return;
      end if;

      for J in IM.Each (Expr_Up) loop
         if J.Is_Defining_Name then
            Result := J.Get_Index;  --  ???
         elsif J.Is_Expression then
            declare
               This_Type : constant Gela.Types.Type_View_Access :=
                 TM.Get (J.Expression_Type);
            begin
               if This_Type.Assigned and then
                 This_Type.Is_Expected_Type (Type_Up)
               then
                  Result := J.Get_Index;
               end if;
            end;
         elsif J.Is_Expression_Category then
            declare
               use type Gela.Interpretations.Interpretation_Index;
               Match  : constant Gela.Interpretations.Type_Matcher_Access :=
                 J.Matcher;
            begin
               Type_Up.Visit (Match.all);

               if Match.Is_Matched and Result = 0 then
                  IM.Get_Expression_Index
                    (Tipe   => Type_Up.Type_View_Index,
                     Result => Result);
               end if;
            end;
         end if;
      end loop;
   end To_Type;

   ----------------------
   -- To_Type_Category --
   ----------------------

   procedure To_Type_Category
     (Comp     : Gela.Compilations.Compilation_Access;
      Up       : Gela.Interpretations.Interpretation_Set_Index;
      Tipe     : Gela.Semantic_Types.Type_View_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      View : constant Gela.Types.Type_View_Access := TM.Get (Tipe);

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      Matcher : Gela.Interpretations.Type_Matcher_Access;
   begin
      Result := 0;

      for J in IM.Categories (Up) loop
         Matcher := J.Matcher;
         View.Visit (Matcher.all);

         if Matcher.Is_Matched then
            Result := J.Get_Index;
         end if;
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
      use type Gela.Semantic_Types.Type_View_Index;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Index      : Gela.Interpretations.Interpretation_Index;
      Type_Index : Gela.Semantic_Types.Type_View_Index;
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
            Type_Up => TM.Get (Type_Index),
            Expr_Up => Expr_Up,
            Result  => Result);
      end if;
   end To_Type_Or_The_Same_Type;

   ------------------
   -- Variant_Part --
   ------------------

   procedure Variant_Part
     (Comp     : Gela.Compilations.Compilation_Access;
      Env      : Gela.Semantic_Types.Env_Index;
      Name_Up  : Gela.Interpretations.Interpretation_Set_Index;
      Variants : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      Tuples : constant Gela.Interpretations.Interpretation_Tuple_Index_Array
        := IM.Get_Tuple_List (Variants);

      Output  : Gela.Interpretations.Interpretation_Index_Array (Tuples'Range);
      Chosen  : Gela.Interpretations.Interpretation_Index := 0;
   begin
      Result := 0;

      for E in Each.Expression (IM, TM, Env, Name_Up) loop
         for J in Tuples'Range loop
            declare
               Tuple : constant Gela.Interpretations
                 .Interpretation_Set_Index_Array := IM.Get_Tuple (Tuples (J));
               List  : Gela.Interpretations.Interpretation_Index_Array
                 (Tuple'Range);
            begin
               for K in Tuple'Range loop
                  To_Type
                    (Comp    => Comp,
                     Env     => Env,
                     Type_Up => TM.Get (E.Expression_Type),
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
         exit;
      end loop;
   end Variant_Part;

end Gela.Resolve;
