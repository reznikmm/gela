with Gela.Defining_Name_Cursors;
with Gela.Elements.Defining_Names;
with Gela.Environments;
with Gela.Type_Managers;

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
      NC : Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
        := ES.Direct_Visible (Env, Symbol);
   begin
      Set := 0;

      while NC.Has_Element loop
         IM.Add_Defining_Name
           (Name   => NC.Element,
            Down   => (1 .. 0 => 0),
            Result => Set);

         NC.Next;
      end loop;
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

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Attr_Function
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
            Down   : Gela.Interpretations.Interpretation_Index_Array) is null;

      end Each;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Self);
            pragma Unreferenced (Name);
            pragma Unreferenced (Down);
         begin
            null;
         end On_Defining_Name;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Expression (Index, Tipe, Down);
         end On_Expression;

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      Visiter    : Each.Visiter;
   begin
      IM.Visit (Set, Visiter);
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
      pragma Unreferenced (Comp);
      pragma Unreferenced (Env);
      pragma Unreferenced (Prefix);
      pragma Unreferenced (Args);
   begin
      Set := 0;
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
            Index  : Gela.Interpretations.Interpretation_Index;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Attr_Function
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
            Down   : Gela.Interpretations.Interpretation_Index_Array) is null;

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
            Index  : Gela.Interpretations.Interpretation_Index;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Self);
            pragma Unreferenced (Down);
         begin
            Self.Result := TM.Type_By_Name (Name);
            Self.Index := Index;
         end On_Defining_Name;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array) is
         begin
            null;
         end On_Expression;

      end Each;

      Visiter : Each.Visiter;
   begin
      IM.Visit (Set, Visiter);
      Index := Visiter.Index;
      Result := Visiter.Result;
   end Get_Subtype;

   procedure Interpretation
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Comp);
      pragma Unreferenced (Env);
   begin
      Result := Gela.Interpretations.Interpretation_Index (Set);
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
            Index  : Gela.Interpretations.Interpretation_Index;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Attr_Function
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
            Down   : Gela.Interpretations.Interpretation_Index_Array) is null;

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      ES : constant Gela.Environments.Environment_Set_Access :=
        Comp.Context.Environment_Set;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
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
                  Down   => (1 => Index),
                  Result => Set);

               NC.Next;
            end loop;
         end On_Defining_Name;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array) is
         begin
            null;
         end On_Expression;

      end Each;

      Visiter : Each.Visiter;
   begin
      Set := 0;
      IM.Visit (Prefix, Visiter);
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
   begin
      Set := 0;

      Comp.Context.Interpretation_Manager.Add_Expression
        (Tipe   => 0,
         Down   => (Gela.Interpretations.Interpretation_Index (Left),
                    Gela.Interpretations.Interpretation_Index (Right)),
         Result => Set);
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

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array) is null;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Attr_Function
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
            Down   : Gela.Interpretations.Interpretation_Index_Array) is null;

      end Each;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Self);
            pragma Unreferenced (Index);
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
            Index  : Gela.Interpretations.Interpretation_Index;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

         overriding procedure On_Attr_Function
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
            Down   : Gela.Interpretations.Interpretation_Index_Array) is null;

      end Each;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Name);
            pragma Unreferenced (Down);
         begin
            Self.Index := Index;
         end On_Defining_Name;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Index  : Gela.Interpretations.Interpretation_Index;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Tipe);
            pragma Unreferenced (Down);
         begin
            Self.Index := Index;
         end On_Expression;

      end Each;

      Visiter    : Each.Visiter;
   begin
      Visiter.Type_Index := Type_Up;

      IM.Visit (Expr_Up, Visiter);

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

end Gela.Resolve;
