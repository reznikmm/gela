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

      Type_Index : Gela.Semantic_Types.Type_Index;
      Index      : Gela.Interpretations.Interpretation_Index;
   begin
      Set := 0;

      if Symbol = Comp.Context.Symbols.Get
        (Gela.Lexical_Types.Last_Symbol)
      then
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
      end if;
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

      Index      : Gela.Interpretations.Interpretation_Index;
      Visiter    : Each.Visiter;
   begin
      Get_Subtype
        (Comp,
         Env    => Env,
         Set    => Type_Up,
         Index  => Index,
         Result => Visiter.Type_Index);

      IM.Visit (Expr_Up, Visiter);

      Result := Visiter.Index;
   end To_Type;

end Gela.Resolve;
