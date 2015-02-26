with Gela.Int.Attr_Functions;
with Gela.Int.Categories;
with Gela.Int.Defining_Names;
with Gela.Int.Expressions;
with Gela.Int.Placeholders;
with Gela.Int.Symbols;
with Gela.Int.Tuples;
with Gela.Int.Visiters;

package body Gela.Plain_Interpretations is

   package Empty_Cursors is
      type Cursor is new Gela.Interpretations.Cursor with null record;

      overriding function Has_Element (Self : Cursor) return Boolean;

      overriding procedure Next (Self : in out Cursor) is null;

      overriding procedure Visit
        (Self   : Cursor;
         Target : access Gela.Interpretations.Up_Visiter'Class) is null;

      overriding function Get_Index
        (Self : Cursor) return Gela.Interpretations.Interpretation_Index;

   end Empty_Cursors;

   package body Empty_Cursors is

      overriding function Has_Element (Self : Cursor) return Boolean is
         pragma Unreferenced (Self);
      begin
         return False;
      end Has_Element;

      overriding function Get_Index
        (Self : Cursor) return Gela.Interpretations.Interpretation_Index
      is
         pragma Unreferenced (Self);
      begin
         return 0;
      end Get_Index;

   end Empty_Cursors;

   -----------------------
   -- Add_Attr_Function --
   -----------------------

   overriding procedure Add_Attr_Function
     (Self   : in out Interpretation_Manager;
      Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
   is
      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Attr_Functions.Attr_Function'
          (Gela.Int.Attr_Functions.Create
             (Down     => Down,
              Kind     => Kind));
   begin
      Self.Plian_Int_Set.Add (Result, Item);
   end Add_Attr_Function;

   -----------------------
   -- Add_Defining_Name --
   -----------------------

   overriding procedure Add_Defining_Name
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
   is
      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Defining_Names.Defining_Name'
          (Gela.Int.Defining_Names.Create
             (Down     => Down,
              Name     => Name));
   begin
      Self.Plian_Int_Set.Add (Result, Item);
   end Add_Defining_Name;

   --------------------
   -- Add_Expression --
   --------------------

   overriding procedure Add_Expression
     (Self   : in out Interpretation_Manager;
      Tipe   : Gela.Semantic_Types.Type_Index;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
   is
      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Expressions.Expression'
          (Gela.Int.Expressions.Create
             (Down            => Down,
              Expression_Type => Tipe));
   begin
      Self.Plian_Int_Set.Add (Result, Item);
   end Add_Expression;

   -----------------------------
   -- Add_Expression_Category --
   -----------------------------

   overriding procedure Add_Expression_Category
     (Self   : in out Interpretation_Manager;
      Kinds  : Gela.Type_Views.Category_Kind_Set;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
   is
      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Categories.Category'
          (Gela.Int.Categories.Create
             (Down  => Down,
              Kinds => Kinds));
   begin
      Self.Plian_Int_Set.Add (Result, Item);
   end Add_Expression_Category;

   ---------------------
   -- Add_Placeholder --
   ---------------------

   overriding procedure Add_Placeholder
     (Self   : in out Interpretation_Manager;
      Kind   : Gela.Interpretations.Placeholder_Kind;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
   is
      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Placeholders.Placeholder'
          (Gela.Int.Placeholders.Create
             (Down => (1 .. 0 => 0),
              Kind => Kind));
   begin
      Self.Plian_Int_Set.Add (Result, Item);
   end Add_Placeholder;

   ----------------
   -- Add_Symbol --
   ----------------

   overriding procedure Add_Symbol
     (Self   : in out Interpretation_Manager;
      Symbol : Gela.Lexical_Types.Symbol;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
   is
      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Symbols.Symbol'
          (Gela.Int.Symbols.Create
             (Down  => (1 .. 0 => 0),
              Value => Symbol));
   begin
      Self.Plian_Int_Set.Add (Result, Item);
   end Add_Symbol;

   ---------------
   -- Add_Tuple --
   ---------------

   overriding procedure Add_Tuple
     (Self   : in out Interpretation_Manager;
      Left   : Gela.Interpretations.Interpretation_Set_Index;
      Right  : Gela.Interpretations.Interpretation_Set_Index;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
   is
      package Each is
         type Visiter is new Gela.Interpretations.Up_Visiter with null record;
         --  Only tuples are expected here

         overriding procedure On_Tuple
           (V     : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array);

      end Each;

      package body Each is

         overriding procedure On_Tuple
           (V     : in out Visiter;
            Value : Gela.Interpretations.Interpretation_Set_Index_Array)
         is
            pragma Unreferenced (V);
            use type Gela.Interpretations.Interpretation_Set_Index_Array;

            Item : constant Gela.Int.Interpretation_Access :=
              new Gela.Int.Tuples.Tuple'
                (Gela.Int.Tuples.Create (Left & Value));
         begin
            Self.Plian_Int_Set.Add (Result, Item);
         end On_Tuple;

      end Each;

      Item : Gela.Int.Interpretation_Access;
      V    : aliased Each.Visiter;
   begin
      if Right = 0 then
         Item := new Gela.Int.Tuples.Tuple'
           (Gela.Int.Tuples.Create (Value => (1 => Left)));

         Self.Plian_Int_Set.Add (Result, Item);
      else
         declare
            Cursor : Gela.Interpretations.Cursor'Class :=
              Self.Get_Cursor (Right);
         begin
            while Cursor.Has_Element loop
               Cursor.Visit (V'Access);
               Cursor.Next;
            end loop;
         end;
      end if;
   end Add_Tuple;

   -----------------------
   -- Get_Defining_Name --
   -----------------------

   overriding procedure Get_Defining_Name
     (Self   : in out Interpretation_Manager;
      Value  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Elements.Defining_Names.Defining_Name_Access)
   is
      package Each is
         type Visiter is new Gela.Interpretations.Down_Visiter with record
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
         end record;

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Each;

      ----------
      -- Each --
      ----------

      package body Each is

         overriding procedure On_Defining_Name
           (Self   : in out Visiter;
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down);
         begin
            Self.Name := Name;
         end On_Defining_Name;

      end Each;

      Visiter : Each.Visiter;
   begin
      Self.Visit (Value, Visiter);
      Result := Visiter.Name;
   end Get_Defining_Name;

   -----------------------------
   -- Get_Down_Interpretation --
   -----------------------------

   overriding procedure Get_Down_Interpretation
     (Self     : in out Interpretation_Manager;
      Value    : Gela.Interpretations.Interpretation_Index;
      Index    : Positive;
      Result   : out Gela.Interpretations.Interpretation_Index)
   is
      Item : Gela.Int.Interpretation_Access;
   begin
      Result := 0;

      if Value = 0 then
         return;
      end if;

      Item := Self.Item_Batches.Element (Value / Batch_Size).Element (Value);

      if Index in Item.Down'Range then
         Result := Item.Down (Index);
      end if;
   end Get_Down_Interpretation;

   ----------------
   -- Get_Cursor --
   ----------------

   overriding function Get_Cursor
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Cursor'Class is
   begin
      if Set = 0 then
         return None : Empty_Cursors.Cursor;
      else
         return Self.Set_Batches.Element (Set / Batch_Size).Get_Cursor (Set);
      end if;
   end Get_Cursor;

   -----------------------------
   -- Get_Defining_Name_Index --
   -----------------------------

   overriding procedure Get_Defining_Name_Index
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Defining_Names.Defining_Name'
          (Gela.Int.Defining_Names.Create
             (Down => (1 .. 0 => 0), Name => Name));
   begin
      Self.Plian_Int_Set.Add (Result, Item);
   end Get_Defining_Name_Index;

   ---------------------
   -- Get_Tuple_Index --
   ---------------------

   overriding procedure Get_Tuple_Index
     (Self   : in out Interpretation_Manager;
      Left   : Gela.Interpretations.Interpretation_Index;
      Right  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      use type Gela.Interpretations.Interpretation_Index_Array;

      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Tuples.Chosen_Tuple'
          (Length => 2, Index => 0, Down => Left & Right);
   begin
      Self.Plian_Int_Set.Add (Result, Item);
   end Get_Tuple_Index;

   ---------------------
   -- Reserve_Indexes --
   ---------------------

   overriding procedure Reserve_Indexes
     (Self : in out Interpretation_Manager;
      Set  : Gela.Int_Sets.Interpretation_Set_Access;
      From : out Gela.Interpretations.Interpretation_Set_Index;
      To   : out Gela.Interpretations.Interpretation_Set_Index) is
   begin
      Self.Set_Batches.Append (Set);
      From := Self.Set_Batches.Last_Index * Batch_Size;
      To := From + Batch_Size - 1;
      From := Gela.Interpretations.Interpretation_Set_Index'Max (1, From);
   end Reserve_Indexes;

   ---------------------
   -- Reserve_Indexes --
   ---------------------

   overriding procedure Reserve_Indexes
     (Self : in out Interpretation_Manager;
      Set  : Gela.Int_Sets.Interpretation_Set_Access;
      From : out Gela.Interpretations.Interpretation_Index;
      To   : out Gela.Interpretations.Interpretation_Index) is
   begin
      Self.Item_Batches.Append (Set);
      From := Self.Item_Batches.Last_Index * Batch_Size;
      To := From + Batch_Size - 1;
      From := Gela.Interpretations.Interpretation_Index'Max (1, From);
   end Reserve_Indexes;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Index;
      Target : in out Gela.Interpretations.Down_Visiter'Class)
   is
      package Switch is
         type Visiter is new Gela.Int.Visiters.Visiter with null record;

         overriding procedure Attr_Function
           (Self  : access Visiter;
            Value : Gela.Int.Attr_Functions.Attr_Function);

         overriding procedure Chosen_Tuple
           (Self  : access Visiter;
            Value : Gela.Int.Tuples.Chosen_Tuple);

         overriding procedure Defining_Name
           (Self  : access Visiter;
            Value : Gela.Int.Defining_Names.Defining_Name);

         overriding procedure Expression
           (Self  : access Visiter;
            Value : Gela.Int.Expressions.Expression);

         overriding procedure Expression_Category
           (Self  : access Visiter;
            Value : Gela.Int.Categories.Category);

         overriding procedure Placeholder
           (Self  : access Visiter;
            Value : Gela.Int.Placeholders.Placeholder);

         overriding procedure Symbol
           (Self  : access Visiter;
            Value : Gela.Int.Symbols.Symbol);

         overriding procedure Tuple
           (Self  : access Visiter;
            Value : Gela.Int.Tuples.Tuple);

      end Switch;

      ------------
      -- Switch --
      ------------

      package body Switch is

         -------------------
         -- Attr_Function --
         -------------------

         overriding procedure Attr_Function
           (Self  : access Visiter;
            Value : Gela.Int.Attr_Functions.Attr_Function)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Attr_Function
              (Kind  => Value.Kind,
               Down  => Value.Down);
         end Attr_Function;

         -------------------
         -- Defining_Name --
         -------------------

         overriding procedure Defining_Name
           (Self  : access Visiter;
            Value : Gela.Int.Defining_Names.Defining_Name)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Defining_Name
              (Name  => Value.Name,
               Down  => Value.Down);
         end Defining_Name;

         ----------------
         -- Expression --
         ----------------

         overriding procedure Expression
           (Self  : access Visiter;
            Value : Gela.Int.Expressions.Expression)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Expression
              (Tipe  => Value.Expression_Type,
               Down  => Value.Down);
         end Expression;

         -------------------------
         -- Expression_Category --
         -------------------------

         overriding procedure Expression_Category
           (Self  : access Visiter;
            Value : Gela.Int.Categories.Category)
         is
            pragma Unreferenced (Self);
         begin
            raise Program_Error with "Unexpected up interpretation in down";
         end Expression_Category;

         -----------------
         -- Placeholder --
         -----------------

         overriding procedure Placeholder
           (Self  : access Visiter;
            Value : Gela.Int.Placeholders.Placeholder)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Placeholder
              (Kind => Value.Placeholder_Kind,
               Down => Value.Down);
         end Placeholder;

         ------------
         -- Symbol --
         ------------

         overriding procedure Symbol
           (Self  : access Visiter;
            Value : Gela.Int.Symbols.Symbol)
         is
            pragma Unreferenced (Self);
         begin
            null;
            --  raise Program_Er with "Unexpected up interpretation in down";
         end Symbol;

         -----------
         -- Tuple --
         -----------

         overriding procedure Tuple
           (Self  : access Visiter;
            Value : Gela.Int.Tuples.Tuple)
         is
            pragma Unreferenced (Self);
         begin
            raise Program_Error with "Unexpected up interpretation in down";
         end Tuple;

         ------------------
         -- Chosen_Tuple --
         ------------------

         overriding procedure Chosen_Tuple
           (Self  : access Visiter;
            Value : Gela.Int.Tuples.Chosen_Tuple)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Tuple (Value.Down);
         end Chosen_Tuple;

      end Switch;

      V : aliased Switch.Visiter;
   begin
      if Index /= 0 then
         Self.Item_Batches.Element (Index / Batch_Size).Element (Index).Visit
           (V'Access);
      end if;
   end Visit;

end Gela.Plain_Interpretations;
