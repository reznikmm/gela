with Gela.Int.Defining_Names;
with Gela.Int.Expressions;
with Gela.Int.Visiters;

package body Gela.Plain_Interpretations is

   -----------------------
   -- Add_Defining_Name --
   -----------------------

   overriding procedure Add_Defining_Name
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
   is
      use type Gela.Interpretations.Interpretation_Index;

      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Defining_Names.Defining_Name'
          (Gela.Int.Defining_Names.Create
             (Children => Down'Length,
              Name     => Name));
   begin
      Item.Down := Down;
      Self.Last_Int := Self.Last_Int + 1;
      Result := Gela.Interpretations.Interpretation_Set_Index (Self.Last_Int);
      Self.Interpretations.Insert (Self.Last_Int, Item);
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
      use type Gela.Interpretations.Interpretation_Index;

      Item : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Expressions.Expression'
          (Gela.Int.Expressions.Create
             (Children => Down'Length,
              Expression_Type => Tipe));
   begin
      Item.Down := Down;
      Self.Last_Int := Self.Last_Int + 1;
      Result := Gela.Interpretations.Interpretation_Set_Index (Self.Last_Int);
      Self.Interpretations.Insert (Self.Last_Int, Item);
   end Add_Expression;

   -----------------------
   -- Get_Defining_Name --
   -----------------------

   overriding procedure Get_Defining_Name
     (Self   : in out Interpretation_Manager;
      Value  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Elements.Defining_Names.Defining_Name_Access)
   is
      package Each is
         type Visiter is new Gela.Interpretations.Visiter with record
            Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
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
            pragma Unreferenced (Index);
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
      use type Gela.Interpretations.Interpretation_Index;

      Item : Gela.Int.Interpretation_Access;
   begin
      Result := 0;

      if Value = 0 then
         return;
      end if;

      Item := Self.Interpretations.Element (Value);

      if Index in Item.Down'Range then
         Result := Item.Down (Index);
      end if;
   end Get_Down_Interpretation;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : Gela.Interpretations.Interpretation_Set_Index)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Value);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : Gela.Interpretations.Interpretation_Index)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Value);
   end Hash;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Target : in out Gela.Interpretations.Visiter'Class) is
   begin
      Self.Visit (Gela.Interpretations.Interpretation_Index (Set), Target);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Index;
      Target : in out Gela.Interpretations.Visiter'Class)
   is
      package Switch is
         type Visiter is new Gela.Int.Visiters.Visiter with null record;

         overriding procedure Defining_Name
           (Self  : access Visiter;
            Value : Gela.Int.Defining_Names.Defining_Name);

         overriding procedure Expression
           (Self  : access Visiter;
            Value : Gela.Int.Expressions.Expression);
      end Switch;

      ------------
      -- Switch --
      ------------

      package body Switch is

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
              (Index => Index,
               Name  => Value.Name,
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
              (Index => Index,
               Tipe  => Value.Expression_Type,
               Down  => Value.Down);
         end Expression;

      end Switch;

      use type Gela.Interpretations.Interpretation_Index;
      V : aliased Switch.Visiter;
   begin
      if Index /= 0 then
         Self.Interpretations.Element (Index).Visit (V'Access);
      end if;
   end Visit;

end Gela.Plain_Interpretations;
