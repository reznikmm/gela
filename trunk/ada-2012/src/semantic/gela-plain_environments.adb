--  with Gela.Plain_Environments.Debug;

package body Gela.Plain_Environments is

   function Name_To_Region
     (Self  : access Environment_Set'Class;
      Index : Gela.Semantic_Types.Env_Index;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access)
         return Region_Item_List;

   package Visible_Cursors is
      --  Cursor over names in Local
      type Defining_Name_Cursor is
        new Gela.Defining_Name_Cursors.Defining_Name_Cursor with
         record
            Set     : Plain_Environment_Set_Access;
            Current : Gela.Name_List_Managers.Defining_Name_Cursor;
         end record;

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean;

      overriding function Element
        (Self : Defining_Name_Cursor)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

      overriding procedure Next
        (Self : in out Defining_Name_Cursor);

      procedure Initialize
        (Self   : in out Defining_Name_Cursor'Class;
         Symbol : Gela.Lexical_Types.Symbol;
         Region : Region_Item_List);
   end Visible_Cursors;

   package body Visible_Cursors is

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean is
      begin
         return Self.Current.Has_Element;
      end Has_Element;

      overriding function Element
        (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access is
      begin
         return Self.Current.Element;
      end Element;

      overriding procedure Next (Self : in out Defining_Name_Cursor) is
      begin
         Self.Current.Next;
      end Next;

      procedure Initialize
        (Self   : in out Defining_Name_Cursor'Class;
         Symbol : Gela.Lexical_Types.Symbol;
         Region : Region_Item_List)
      is
         Local  : constant Gela.Name_List_Managers.List :=
           Self.Set.Region.Head (Region).Local;
      begin
         Self.Current := Self.Set.Names.Find (Local, Symbol);
      end Initialize;

   end Visible_Cursors;

   package Direct_Visible_Cursors is
      --  Cursor over names in Local then go to enclosing region, etc
      type Defining_Name_Cursor is
        new Visible_Cursors.Defining_Name_Cursor with
         record
            Region  : Region_Item_List;
         end record;

      overriding procedure Next
        (Self : in out Defining_Name_Cursor);

      procedure Initialize
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol);
   end Direct_Visible_Cursors;

   ----------------------------
   -- Direct_Visible_Cursors --
   ----------------------------

   package body Direct_Visible_Cursors is

      overriding procedure Next (Self : in out Defining_Name_Cursor) is
         use type Region_Item_List;
         Symbol : constant Gela.Lexical_Types.Symbol := Self.Current.Symbol;
         Region : Region_Item_List;
      begin
         Visible_Cursors.Defining_Name_Cursor (Self).Next;

         while not Self.Has_Element loop
            Region := Self.Set.Region.Tail (Self.Region);

            if Region /= Region_Item_Lists.Empty then
               Self.Region := Region;
               Visible_Cursors.Initialize (Self, Symbol, Region);
            else
               return;
            end if;
         end loop;
      end Next;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol)
      is
         use type Region_Item_List;
         Region : Region_Item_List := Self.Region;
      begin
         while Region /= Region_Item_Lists.Empty loop
            Self.Region := Region;
            Visible_Cursors.Initialize (Self, Symbol, Region);

            exit when Self.Has_Element;
            Region := Self.Set.Region.Tail (Self.Region);
         end loop;
      end Initialize;

   end Direct_Visible_Cursors;

   package Use_Package_Cursors is
      --  Cursor over names in each used package
      type Defining_Name_Cursor is
        new Visible_Cursors.Defining_Name_Cursor with
         record
            Env      : Env_Item_Index;
            Region   : Region_Item_List;
            --  Position in Env_Item.Nested_Region_List list
            Use_Name : Defining_Name_List;
            --  Position in Region.Use_Package list
         end record;

      overriding procedure Next
        (Self : in out Defining_Name_Cursor);

      function Name_To_Region
        (Self : Defining_Name_Cursor;
         Name : Gela.Elements.Defining_Names.Defining_Name_Access)
         return Region_Item_List;

      procedure Initialize
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol);

   end Use_Package_Cursors;

   -------------------------
   -- Use_Package_Cursors --
   -------------------------

   package body Use_Package_Cursors is

      overriding procedure Next (Self : in out Defining_Name_Cursor) is
         use type Region_Item_List;
         use type Defining_Name_List;

         Symbol : constant Gela.Lexical_Types.Symbol := Self.Current.Symbol;
         Region : Region_Item_List;
      begin
         Visible_Cursors.Defining_Name_Cursor (Self).Next;

         while not Self.Current.Has_Element loop
            Region := Region_Item_Lists.Empty;

            while Region = Region_Item_Lists.Empty loop
               --  Next name in use clauses of Region
               Self.Use_Name := Self.Set.Use_Package.Tail (Self.Use_Name);

               while Self.Use_Name = Defining_Name_Lists.Empty loop
                  Self.Region := Self.Set.Region.Tail (Self.Region);

                  if Self.Region = Region_Item_Lists.Empty then
                     return;
                  end if;

                  Self.Use_Name :=
                    Self.Set.Region.Head (Self.Region).Use_Package;
               end loop;

               Region := Self.Name_To_Region
                 (Self.Set.Use_Package.Head (Self.Use_Name));
            end loop;

            Visible_Cursors.Initialize (Self, Symbol, Region);
         end loop;
      end Next;

      --------------------
      -- Name_To_Region --
      --------------------

      function Name_To_Region
        (Self : Defining_Name_Cursor;
         Name : Gela.Elements.Defining_Names.Defining_Name_Access)
         return Region_Item_List is
      begin
         return Name_To_Region (Self.Set, Self.Env, Name);
      end Name_To_Region;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol)
      is
         use type Region_Item_List;
         use type Defining_Name_List;

         Env    : constant Env_Item := Self.Set.Env.Element (Self.Env);
         Target : Region_Item_List;
         Local  : Gela.Name_List_Managers.List;
      begin
         Self.Region := Env.Region_List (Nested);

         while Self.Region /= Region_Item_Lists.Empty loop
            Self.Use_Name := Self.Set.Region.Head (Self.Region).Use_Package;

            while Self.Use_Name /= Defining_Name_Lists.Empty loop
               Target := Self.Name_To_Region
                 (Self.Set.Use_Package.Head (Self.Use_Name));

               if Target /= Region_Item_Lists.Empty then
                  Local := Self.Set.Region.Head (Target).Local;
                  Self.Current := Self.Set.Names.Find (Local, Symbol);

                  if Self.Has_Element then
                     return;
                  end if;
               end if;

               Self.Use_Name := Self.Set.Use_Package.Tail (Self.Use_Name);
            end loop;

            Self.Region := Self.Set.Region.Tail (Self.Region);
         end loop;
      end Initialize;

   end Use_Package_Cursors;

   overriding function Add_Completion
     (Self       : in out Environment_Set;
      Index      : Gela.Semantic_Types.Env_Index;
      Name       : Gela.Elements.Defining_Names.Defining_Name_Access;
      Completion : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index
   is
      Env_Index : Gela.Semantic_Types.Env_Index;
      Env : Env_Item;
      Reg : Region_Item;
   begin
      if Index in 0 | Self.Library_Level_Environment then
         --  Fix constraint_error because library_bodies doesn have env yet
         return Index;
      end if;

      Env := Self.Env.Element (Index);
      Reg := Self.Region.Head (Env.Region_List (Nested));

      Self.Use_Package.Prepend
        (Value  => Name,
         Input  => Reg.Completion,
         Output => Reg.Completion);

      Self.Use_Package.Prepend
        (Value  => Completion,
         Input  => Reg.Completion,
         Output => Reg.Completion);

      --  Replace head of Nested_Region_List with Reg
      Self.Region.Prepend
        (Value  => Reg,
         Input  => Self.Region.Tail (Env.Region_List (Nested)),
         Output => Env.Region_List (Nested));

      Env_Index := Self.Env.Find_Index (Env);

      if Env_Index = 0 then
         Self.Env.Append (Env);
         Env_Index := Self.Env.Last_Index;
      end if;

      return Env_Index;
   end Add_Completion;

   -----------------
   -- Completions --
   -----------------

   overriding function Completions
     (Self       : in out Environment_Set;
      Index      : Gela.Semantic_Types.Env_Index;
      Name       : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Environments.Completion_List
   is
      use type Region_Item_List;
      use type Gela.Elements.Defining_Names.Defining_Name_Access;

      procedure Find_Completion
        (List    : Defining_Name_List;
         Result  : out Gela.Elements.Defining_Names.Defining_Name_Access;
         Restart : in out Boolean);

      ---------------------
      -- Find_Completion --
      ---------------------

      procedure Find_Completion
        (List    : Defining_Name_List;
         Result  : out Gela.Elements.Defining_Names.Defining_Name_Access;
         Restart : in out Boolean)
      is
         use type Defining_Name_List;
         Next   : Defining_Name_List := List;
         Completion : Gela.Elements.Defining_Names.Defining_Name_Access;
      begin
         while Next /= Defining_Name_Lists.Empty loop
            Result := Self.Use_Package.Head (Next);
            Next := Self.Use_Package.Tail (Next);
            Completion := Self.Use_Package.Head (Next);

            if Completion = Name then
               return;
            elsif Result = Name then
               Result := Completion;
               Restart := True;
            end if;

            Next := Self.Use_Package.Tail (Next);
         end loop;

         Result := null;
      end Find_Completion;

      Env     : Env_Item;
      Next    : Region_Item_List;
      Result  : Gela.Elements.Defining_Names.Defining_Name_Access;
      Data    : Gela.Environments.Completion_Array
        (1 .. Gela.Environments.Completion_Index'Last);
      Last    : Gela.Environments.Completion_Index := 0;
      Restart : Boolean := False;
   begin
      if Index = Gela.Library_Environments.Library_Env then
         return Self.Lib.Completions (Index, Name);
      else
         Env := Self.Env.Element (Index);
      end if;

      for J of Env.Region_List loop
         Next := J;
         while Next /= Region_Item_Lists.Empty loop
            Find_Completion
              (Self.Region.Head (Next).Completion, Result, Restart);

            if Restart then
               return Completions (Self, Index, Result);
            elsif Result.Assigned then
               Last := Last + 1;
               Data (Last) := Result;
            end if;

            Next := Self.Region.Tail (Next);
         end loop;
      end loop;

      return (Last, Data (1 .. Last));
   end Completions;

   -----------------------
   -- Add_Defining_Name --
   -----------------------

   overriding function Add_Defining_Name
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index
   is
      use type Region_Item_List;

      procedure Update_Lib_Unit_Env
        (Old_Env : Gela.Semantic_Types.Env_Index;
         New_Env : Env_Item_Index);

      -------------------------
      -- Update_Lib_Unit_Env --
      -------------------------

      procedure Update_Lib_Unit_Env
        (Old_Env : Gela.Semantic_Types.Env_Index;
         New_Env : Env_Item_Index)
      is
         Cursor : Env_Maps.Cursor := Self.Lib_Env.Find (Old_Env);
         Symbol : Gela.Lexical_Types.Symbol;
      begin
         if Env_Maps.Has_Element (Cursor) then
            Symbol := Env_Maps.Element (Cursor);
            Self.Lib_Env.Delete (Cursor);
            Self.Lib_Env.Insert (New_Env, Symbol);
            Self.Units_Env.Replace (Symbol, New_Env);
         end if;
      end Update_Lib_Unit_Env;

      Env       : Env_Item;
      Env_Index : Gela.Semantic_Types.Env_Index;
      Reg       : Region_Item;
   begin
      if Index in Env_Item_Index then
         Env := Self.Env.Element (Index);
      else
         Env := (Region_List =>
                   (Nested | Other | Withed => Region_Item_Lists.Empty));
      end if;

      if Env.Region_List (Nested) = Region_Item_Lists.Empty then
         Reg := (Name => null,
                 Local => Self.Names.Empty_List,
                 Use_Package | Completion => Defining_Name_Lists.Empty);
      else
         Reg := Self.Region.Head (Env.Region_List (Nested));
      end if;

      Self.Names.Append
        (Symbol => Symbol,
         Name   => Name,
         Input  => Reg.Local,
         Output => Reg.Local);

      if Env.Region_List (Nested) = Region_Item_Lists.Empty then
         --  Create Nested_Region_List as (Reg)
         Self.Region.Prepend
           (Value  => Reg,
            Input  => Region_Item_Lists.Empty,
            Output => Env.Region_List (Nested));
      else
         --  Replace head of Nested_Region_List with Reg
         Self.Region.Prepend
           (Value  => Reg,
            Input  => Self.Region.Tail (Env.Region_List (Nested)),
            Output => Env.Region_List (Nested));
      end if;

      Env_Index := Self.Env.Find_Index (Env);

      if Env_Index = 0 then
         Self.Env.Append (Env);
         Env_Index := Self.Env.Last_Index;
      end if;

      Update_Lib_Unit_Env (Index, Env_Index);

      return Env_Index;
   end Add_Defining_Name;

   ---------------------
   -- Add_Use_Package --
   ---------------------

   overriding function Add_Use_Package
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index
   is

      Env_Index : Gela.Semantic_Types.Env_Index;
      Env : Env_Item;
      Reg : Region_Item;
   begin
      if Index in 0 | Self.Library_Level_Environment then
         --  Fix constraint_error because library_bodies doesn have env yet
         return Index;
      end if;

      Env := Self.Env.Element (Index);
      Reg := Self.Region.Head (Env.Region_List (Nested));

      Self.Use_Package.Prepend
        (Value  => Name,
         Input  => Reg.Use_Package,
         Output => Reg.Use_Package);

      --  Replace head of Nested_Region_List with Reg
      Self.Region.Prepend
        (Value  => Reg,
         Input  => Self.Region.Tail (Env.Region_List (Nested)),
         Output => Env.Region_List (Nested));

      Env_Index := Self.Env.Find_Index (Env);

      if Env_Index = 0 then
         Self.Env.Append (Env);
         Env_Index := Self.Env.Last_Index;
      end if;

      return Env_Index;
   end Add_Use_Package;

   ---------------------
   -- Add_With_Clause --
   ---------------------

   overriding function Add_With_Clause
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index
   is
      procedure Append (Item : Region_Item);

      Env_Index : Gela.Semantic_Types.Env_Index;
      Env    : Env_Item := Self.Env.Element (Index);
      Target : Gela.Semantic_Types.Env_Index :=
        Self.Library_Unit_Environment (Symbol);
      Target_Env : Env_Item;
      List   : Region_Item_List;

      procedure Append (Item : Region_Item) is
      begin
         Self.Region.Prepend
           (Value  => Item,
            Input  => Env.Region_List (Withed),
            Output => Env.Region_List (Withed));
      end Append;

   begin
      if Target in 0 | Self.Library_Level_Environment then
         --  Fix constraint_error because library_bodies doesn have env yet
         return Index;
      end if;

      Target := Self.Leave_Declarative_Region (Target);
      Target_Env := Self.Env.Element (Target);
      List := Target_Env.Region_List (Other);

--        Gela.Plain_Environments.Debug
--          (Self  => Self'Access,
--           Index => Target);
--
      Self.Region.For_Each (List, Append'Access);

      Env_Index := Self.Env.Find_Index (Env);

      if Env_Index = 0 then
         Self.Env.Append (Env);
         Env_Index := Self.Env.Last_Index;
      end if;

      return Env_Index;
   end Add_With_Clause;

   --------------------
   -- Direct_Visible --
   --------------------

   overriding function Direct_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
   is
      Env : Env_Item;
   begin
      if Index = Gela.Library_Environments.Library_Env then
         return Self.Lib.Direct_Visible (Index, Symbol);
      elsif Index not in Env_Item_Index then
         return None : constant Direct_Visible_Cursors.Defining_Name_Cursor :=
           (others => <>);
      end if;

      Env := Self.Env.Element (Index);

      return Result : Direct_Visible_Cursors.Defining_Name_Cursor :=
        (Set    => Plain_Environment_Set_Access (Self),
         Region => Env.Region_List (Nested),
         others => <>)
      do
         Result.Initialize (Symbol);
      end return;
   end Direct_Visible;

   -----------------------
   -- Empty_Environment --
   -----------------------

   overriding function Empty_Environment
     (Self  : Environment_Set)
      return Gela.Semantic_Types.Env_Index is
   begin
      return Self.Lib.Empty_Environment;
   end Empty_Environment;

   -----------------------------
   -- Enter_Completion_Region --
   -----------------------------

   overriding function Enter_Completion_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index
   is
      use type Region_Item_List;
      Env   : Env_Item;
      Found : Gela.Semantic_Types.Env_Index;
      Spec  : constant Region_Item_List :=
        Name_To_Region (Self, Index, Region);
      Next  : Region_Item :=
        (Name        => Region,
         Local       => Self.Names.Empty_List,
         Use_Package => Defining_Name_Lists.Empty,
         Completion  => Defining_Name_Lists.Empty);
   begin
      if Index in Env_Item_Index then
         Env := Self.Env.Element (Index);
      else
         Env := (Region_List =>
                   (Nested | Other | Withed => Region_Item_Lists.Empty));
      end if;

      if Spec /= Region_Item_Lists.Empty then
         Next := Self.Region.Head (Spec);
      end if;

--     Shall we delete region with the same Name from Other_Region_List?

      Self.Region.Prepend
        (Value  => Next,
         Input  => Env.Region_List (Nested),
         Output => Env.Region_List (Nested));

      Found := Self.Env.Find_Index (Env);

      if Found not in Env_Item_Index then
         Self.Env.Append (Env);
         Found := Self.Env.Last_Index;
      end if;

      return Found;
   end Enter_Completion_Region;

   ------------------------------
   -- Enter_Declarative_Region --
   ------------------------------

   overriding function Enter_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index
   is
      Env   : Env_Item;
      Found : Gela.Semantic_Types.Env_Index;
      Next  : constant Region_Item :=
        (Name        => Region,
         Local       => Self.Names.Empty_List,
         Use_Package => Defining_Name_Lists.Empty,
         Completion  => Defining_Name_Lists.Empty);
   begin
      if Index in Env_Item_Index then
         Env := Self.Env.Element (Index);
      else
         Env := (Region_List =>
                   (Nested | Other | Withed => Region_Item_Lists.Empty));
      end if;

      Self.Region.Prepend
        (Value  => Next,
         Input  => Env.Region_List (Nested),
         Output => Env.Region_List (Nested));

--     Shall we delete region with the same Name from Other_Region_List?
--        Self.Region.Delete
--          (Input  => Env.Other_Region_List,
--           Value  => Next,
--           Output => Env.Other_Region_List);

      Found := Self.Env.Find_Index (Env);

      if Found not in Env_Item_Index then
         Self.Env.Append (Env);
         Found := Self.Env.Last_Index;
      end if;

      return Found;
   end Enter_Declarative_Region;

   ----------
   -- Hash --
   ----------

   function Hash
     (X : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (X);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (X : Gela.Semantic_Types.Env_Index) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (X);
   end Hash;

   ------------------------------
   -- Leave_Declarative_Region --
   ------------------------------

   overriding function Leave_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index)
      return Gela.Semantic_Types.Env_Index
   is
      Found  : Gela.Semantic_Types.Env_Index;
      Env    : Env_Item := Self.Env.Element (Index);
      Region : constant Region_Item :=
        Self.Region.Head (Env.Region_List (Nested));
   begin
      --  Push top region to Other_Region_List
      Self.Region.Prepend
        (Value  => Region,
         Input  => Env.Region_List (Other),
         Output => Env.Region_List (Other));

      --  Pop top region from Nested_Region_List
      Env.Region_List (Nested) := Self.Region.Tail (Env.Region_List (Nested));

      Found := Self.Env.Find_Index (Env);

      if Found not in Env_Item_Index then
         Self.Env.Append (Env);
         Found := Self.Env.Last_Index;
      end if;

      return Found;
   end Leave_Declarative_Region;

   -------------------------------
   -- Library_Level_Environment --
   -------------------------------

   overriding function Library_Level_Environment
     (Self  : Environment_Set)
      return Gela.Semantic_Types.Env_Index is
   begin
      return Self.Lib.Library_Level_Environment;
   end Library_Level_Environment;

   ------------------------------
   -- Library_Unit_Environment --
   ------------------------------

   overriding function Library_Unit_Environment
     (Self   : access Environment_Set;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index
   is
      use type Gela.Lexical_Types.Symbol;
   begin
      if Symbol = 0 then
         return 0;
      else
         return Self.Units_Env.Element (Symbol);
      end if;
   end Library_Unit_Environment;

   --------------------
   -- Name_To_Region --
   --------------------

   function Name_To_Region
     (Self  : access Environment_Set'Class;
      Index : Gela.Semantic_Types.Env_Index;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access)
         return Region_Item_List
   is
      use type Region_Item_List;
      use type Gela.Elements.Defining_Names.Defining_Name_Access;

      Env  : constant Env_Item := Self.Env.Element (Index);
      Next : Region_Item_List;
   begin
      for J of Env.Region_List loop
         Next := J;
         while Next /= Region_Item_Lists.Empty loop
            if Self.Region.Head (Next).Name = Name then
               return Next;
            end if;

            Next := Self.Region.Tail (Next);
         end loop;
      end loop;

      return Region_Item_Lists.Empty;
   end Name_To_Region;

   ----------------------------------
   -- Set_Library_Unit_Environment --
   ----------------------------------

   overriding procedure Set_Library_Unit_Environment
     (Self   : access Environment_Set;
      Symbol : Gela.Lexical_Types.Symbol;
      Value  : Gela.Semantic_Types.Env_Index) is
   begin
      Self.Units_Env.Include (Symbol, Value);
      Self.Lib_Env.Include (Value, Symbol);
   end Set_Library_Unit_Environment;

   -----------------
   -- Use_Visible --
   -----------------

   overriding function Use_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class is
   begin
      if Index = Gela.Library_Environments.Library_Env then
         return Self.Lib.Use_Visible (Index, Symbol);
      end if;

      return Result : Use_Package_Cursors.Defining_Name_Cursor :=
        (Set      => Plain_Environment_Set_Access (Self),
         Env      => Index,
         Region   => Region_Item_Lists.Empty,
         Use_Name => Defining_Name_Lists.Empty,
         others   => <>)
      do
         Result.Initialize (Symbol);
      end return;
   end Use_Visible;

   -------------
   -- Visible --
   -------------

   overriding function Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : access Boolean)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
   is
      use type Region_Item_List;

      Item : Region_Item_List;
   begin
      Found.all := False;

      if Index = Gela.Library_Environments.Library_Env then
         return Self.Lib.Visible (Index, Region, Symbol, Found);
      elsif Index not in Env_Item_Index then
         return None : constant Visible_Cursors.Defining_Name_Cursor :=
           (others => <>);
      end if;

      if Region.Assigned then
         Item := Name_To_Region (Self, Index, Region);
      else
         declare
            Env  : constant Env_Item := Self.Env.Element (Index);
         begin
            Item := Env.Region_List (Nested);
         end;
      end if;

      if Item = Region_Item_Lists.Empty then
         return None : constant Visible_Cursors.Defining_Name_Cursor :=
           (others => <>);
      end if;

      Found.all := True;

      return Result : Visible_Cursors.Defining_Name_Cursor :=
        (Set    => Plain_Environment_Set_Access (Self),
         others => <>)
      do
         Result.Initialize (Symbol, Item);
      end return;
   end Visible;

end Gela.Plain_Environments;
