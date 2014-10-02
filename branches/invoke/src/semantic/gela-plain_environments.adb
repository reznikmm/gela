package body Gela.Plain_Environments is

   package Cursors is
      --  Cursor over names in Direct_Visible_Item_List with some Symbol
      type Defining_Name_Cursor is
        new Gela.Defining_Name_Cursors.Defining_Name_Cursor with
         record
            Set    : Plain_Environment_Set_Access;
            Name   : Direct_Visible_Item_Count;
            --  Points to Direct_Visible_Item_List
         end record;

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean;

      overriding function Element
        (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access;

      overriding procedure Next
        (Self : in out Defining_Name_Cursor);

      procedure Internal_Next
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol);
   end Cursors;

   -------------
   -- Cursors --
   -------------

   package body Cursors is

      -----------------
      -- Has_Element --
      -----------------

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean is
      begin
         return Self.Name in Direct_Visible_Item_Index;
      end Has_Element;

      -------------
      -- Element --
      -------------

      overriding function Element
        (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access is
      begin
         return Self.Set.Direct_Visible.Head (Self.Name).Name;
      end Element;

      ----------
      -- Next --
      ----------

      overriding procedure Next (Self : in out Defining_Name_Cursor) is
         Set    : constant Plain_Environment_Set_Access := Self.Set;
         Symbol : constant Gela.Lexical_Types.Symbol :=
           Set.Direct_Visible.Head (Self.Name).Symbol;
      begin
         Self.Name := Set.Direct_Visible.Tail (Self.Name);

         Defining_Name_Cursor'Class (Self).Internal_Next (Symbol);
      end Next;

      -------------------
      -- Internal_Next --
      -------------------

      procedure Internal_Next
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol)
      is
         use type Gela.Lexical_Types.Symbol;

         Set : constant Plain_Environment_Set_Access := Self.Set;
      begin
         while Self.Name in Direct_Visible_Item_Index
           and then Set.Direct_Visible.Head (Self.Name).Symbol /= Symbol
         loop
            Self.Name := Set.Direct_Visible.Tail (Self.Name);
         end loop;
      end Internal_Next;

   end Cursors;

   package Direct_Visible_Cursors is
      --  Cursor over names in Local then go to enclosing region, etc
      type Defining_Name_Cursor is
        new Cursors.Defining_Name_Cursor with
         record
            Region : Region_Item_Index;
         end record;

      procedure Internal_Next
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol);
   end Direct_Visible_Cursors;

   ----------------------------
   -- Direct_Visible_Cursors --
   ----------------------------

   package body Direct_Visible_Cursors is

      -------------------
      -- Internal_Next --
      -------------------

      procedure Internal_Next
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol)
      is
         use type Gela.Lexical_Types.Symbol;

         Region : Region_Item_Count;
         Set    : constant Plain_Environment_Set_Access := Self.Set;
      begin
         loop
            Cursors.Defining_Name_Cursor (Self).Internal_Next (Symbol);
            exit when Self.Has_Element;
            Region := Set.Region.Tail (Self.Region);

            if Region in Region_Item_Index then
               Self.Region := Region;
               Self.Name := Set.Region.Head (Self.Region).Local;
            else
               return;
            end if;
         end loop;
      end Internal_Next;

   end Direct_Visible_Cursors;

   package Use_Package_Cursors is
      --  Cursor over names in each used package
      type Defining_Name_Cursor is
        new Cursors.Defining_Name_Cursor with
         record
            Env      : Env_Item_Index;
            Region   : Region_Item_Count;
            --  Position in Env_Item.Nested_Region_List list
            Use_Name : Defining_Name_Item_Count;
            --  Position in Region.Use_Package list
         end record;

      function Name_To_Region
        (Self : Defining_Name_Cursor;
         Name : Gela.Elements.Defining_Names.Defining_Name_Access)
         return Region_Item_Count;

      procedure Internal_Next
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol);

      procedure Initialize
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol);

   end Use_Package_Cursors;

   ----------------------------
   -- Direct_Visible_Cursors --
   ----------------------------

   package body Use_Package_Cursors is

      -------------------
      -- Internal_Next --
      -------------------

      procedure Internal_Next
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol)
      is
         use type Gela.Lexical_Types.Symbol;
         use type Defining_Name_Item_Count;
         use type Region_Item_Count;

         Name   : Defining_Name_Item_Count;
         Target : Region_Item_Count;
         Set    : constant Plain_Environment_Set_Access := Self.Set;
      begin
         loop
            --  Next name in Region.Local
            Cursors.Defining_Name_Cursor (Self).Internal_Next (Symbol);
            exit when Self.Has_Element;

            Target := 0;

            while Target = 0 loop
               --  Next name in use clauses of Region
               Name := Self.Set.Use_Package.Tail (Self.Use_Name);

               while Name = 0 loop
                  Self.Region := Self.Set.Region.Tail (Self.Region);

                  if Self.Region = 0 then
                     return;
                  end if;

                  Name := Self.Set.Region.Head (Self.Region).Use_Package;
               end loop;

               Target := Self.Name_To_Region
                 (Self.Set.Use_Package.Head (Name));
            end loop;

            Self.Name := Set.Region.Head (Target).Local;
         end loop;
      end Internal_Next;

      --------------------
      -- Name_To_Region --
      --------------------

      function Name_To_Region
        (Self : Defining_Name_Cursor;
         Name : Gela.Elements.Defining_Names.Defining_Name_Access)
         return Region_Item_Count
      is
         use type Region_Item_Count;
         use type Gela.Elements.Defining_Names.Defining_Name_Access;

         Env  : constant Env_Item := Self.Set.Env.Element (Self.Env);
         Next : Region_Item_Count := Env.Nested_Region_List;
      begin
         while Next /= 0 loop
            if Self.Set.Region.Head (Next).Name = Name then
               return Next;
            end if;

            Next := Self.Set.Region.Tail (Next);
         end loop;

         Next := Env.Other_Region_List;

         while Next /= 0 loop
            if Self.Set.Region.Head (Next).Name = Name then
               return Next;
            end if;

            Next := Self.Set.Region.Tail (Next);
         end loop;

         return 0;
      end Name_To_Region;

      procedure Initialize
        (Self   : in out Defining_Name_Cursor;
         Symbol : Gela.Lexical_Types.Symbol)
      is
         use type Region_Item_Count;
         use type Defining_Name_Item_Count;
         Target : Region_Item_Count;
      begin
         Self.Region := Self.Set.Env.Element (Self.Env).Nested_Region_List;

         while Self.Region /= 0 loop
            Self.Use_Name := Self.Set.Region.Head (Self.Region).Use_Package;

            while Self.Use_Name /= 0 loop
               Target := Self.Name_To_Region
                 (Self.Set.Use_Package.Head (Self.Use_Name));

               if Target /= 0 then
                  Self.Name := Self.Set.Region.Head (Target).Local;
                  Cursors.Defining_Name_Cursor (Self).Internal_Next (Symbol);

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
      use type Region_Item_Count;

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
      DV        : constant Direct_Visible_Item :=
        (Symbol => Symbol,
         Name   => Name);
   begin
      if Index in Env_Item_Index then
         Env := Self.Env.Element (Index);
      else
         Env := (Nested_Region_List => 0, Other_Region_List => 0);
      end if;

      if Env.Nested_Region_List = 0 then
         Reg := (Name => null, Local => 0, Use_Package => 0);
      else
         Reg := Self.Region.Head (Env.Nested_Region_List);
      end if;

      Self.Direct_Visible.Prepend
        (Value  => DV,
         Input  => Reg.Local,
         Output => Reg.Local);

      if Env.Nested_Region_List = 0 then
         --  Create Nested_Region_List as (Reg)
         Self.Region.Prepend
           (Value  => Reg,
            Input  => 0,
            Output => Env.Nested_Region_List);
      else
         --  Replace head of Nested_Region_List with Reg
         Self.Region.Prepend
           (Value  => Reg,
            Input  => Self.Region.Tail (Env.Nested_Region_List),
            Output => Env.Nested_Region_List);
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
      Env : Env_Item := Self.Env.Element (Index);
      Reg : Region_Item := Self.Region.Head (Env.Nested_Region_List);
   begin
      Self.Use_Package.Prepend
        (Value  => Name,
         Input  => Reg.Use_Package,
         Output => Reg.Use_Package);

      --  Replace head of Nested_Region_List with Reg
      Self.Region.Prepend
        (Value  => Reg,
         Input  => Self.Region.Tail (Env.Nested_Region_List),
         Output => Env.Nested_Region_List);

      Env_Index := Self.Env.Find_Index (Env);

      if Env_Index = 0 then
         Self.Env.Append (Env);
         Env_Index := Self.Env.Last_Index;
      end if;

      return Env_Index;
   end Add_Use_Package;

   --------------------
   -- Direct_Visible --
   --------------------

   overriding function Direct_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
   is
      use type Gela.Lexical_Types.Symbol;

      Env : Env_Item;
   begin
      if Index = Gela.Library_Environments.Library_Env then
         return Self.Lib.Direct_Visible (Index, Symbol);
      elsif Index not in Env_Item_Index then
         return Direct_Visible_Cursors.Defining_Name_Cursor'
           (Set => null, Region => Region_Item_Index'First, Name => 0);
      end if;

      Env := Self.Env.Element (Index);

      return Result : Direct_Visible_Cursors.Defining_Name_Cursor :=
        (Set    => Plain_Environment_Set_Access (Self),
         Region => Env.Nested_Region_List,
         Name   => Self.Region.Head (Env.Nested_Region_List).Local)
      do
         Result.Internal_Next (Symbol);
      end return;
   end Direct_Visible;

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
         Local       => 0,
         Use_Package => 0);
   begin
      if Index in Env_Item_Index then
         Env := Self.Env.Element (Index);
      else
         Env := (Nested_Region_List => 0, Other_Region_List => 0);
      end if;

      Self.Region.Prepend
        (Value  => Next,
         Input  => Env.Nested_Region_List,
         Output => Env.Nested_Region_List);

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
        Self.Region.Head (Env.Nested_Region_List);
   begin
      --  Push top region to Other_Region_List
      Self.Region.Prepend
        (Value  => Region,
         Input  => Env.Other_Region_List,
         Output => Env.Other_Region_List);

      --  Pop top region from Nested_Region_List
      Env.Nested_Region_List := Self.Region.Tail (Env.Nested_Region_List);

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
         Region   => 0,
         Use_Name => 0,
         Name     => 0)
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
      pragma Unreferenced (Index);
      Lib_Env : constant Gela.Semantic_Types.Env_Index :=
        Self.Library_Level_Environment;
   begin
      return Self.Lib.Visible (Lib_Env, Region, Symbol, Found);
   end Visible;

end Gela.Plain_Environments;
