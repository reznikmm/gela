package body Gela.Plain_Environments is

   package Cursors is

      --  Direct Visible cursor
      type Defining_Name_Cursor is
        new Gela.Defining_Name_Cursors.Defining_Name_Cursor with
         record
            Set    : Plain_Environment_Set_Access;
            Region : Region_Item_Index;
            Name   : Direct_Visible_Item_Count;
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

         Internal_Next (Self, Symbol);
      end Next;

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
            if Self.Name in Direct_Visible_Item_Index then
               if Set.Direct_Visible.Head (Self.Name).Symbol = Symbol then
                  return;
               else
                  Self.Name := Set.Direct_Visible.Tail (Self.Name);
               end if;
            else
               Region := Set.Region.Tail (Self.Region);

               if Region in Region_Item_Index then
                  Self.Region := Region;
                  Self.Name := Set.Region.Head (Self.Region).Local;
               else
                  return;
               end if;
            end if;
         end loop;
      end Internal_Next;

   end Cursors;

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
      use type Gela.Semantic_Types.Env_Index;
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
         Reg := (Name => null, Local => 0);
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
      return Gela.Semantic_Types.Env_Index is
      pragma Unreferenced (Self, Name, Index);
   begin
      return 0;
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
      use type Gela.Semantic_Types.Env_Index;
      use type Gela.Lexical_Types.Symbol;

      Env : Env_Item;
   begin
      if Index = Gela.Library_Environments.Library_Env then
         return Self.Lib.Direct_Visible (Index, Symbol);
      elsif Index not in Env_Item_Index then
         return Cursors.Defining_Name_Cursor'
           (null, Region_Item_Index'First, 0);
      end if;

      Env := Self.Env.Element (Index);

      return Result : Cursors.Defining_Name_Cursor :=
        (Set    => Plain_Environment_Set_Access (Self),
         Region => Env.Nested_Region_List,
         Name   => Self.Region.Head (Env.Nested_Region_List).Local)
      do
         Cursors.Internal_Next (Result, Symbol);
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
        (Name   => Region,
         Local  => 0);
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

   overriding procedure Library_Level_Environment
     (Self  : in out Environment_Set;
      Value : out Gela.Semantic_Types.Env_Index) is
   begin
      Self.Lib.Library_Level_Environment (Value);
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
      Lib_Env : Gela.Semantic_Types.Env_Index;
   begin
      Self.Library_Level_Environment (Lib_Env);
      return Self.Lib.Visible (Lib_Env, Region, Symbol, Found);
   end Visible;

end Gela.Plain_Environments;
