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
         return Self.Set.Direct_Visible.Element (Self.Name).Name;
      end Element;

      ----------
      -- Next --
      ----------

      overriding procedure Next (Self : in out Defining_Name_Cursor) is
         Set    : constant Plain_Environment_Set_Access := Self.Set;
         Symbol : constant Gela.Lexical_Types.Symbol :=
           Set.Direct_Visible.Element (Self.Name).Symbol;
      begin
         Self.Name := Set.Direct_Visible.Element (Self.Name).Prev;

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

         Region : Gela.Semantic_Types.Env_Index;
         Set    : constant Plain_Environment_Set_Access := Self.Set;
      begin
         loop
            if Self.Name in Direct_Visible_Item_Index then
               if Set.Direct_Visible.Element (Self.Name).Symbol = Symbol then
                  return;
               else
                  Self.Name := Set.Direct_Visible.Element (Self.Name).Prev;
               end if;
            else
               Region := Set.Region.Element (Self.Region).Prev;

               if Region in Region_Item_Index then
                  Self.Region := Region;
                  Self.Name := Set.Region.Element (Self.Region).Local;
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
      Region : Region_Item;
      Found  : Direct_Visible_Item_Count;
      Next   : Direct_Visible_Item :=
        (Prev   => 0,
         Symbol => Symbol,
         Name   => Name);
   begin
      if Index in Region_Item_Index then
         Region := Self.Region.Element (Index);
         Next.Prev := Region.Local;
      else
         Region := (Prev   => 0,
                    Region => null,
                    Local  => 0);
      end if;

      Found := Self.Direct_Visible.Find_Index (Next);

      if Found in Direct_Visible_Item_Index then
         Region.Local := Found;

         declare
            Result : constant Gela.Semantic_Types.Env_Index :=
              Self.Region.Find_Index (Region);
         begin
            if Result in Region_Item_Index then
               return Result;
            end if;
         end;
      end if;

      Self.Direct_Visible.Append (Next);
      Region.Local := Self.Direct_Visible.Last_Index;
      Self.Region.Append (Region);

      return Self.Region.Last_Index;
   end Add_Defining_Name;

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

   begin
      if Index = Gela.Library_Environments.Library_Env then
         return Self.Lib.Direct_Visible (Index, Symbol);
      elsif Index not in Region_Item_Index then
         return Cursors.Defining_Name_Cursor'
           (null, Region_Item_Index'First, 0);
      end if;

      return Result : Cursors.Defining_Name_Cursor :=
        (Set    => Plain_Environment_Set_Access (Self),
         Region => Index,
         Name   => Self.Region.Element (Index).Local)
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
      Next : constant Region_Item :=
        (Prev   => Index,
         Region => Region,
         Local  => 0);

      Found : constant Gela.Semantic_Types.Env_Index :=
        Self.Region.Find_Index (Next);
   begin
      if Found in Region_Item_Index then
         return Found;
      end if;

      Self.Region.Append (Next);

      return Self.Region.Last_Index;
   end Enter_Declarative_Region;

   ------------------------------
   -- Leave_Declarative_Region --
   ------------------------------

   overriding function Leave_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index)
      return Gela.Semantic_Types.Env_Index
   is
      Region : constant Region_Item := Self.Region.Element (Index);
   begin
      return Region.Prev;
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
   begin
      return Self.Lib.Visible (Index, Region, Symbol, Found);
   end Visible;

end Gela.Plain_Environments;
