package body Gela.Plain_Environments is

   package Cursors is

      type Defining_Name_Cursor is
        new Gela.Defining_Name_Cursors.Defining_Name_Cursor with
         record
            Set   : Plain_Environment_Set_Access;
            Index : Gela.Semantic_Types.Env_Index;
         end record;

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean;

      overriding function Element
        (Self : Defining_Name_Cursor)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

      overriding procedure Next
        (Self : in out Defining_Name_Cursor);

   end Cursors;

   package body Cursors is

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean is
      begin
         return Self.Index in Change_Index;
      end Has_Element;

      -------------
      -- Element --
      -------------

      overriding function Element
        (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access is
      begin
         return Self.Set.Set.Element (Self.Index).DV_Name;
      end Element;

      ----------
      -- Next --
      ----------

      overriding procedure Next (Self : in out Defining_Name_Cursor) is
         use type Gela.Lexical_Types.Symbol;

         Prev : constant Change := Self.Set.Set.Element (Self.Index);
         Next : Gela.Semantic_Types.Env_Index := Prev.Prev;
         Item : Change;
      begin
         while Next in Change_Index loop
            Item := Self.Set.Set.Element (Next);

            if  --  Item.Kind = Direct_Visible and then
              Item.DV_Symbol = Prev.DV_Symbol
            then
               exit;
            end if;

            Next := Item.Prev;
         end loop;

         Self.Index := Next;
      end Next;

   end Cursors;

   ------------------------
   -- Add_Direct_Visible --
   ------------------------

   overriding function Add_Direct_Visible
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index
   is
      Next : constant Change :=
        (Kind      => Direct_Visible,
         Prev      => Index,
         DV_Symbol => Symbol,
         DV_Name   => Name);

      Found : constant Gela.Semantic_Types.Env_Index :=
        Self.Set.Find_Index (Next);
   begin
      if Found in Change_Index then
         return Found;
      end if;

      Self.Set.Append (Next);

      return Self.Set.Last_Index;
   end Add_Direct_Visible;

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

      Next  : Gela.Semantic_Types.Env_Index := Index;
      Item  : Change;
   begin
      if Index = Gela.Library_Environments.Library_Env then
         return Self.Lib.Direct_Visible (Index, Symbol);
      end if;

      while Next in Change_Index loop
         Item := Self.Set.Element (Next);

         if  --  Item.Kind = Direct_Visible and then
           Item.DV_Symbol = Symbol then
            exit;
         end if;

         Next := Item.Prev;
      end loop;

      return Result : constant Cursors.Defining_Name_Cursor :=
        (Set   => Plain_Environment_Set_Access (Self),
         Index => Next);
   end Direct_Visible;

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
