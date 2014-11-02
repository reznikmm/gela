package body Gela.Name_List_Managers is

   ------------
   -- Append --
   ------------

   procedure Append
     (Self   : in out Name_List_Manager;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Input  : List;
      Output : out List)
   is
      Value : constant Pair := (Symbol, Name);
   begin
      Self.Pair_List.Prepend
        (Value  => Value,
         Input  => Input.Index,
         Output => Output.Index);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self   : in out Name_List_Manager;
      Key    : Gela.Elements.Defining_Names.Defining_Name_Access;
      Value  : List;
      Input  : Map;
      Output : out Map)
   is
   begin
      null;
   end Append;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self : Defining_Name_Cursor)
      return Gela.Elements.Defining_Names.Defining_Name_Access
   is
   begin
      return Self.Set.Pair_List.Head (Self.Name).Name;
   end Element;

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List (Self : Name_List_Manager) return List is
      pragma Unreferenced (Self);
   begin
      return (Index => 0);
   end Empty_List;

   ----------
   -- Find --
   ----------

   function Find
     (Self   : access Name_List_Manager'Class;
      Input  : List;
      Symbol : Gela.Lexical_Types.Symbol)
      return Defining_Name_Cursor is
   begin
      return Result : Defining_Name_Cursor := (Self, Input.Index) do
         Result.Internal_Next (Symbol);
      end return;
   end Find;

   ---------------
   -- Empty_Map --
   ---------------

   function Empty_Map (Self : Name_List_Manager) return Map is
      pragma Unreferenced (Self);
   begin
      return (null record);
   end Empty_Map;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Defining_Name_Cursor) return Boolean
   is
      use type Pair_Peristent_Lists.Count_Type;
   begin
      return Self.Name > 0;
   end Has_Element;

   -------------------
   -- Internal_Next --
   -------------------

   procedure Internal_Next
     (Self   : in out Defining_Name_Cursor;
      Symbol : Gela.Lexical_Types.Symbol)
   is
      use type Gela.Lexical_Types.Symbol;
      use type Pair_Peristent_Lists.Count_Type;
   begin
      while Self.Name > 0
        and then Self.Set.Pair_List.Head (Self.Name).Symbol /= Symbol
      loop
         Self.Name := Self.Set.Pair_List.Tail (Self.Name);
      end loop;
   end Internal_Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : in out Defining_Name_Cursor)
   is
      Symbol : constant Gela.Lexical_Types.Symbol := Self.Symbol;
   begin
      Self.Name := Self.Set.Pair_List.Tail (Self.Name);
      Self.Internal_Next (Symbol);
   end Next;

   ------------
   -- Symbol --
   ------------

   function Symbol
     (Self : Defining_Name_Cursor) return Gela.Lexical_Types.Symbol is
   begin
      return Self.Set.Pair_List.Head (Self.Name).Symbol;
   end Symbol;

end Gela.Name_List_Managers;
