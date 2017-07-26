with Gela.Int.Attr_Functions;
with Gela.Int.Categories;
with Gela.Int.Defining_Names;
with Gela.Int.Expressions;
with Gela.Int.Symbols;

package body Gela.Plain_Int_Sets.Cursors is

   generic
      type T is new Gela.Int.Interpretation with private;
   procedure Step (Self : in out Base_Cursor'Class);

   -----------------------
   -- Generic_Iterators --
   -----------------------

   package body Generic_Iterators is

      overriding function First (Self : Iterator) return Cursor'Class is
      begin
         return Self.Cursor;
      end First;

      overriding function Next
        (Self     : Iterator;
         Position : Cursor'Class) return Cursor'Class
      is
         pragma Unreferenced (Self);
      begin
         return Result : Cursor'Class := Position do
            Result.Next;
         end return;
      end Next;

   end Generic_Iterators;

   ----------
   -- Step --
   ----------

   procedure Step (Self : in out Base_Cursor'Class) is
   begin
      while Int_Lists.Has_Element (Self.Pos) loop

         exit when Int_Lists.Element (Self.Pos).all in T;

         Int_Lists.Next (Self.Pos);
      end loop;
   end Step;

   procedure Category_Step is new Step (Gela.Int.Categories.Category);
   procedure Defining_Name_Step is
     new Step (Gela.Int.Defining_Names.Defining_Name);
   procedure Expression_Step is new Step (Gela.Int.Expressions.Expression);
   procedure Symbol_Step is new Step (Gela.Int.Symbols.Symbol);
   procedure Profile_Step is new Step (Gela.Int.Attr_Functions.Attr_Function);

   --------------------
   -- Attribute_Kind --
   --------------------

   overriding function Attribute_Kind
     (Self : Profile_Cursor)
      return Gela.Lexical_Types.Predefined_Symbols.Attribute
   is
      Item   : constant Gela.Int.Interpretation_Access :=
        Int_Lists.Element (Self.Pos);
   begin
      return Gela.Int.Attr_Functions.Attr_Function (Item.all).Kind;
   end Attribute_Kind;

   ------------------------
   -- Corresponding_Type --
   ------------------------

   overriding function Corresponding_Type
     (Self : Profile_Cursor)
      return Gela.Semantic_Types.Type_Index
   is
      Item   : constant Gela.Int.Interpretation_Access :=
        Int_Lists.Element (Self.Pos);
   begin
      return Gela.Int.Attr_Functions.Attr_Function (Item.all).Tipe;
   end Corresponding_Type;

   -------------------
   -- Defining_Name --
   -------------------

   overriding function Defining_Name
     (Self : Defining_Name_Cursor)
      return Gela.Elements.Defining_Names.Defining_Name_Access
   is
      Item   : constant Gela.Int.Interpretation_Access :=
        Int_Lists.Element (Self.Pos);
   begin
      return Gela.Int.Defining_Names.Defining_Name (Item.all).Name;
   end Defining_Name;

   ---------------------
   -- Expression_Type --
   ---------------------

   overriding function Expression_Type
     (Self : Expression_Cursor)
      return Gela.Semantic_Types.Type_Index
   is
      Item   : constant Gela.Int.Interpretation_Access :=
        Int_Lists.Element (Self.Pos);
   begin
      return Gela.Int.Expressions.Expression (Item.all).Expression_Type;
   end Expression_Type;

   ---------------
   -- Get_Index --
   ---------------

   overriding function Get_Index
     (Self : Base_Cursor)
      return Gela.Interpretations.Interpretation_Index
   is
      use type Gela.Interpretations.Interpretation_Index;

      Item   : constant Gela.Int.Interpretation_Access :=
        Int_Lists.Element (Self.Pos);
      Result : Gela.Interpretations.Interpretation_Index;
   begin
      if Item.Index /= 0 then
         return Item.Index;
      end if;

      Self.Set.Add (Result, Item);

      return Result;
   end Get_Index;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Self : Base_Cursor) return Boolean is
   begin
      return Int_Lists.Has_Element (Self.Pos);
   end Has_Element;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self  : out Category_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index) is
   begin
      Self := (Set, Set.Map (Index).First);
      Category_Step (Self);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self  : out Defining_Name_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index) is
   begin
      Self := (Set, Set.Map (Index).First);
      Defining_Name_Step (Self);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self  : out Expression_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index) is
   begin
      Self := (Set, Set.Map (Index).First);
      Expression_Step (Self);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self  : out Profile_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index) is
   begin
      Self := (Set, Set.Map (Index).First);
      Profile_Step (Self);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self  : out Symbol_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index) is
   begin
      Self := (Set, Set.Map (Index).First);
      Symbol_Step (Self);
   end Initialize;

   -------------
   -- Matcher --
   -------------

   overriding function Matcher
     (Self : Category_Cursor)
      return Gela.Interpretations.Type_Matcher_Access
   is
      Item   : constant Gela.Int.Interpretation_Access :=
        Int_Lists.Element (Self.Pos);
   begin
      return Gela.Int.Categories.Category (Item.all).Match;
   end Matcher;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Category_Cursor) is
   begin
      Int_Lists.Next (Self.Pos);
      Category_Step (Self);
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Defining_Name_Cursor) is
   begin
      Int_Lists.Next (Self.Pos);
      Defining_Name_Step (Self);
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Expression_Cursor) is
   begin
      Int_Lists.Next (Self.Pos);
      Expression_Step (Self);
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Symbol_Cursor) is
   begin
      Int_Lists.Next (Self.Pos);
      Symbol_Step (Self);
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Profile_Cursor) is
   begin
      Int_Lists.Next (Self.Pos);
      Profile_Step (Self);
   end Next;

   ------------
   -- Symbol --
   ------------

   overriding function Symbol
     (Self : Symbol_Cursor)
      return Gela.Lexical_Types.Symbol
   is
      Item   : constant Gela.Int.Interpretation_Access :=
        Int_Lists.Element (Self.Pos);
   begin
      return Gela.Int.Symbols.Symbol (Item.all).Get_Symbol;
   end Symbol;

end Gela.Plain_Int_Sets.Cursors;
