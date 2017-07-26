with Gela.Int.Attr_Functions;
with Gela.Int.Categories;
with Gela.Int.Defining_Names;
with Gela.Int.Expressions;
with Gela.Int.Placeholders;
with Gela.Int.Symbols;
with Gela.Int.Tuples;
with Gela.Int.Visiters;

with Gela.Plain_Int_Sets.Cursors;

package body Gela.Plain_Int_Sets is

   package Category_Iterators is new Cursors.Generic_Iterators
     (Cursor         => Gela.Interpretations.Category_Cursor,
      Next           => Gela.Interpretations.Next,
      Some_Cursor    => Cursors.Category_Cursor,
      Iterators      => Gela.Interpretations.Category_Iterators);

   package Defining_Name_Iterators is new Cursors.Generic_Iterators
     (Cursor         => Gela.Interpretations.Defining_Name_Cursor,
      Next           => Gela.Interpretations.Next,
      Some_Cursor    => Cursors.Defining_Name_Cursor,
      Iterators      => Gela.Interpretations.Defining_Name_Iterators);

   package Expression_Iterators is new Cursors.Generic_Iterators
     (Cursor         => Gela.Interpretations.Expression_Cursor,
      Next           => Gela.Interpretations.Next,
      Some_Cursor    => Cursors.Expression_Cursor,
      Iterators      => Gela.Interpretations.Expression_Iterators);

   package Symbol_Iterators is new Cursors.Generic_Iterators
     (Cursor         => Gela.Interpretations.Symbol_Cursor,
      Next           => Gela.Interpretations.Next,
      Some_Cursor    => Cursors.Symbol_Cursor,
      Iterators      => Gela.Interpretations.Symbol_Iterators);

   package Profile_Iterators is new Cursors.Generic_Iterators
     (Cursor         => Gela.Interpretations.Profile_Cursor,
      Next           => Gela.Interpretations.Next,
      Some_Cursor    => Cursors.Profile_Cursor,
      Iterators      => Gela.Interpretations.Profile_Iterators);

   ---------
   -- Add --
   ---------

   not overriding procedure Add
     (Self  : access Interpretation_Set;
      Index : in out Gela.Interpretations.Interpretation_Set_Index;
      Item  : Gela.Int.Interpretation_Access)
   is
      use type Gela.Interpretations.Interpretation_Set_Index;

      procedure Update
        (Key     : Gela.Interpretations.Interpretation_Set_Index;
         Element : in out Int_Lists.List);

      ------------
      -- Update --
      ------------

      procedure Update
        (Key     : Gela.Interpretations.Interpretation_Set_Index;
         Element : in out Int_Lists.List)
      is
         pragma Unreferenced (Key);
      begin
         Element.Append (Item);
      end Update;

      Pos : Int_List_Maps.Cursor;
      Ok  : Boolean;
   begin
      if Index = 0 then
         if Self.Set_From = Self.Set_To then
            Self.Ids.Reserve_Indexes
              (Gela.Int_Sets.Interpretation_Set_Access (Self),
               Self.Set_From,
               Self.Set_To);
         end if;

         Index := Self.Set_From;
         Self.Set_From := Self.Set_From + 1;
         Self.Map.Insert (Index, Int_Lists.Empty_List, Pos, Ok);
      else
         Pos := Self.Map.Find (Index);
      end if;

      Self.Map.Update_Element (Pos, Update'Access);
   end Add;

   ---------
   -- Add --
   ---------

   not overriding procedure Add
     (Self  : access Interpretation_Set;
      Index : out Gela.Interpretations.Interpretation_Index;
      Item  : Gela.Int.Interpretation_Access)
   is
      use type Gela.Interpretations.Interpretation_Index;
   begin
      if Self.Item_From = Self.Item_To then
         Self.Ids.Reserve_Indexes
              (Gela.Int_Sets.Interpretation_Set_Access (Self),
               Self.Item_From,
               Self.Item_To);
      end if;

      Index := Self.Item_From;
      Self.Item_From := Self.Item_From + 1;
      Self.Int_Map.Insert (Index, Item);
      Item.Index := Index;
   end Add;

   ----------------
   -- Categories --
   ----------------

   overriding function Categories
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Category_Iterators
                 .Forward_Iterator'Class
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
   begin
      return Result : Category_Iterators.Iterator do
         if Index /= 0 then
            Result.Cursor.Initialize (Self, Index);
         end if;
      end return;
   end Categories;

   --------------------
   -- Defining_Names --
   --------------------

   overriding function Defining_Names
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Defining_Name_Iterators
                 .Forward_Iterator'Class
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
   begin
      return Result : Defining_Name_Iterators.Iterator do
         if Index /= 0 then
            Result.Cursor.Initialize (Self, Index);
         end if;
      end return;
   end Defining_Names;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Index)
      return Gela.Int.Interpretation_Access is
   begin
      return Self.Int_Map.Element (Index);
   end Element;

   -----------------
   -- Expressions --
   -----------------

   overriding function Expressions
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Expression_Iterators
                 .Forward_Iterator'Class
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
   begin
      return Result : Expression_Iterators.Iterator do
         if Index /= 0 then
            Result.Cursor.Initialize (Self, Index);
         end if;
      end return;
   end Expressions;

   ----------------
   -- Get_Cursor --
   ----------------

   overriding function Get_Cursor
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Cursor'Class
   is
   begin
      return Result : Cursor do

         declare
            procedure Get
              (Key     : Gela.Interpretations.Interpretation_Set_Index;
               Element : Int_Lists.List);

            ---------
            -- Get --
            ---------

            procedure Get
              (Key     : Gela.Interpretations.Interpretation_Set_Index;
               Element : Int_Lists.List)
            is
               pragma Unreferenced (Key);
            begin
               Result.Pos := Element.First;
            end Get;

            use type Gela.Interpretations.Interpretation_Set_Index;
         begin
            if Index /= 0 then
               Int_List_Maps.Query_Element (Self.Map.Find (Index), Get'Access);
            end if;

            Result.Set := Self;
         end;

      end return;
   end Get_Cursor;

   ---------------
   -- Get_Index --
   ---------------

   overriding function Get_Index
     (Self : Cursor)
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

   overriding function Has_Element (Self : Cursor) return Boolean is
   begin
      return Int_Lists.Has_Element (Self.Pos);
   end Has_Element;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : Gela.Interpretations.Interpretation_Index)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Value);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : Gela.Interpretations.Interpretation_Set_Index)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Value);
   end Hash;

   --------------
   -- Profiles --
   --------------

   overriding function Profiles
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Profile_Iterators
                 .Forward_Iterator'Class
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
   begin
      return Result : Profile_Iterators.Iterator do
         if Index /= 0 then
            Result.Cursor.Initialize (Self, Index);
         end if;
      end return;
   end Profiles;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Cursor) is
   begin
      Int_Lists.Next (Self.Pos);
   end Next;

   -------------
   -- Symbols --
   -------------

   overriding function Symbols
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Symbol_Iterators
                 .Forward_Iterator'Class
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
   begin
      return Result : Symbol_Iterators.Iterator do
         if Index /= 0 then
            Result.Cursor.Initialize (Self, Index);
         end if;
      end return;
   end Symbols;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self   : Cursor;
      Target : access Gela.Interpretations.Up_Visiter'Class)
   is
      package Each is
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

      end Each;

      package body Each is

         overriding procedure Defining_Name
           (Self  : access Visiter;
            Value : Gela.Int.Defining_Names.Defining_Name)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Defining_Name
              (Name   => Value.Name,
               Cursor => Visit.Self);
         end Defining_Name;

         overriding procedure Expression
           (Self  : access Visiter;
            Value : Gela.Int.Expressions.Expression)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Expression
              (Tipe   => Value.Expression_Type,
               Cursor => Visit.Self);
         end Expression;

         overriding procedure Expression_Category
           (Self  : access Visiter;
            Value : Gela.Int.Categories.Category)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Expression_Category
              (Match  => Value.Match,
               Cursor => Visit.Self);
         end Expression_Category;

         overriding procedure Attr_Function
           (Self  : access Visiter;
            Value : Gela.Int.Attr_Functions.Attr_Function)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Attr_Function
              (Kind   => Value.Kind,
               Tipe   => Value.Tipe,
               Cursor => Visit.Self);
         end Attr_Function;

         overriding procedure Placeholder
           (Self  : access Visiter;
            Value : Gela.Int.Placeholders.Placeholder)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Placeholder
              (Kind   => Value.Placeholder_Kind,
               Cursor => Visit.Self);
         end Placeholder;

         overriding procedure Symbol
           (Self  : access Visiter;
            Value : Gela.Int.Symbols.Symbol)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Symbol
              (Symbol => Value.Get_Symbol,
               Cursor => Visit.Self);
         end Symbol;

         overriding procedure Tuple
           (Self  : access Visiter;
            Value : Gela.Int.Tuples.Tuple) is
         begin
            null;
         end Tuple;

         overriding procedure Chosen_Tuple
           (Self  : access Visiter;
            Value : Gela.Int.Tuples.Chosen_Tuple)
         is
            pragma Unreferenced (Self, Value);
         begin
            raise Constraint_Error with "Unexpected down interpretation in up";
         end Chosen_Tuple;

      end Each;

      V : aliased Each.Visiter;
   begin
      Int_Lists.Element (Self.Pos).Visit (V'Access);
   end Visit;

end Gela.Plain_Int_Sets;
