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

   package Any_Iterators is new Cursors.Generic_Iterators
     (Cursor         => Gela.Interpretations.Any_Cursor,
      Next           => Gela.Interpretations.Next,
      Some_Cursor    => Cursors.Any_Cursor,
      Iterators      => Gela.Interpretations.Any_Iterators);

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

   ----------
   -- Each --
   ----------

   overriding function Each
     (Self   : access Interpretation_Set;
      Index  : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Any_Iterators
                 .Forward_Iterator'Class
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
   begin
      return Result : Any_Iterators.Iterator do
         if Index /= 0 then
            Result.Cursor.Initialize (Self, Index);
         end if;
      end return;
   end Each;

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

end Gela.Plain_Int_Sets;
