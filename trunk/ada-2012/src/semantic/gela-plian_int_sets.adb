with Gela.Int.Attr_Functions;
with Gela.Int.Defining_Names;
with Gela.Int.Expressions;
with Gela.Int.Visiters;

package body Gela.Plian_Int_Sets is

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

      Result : Gela.Interpretations.Interpretation_Index;
   begin
      if Self.Set.Item_From = Self.Set.Item_To then
         Self.Set.Ids.Reserve_Indexes
              (Gela.Int_Sets.Interpretation_Set_Access (Self.Set),
               Self.Set.Item_From,
               Self.Set.Item_To);
      end if;

      Result := Self.Set.Item_From;
      Self.Set.Item_From := Self.Set.Item_From + 1;
      Self.Set.Int_Map.Insert (Result, Int_Lists.Element (Self.Pos));

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


   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Cursor) is
   begin
      Int_Lists.Next (Self.Pos);
   end Next;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self   : Cursor;
      Target : access Gela.Interpretations.Visiter'Class)
   is
      package Each is
         type Visiter is new Gela.Int.Visiters.Visiter with null record;

         overriding procedure Defining_Name
           (Self  : access Visiter;
            Value : Gela.Int.Defining_Names.Defining_Name);

         overriding procedure Expression
           (Self  : access Visiter;
            Value : Gela.Int.Expressions.Expression);

         overriding procedure Attr_Function
           (Self  : access Visiter;
            Value : Gela.Int.Attr_Functions.Attr_Function);

      end Each;

      package body Each is

         overriding procedure Defining_Name
           (Self  : access Visiter;
            Value : Gela.Int.Defining_Names.Defining_Name)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Defining_Name
              (Name => Value.Name,
               Down => Value.Down);
         end Defining_Name;

         overriding procedure Expression
           (Self  : access Visiter;
            Value : Gela.Int.Expressions.Expression)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Expression
              (Tipe => Value.Expression_Type,
               Down => Value.Down);
         end Expression;

         overriding procedure Attr_Function
           (Self  : access Visiter;
            Value : Gela.Int.Attr_Functions.Attr_Function)
         is
            pragma Unreferenced (Self);
         begin
            Target.On_Attr_Function
              (Kind => Value.Kind,
               Down => Value.Down);
         end Attr_Function;

      end Each;

      V : aliased Each.Visiter;
   begin
      Int_Lists.Element (Self.Pos).Visit (V'Access);
   end Visit;

end Gela.Plian_Int_Sets;
