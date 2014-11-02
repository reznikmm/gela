package body Gela.Plain_Compilation_Unit_Sets is

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Self : in out Compilation_Unit_Set;
      Item : Gela.Compilation_Units.Compilation_Unit_Access) is
   begin
      Self.Map.Insert (Item.Name, Item);
   end Add;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self : Compilation_Unit_Cursor)
      return Gela.Compilation_Units.Compilation_Unit_Access is
   begin
      return Maps.Element (Self.Value);
   end Element;

   ----------
   -- Find --
   ----------

   overriding function Find
     (Self   : Compilation_Unit_Set;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Compilation_Units.Compilation_Unit_Access
   is
      Pos : constant Maps.Cursor := Self.Map.Find (Symbol);
   begin
      if Maps.Has_Element (Pos) then
         return Maps.Element (Pos);
      else
         return null;
      end if;
   end Find;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Compilation_Unit_Set)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor'Class is
   begin
      return Compilation_Unit_Cursor'(Value => Self.Map.First);
   end First;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Compilation_Unit_Cursor) return Boolean is
   begin
      return Maps.Has_Element (Self.Value);
   end Has_Element;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Item);
   end Hash;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Self : Compilation_Unit_Set) return Boolean is
   begin
      return Self.Map.Is_Empty;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   overriding function Length (Self : Compilation_Unit_Set) return Natural is
   begin
      return Natural (Self.Map.Length);
   end Length;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Compilation_Unit_Cursor) is
   begin
      Maps.Next (Self.Value);
   end Next;

end Gela.Plain_Compilation_Unit_Sets;
