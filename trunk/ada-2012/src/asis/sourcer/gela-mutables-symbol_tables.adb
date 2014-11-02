package body Gela.Mutables.Symbol_Tables is

   -------------------
   -- Append_Denote --
   -------------------

   not overriding procedure Append_Denote
     (Self      : in out Symbol_Table;
      Payload   : Gela.Types.Payload;
      Type_Name : Gela.Nodes.Defining_Name;
      Value     : Gela.Nodes.Defining_Name)
   is
      Index : constant Positive := Positive (Payload);
      Head : Head_Record := Self.Head.Element (Index);
      Item : constant Denote_Item :=
        (Next      => Head.Denote,
         Type_Name => Type_Name,
         Name      => Value);
   begin
      Self.Denote_Store.Append (Item);
      Head.Denote := Self.Denote_Store.Last_Index;
      Self.Head.Replace_Element (Index, Head);
   end Append_Denote;

   ---------------------------
   -- Append_Direct_Visible --
   ---------------------------

   not overriding procedure Append_Direct_Visible
     (Self    : in out Symbol_Table;
      Payload : Gela.Types.Payload;
      Symbol  : Gela.Types.Symbol;
      Value   : Gela.Nodes.Defining_Name)
   is
      Index : constant Positive := Positive (Payload);
      Head : Head_Record := Self.Head.Element (Index);
      Item : constant Direct_Visible_Item :=
        (Next   => Head.Direct,
         Symbol => Symbol,
         Name   => Value);
   begin
      Self.Direct_Visible_Store.Append (Item);
      Head.Direct := Self.Direct_Visible_Store.Last_Index;
      Self.Head.Replace_Element (Index, Head);
   end Append_Direct_Visible;

   ---------------------------
   -- Append_Region_Visible --
   ---------------------------

   not overriding procedure Append_Region_Visible
     (Self    : in out Symbol_Table;
      Payload : Gela.Types.Payload;
      Region  : Gela.Nodes.Declarative_Region;
      Symbol  : Gela.Types.Symbol;
      Value   : Gela.Nodes.Defining_Name)
   is
      Index : constant Positive := Positive (Payload);
      Head : Head_Record := Self.Head.Element (Index);
      Item : constant Region_Visible_Item :=
        (Next   => Head.Region,
         Region => Region,
         Symbol => Symbol,
         Name   => Value);
   begin
      Self.Region_Visible_Store.Append (Item);
      Head.Region := Self.Region_Visible_Store.Last_Index;
      Self.Head.Replace_Element (Index, Head);
   end Append_Region_Visible;

   -----------------
   -- Clone_Table --
   -----------------

   not overriding procedure Clone_Table
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Types.Payload)
   is
      Index : constant Positive := Positive (Payload);
      Head  : constant Head_Record := Self.Head.Element (Index);
   begin
      Self.Head.Append (Head);
      Payload := Gela.Types.Payload (Self.Head.Last_Index);
   end Clone_Table;

   ------------------------
   -- Create_Empty_Table --
   ------------------------

   not overriding procedure Create_Empty_Table
     (Self    : in out Symbol_Table;
      Payload : out Gela.Types.Payload)
   is
      Head : constant Head_Record := (0, 0, 0);
   begin
      Self.Head.Append (Head);
      Payload := Gela.Types.Payload (Self.Head.Last_Index);
   end Create_Empty_Table;

   ------------
   -- Denote --
   ------------

   overriding function Denote
     (Self      : access Symbol_Table;
      Payload   : Gela.Types.Payload;
      Type_Name : Gela.Nodes.Defining_Name)
      return Gela.Nodes.Defining_Name
   is
      use type Gela.Nodes.Defining_Name;
      Index : constant Positive := Positive (Payload);
      Head  : constant Head_Record := Self.Head.Element (Index);
      Pos   : Denote_Item_Index := Head.Denote;
   begin
      while Pos /= 0 loop
         declare
            Item : constant Denote_Item := Self.Denote_Store.Element (Pos);
         begin
            if Item.Type_Name = Type_Name then
               return Item.Name;
            end if;
            Pos := Item.Next;
         end;
      end loop;

      return (null, 0);
   end Denote;

   --------------------
   -- Direct_Visible --
   --------------------

   overriding function Direct_Visible
     (Self    : access Symbol_Table;
      Payload : Gela.Types.Payload;
      Symbol  : Gela.Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor
   is
      use type Gela.Types.Symbol;
      Index : constant Positive := Positive (Payload);
      Head  : constant Head_Record := Self.Head.Element (Index);
      Pos   : Direct_Visible_Item_Index := Head.Direct;
   begin
      while Pos /= 0 loop
         declare
            Item : constant Direct_Visible_Item :=
              Self.Direct_Visible_Store.Element (Pos);
         begin
            if Item.Symbol = Symbol then
               return (Self.Direct_Cursor'Access, Gela.Types.Payload (Pos));
            end if;
            Pos := Item.Next;
         end;
      end loop;

      return (null, 0);
   end Direct_Visible;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self    : access Direct_Visible_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Nodes.Defining_Name
   is
      Index : constant Direct_Visible_Item_Index :=
        Direct_Visible_Item_Index (Payload);
   begin
      return Self.Table.Direct_Visible_Store.Element (Index).Name;
   end Element;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self    : access Region_Visible_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Nodes.Defining_Name
   is
      Index : constant Region_Visible_Item_Index :=
        Region_Visible_Item_Index (Payload);
   begin
      return Self.Table.Region_Visible_Store.Element (Index).Name;
   end Element;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self    : access Direct_Visible_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor
   is
      use type Gela.Types.Symbol;
      Pos : Direct_Visible_Item_Index :=
        Direct_Visible_Item_Index (Payload);
      Prev : constant Direct_Visible_Item :=
        Self.Table.Direct_Visible_Store.Element (Pos);
   begin
      while Pos /= 0 loop
         declare
            Item : constant Direct_Visible_Item :=
              Self.Table.Direct_Visible_Store.Element (Pos);
         begin
            if Item.Symbol = Prev.Symbol then
               return (Self, Gela.Types.Payload (Pos));
            end if;
            Pos := Item.Next;
         end;
      end loop;

      return (null, 0);
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self    : access Region_Visible_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor
   is
      use type Gela.Types.Symbol;
      use type Gela.Nodes.Declarative_Region;
      Pos : Region_Visible_Item_Index :=
        Region_Visible_Item_Index (Payload);
      Prev : constant Region_Visible_Item :=
        Self.Table.Region_Visible_Store.Element (Pos);
   begin
      while Pos /= 0 loop
         declare
            Item : constant Region_Visible_Item :=
              Self.Table.Region_Visible_Store.Element (Pos);
         begin
            if Item.Symbol = Prev.Symbol and Item.Region = Prev.Region then
               return (Self, Gela.Types.Payload (Pos));
            end if;
            Pos := Item.Next;
         end;
      end loop;

      return (null, 0);
   end Next;

   -------------
   -- Visible --
   -------------

   overriding function Visible
     (Self    : access Symbol_Table;
      Payload : Gela.Types.Payload;
      Region  : Gela.Nodes.Declarative_Region;
      Symbol  : Gela.Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor
   is
      use type Gela.Types.Symbol;
      use type Gela.Nodes.Declarative_Region;
      Index : constant Positive := Positive (Payload);
      Head  : constant Head_Record := Self.Head.Element (Index);
      Pos   : Region_Visible_Item_Index := Head.Region;
   begin
      while Pos /= 0 loop
         declare
            Item : constant Region_Visible_Item :=
              Self.Region_Visible_Store.Element (Pos);
         begin
            if Item.Symbol = Symbol and Item.Region = Region then
               return (Self.Direct_Cursor'Access, Gela.Types.Payload (Pos));
            end if;
            Pos := Item.Next;
         end;
      end loop;

      return (null, 0);
   end Visible;

end Gela.Mutables.Symbol_Tables;
