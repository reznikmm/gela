package body Gela.Nodes is

   ---------------------------
   -- Enclosing_Compilation --
   ---------------------------

   overriding function Enclosing_Compilation
     (Self  : Node) return Gela.Compilations.Compilation_Access is
   begin
      return Self.Enclosing_Compilation;
   end Enclosing_Compilation;

   --------------------------------
   -- Enclosing_Compilation_Unit --
   --------------------------------

   overriding function Enclosing_Compilation_Unit
     (Self  : Node) return Gela.Compilation_Units.Compilation_Unit_Access
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Enclosing_Compilation_Unit;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   overriding function Enclosing_Element
     (Self  : Node) return Gela.Elements.Element_Access is
   begin
      return Self.Enclosing_Element;
   end Enclosing_Element;

   -----------------
   -- First_Token --
   -----------------

   overriding function First_Token
     (Self  : Node) return Gela.Lexical_Types.Token_Count
   is
      pragma Unreferenced (Self);
   begin
      return 0;
   end First_Token;

   -------------------------
   -- Is_Part_Of_Implicit --
   -------------------------

   overriding function Is_Part_Of_Implicit (Self  : Node) return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   --------------------------
   -- Is_Part_Of_Inherited --
   --------------------------

   overriding function Is_Part_Of_Inherited (Self : Node) return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   -------------------------
   -- Is_Part_Of_Instance --
   -------------------------

   overriding function Is_Part_Of_Instance (Self  : Node) return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   ----------------
   -- Last_Token --
   ----------------

   overriding function Last_Token
     (Self  : Node)
      return Gela.Lexical_Types.Token_Count
   is
      pragma Unreferenced (Self);
   begin
      return 0;
   end Last_Token;

   --------------------
   -- Node_Sequences --
   --------------------

   package body Node_Sequences is

      ------------
      -- Append --
      ------------

      overriding procedure Append
        (Self : in out Sequence;
         Item : Generic_Element_Sequences.Item_Access)
      is
         X : constant Node_Access := Node_Access (Item);
         Y : constant Generic_Element_Sequences.Item_Access :=
           Generic_Element_Sequences.Item_Access (X);
      begin
         Self.List.Append (Y);
      end Append;

      -------------
      -- Element --
      -------------

      overriding function Element
        (Self : Sequence_Cursor) return Generic_Element_Sequences.Item_Access
      is
      begin
         return Lists.Element (Self.Data);
      end Element;

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Sequence)
         return Generic_Element_Sequences.Sequence_Cursor'Class is
      begin
         return Sequence_Cursor'(Data => Self.List.First);
      end First;

      -----------------
      -- Has_Element --
      -----------------

      overriding function Has_Element
        (Self : Sequence_Cursor) return Boolean is
      begin
         return Lists.Has_Element (Self.Data);
      end Has_Element;

      --------------
      -- Is_Empty --
      --------------

      overriding function Is_Empty (Self : Sequence) return Boolean is
      begin
         return Self.List.Is_Empty;
      end Is_Empty;

      ------------
      -- Length --
      ------------

      overriding function Length (Self : Sequence) return Natural is
      begin
         return Natural (Self.List.Length);
      end Length;

      ----------
      -- Next --
      ----------

      overriding procedure Next (Self : in out Sequence_Cursor) is
      begin
         Lists.Next (Self.Data);
      end Next;

      -------------
      -- Prepend --
      -------------

      overriding procedure Prepend
        (Self : in out Sequence;
         Item : Generic_Element_Sequences.Item_Access)
      is
         X : constant Node_Access := Node_Access (Item);
         Y : constant Generic_Element_Sequences.Item_Access :=
           Generic_Element_Sequences.Item_Access (X);
      begin
         Self.List.Prepend (Y);
      end Prepend;

   end Node_Sequences;

end Gela.Nodes;
