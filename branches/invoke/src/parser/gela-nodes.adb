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
      use Gela.LARL_Parsers_Nodes;
      use type Gela.Lexical_Types.Token_Count;
      use type Gela.Elements.Element_Access;

      Nested   : constant Nested_Kind_Array := Node'Class (Self).Nested;
      Result   : Gela.Lexical_Types.Token_Count;
      Element  : Gela.Elements.Element_Access;
      Sequence : Gela.Elements.Element_Sequence_Access;
   begin
      for J in Nested'Range loop
         case Nested (J) is
            when Gela.Elements.Nested_Token =>
               Result := -Self.Children (J);

               if Result /= 0 then
                  return Result;
               end if;

            when Gela.Elements.Nested_Element =>
               Element := -Self.Children (J);

               if Element /= null then
                  Result := Element.First_Token;

                  if Result /= 0 then
                     return Result;
                  end if;
               end if;
            when Gela.Elements.Nested_Sequence =>
               Sequence := -Self.Children (J);

               declare
                  Cursor : Gela.Elements.Element_Sequence_Cursor :=
                    Sequence.First;
               begin
                  while Cursor.Has_Element loop
                     Element := Cursor.Element;

                     if Element /= null then
                        Result := Element.First_Token;

                        if Result /= 0 then
                           return Result;
                        end if;
                     end if;

                     Cursor.Next;
                  end loop;
               end;
         end case;
      end loop;

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
      use Gela.LARL_Parsers_Nodes;
      use type Gela.Lexical_Types.Token_Count;
      use type Gela.Elements.Element_Access;

      Nested   : constant Nested_Kind_Array := Node'Class (Self).Nested;
      Result   : Gela.Lexical_Types.Token_Count;
      Element  : Gela.Elements.Element_Access;
      Sequence : Gela.Elements.Element_Sequence_Access;
   begin
      for J in reverse Nested'Range loop
         case Nested (J) is
            when Gela.Elements.Nested_Token =>
               Result := -Self.Children (J);

               if Result /= 0 then
                  return Result;
               end if;

            when Gela.Elements.Nested_Element =>
               Element := -Self.Children (J);

               if Element /= null then
                  Result := Element.Last_Token;

                  if Result /= 0 then
                     return Result;
                  end if;
               end if;
            when Gela.Elements.Nested_Sequence =>
               Sequence := -Self.Children (J);

               declare
                  Cursor : Gela.Elements.Element_Sequence_Cursor :=
                    Sequence.First;
                  List   : array (1 .. Sequence.Length) of
                    Gela.Elements.Element_Access;
                  Index  : Positive := 1;
               begin
                  while Cursor.Has_Element loop
                     List (Index) := Cursor.Element;
                     Index := Index + 1;
                     Cursor.Next;
                  end loop;

                  for J in reverse List'Range loop
                     Element := List (J);

                     if Element /= null then
                        Result := Element.Last_Token;

                        if Result /= 0 then
                           return Result;
                        end if;
                     end if;

                  end loop;
               end;
         end case;
      end loop;

      return 0;
   end Last_Token;

   ------------------
   -- Nested_Items --
   ------------------

   overriding function Nested_Items
     (Self  : Node) return Gela.Elements.Nested_Array
   is
      pragma Unreferenced (Self);
   begin
      return (1 => (Gela.Elements.Nested_Token, 1));
   end Nested_Items;

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

      -------------
      -- Element --
      -------------

      overriding function Element
        (Self : Sequence_Cursor) return Gela.Elements.Element_Access
      is
         Result : constant Generic_Element_Sequences.Item_Access :=
           Self.Element;
      begin
         return Gela.Elements.Element_Access (Result);
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

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Sequence)
         return Gela.Elements.Element_Sequences.Sequence_Cursor'Class is
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
