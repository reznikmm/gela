with Gela.A4G.Contexts;

with System.Address_To_Access_Conversions;

package body Gela.A4G.Symbols is

   package Conv is new System.Address_To_Access_Conversions
     (Object => Gela.Symbols.Symbol_List'Class);

   ------------
   -- Append --
   ------------

   overriding function Append
     (Left  : access Symbol;
      Right : Gela.Symbols.Symbol_Access)
      return Gela.Symbols.Symbol_Access
   is
      Image   : League.Strings.Universal_String := Left.Image;
   begin
      Image.Append ('.');
      Image.Append (Right.Image);

      return Left.Context.Symbol (Image);
   end Append;

   ------------
   -- Append --
   ------------

   overriding function Append
     (Head  : access Symbol_List;
      Tail  : Gela.Symbols.Symbol_List_Access)
      return Gela.Symbols.Symbol_List_Access
   is
      A : constant Gela.Symbols.Symbol_Access := Head.Head;
      B : constant Gela.Symbols.Symbol_List_Access := Head.Tail;
      C : Gela.Symbols.Symbol_List_Access;
   begin
      if B.Assigned then
         C := B.Append (Tail);
      else
         C := Tail;
      end if;

      return A.Create_List (C);
   end Append;

   ------------
   -- Create --
   ------------

   function Create
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Image   : League.Strings.Universal_String;
      Folded  : League.Strings.Universal_String)
      return Gela.Symbols.Symbol_Access
   is
      Dot : constant Natural := Image.Index ('.');
   begin
      if Dot > 0 then
         declare
            Prefix : constant Gela.Symbols.Symbol_Access :=
              Context.Symbol (Image.Head (Dot - 1));
            Selector : constant Gela.Symbols.Symbol_Access :=
              Context.Symbol (Image.Tail_From (Dot + 1));
            Result : constant Symbol_Access := new Compound_Symbol'
              (Context, Image, Folded, List_Maps.Empty_Map, Prefix, Selector);
         begin
            return Gela.Symbols.Symbol_Access (Result);
         end;
      else
         declare
            Result : constant Symbol_Access :=
              new Symbol'(Context, Image, Folded, Lists => <>);
         begin
            return Gela.Symbols.Symbol_Access (Result);
         end;
      end if;
   end Create;

   -----------------
   -- Create_List --
   -----------------

   overriding function Create_List
     (Head  : access Symbol;
      Tail  : Gela.Symbols.Symbol_List_Access := null)
      return Gela.Symbols.Symbol_List_Access
   is
      Addr : constant System.Address :=
        Conv.To_Address (Conv.Object_Pointer (Tail));
      Pos  : constant List_Maps.Cursor := Head.Lists.Find (Addr);
   begin
      if List_Maps.Has_Element (Pos) then
         return List_Maps.Element (Pos);
      end if;

      declare
         Result : constant Symbol_List_Access := new Symbol_List'(Head, Tail);
      begin
         Head.Lists.Insert (Addr, Gela.Symbols.Symbol_List_Access (Result));

         return Gela.Symbols.Symbol_List_Access (Result);
      end;
   end Create_List;

   ------------
   -- Folded --
   ------------

   overriding function Folded
     (Self  : access Symbol) return League.Strings.Universal_String is
   begin
      return Self.Folded;
   end Folded;

   ----------
   -- Head --
   ----------

   overriding function Head
     (Self  : access Symbol_List) return Gela.Symbols.Symbol_Access is
   begin
      return Self.Head;
   end Head;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Self  : access Symbol)
      return League.Strings.Universal_String
   is
   begin
      return Self.Image;
   end Image;

   ------------
   -- Prefix --
   ------------

   overriding function Prefix
     (Self  : access Symbol)
      return Gela.Symbols.Symbol_Access
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Prefix;

   ------------
   -- Prefix --
   ------------

   overriding function Prefix
     (Self  : access Compound_Symbol) return Gela.Symbols.Symbol_Access is
   begin
      return Self.Prefix;
   end Prefix;

   --------------
   -- Selector --
   --------------

   overriding function Selector
     (Self  : access Symbol)
      return Gela.Symbols.Symbol_Access
   is
   begin
      return Gela.Symbols.Symbol_Access (Self);
   end Selector;

   --------------
   -- Selector --
   --------------

   overriding function Selector
     (Self  : access Compound_Symbol) return Gela.Symbols.Symbol_Access is
   begin
      return Self.Selector;
   end Selector;

   ----------
   -- Tail --
   ----------

   overriding function Tail
     (Self  : access Symbol_List) return Gela.Symbols.Symbol_List_Access is
   begin
      return Self.Tail;
   end Tail;

end Gela.A4G.Symbols;
