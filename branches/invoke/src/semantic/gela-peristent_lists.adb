package body Gela.Peristent_Lists is

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self   : in out Container;
      Input  : Count_Type;
      Value  : Element_Type;
      Output : out Count_Type)
   is
      procedure Count_Each (Item : Element_Type);

      Count : Natural := 0;
      Found : Boolean := False;

      procedure Count_Each (Item : Element_Type) is
      begin
         if Value = Item then
            Found := True;
         end if;

         Count := Count + 1;
      end Count_Each;

   begin
      For_Each (Self, Input, Count_Each'Access);

      if Found then
         declare
            procedure Fill_Each (Item : Element_Type);

            Data : array (1 .. Count) of Element_Type;

            procedure Fill_Each (Item : Element_Type) is
            begin
               if Value /= Item then
                  Count := Count + 1;
                  Data (Count) := Item;
               end if;
            end Fill_Each;
         begin
            Count := 0;
            For_Each (Self, Input, Fill_Each'Access);
            Output := 0;

            for J in 1 .. Count loop
               Prepend (Self, Data (J), Output, Output);
            end loop;
         end;
      else
         Output := Input;
      end if;
   end Delete;

   --------------
   -- For_Each --
   --------------

   procedure For_Each
     (Self   : Container;
      Input  : Count_Type;
      Action : access procedure (Value : Element_Type))
   is
      Index : Count_Type := Input;
   begin
      while Index /= 0 loop
         declare
            X : constant Link := Self.Links.Element (Index);
         begin
            Action (Self.Elements.Element (X.Value));
            Index := X.Next;
         end;
      end loop;
   end For_Each;

   ----------
   -- Head --
   ----------

   function Head
     (Self   : Container;
      Index  : Index_Type)
      return Element_Type
   is
      X : constant Link := Self.Links.Element (Index);
   begin
      return Self.Elements.Element (X.Value);
   end Head;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self   : in out Container;
      Value  : Element_Type;
      Input  : Count_Type := 0;
      Output : out Index_Type)
   is
      Value_Index : Natural := Self.Elements.Find_Index (Value);
      New_Link    : Link;
      Result      : Count_Type;
   begin
      if Value_Index = 0 then
         Self.Elements.Append (Value);
         Value_Index := Self.Elements.Last_Index;
      end if;

      New_Link := (Value => Value_Index, Next => Input);
      Result := Self.Links.Find_Index (New_Link);

      if Result = 0 then
         Self.Links.Append (New_Link);
         Output := Self.Links.Last_Index;
      else
         Output := Result;
      end if;
   end Prepend;

   ----------
   -- Tail --
   ----------

   function Tail
     (Self   : Container;
      Index  : Index_Type) return Count_Type is
   begin
      return Self.Links.Element (Index).Next;
   end Tail;

end Gela.Peristent_Lists;
