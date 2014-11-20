with League.Strings.Hash;

package body Gela.Plain_Value_Sets is

   ------------
   -- Concat --
   ------------

   overriding procedure Apply
     (Self  : in out Value_Set;
      Name  : Gela.Semantic_Types.Static_Operator;
      Args  : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      use type League.Strings.Universal_String;
      use type Gela.Semantic_Types.Value_Index;
   begin
      Value := 0;

      if Args = 0 then
         return;
      end if;

      case Name is
         when Gela.Semantic_Types.Concat =>
            declare
               Item : constant Gela.Plain_Value_Sets.Value :=
                 Self.Vector.Element (Args);
            begin
               if Item.Kind = List_Value and then
                 Self.Vector.Element (Item.Head).Kind = String_Value and then
                 Self.Vector.Element (Item.Tail).Kind = String_Value
               then
                  Self.String_Literal
                    (Self.Image (Item.Head) & Self.Image (Item.Tail),
                     Value);
               end if;
            end;
      end case;
   end Apply;

   ----------
   -- List --
   ----------

   overriding procedure List
     (Self  : in out Value_Set;
      Head  : Gela.Semantic_Types.Value_Index;
      Tail  : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      use type Gela.Semantic_Types.Value_Index;
   begin
      if Tail = 0 then
         Value := 0;
      elsif Head = 0 then
         Value := Tail;
      else
         Self.Put_Value ((List_Value, Head, Tail), Value);
      end if;
   end List;

   ----------
   -- Hash --
   ----------

   function Hash (X : Value) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      case X.Kind is
         when String_Value =>
            return League.Strings.Hash (X.String);
         when List_Value =>
            return 65_213 * Ada.Containers.Hash_Type (X.Head) +
              Ada.Containers.Hash_Type (X.Tail);
      end case;
   end Hash;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Self  : Value_Set;
      Value : Gela.Semantic_Types.Value_Index)
      return League.Strings.Universal_String is
   begin
      return Self.Vector.Element (Value).String;
   end Image;

   ---------------
   -- Put_Value --
   ---------------

   not overriding procedure Put_Value
     (Self  : in out Value_Set;
      Item  : Value;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      Pos  : constant Hash_Maps.Cursor := Self.Map.Find (Item);
   begin
      if Hash_Maps.Has_Element (Pos) then
         Value := Hash_Maps.Element (Pos);
      else
         Self.Vector.Append (Item);
         Value := Self.Vector.Last_Index;
         Self.Map.Insert (Item, Value);
      end if;
   end Put_Value;

   --------------------
   -- String_Literal --
   --------------------

   overriding procedure String_Literal
     (Self  : in out Value_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      Item : constant Gela.Plain_Value_Sets.Value := (String_Value, Image);
   begin
      Self.Put_Value (Item, Value);
   end String_Literal;


end Gela.Plain_Value_Sets;
