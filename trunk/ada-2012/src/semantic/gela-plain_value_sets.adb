with League.Strings.Hash;

package body Gela.Plain_Value_Sets is

   ------------
   -- Concat --
   ------------

   overriding procedure Concat
     (Self  : in out Value_Set;
      Left  : Gela.Semantic_Types.Value_Index;
      Right : Gela.Semantic_Types.Value_Index;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      use type League.Strings.Universal_String;

      Image : constant League.Strings.Universal_String :=
        Self.Image (Left) & Self.Image (Right);
   begin
      Self.String_Literal (Image, Value);
   end Concat;

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

   ----------
   -- Hash --
   ----------

   function Hash (X : Value) return Ada.Containers.Hash_Type is
   begin
      return League.Strings.Hash (X.String);
   end Hash;

   --------------------
   -- String_Literal --
   --------------------

   overriding procedure String_Literal
     (Self  : in out Value_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Semantic_Types.Value_Index)
   is
      Item : constant Gela.Plain_Value_Sets.Value := (String_Value, Image);
      Pos  : constant Hash_Maps.Cursor := Self.Map.Find (Item);
   begin
      if Hash_Maps.Has_Element (Pos) then
         Value := Hash_Maps.Element (Pos);
      else
         Self.Vector.Append (Item);
         Value := Self.Vector.Last_Index;
         Self.Map.Insert (Item, Value);
      end if;
   end String_Literal;


end Gela.Plain_Value_Sets;
