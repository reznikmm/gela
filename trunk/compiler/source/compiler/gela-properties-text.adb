with Ada.Characters.Latin_1;

package body Gela.Properties.Text is

   -------------
   -- Literal --
   -------------

   function Literal
     (Self  : access Text_Container;
      Value : String)
      return Text
   is
      Pos : constant String_Hash_Maps.Cursor := Self.String_Map.Find (Value);
   begin
      if Value = "" then
         return 0;
      elsif String_Hash_Maps.Has_Element (Pos) then
         return String_Hash_Maps.Element (Pos);
      end if;

      Self.String_Vector.Append
        (Ada.Strings.Unbounded.To_Unbounded_String (Value));

      return Result : constant Text :=
        2 * Text (Self.String_Vector.Last_Index)
      do
         Self.String_Map.Insert (Value, Result);
      end return;
   end Literal;

   --------------
   -- New_Line --
   --------------

   function New_Line
     (Self  : access Text_Container) return Text is
   begin
      return Self.Literal ((1 => Ada.Characters.Latin_1.LF));
   end New_Line;

   ----------
   -- Join --
   ----------

   function Join
     (Self  : access Text_Container;
      Left  : Text;
      Right : String)
      return Text is
   begin
      return Self.Join (Left, Self.Literal (Right));
   end Join;

   ----------
   -- Join --
   ----------

   function Join
     (Self  : access Text_Container;
      Left  : String;
      Right : Text)
      return Text is
   begin
      return Self.Join (Self.Literal (Left), Right);
   end Join;

   ----------
   -- Join --
   ----------

   function Join
     (Self  : access Text_Container;
      Left  : Text;
      Right : Text)
      return Text
   is
      Node : constant Join_Node := (Left, Right);
      Pos : constant Join_Node_Hash_Maps.Cursor := Self.Join_Map.Find (Node);
   begin
      if Left = 0 then
         return Right;
      elsif Right = 0 then
         return Left;
      elsif Join_Node_Hash_Maps.Has_Element (Pos) then
         return Join_Node_Hash_Maps.Element (Pos);
      end if;

      Self.Join_Vector.Append (Node);

      return Result : constant Text :=
        2 * Text (Self.Join_Vector.Last_Index) + 1
      do
         Self.Join_Map.Insert (Node, Result);
      end return;
   end Join;

   ----------
   -- Join --
   ----------

   function Join
     (Self  : access Text_Container;
      Left  : Text;
      Right : Natural) return Text
   is
      Image : constant String := Natural'Image (Right);
   begin
      return Self.Join (Left, Image (2 .. Image'Last));
   end Join;

   -------------------
   -- Join_New_Line --
   -------------------

   function Join_New_Line
     (Self  : access Text_Container;
      Left  : Text) return Text is
   begin
      return Self.Join (Left, Self.New_Line);
   end Join_New_Line;

   -----------
   -- Value --
   -----------

   function Value
     (Self : access Text_Container;
      Item : Text)
      return String
   is
      procedure Add (Element : Text);
      Result : Ada.Strings.Unbounded.Unbounded_String;

      procedure Add (Element : Text) is
         Index : constant Positive := Positive (Element / 2);
      begin
         if (Element and 1) = 0 then
            Ada.Strings.Unbounded.Append
              (Result,
               Self.String_Vector.Element (Index));
         else
            declare
               Node : constant Join_Node := Self.Join_Vector.Element (Index);
            begin
               Add (Node.Left);
               Add (Node.Right);
            end;
         end if;
      end Add;
   begin
      if Item /= 0 then
         Add (Item);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Value;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Join_Node) return Ada.Containers.Hash_Type is
      --  Cantor Pairing Function
      S : constant Text := Item.Left + Item.Right;
   begin
      return Ada.Containers.Hash_Type (S * (S + 1) / 2 + Item.Right);
   end Hash;

end Gela.Properties.Text;
