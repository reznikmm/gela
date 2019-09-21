------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.String_Vectors;
with League.Characters;

package body Writers is

   New_Line : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Writer) is
   begin
      Self.Text.Clear;
      Self.Last_Line.Clear;
   end Clear;

   procedure N (Self : in out Writer; Text : Wide_Wide_String) is
   begin
      Self.Last_Line.Append (Text);
   end N;

   procedure N
     (Self : in out Writer;
      Text : League.Strings.Universal_String) is
   begin
      Self.N (Text.To_Wide_Wide_String);
   end N;

   procedure N
     (Self : in out Writer;
      Text : Wide_Wide_String;
      Copy : in out Writer'Class) is
   begin
      Self.N (Text);
      Copy.N (Text);
   end N;

   procedure N
     (Self : in out Writer;
      Text : League.Strings.Universal_String;
      Copy : in out Writer'Class) is
   begin
      Self.N (Text);
      Copy.N (Text);
   end N;

   -------
   -- N --
   -------

   procedure N
     (Self  : in out Writer;
      Value : Natural)
   is
      Image : constant Wide_Wide_String := Natural'Wide_Wide_Image (Value);
   begin
      Self.N (Image (2 .. Image'Last));
   end N;

   procedure N
     (Self  : in out Writer;
      Value : Writer'Class)
   is
      Text : constant League.Strings.Universal_String := Value.Text;
      List : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (New_Line);
   begin
      for J in 1 .. List.Length loop
         Self.P (List.Element (J));
      end loop;
   end N;

   procedure P
     (Self : in out Writer;
      Text : Wide_Wide_String := "";
      Copy : in out Writer'Class) is
   begin
      Self.P (Text);
      Copy.P (Text);
   end P;

   procedure P
     (Self   : in out Writer;
      Text   : League.Strings.Universal_String;
      Copy : in out Writer'Class) is
   begin
      Self.P (Text);
      Copy.P (Text);
   end P;

   procedure P
     (Self   : in out Writer;
      Text   : League.Strings.Universal_String) is
   begin
      if Text.Index (New_Line) > 0 then
         declare
            List : League.String_Vectors.Universal_String_Vector;
         begin
            List := Text.Split (New_Line);
            for J in 1 .. List.Length loop
               Self.P (List.Element (J));
            end loop;
         end;
      else
         Self.P (Text.To_Wide_Wide_String);
      end if;
   end P;

   procedure P
     (Self   : in out Writer;
      Text   : Wide_Wide_String := "")
   is
      function Get_Prefix
        (X : League.Strings.Universal_String)
         return League.Strings.Universal_String;

      ----------------
      -- Get_Prefix --
      ----------------

      function Get_Prefix
        (X : League.Strings.Universal_String)
         return League.Strings.Universal_String
      is
         use type League.Characters.Universal_Character;
      begin
         for J in 1 .. X.Length loop
            if X.Element (J) /= ' ' then
               return X.Slice (1, J - 1);
            end if;
         end loop;
         raise Constraint_Error;
      end Get_Prefix;
   begin
      Self.N (Text);

      if Self.Last_Line.Length > 78 then
         declare
            Length : Natural;
            List   : constant League.String_Vectors.Universal_String_Vector :=
              Self.Last_Line.Split ('.');
            Prefix : League.Strings.Universal_String :=
              Get_Prefix (Self.Last_Line);
         begin
            Self.Text.Append (List.Element (1));
            Length := List.Element (1).Length;

            for J in 2 .. List.Length loop
               if Length + List.Element (J).Length < 78 then
                  Self.Text.Append ('.');
                  Self.Text.Append (List.Element (J));
                  Length := Length + List.Element (J).Length + 1;
               else
                  Self.Text.Append ('.');
                  Self.Text.Append (New_Line);
                  Prefix.Append ("  ");
                  Self.Text.Append (Prefix);
                  Self.Text.Append (List.Element (J));
                  Length := Prefix.Length + List.Element (J).Length;
               end if;
            end loop;
         end;
      else
         Self.Text.Append (Self.Last_Line);
      end if;

      Self.Last_Line.Clear;
      Self.Text.Append (New_Line);
   end P;

   ----------
   -- Text --
   ----------

   function Text
     (Self : Writer) return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
   begin
      return Self.Text & Self.Last_Line;
   end Text;

end Writers;
