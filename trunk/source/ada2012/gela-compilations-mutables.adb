with League.Character_Sets;
with League.Characters.Latin;

package body Gela.Compilations.Mutables is

   function Count_Lines
     (Source : League.Strings.Universal_String)
      return Line_Count;

   Set : League.Character_Sets.Universal_Character_Set;

   -----------------
   -- Count_Lines --
   -----------------

   function Count_Lines
     (Source : League.Strings.Universal_String)
      return Line_Count
   is
      use type League.Characters.Universal_Character;

      Result  : Line_Count := 1;
      Element : League.Characters.Universal_Character;
      Last_CR : Boolean := False;
   begin
      for J in 1 .. Source.Length loop
         Element := Source.Element (J);

         if Element = League.Characters.Latin.Carriage_Return then
            Result := Result + 1;
            Last_CR := True;
         elsif Element = League.Characters.Latin.Line_Feed then
            Result := Result + Boolean'Pos (not Last_CR);
            Last_CR := False;
         elsif Set.Has (Element) then
            Result := Result + 1;
            Last_CR := False;
         end if;
      end loop;

      return Result;
   end Count_Lines;

   ------------
   -- Create --
   ------------

   not overriding function Create
     (Source : League.Strings.Universal_String)
      return Mutable_Compilation
   is
      NFKC  : constant League.Strings.Universal_String := Source.To_NFKC;
      Total_Lines : constant Line_Count := Count_Lines (NFKC);
   begin
      return Result : constant Mutable_Compilation :=
        (Text  => NFKC,
         Lines => new Line_Offset_Array (1 .. Total_Lines))
      do
         null;
      end return;
   end Create;

   ---------------
   -- Last_Line --
   ---------------

   overriding function Last_Line
     (Self : access Mutable_Compilation)
      return Line_Count is
   begin
      return Self.Lines'Last;
   end Last_Line;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Self  : access Mutable_Compilation;
      Index : Line_Index)
      return Line_Offset is
   begin
      return Self.Lines (Index);
   end Line;

   ----------
   -- Text --
   ----------

   overriding function Text
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Text;
   end Text;

begin
   null;
   --  Set := League.Character_Sets.Internals.To_Set
end Gela.Compilations.Mutables;
