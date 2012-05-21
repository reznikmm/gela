with Asis.Gela.Elements;
with Asis.Gela.Properties;

package body Asis.Gela.Text_Positions is

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Item : Text_Position) return Boolean is
   begin
      return Item.Line = 0;
   end Is_Nil;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Text_Position) return Boolean is
   begin
      return Left.Line < Right.Line or else
        (Left.Line = Right.Line and then Left.Column < Right.Column);
   end "<";

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (Item : Text_Position) return Wide_String is
      Line_Image   : constant Wide_String :=
        ASIS_Natural'Wide_Image (Item.Line);
      Column_Image : constant Wide_String :=
        ASIS_Natural'Wide_Image (Item.Column);
   begin
      return Line_Image (2 .. Line_Image'Last) & ":" &
        Column_Image (2 .. Column_Image'Last);
   end To_Wide_String;

   --------------------
   -- Start_Position --
   --------------------

   function Start_Position
     (C       : Compilations.Compilation;
      Element : Element_Index)
      return Text_Position
   is
      use Asis.Gela.Elements;
      use Asis.Gela.Properties;
   begin
      return (Line   => Get (C, Element, Start_Line),
              Column => Get (C, Element, Start_Column));
   end Start_Position;

   ------------------
   -- End_Position --
   ------------------

   function End_Position
     (C       : Compilations.Compilation;
      Element : Element_Index)
      return Text_Position
   is
      use Asis.Gela.Elements;
      use Asis.Gela.Properties;
   begin
      return (Line   => Get (C, Element, End_Line),
              Column => Get (C, Element, End_Column));
   end End_Position;

end Asis.Gela.Text_Positions;
