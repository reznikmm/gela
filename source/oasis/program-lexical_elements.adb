--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Lexical_Elements is

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Location) return Boolean is
   begin
      return Left.Line < Right.Line or
        (Left.Line = Right.Line and Left.Column < Right.Column);
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Location) return Boolean is
   begin
      return Left.Line > Right.Line or
        (Left.Line = Right.Line and Left.Column > Right.Column);
   end ">";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Location) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Location) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   -----------
   -- First --
   -----------

   function First
     (Self : Lexical_Element_Vector'Class)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Element (Self.First_Index);
   end First;

   ----------------
   -- From_Image --
   ----------------

   function From_Image (Self : Lexical_Element'Class) return Program.Text is
      Value : constant Location := Self.From;
      Line_Image : constant Wide_Wide_String := Value.Line'Wide_Wide_Image;
      Column_Image : constant Wide_Wide_String := Value.Column'Wide_Wide_Image;
   begin
      return Line_Image (2 .. Line_Image'Last) & ':' &
        Column_Image (2 .. Column_Image'Last);
   end From_Image;

   ----------
   -- Last --
   ----------

   function Last
     (Self : Lexical_Element_Vector'Class)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Element (Self.Last_Index);
   end Last;

end Program.Lexical_Elements;
