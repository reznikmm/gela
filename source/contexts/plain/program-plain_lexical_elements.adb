--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Plain_Lexical_Elements is

   type Lexical_Element_Access is access Lexical_Element;

   ------------
   -- Append --
   ------------

   not overriding procedure Append
     (Self   : aliased in out Lexical_Element_Vector;
      Span   : Program.Source_Buffers.Span;
      Kind   : Program.Lexical_Elements.Lexical_Element_Kind;
      Symbol : Program.Symbols.Symbol)
   is
      Item : constant Lexical_Element_Access := new Lexical_Element'
        (Vector => Self'Unchecked_Access,
         Span   => Span,
         Kind   => Kind,
         Symbol => Symbol);
   begin
      Self.Vector.Append
        (Program.Lexical_Elements.Lexical_Element_Access (Item));
   end Append;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Lexical_Element_Vector;
      Index : Positive)
        return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Vector.Element (Index);
   end Element;

   ----------
   -- From --
   ----------

   overriding function From
     (Self  : Lexical_Element) return Program.Lexical_Elements.Location
   is
      From_Line   : Positive;
      To_Line     : Positive;
      From_Column : Positive;
      To_Column   : Positive;
   begin
      Self.Vector.Buffer.Get_Span
        (Self.Span, From_Line, To_Line, From_Column, To_Column);

      return (From_Line, From_Column);
   end From;

   -----------
   -- Image --
   -----------

   overriding function Image (Self : Lexical_Element) return Program.Text is
   begin
      return Self.Vector.Buffer.Text (Self.Span);
   end Image;

   ----------
   -- Kind --
   ----------

   overriding function Kind (Self : Lexical_Element)
     return Program.Lexical_Elements.Lexical_Element_Kind is
   begin
      return Self.Kind;
   end Kind;

   ----------------
   -- Last_Index --
   ----------------

   overriding function Last_Index
     (Self : Lexical_Element_Vector) return Positive is
   begin
      return Self.Vector.Last_Index;
   end Last_Index;

   ------------
   -- Symbol --
   ------------

   function Symbol
     (Self : Lexical_Element'Class) return Program.Symbols.Symbol is
   begin
      return Self.Symbol;
   end Symbol;

end Program.Plain_Lexical_Elements;
