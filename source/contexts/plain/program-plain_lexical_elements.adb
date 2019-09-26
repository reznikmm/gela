--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Plain_Lexical_Elements is

   type Lexical_Element_Access is access Lexical_Element;

   ------------
   -- Append --
   ------------

   not overriding procedure Append
     (Self   : in out Lexical_Element_Vector;
      Buffer : Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
      Kind   : Program.Lexical_Elements.Lexical_Element_Kind)
   is
      Item : constant Lexical_Element_Access := new Lexical_Element'
        (Buffer => Buffer,
         Span   => Span,
         Kind   => Kind);
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

   -----------
   -- Image --
   -----------

   overriding function Image (Self : Lexical_Element) return Text is
   begin
      return Self.Buffer.Text (Self.Span);
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

end Program.Plain_Lexical_Elements;
