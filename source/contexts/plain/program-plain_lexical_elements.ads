--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Source_Buffers;
with Program.Symbols;

private with Ada.Containers.Vectors;

package Program.Plain_Lexical_Elements is
   pragma Preelaborate;

   type Lexical_Element is new Program.Lexical_Elements.Lexical_Element
     with private;

   function Symbol (Self : Lexical_Element'Class)
      return Program.Symbols.Symbol;

   type Lexical_Element_Vector is limited
     new Program.Lexical_Elements.Lexical_Element_Vector with private;

   not overriding procedure Append
     (Self   : in out Lexical_Element_Vector;
      Buffer : Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
      Kind   : Program.Lexical_Elements.Lexical_Element_Kind;
      Symbol : Program.Symbols.Symbol);

private

   type Lexical_Element is new Program.Lexical_Elements.Lexical_Element
   with record
      Buffer : Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
      Kind   : Program.Lexical_Elements.Lexical_Element_Kind;
      Symbol : Program.Symbols.Symbol;
   end record;

   overriding function Image (Self  : Lexical_Element) return Text;

   overriding function Kind (Self  : Lexical_Element)
     return Program.Lexical_Elements.Lexical_Element_Kind;

   package Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Program.Lexical_Elements.Lexical_Element_Access,
      "="          => Program.Lexical_Elements."=");

   type Lexical_Element_Vector is limited
     new Program.Lexical_Elements.Lexical_Element_Vector with
   record
      Vector : Vectors.Vector;
   end record;

   overriding function First_Index (Self : Lexical_Element_Vector)
     return Positive is (1);

   overriding function Last_Index (Self : Lexical_Element_Vector)
     return Positive;

   overriding function Element
     (Self  : Lexical_Element_Vector;
      Index : Positive)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

end Program.Plain_Lexical_Elements;
