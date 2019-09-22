--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilations;
with Program.Lexical_Elements;
with Program.Contexts;

private with Ada.Containers.Vectors;
private with Ada.Strings.Wide_Wide_Unbounded;
private with Program.Source_Buffers;

package Program.Plain_Compilations is
   pragma Preelaborate;

   type Compilation is
     limited new Program.Compilations.Compilation with private;

   overriding function Context (Self : Compilation)
     return not null Program.Contexts.Context_Access;
   --  Return corresponding context

   overriding function Text_Name (Self : Compilation) return Text;

   overriding function Object_Name (Self : Compilation) return Text;

   overriding function Line_Count (Self : Compilation) return Natural;

   overriding function Line
     (Self  : Compilation;
      Index : Positive) return Text;

   overriding function Lexical_Element_Count (Self : Compilation)
     return Natural;

   overriding function Lexical_Element
     (Self  : Compilation;
      Index : Positive) return Program.Lexical_Elements.Lexical_Element_Access;

private

   package Span_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Program.Source_Buffers.Span,
      "="          => Program.Source_Buffers."=");

   type Compilation is limited new Compilations.Compilation with record
      Context     : Program.Contexts.Context_Access;
      Text_Name   : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Object_Name : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Source      : Program.Source_Buffers.Source_Buffer_Access;
      Tokens      : Program.Lexical_Elements.Lexical_Element_Vector_Access;
      Line_Spans  : Span_Vectors.Vector;
   end record;

end Program.Plain_Compilations;
