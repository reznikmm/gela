--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with System.Storage_Pools.Subpools;

with Program.Compilation_Units;
with Program.Compilations;
with Program.Contexts;
with Program.Lexical_Elements;
with Program.Parsers;
with Program.Plain_Lexical_Elements;

private with Ada.Containers.Vectors;
private with Ada.Strings.Wide_Wide_Unbounded;
private with Program.Source_Buffers;
private with Program.Plain_Source_Buffers;
private with Program.Plain_Contexts;

package Program.Plain_Compilations is
   pragma Preelaborate;

   type Compilation
     (Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle)
       is limited new Program.Compilations.Compilation
          and Program.Plain_Lexical_Elements.Line_Buffer with private;

   procedure Initialize
     (Self    : in out Compilation'Class;
      Context : not null Program.Contexts.Context_Access);

   overriding function Context (Self : Compilation)
     return not null Program.Contexts.Context_Access;
   --  Return corresponding context

   overriding function Text_Name (Self : Compilation) return Program.Text;

   overriding function Object_Name (Self : Compilation) return Program.Text;

   overriding function Line_Count (Self : Compilation) return Natural;

   overriding function Line
     (Self  : Compilation;
      Index : Positive) return Program.Text;

   overriding function Lexical_Element_Count (Self : Compilation)
     return Natural;

   overriding function Lexical_Element
     (Self  : Compilation;
      Index : Positive) return Program.Lexical_Elements.Lexical_Element_Access;

   not overriding procedure Parse_File
     (Self      : aliased in out Compilation;
      Text_Name : Program.Text;
      Units     : out Program.Parsers.Unit_Vectors.Vector;
      Pragmas   : out Program.Parsers.Element_Vectors.Vector;
      Standard  : Boolean := False);

private

   package Span_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Program.Source_Buffers.Span,
      "="          => Program.Source_Buffers."=");

   type Plain_Context_Access is access all Program.Plain_Contexts.Context;

   type Compilation
     (Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle)
   is limited new Compilations.Compilation
     and Plain_Lexical_Elements.Line_Buffer with
   record
      Context     : Plain_Context_Access;
      Text_Name   : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Object_Name : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Buffer      : aliased Program.Plain_Source_Buffers.Source_Buffer;
      Tokens      : aliased Plain_Lexical_Elements.Lexical_Element_Vector
                              (Compilation'Unchecked_Access);
      Line_Spans  : Span_Vectors.Vector;
   end record;

   overriding function Text
     (Self : Compilation;
      Span : Program.Source_Buffers.Span) return Program.Text;

   overriding procedure Get_Span
     (Self        : Compilation;
      Span        : Program.Source_Buffers.Span;
      From_Line   : out Positive;
      To_Line     : out Positive;
      From_Column : out Positive;
      To_Column   : out Positive);

end Program.Plain_Compilations;
