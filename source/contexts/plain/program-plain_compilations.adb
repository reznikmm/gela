--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Plain_Compilations is

   -------------
   -- Context --
   -------------

   overriding function Context
     (Self : Compilation) return not null Program.Contexts.Context_Access is
   begin
      return Self.Context;
   end Context;

   ---------------------
   -- Lexical_Element --
   ---------------------

   overriding function Lexical_Element
     (Self  : Compilation;
      Index : Positive)
        return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Tokens.Element (Index);
   end Lexical_Element;

   ---------------------------
   -- Lexical_Element_Count --
   ---------------------------

   overriding function Lexical_Element_Count
     (Self : Compilation) return Natural is
   begin
      return Self.Tokens.Length;
   end Lexical_Element_Count;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Self  : Compilation;
      Index : Positive) return Text is
   begin
      return Self.Source.Text (Self.Line_Spans (Index));
   end Line;

   ----------------
   -- Line_Count --
   ----------------

   overriding function Line_Count (Self : Compilation) return Natural is
   begin
      return Self.Line_Spans.Last_Index;
   end Line_Count;

   -----------------
   -- Object_Name --
   -----------------

   overriding function Object_Name (Self : Compilation) return Text is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
        (Self.Object_Name);
   end Object_Name;

   ---------------
   -- Text_Name --
   ---------------

   overriding function Text_Name (Self : Compilation) return Text is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
        (Self.Text_Name);
   end Text_Name;

end Program.Plain_Compilations;
