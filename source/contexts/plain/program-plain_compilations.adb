--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Lexical_Handlers;
with Program.Scanner_Destinations;
with Program.Scanners;
with Program.Symbols;

package body Program.Plain_Compilations is

   type Scanner_Destination
     (Comp    : not null access Compilation;
      Scanner : not null access Program.Scanners.Scanner)
   is
     new Program.Scanner_Destinations.Scanner_Destination with
   record
      Line_From : Positive := 1;
   end record;

   overriding procedure New_Line
     (Self   : in out Scanner_Destination;
      Unused : Positive);

   overriding procedure New_Token
     (Self  : in out Scanner_Destination;
      Token : Program.Scanner_Destinations.Token);

   procedure Read_All_Tokens
     (Self   : access Compilation;
      Buffer : Program.Source_Buffers.Source_Buffer_Access);

   -------------
   -- Context --
   -------------

   overriding function Context
     (Self : Compilation) return not null Program.Contexts.Context_Access is
   begin
      return Program.Contexts.Context_Access (Self.Context);
   end Context;

   --------------
   -- Get_Span --
   --------------

   overriding procedure Get_Span
     (Self        : Compilation;
      Span        : Program.Source_Buffers.Span;
      From_Line   : out Positive;
      To_Line     : out Positive;
      From_Column : out Positive;
      To_Column   : out Positive) is
   begin
      for J in Self.Line_Spans.First_Index .. Self.Line_Spans.Last_Index loop
         declare
            SJ : constant Program.Source_Buffers.Span := Self.Line_Spans (J);
         begin
            if Span.From in SJ.From .. SJ.To then
               From_Line := J;
               From_Column := Span.From - SJ.From + 1;

               for K in J .. Self.Line_Spans.Last_Index loop
                  declare
                     SK : constant Source_Buffers.Span := Self.Line_Spans (K);
                  begin
                     if Span.To in SK.From .. SK.To then
                        To_Line := K;
                        To_Column := Span.To - SK.From + 1;

                        return;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;

      raise Constraint_Error;
   end Get_Span;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Compilation'Class;
      Context : not null Program.Contexts.Context_Access) is
   begin
      Self.Context := Plain_Context_Access (Context);
   end Initialize;

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
      Index : Positive) return Program.Text is
   begin
      return Self.Buffer.Text (Self.Line_Spans (Index));
   end Line;

   ----------------
   -- Line_Count --
   ----------------

   overriding function Line_Count (Self : Compilation) return Natural is
   begin
      return Self.Line_Spans.Last_Index;
   end Line_Count;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self   : in out Scanner_Destination;
      Unused : Positive)
   is
      Span : constant Program.Source_Buffers.Span :=
        Self.Scanner.Get_Span;
   begin
      Self.Comp.Line_Spans.Append
        ((From => Self.Line_From,
          To   => Span.From - 1));
      Self.Line_From := Span.To + 1;
   end New_Line;

   ---------------
   -- New_Token --
   ---------------

   overriding procedure New_Token
     (Self  : in out Scanner_Destination;
      Token : Program.Scanner_Destinations.Token)
   is
      use type Program.Lexical_Elements.Lexical_Element_Kind;
      Symbol : Program.Symbols.Symbol := Program.Symbols.No_Symbol;
   begin
      if Token.Kind = Program.Lexical_Elements.Identifier
      then
         Self.Comp.Context.Find_Or_Create_Symbol
           (Self.Comp.Buffer'Unchecked_Access, Token.Span, Symbol);
      elsif Token.Kind in
        Program.Lexical_Elements.Character_Literal
        | Program.Lexical_Elements.String_Literal
      then
         Symbol := Self.Comp.Context.Find (Self.Comp.Buffer.Text (Token.Span));
      end if;

      Self.Comp.Tokens.Append
        (Token.Span,
         Token.Kind,
         Symbol);
   end New_Token;

   -----------------
   -- Object_Name --
   -----------------

   overriding function Object_Name (Self : Compilation) return Program.Text is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
        (Self.Object_Name);
   end Object_Name;

   ----------------
   -- Parse_File --
   ----------------

   not overriding procedure Parse_File
     (Self      : aliased in out Compilation;
      Text_Name : Program.Text;
      Units     : out Program.Parsers.Unit_Vectors.Vector;
      Pragmas   : out Program.Parsers.Element_Vectors.Vector;
      Standard  : Boolean := False) is
   begin
      Self.Text_Name := Ada.Strings.Wide_Wide_Unbounded.
        To_Unbounded_Wide_Wide_String (Text_Name);

      Self.Buffer.Initialize (Text_Name);
      Self.Read_All_Tokens (Self.Buffer'Unchecked_Access);
      Program.Parsers.Parse
        (Self'Unchecked_Access,
         Self.Tokens'Unchecked_Access,
         Self.Subpool,
         Units,
         Pragmas,
         Standard);
   end Parse_File;

   ---------------------
   -- Read_All_Tokens --
   ---------------------

   procedure Read_All_Tokens
     (Self   : access Compilation;
      Buffer : Program.Source_Buffers.Source_Buffer_Access)
   is
      Token   : Program.Lexical_Elements.Lexical_Element_Kind;
      Scanner : aliased Program.Scanners.Scanner;
      Dest    : aliased Scanner_Destination (Self, Scanner'Access);
      Handler : aliased Program.Lexical_Handlers.Handler (Dest'Access);
   begin
      Buffer.Rewind;
      Scanner.Set_Source (Buffer);
      Scanner.Set_Handler (Handler'Unchecked_Access);

      loop
         Scanner.Get_Token (Token);
         exit when Token in Program.Lexical_Elements.End_Of_Input;
      end loop;
   end Read_All_Tokens;

   ----------
   -- Text --
   ----------

   overriding function Text
     (Self : Compilation;
      Span : Program.Source_Buffers.Span) return Program.Text is
   begin
      return Self.Buffer.Text (Span);
   end Text;

   ---------------
   -- Text_Name --
   ---------------

   overriding function Text_Name (Self : Compilation) return Program.Text is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
        (Self.Text_Name);
   end Text_Name;

end Program.Plain_Compilations;
