--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Handlers;
with Program.Parsers;
with Program.Resolve_Standard;
with Program.Scanner_Destinations;
with Program.Scanners;
with Program.Symbols;
with Program.Visibility;

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
      Index : Positive) return Text is
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
      Symbol : Program.Symbols.Symbol := Program.Symbols.No_Symbol;
   begin
      if Token.Kind in
        Program.Lexical_Elements.Character_Literal
        | Program.Lexical_Elements.String_Literal
        | Program.Lexical_Elements.Identifier
      then
         Self.Comp.Context.Find_Or_Create_Symbol
           (Self.Comp.Buffer'Unchecked_Access, Token.Span, Symbol);
      end if;

      Self.Comp.Tokens.Append
        (Self.Comp.Buffer'Unchecked_Access,
         Token.Span,
         Token.Kind,
         Symbol);
   end New_Token;

   -----------------
   -- Object_Name --
   -----------------

   overriding function Object_Name (Self : Compilation) return Text is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
        (Self.Object_Name);
   end Object_Name;

   ----------------
   -- Parse_File --
   ----------------

   not overriding procedure Parse_File
     (Self      : aliased in out Compilation;
      Text_Name : Text;
      Unit      : out Program.Compilation_Units.Compilation_Unit_Access)
   is
      Env     : Program.Visibility.Context;
      Units   : Program.Parsers.Unit_Vectors.Vector;
      Pragmas : Program.Parsers.Element_Vectors.Vector;
   begin
      Self.Buffer.Initialize (Text_Name);
      Self.Read_All_Tokens (Self.Buffer'Unchecked_Access);
      Program.Parsers.Parse
        (Self'Unchecked_Access,
         Self.Tokens'Unchecked_Access,
         Self.Subpool,
         Units,
         Pragmas);

      Env.Create_Empty_Context;

      Program.Resolve_Standard (Unit => Units (1));
      Unit := Units (1);
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

   ---------------
   -- Text_Name --
   ---------------

   overriding function Text_Name (Self : Compilation) return Text is
   begin
      return Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
        (Self.Text_Name);
   end Text_Name;

end Program.Plain_Compilations;
