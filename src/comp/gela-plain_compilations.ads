with Ada.Containers.Vectors;

with League.Calendars;
with League.String_Vectors;
with League.Strings;

with Gela.Compilations;
with Gela.Contexts;
with Gela.Element_Factories;
with Gela.Lexers;
with Gela.Lexical_Types;
with Gela.Parsers;

package Gela.Plain_Compilations is
   pragma Preelaborate;

   type Compilation (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Compilations.Compilation
       and Gela.Lexers.Lexer_Destination
       and Gela.Parsers.Parser_Input with private;

   type Compilation_Access is access all Compilation;

   not overriding procedure Initialize
     (Self      : in out Compilation;
      Text_Name : League.Strings.Universal_String;
      Source    : League.Strings.Universal_String;
      Factory   : Gela.Element_Factories.Element_Factory_Access);

private

   package Token_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Gela.Lexical_Types.Token_Index,
      Element_Type => Gela.Lexical_Types.Token,
      "="          => Gela.Lexical_Types."=");

   package Line_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Gela.Lexical_Types.Line_Index,
      Element_Type => Gela.Lexical_Types.Line_Span,
      "="          => Gela.Lexical_Types."=");

   type Compilation (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Compilations.Compilation
       and Gela.Lexers.Lexer_Destination
       and Gela.Parsers.Parser_Input with
   record
      Index  : Gela.Lexical_Types.Token_Index := 1;
      Tokens : Token_Vectors.Vector;
      Lines  : Line_Vectors.Vector;
      Update : League.Calendars.Date_Time;
      Text_Name : League.Strings.Universal_String;
      Source    : League.Strings.Universal_String;
      Factory   : Gela.Element_Factories.Element_Factory_Access;
   end record;

   overriding function Context
     (Self : Compilation) return Gela.Contexts.Context_Access;

   overriding function Text_Name
     (Self : Compilation) return League.Strings.Universal_String;

   overriding function Object_Name
     (Self : Compilation) return League.Strings.Universal_String;

   overriding function Compilation_Command_Line_Options
     (Self : Compilation)
      return League.String_Vectors.Universal_String_Vector;

   overriding function Time_Of_Last_Update
     (Self : Compilation)
      return League.Calendars.Date_Time;

   overriding function Compilation_CPU_Duration
     (Self : Compilation) return Duration;

   overriding function Source
     (Self : Compilation) return League.Strings.Universal_String;

   overriding function Line_Count
     (Self : Compilation) return Gela.Lexical_Types.Line_Count;

   overriding function Get_Line_Span
     (Self  : Compilation;
      Index : Gela.Lexical_Types.Line_Index)
      return Gela.Lexical_Types.Line_Span;

   overriding function Token_Count
     (Self : Compilation) return Gela.Lexical_Types.Token_Count;

   overriding function Get_Token
     (Self  : Compilation;
      Index : Gela.Lexical_Types.Token_Index)
      return Gela.Lexical_Types.Token;

   overriding procedure Next_Token
     (Self  : in out Compilation;
      Token : out Gela.Lexical_Types.Token_Kind;
      Index : out Gela.Lexical_Types.Token_Index);

   overriding procedure New_Token
     (Self  : in out Compilation;
      Token : Gela.Lexical_Types.Token);

   overriding procedure New_Line
     (Self  : in out Compilation;
      Line  : Gela.Lexical_Types.Line_Span);

   overriding function Factory
     (Self : Compilation) return Gela.Element_Factories.Element_Factory_Access;

end Gela.Plain_Compilations;