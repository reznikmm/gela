------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with League.Calendars;
with Gela.Lexical;
with Gela.Types;

package Gela.Compilations is
   pragma Preelaborate;

   type Abstract_Compilation is limited interface;
   type Abstract_Compilation_Access is access all Abstract_Compilation'Class;

   function Text_Name
     (Self    : access Abstract_Compilation)
      return League.Strings.Universal_String is abstract;
   --  Returns the name of the text, or other structure, that was the source
   --  of the compilation that resulted in this Compilation_Unit.

   function Object_Name
     (Self    : access Abstract_Compilation)
      return League.Strings.Universal_String is abstract;

   function Compilation_Command_Line_Options
     (Self    : access Abstract_Compilation)
      return League.Strings.Universal_String is abstract;

   function Time_Of_Last_Update
     (Self    : access Abstract_Compilation)
      return League.Calendars.Date_Time is abstract;

   function Compilation_CPU_Duration
     (Self    : access Abstract_Compilation)
      return Duration is abstract;

   function Text
     (Self    : access Abstract_Compilation)
      return League.Strings.Universal_String is abstract;
   --  Return any text data of given compilation. This includes implicit
   --  text such as inherited names and normalized identifiers

   subtype Text_Index is Gela.Lexical.Text_Index;
   --  Index inside Text string

   function Last_Line
     (Self    : access Abstract_Compilation) return Gela.Lexical.Line_Count
     is abstract;
   --  Return count of lines in compilation sources

   function Line
     (Self    : access Abstract_Compilation;
      Index   : Gela.Lexical.Line_Index)
      return Gela.Lexical.Line_Offset is abstract;
   --  Return offsets for line with given Index

   function Get_Token
     (Self    : access Abstract_Compilation;
      Index   : Positive)
      return Gela.Types.Token is abstract;

   function Symbols
     (Self    : access Abstract_Compilation)
      return Gela.Types.Symbol_Set_Access is abstract;

end Gela.Compilations;
