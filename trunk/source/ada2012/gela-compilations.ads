------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with Gela.Lexical;

package Gela.Compilations is

   type Compilation is abstract tagged limited private;

   function Text
     (Self : access Compilation)
      return League.Strings.Universal_String is abstract;
   --  Return any text data of given compilation. This includes implicit
   --  text such as inherited names and normalized identifiers

   subtype Text_Index is Gela.Lexical.Text_Index;
   --  Index inside Text string

   type Line_Offset is record
      First   : Text_Index;  --  Position of first character of line
      Last    : Text_Index;  --  Position of last character of line
      Comment : Text_Index;  --  Position of first character of comment in line
   end record;

   function Last_Line
     (Self : access Compilation) return Gela.Lexical.Line_Count
     is abstract;
   --  Return count of lines in compilation sources

   function Line
     (Self  : access Compilation;
      Index : Gela.Lexical.Line_Index) return Line_Offset is abstract;
   --  Return offsets for line with given Index

private

   type Compilation is abstract tagged limited null record;

end Gela.Compilations;
