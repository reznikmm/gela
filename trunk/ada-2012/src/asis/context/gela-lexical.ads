------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Lexical is
   pragma Pure;

   subtype Text_Index is Natural;
   --  Index inside Text string

   type Line_Count is new Natural;
   --  Type for indexing lines
   subtype Line_Index is Line_Count range 1 .. Line_Count'Last;

   type Colon_Count is new Natural;
   --  Type for indexing colons inside line
   subtype Colon_Index is Colon_Count range 1 .. Colon_Count'Last;

   type Position is record
      Line  : Line_Index;
      Colon : Colon_Index;
   end record;

   type Line_Offset is record
      First   : Text_Index;  --  Position of first character of line
      Last    : Text_Index;  --  Position of last character of line
      Comment : Text_Index;  --  Position of first character of comment in line
   end record;

end Gela.Lexical;
