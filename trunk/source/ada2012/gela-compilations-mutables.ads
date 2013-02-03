------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Compilations.Mutables is

   type Mutable_Compilation is new Compilation with private;

   overriding function Text
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String;

   overriding function Last_Line
     (Self : access Mutable_Compilation) return Line_Count;

   overriding function Line
     (Self  : access Mutable_Compilation;
      Index : Line_Index) return Line_Offset;

   not overriding function Create
     (Source : League.Strings.Universal_String)
      return Mutable_Compilation;

private

   type Line_Offset_Array is array (Line_Index range <>) of Line_Offset;
   type Line_Offset_Array_Access is access all Line_Offset_Array;

   type Mutable_Compilation is new Compilation with record
      Text  : League.Strings.Universal_String;
      Lines : Line_Offset_Array_Access;
   end record;

end Gela.Compilations.Mutables;
