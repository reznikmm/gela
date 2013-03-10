------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Properties.Tokens is

   use type X;

   Tag       : constant X := 0;
   Count     : constant X := Tag + 1;
   Value     : constant X := Count + 1;
   Line      : constant X := Value + 1;
   First     : constant X := Line + 1;
   Last      : constant X := First + 1;
   Separator : constant X := Last + 1;
   Symbol    : constant X := Separator + 1;

   Size      : constant X := Symbol + 1;

end Gela.Properties.Tokens;
