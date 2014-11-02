------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

limited with Gela.Mutables.Compilations;

package Gela.Mutables is
   pragma Preelaborate;

   type Mutable_Compilation_Access is
     access all Gela.Mutables.Compilations.Compilation'Class;

end Gela.Mutables;
