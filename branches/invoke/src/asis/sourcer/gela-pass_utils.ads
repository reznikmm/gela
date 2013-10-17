------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;
with Gela.Mutables;

package Gela.Pass_Utils is
   pragma Preelaborate;

   function Create_Unit_Declaration
     (Comp       : Gela.Mutables.Mutable_Compilation_Access;
      Full_Name  : Gela.Types.Symbol;
      Is_Private : Boolean;
      Unit_Kind  : Gela.Types.Unit_Kinds)
      return Gela.Types.Payload;

   function Create_Unit_Body
     (Comp       : Gela.Mutables.Mutable_Compilation_Access;
      Full_Name  : Gela.Types.Symbol;
      Unit_Kind  : Gela.Types.Unit_Kinds)
      return Gela.Types.Payload;

   function Create_Subunit
     (Comp       : Gela.Mutables.Mutable_Compilation_Access;
      Parent     : Gela.Types.Symbol;
      Name       : Gela.Types.Symbol;
      Unit_Kind  : Gela.Types.Unit_Kinds)
      return Gela.Types.Payload;

end Gela.Pass_Utils;
