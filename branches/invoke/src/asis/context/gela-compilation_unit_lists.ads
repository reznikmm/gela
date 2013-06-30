------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;

package Gela.Compilation_Unit_Lists is

   type Abstract_Compilation_Unit_List is interface;

   function Length
     (Self    : Abstract_Compilation_Unit_List;
      Payload : Gela.Types.Payload)
      return Natural is abstract;

   function Element
     (Self    : Abstract_Compilation_Unit_List;
      Payload : Gela.Types.Payload;
      Index   : Positive)
      return Gela.Types.Compilation_Unit is abstract;

end Gela.Compilation_Unit_Lists;
