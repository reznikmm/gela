------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Types;

package Gela.Contexts is

   type Context is interface;

   function Length (Self : access Context) return Natural is abstract;

   function Container
     (Self  : access Context;
      Index : Positive)
      return Gela.Types.Container_Access is abstract;

   function Create_Container
     (Self  : access Context;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Container_Access is abstract;

   function Unit_Fabric
     (Self : access Context)
     return Gela.Types.Unit_Fabric_Access is abstract;

end Gela.Contexts;
