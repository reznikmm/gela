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

package Gela.Unit_Containers is

   type Unit_Container is interface;

   function Parent
     (Self : access Unit_Container)
      return Gela.Types.Context_Access is abstract;

   function Length (Self : access Unit_Container) return Natural is abstract;

   function Unit
     (Self  : access Unit_Container;
      Index : Positive)
      return Gela.Types.Compilation_Unit is abstract;

   function Library_Unit_Declaration
     (Self  : access Unit_Container;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit is abstract;

   function Compilation_Unit_Body
     (Self  : access Unit_Container;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit is abstract;

end Gela.Unit_Containers;
