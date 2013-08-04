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
   pragma Preelaborate;

   type Unit_Container is limited interface;

   function Parent
     (Self : access Unit_Container)
      return Gela.Types.Context_Access is abstract;

   function Library_Unit_Declaration
     (Self  : access Unit_Container;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit is abstract;

   function Compilation_Unit_Body
     (Self  : access Unit_Container;
      Name  : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit is abstract;

   function Library_Unit_Declarations
     (Self  : access Unit_Container)
      return Gela.Types.Compilation_Unit_List is abstract;

   function Compilation_Unit_Bodies
     (Self  : access Unit_Container)
      return Gela.Types.Compilation_Unit_List is abstract;

end Gela.Unit_Containers;
