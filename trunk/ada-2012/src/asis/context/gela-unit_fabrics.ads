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

package Gela.Unit_Fabrics is

   type Unit_Fabric is limited interface;

   function Create_Compilation
     (Self : access Unit_Fabric;
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String)
      return Gela.Types.Compilation_Access is abstract;

   function Create_Declaration
     (Self        : access Unit_Fabric;
      Compilation : Gela.Types.Compilation_Access;
      Name        : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit is abstract;

   function Create_Body
     (Self        : access Unit_Fabric;
      Compilation : Gela.Types.Compilation_Access;
      Name        : League.Strings.Universal_String)
      return Gela.Types.Compilation_Unit is abstract;

end Gela.Unit_Fabrics;
