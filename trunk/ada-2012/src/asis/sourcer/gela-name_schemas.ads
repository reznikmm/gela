------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

package Gela.Name_Schemas is
   pragma Preelaborate;

   type Name_Schema is interface;
   type Name_Schema_Access is access all Name_Schema'Class;

   function Declaration_Name
     (Self  : access Name_Schema;
      Unit  : League.Strings.Universal_String)
      return League.Strings.Universal_String is abstract;

   function Body_Name
     (Self  : access Name_Schema;
      Unit  : League.Strings.Universal_String)
     return League.Strings.Universal_String is abstract;

end Gela.Name_Schemas;
