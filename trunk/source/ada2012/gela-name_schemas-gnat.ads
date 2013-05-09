------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

package Gela.Name_Schemas.GNAT is

   type GNAT_Name_Schema is new Name_Schema with null record;

   overriding function Declaration_Name
     (Self  : access GNAT_Name_Schema;
      Unit  : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   overriding function Body_Name
     (Self  : access GNAT_Name_Schema;
      Unit  : League.Strings.Universal_String)
     return League.Strings.Universal_String;

end Gela.Name_Schemas.GNAT;
