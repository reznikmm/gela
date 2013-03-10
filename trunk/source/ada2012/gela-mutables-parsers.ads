------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Elements;
with Gela.Mutables.Productions;
with Gela.Types;

package Gela.Mutables.Parsers is

   type Parser (Compilation : Mutable_Compilation_Access) is tagged private;

   procedure Parse (Self : access Parser);

private

   type Parser (Compilation : Mutable_Compilation_Access) is tagged record
      Production : aliased Gela.Mutables.Productions.Production (Compilation);
   end record;

   function Get_Element
     (Self : access Parser;
      Object : Gela.Types.Payload)
      return access Gela.Mutables.Elements.Element'Class;

end Gela.Mutables.Parsers;
