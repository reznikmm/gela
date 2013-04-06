------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Grammars;
with Gela.Stores.Elements;
with Gela.Types;
with Gela.Nodes;

with Gela.Stores.Base_Fabrics;

package Gela.Stores.Fabrics is

   type X is array (Positive range <>) of Gela.Nodes.Node_Access;

   type Fabric is new Gela.Stores.Base_Fabrics.Base_Fabric with null record;

   function To_Element
     (Self    : access Fabric;
      Payload : Gela.Types.Payload) return Gela.Stores.Elements.Element_Access;

   function To_Node
     (Self    : access Fabric;
      Payload : Gela.Types.Payload) return Gela.Nodes.Node_Access;

   function Create_Token (Self : access Fabric) return Gela.Types.Payload;

   function Create_Production
     (Self       : access Fabric;
      Production : Gela.Grammars.Production_Index)
      return Gela.Types.Payload;

   function Create_Switch
     (Self : access Fabric;
      NT   : Gela.Grammars.Non_Terminal_Index)
      return Gela.Types.Payload;

end Gela.Stores.Fabrics;
