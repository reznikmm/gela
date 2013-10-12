------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Grammars;
with Gela.Types;
with Gela.Nodes;

with Gela.Stores.Base_Fabrics;

package Gela.Stores.Fabrics is
   pragma Preelaborate;

   type X is array (Positive range <>) of Gela.Nodes.Node_Access;

   type Fabric is new Gela.Stores.Base_Fabrics.Base_Fabric with null record;

   function To_Element
     (Self    : access Fabric;
      Payload : Gela.Types.Payload) return Gela.Stores.Element_Access;

   function To_Node
     (Self    : access Fabric;
      Payload : Gela.Types.Payload) return Gela.Nodes.Node_Access;

   function Create_Token (Self : access Fabric) return Gela.Types.Payload;

   overriding function Create_Production
     (Self       : access Fabric;
      Production : Gela.Grammars.Production_Index)
      return Gela.Types.Payload;

   overriding function Create_Sequence
     (Self : access Fabric;
      Tag  : Positive)
      return Gela.Types.Payload;

   function Create_Switch
     (Self : access Fabric;
      NT   : Gela.Grammars.Non_Terminal_Index)
      return Gela.Types.Payload;

   function Infix_Call
     (Self   : access Fabric;
      Prefix : Gela.Nodes.Token;
      Left   : Gela.Nodes.Simple_Expression;
      Right  : Gela.Nodes.Simple_Expression := (null, 0))
      return Gela.Nodes.Function_Call;

   function To_Defining_Program_Unit_Name
     (Self  : access Fabric;
      Value : Gela.Nodes.Selected_Identifier)
      return Gela.Nodes.Defining_Program_Unit_Name;

   function To_Subtype_Indication
     (Self       : access Fabric;
      Not_Token  : Gela.Nodes.Token;
      Null_Token : Gela.Nodes.Token;
      Mark       : Gela.Nodes.Subtype_Mark;
      Constraint : Gela.Nodes.Scalar_Constraint)
      return Gela.Nodes.Subtype_Indication;

end Gela.Stores.Fabrics;
