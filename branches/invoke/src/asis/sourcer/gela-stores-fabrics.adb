------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------
with Gela.Nodes.Convertions;
with Gela.Nodes.Identifiers;
with Gela.Nodes.Selected_Identifiers;

package body Gela.Stores.Fabrics is

   Token_Tag : constant := 0;

   -----------------------
   -- Create_Production --
   -----------------------

   overriding function Create_Production
     (Self       : access Fabric;
      Production : Gela.Grammars.Production_Index)
      return Gela.Types.Payload
   is
      Item : Index;
      Size : constant Positive := Element_Access
        (Self.Map (Positive (Production))).Size (0);
   begin
      Item := Self.Store.Allocate (Size);
      Self.Store.Set (Item, Element (Production));  --  Tag
      Self.Store.Set (Item + 1, 1);      --  Count

      return Gela.Types.Payload (Item);
   end Create_Production;

   ---------------------
   -- Create_Sequence --
   ---------------------

   overriding function Create_Sequence
     (Self : access Fabric;
      Tag  : Positive)
      return Gela.Types.Payload
   is
      Item : Index;
      Size : constant Positive := 4;
   begin
      Item := Self.Store.Allocate (Size);
      Self.Store.Set (Item, Element (Tag));  --  Tag
      Self.Store.Set (Item + 1, 1);      --  Count

      return Gela.Types.Payload (Item);
   end Create_Sequence;

   -------------------
   -- Create_Switch --
   -------------------

   function Create_Switch
     (Self : access Fabric;
      NT   : Gela.Grammars.Non_Terminal_Index)
      return Gela.Types.Payload
   is
      use type Gela.Nodes.Node_Access;

      Item : Index;
      Size : constant := 5;
   begin
      if Self.Map (Positive (NT) + Base_Fabrics.Last_Production) = null then
         raise Constraint_Error;
      end if;

      Item := Self.Store.Allocate (Size);
      Self.Store.Set
        (Item, Element (NT) + Base_Fabrics.Last_Production);  --  Tag
      Self.Store.Set (Item + 1, 1);      --  Count
      Self.Store.Set (Item + 2, Size);   --  Size

      return Gela.Types.Payload (Item);
   end Create_Switch;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token (Self : access Fabric) return Gela.Types.Payload is
      Item : Index;
   begin
      Item := Self.Store.Allocate (Self.Token.Size (0));
      Self.Store.Set (Item, Token_Tag);  --  Tag
      Self.Store.Set (Item + 1, 1);      --  Count
      return Gela.Types.Payload (Item);
   end Create_Token;

   ----------------
   -- Infix_Call --
   ----------------

   function Infix_Call
     (Self   : access Fabric;
      Prefix : Gela.Nodes.Token;
      Left   : Gela.Nodes.Simple_Expression;
      Right  : Gela.Nodes.Simple_Expression := (null, 0))
      return Gela.Nodes.Function_Call
   is
      use Gela.Nodes.Convertions;
      use type Gela.Nodes.Simple_Expression_Access;

      Conv : Gela.Nodes.Element := +Left;
      Name : constant Gela.Nodes.Element := +Self.Operator_Symbol (Prefix);
      Arg  : Gela.Nodes.Association := Self.Association
        (Array_Component_Choices => Self.Discrete_Choice_Sequence,
         Arrow_Token             => (null, 0),
         Component_Expression    => -Conv);
      Args : Gela.Nodes.Association_Sequence := Self.Association_Sequence;
   begin
      Args.Object.Append (Args.Payload, +Arg);

      if Right.Object /= null then
         Conv := +Right;

         Arg := Self.Association
           (Array_Component_Choices => Self.Discrete_Choice_Sequence,
            Arrow_Token             => (null, 0),
            Component_Expression    => -Conv);

         Args.Object.Append  (Args.Payload, +Arg);
      end if;

      return Self.Function_Call
        (Prefix                   => -Name,
         Function_Call_Parameters => Self.Record_Aggregate
           (Left_Token                    => (null, 0),
            Record_Component_Associations => Args,
            Right_Token                   => (null, 0)));
   end Infix_Call;

   -----------------------------------
   -- To_Defining_Program_Unit_Name --
   -----------------------------------

   function To_Defining_Program_Unit_Name
     (Self  : access Fabric;
      Value : Gela.Nodes.Selected_Identifier)
      return Gela.Nodes.Defining_Program_Unit_Name
   is
      use Gela.Nodes.Convertions;
      Result : Gela.Nodes.Element;
      Id  : Gela.Nodes.Identifier := Value.Object.Selector (Value.Payload);
   begin
      Result := +Self.Defining_Expanded_Unit_Name
        (Value.Object.Prefix (Value.Payload),
         Value.Object.Dot_Token (Value.Payload),
         Self.Defining_Identifier (Id.Object.Identifier (Id.Payload)));

      return -Result;
   end To_Defining_Program_Unit_Name;

   ------------------------------------
   -- To_Discrete_Subtype_Indication --
   ------------------------------------

   function To_Discrete_Subtype_Indication
     (Self  : access Fabric;
      Value : Gela.Nodes.Subtype_Indication)
      return Gela.Nodes.Discrete_Subtype_Indication
   is
      use Gela.Nodes.Convertions;
      Constraint : constant Gela.Nodes.Element :=
        +Value.Object.Subtype_Constraint (Value.Payload);
   begin
      --  FIXME: Check to be range_constraint
      return Self.Discrete_Subtype_Indication
        (Value.Object.Subtype_Mark (Value.Payload),
         -Constraint);
   end To_Discrete_Subtype_Indication;

   ----------------
   -- To_Element --
   ----------------

   function To_Element
     (Self    : access Fabric;
      Payload : Gela.Types.Payload)
      return Gela.Stores.Element_Access is
   begin
      return Gela.Stores.Element_Access (Self.To_Node (Payload));
   end To_Element;

   -------------
   -- To_Node --
   -------------

   function To_Node
     (Self    : access Fabric;
      Payload : Gela.Types.Payload) return Gela.Nodes.Node_Access
   is
      use type Gela.Types.Payload;
   begin
      if Payload = 0 then
         return null;
      else
         declare
            Item : constant Element := Self.Store.Get (Index (Payload));
         begin
            return Self.Map (Natural (Item));
         end;
      end if;
   end To_Node;

end Gela.Stores.Fabrics;
