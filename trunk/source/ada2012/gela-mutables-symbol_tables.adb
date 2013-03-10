------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Mutables.Symbol_Tables is

   type Colors is (Red, Black);

   type Node;
   type Node_Access is access all Node;

   type Tree is record
      Root    : Node_Access;
      Levels  : Natural := 0;  --  Number of black nodes
      Version : Natural := 0;
      Frozen  : Boolean := False;
   end record;

   type Node is record
      Name    : Gela.Types.Symbol;
      Value   : Gela.Types.Defining_Name;
      Left    : Node_Access;
      Right   : Node_Access;
      Version : Natural := 0;
      Color   : Colors;
   end record;

   type Node_Access_Array is array (Positive range <>) of Node_Access;

   Empty : aliased Tree :=
     (Root    => null,
      Levels  => 0,
      Version => 0,
      Frozen  => True);

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Types.Payload;
      Name    : Gela.Types.Symbol;
      Value   : Gela.Types.Defining_Name)
   is
      pragma Unreferenced (Payload);

      use type Gela.Types.Symbol;

      procedure Rotate_Left (P : not null Node_Access);
      procedure Rotate_Right (P : not null Node_Access);
      procedure Mutate (P : in out not null Node_Access; Level : Positive);

      Tree : Tree_Access renames Self.Tree;

      Parents : Node_Access_Array (1 .. 2 * Tree.Levels);

      ------------
      -- Mutate --
      ------------

      procedure Mutate (P : in out not null Node_Access; Level : Positive) is
         Old_Node : constant not null Node_Access := P;
      begin
         if P.Version = Tree.Version then
            return;
         end if;

         P := new Node'(Name    => Old_Node.Name,
                        Value   => Old_Node.Value,
                        Left    => Old_Node.Left,
                        Right   => Old_Node.Right,
                        Version => Tree.Version,
                        Color   => Old_Node.Color);

         if Level > 1 then
            Mutate (Parents (Level - 1), Level - 1);

            if Parents (Level - 1).Left = Old_Node then
               Parents (Level - 1).Left := P;
            else
               Parents (Level - 1).Right := P;
            end if;
         else
            Tree.Root := P;
         end if;
      end Mutate;

      -----------------
      -- Rotate_Left --
      -----------------

      procedure Rotate_Left (P : not null Node_Access) is
         N : constant Node_Access := P.Right;
      begin
         P.Right := N.Left;
         N.Left := P;
      end Rotate_Left;

      ------------------
      -- Rotate_Right --
      ------------------

      procedure Rotate_Right (P : not null Node_Access) is
         N : constant Node_Access := P.Left;
      begin
         P.Left := N.Right;
         N.Right := P;
      end Rotate_Right;

      Item   : Node_Access := Tree.Root;
      Level  : Positive := 1;  --  Level of Item
   begin
      if Tree.Frozen then
         Tree := new Symbol_Tables.Tree'
           (Root    => Tree.Root,
            Levels  => Tree.Levels,
            Version => Tree.Version + 1,
            Frozen  => False);
      end if;

      while Item /= null loop
         Parents (Level) := Item;
         Level := Level + 1;

         if Name = Item.Name then
            Mutate (Item, Level);
            Item.Value := Value;
            return;
         elsif Name < Item.Name then
            Item := Item.Left;
         else
            Item := Item.Right;
         end if;
      end loop;

      Item := new Node'(Name    => Name,
                        Value   => Value,
                        Left    => null,
                        Right   => null,
                        Version => Tree.Version,
                        Color   => Red);

      if Level = 1 then  --  Root
         Tree.Root := Item;
      elsif Name < Parents (Level - 1).Name then
         Mutate (Parents (Level - 1), Level - 1);
         Parents (Level - 1).Left := Item;
      else
         Mutate (Parents (Level - 1), Level - 1);
         Parents (Level - 1).Right := Item;
      end if;

      while Level > 2 and then Parents (Level - 1).Color = Red loop
         declare
            Parent      : Node_Access renames Parents (Level - 1);
            Grandparent : constant Node_Access := Parents (Level - 2);
            Uncle       : Node_Access;
         begin
            if Grandparent.Left = Parent then
               Uncle := Grandparent.Right;
            else
               Uncle := Grandparent.Left;
            end if;

            if Uncle /= null and then Uncle.Color = Red then
               Mutate (Uncle, Level - 1);
               Uncle.Color := Black;
               Parent.Color := Black;
               Grandparent.Color := Red;
               Item := Grandparent;
               Level := Level - 2;
            else
               if Item = Parent.Right and Parent = Grandparent.Left then
                  Rotate_Left (Parent);
                  Grandparent.Left := Item;
                  Parent := Item;
                  Item := Item.Left;
               elsif Item = Parent.Left and Parent = Grandparent.Right then
                  Rotate_Right (Parent);
                  Grandparent.Right := Item;
                  Parent := Item;
                  Item := Item.Right;
               end if;

               Parent.Color := Black;
               Grandparent.Color := Red;

               if Item = Parent.Left then
                  Rotate_Right (Grandparent);
               else
                  Rotate_Left (Grandparent);
               end if;

               if Level > 2 then
                  if Parents (Level - 3).Left = Grandparent then
                     Parents (Level - 3).Left := Parent;
                  else
                     Parents (Level - 3).Right := Parent;
                  end if;
               else
                  Tree.Root := Parent;
               end if;

               exit;
            end if;
         end;
      end loop;

      if Tree.Root.Color = Red then
         Tree.Root.Color := Black;
         Tree.Levels := Tree.Levels + 1;
      end if;
   end Append;

   ----------
   -- Copy --
   ----------

   overriding procedure Copy
     (Self     : in out Symbol_Table;
      Payload  : Gela.Types.Payload;
      Target   : out Gela.Types.Symbol_Table) is
   begin
      Target.Payload := Payload;
      Symbol_Table (Target.Object.all).Tree := Self.Tree;
      Self.Tree.Frozen := True;
   end Copy;

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree return Tree_Access is
   begin
      return Empty'Access;
   end Empty_Tree;

   ----------
   -- Find --
   ----------

   overriding function Find
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Types.Payload;
      Name    : Gela.Types.Symbol)
      return Gela.Types.Defining_Name
   is
      pragma Unreferenced (Payload);

      use type Gela.Types.Symbol;

      Item : Node_Access := Self.Tree.Root;
   begin
      while Item /= null loop
         if Item.Name = Name then
            return Item.Value;
         elsif Name < Item.Name then
            Item := Item.Left;
         else
            Item := Item.Right;
         end if;
      end loop;

      return (null, 0);
   end Find;

end Gela.Mutables.Symbol_Tables;
