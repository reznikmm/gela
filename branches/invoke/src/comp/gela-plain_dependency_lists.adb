with Ada.Containers.Hashed_Maps;

with Gela.Semantic_Types;
with Gela.Symbol_Sets;
with Gela.Compilations;
with Gela.Compilation_Units;

package body Gela.Plain_Dependency_Lists is

   function Hash
     (Item : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type;
   --  Support hashing of symbols

   function Prefix
     (Unit : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
         return Gela.Lexical_Types.Symbol;
   --  Return symbol of parent unit

   -------------------
   -- Add_Body_Unit --
   -------------------

   overriding procedure Add_Body_Unit
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
   is
   begin
      if Self.Ordered.Contains ((Unit_Body, Name)) then
         --  Double append not allowed
         raise Constraint_Error;
      end if;

      Self.Queued.Insert ((Unit_Body, Name));
      Self.Queue.Prepend
        ((Unit_Body, Name, Withed, Limited_With, Unit, False));
   end Add_Body_Unit;

   ----------------------------------
   -- Add_Library_Unit_Declaration --
   ----------------------------------

   overriding procedure Add_Library_Unit_Declaration
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
   is
   begin
      if Self.Ordered.Contains ((Unit_Declaration, Name)) then
         --  Double append not allowed
         raise Constraint_Error;
      end if;

      Self.Queued.Insert ((Unit_Declaration, Name));
      Self.Queue.Prepend
        ((Unit_Declaration, Name, Withed, Limited_With, Unit));
   end Add_Library_Unit_Declaration;

   -----------------
   -- Add_Subunit --
   -----------------

   overriding procedure Add_Subunit
     (Self         : in out Dependency_List;
      Parent       : Gela.Lexical_Types.Symbol;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
   is
   begin
      if Self.Ordered.Contains ((Subunit, Name)) then
         --  Double append not allowed
         raise Constraint_Error;
      end if;

      Self.Queued.Insert ((Subunit, Name));
      Self.Queue.Prepend
        ((Subunit, Name, Withed, Limited_With, Unit, Parent));
   end Add_Subunit;

   ------------------
   -- Create_Units --
   ------------------

   overriding procedure Create_Units
     (Self    : in out Dependency_List;
      Factory : Gela.Compilation_Unit_Factories.
        Compilation_Unit_Factory_Access)
   is
      package Package_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Gela.Lexical_Types.Symbol,
         Element_Type    => Gela.Compilation_Units.Package_Unit_Access,
         Hash            => Hash,
         Equivalent_Keys => Gela.Lexical_Types."=",
         "="             => Gela.Compilation_Units."=");

      package Body_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Gela.Lexical_Types.Symbol,
         Element_Type    => Gela.Compilation_Units.Body_Unit_Access,
         Hash            => Hash,
         Equivalent_Keys => Gela.Lexical_Types."=",
         "="             => Gela.Compilation_Units."=");

      package Spec_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Gela.Lexical_Types.Symbol,
         Element_Type    =>
            Gela.Compilation_Units.Library_Unit_Declaration_Access,
         Hash            => Hash,
         Equivalent_Keys => Gela.Lexical_Types."=",
         "="             => Gela.Compilation_Units."=");

      Packages  : Package_Maps.Map;
      --  Map of packages placed in Order list
      Bodies    : Body_Maps.Map;
      --  Map of unit bodies placed in Order list
      Specs     : Spec_Maps.Map;
      --  Map of unit declarations placed in Order list

      Pos    : Unit_Data_Lists.Cursor := Self.Order.First;
      Item   : Unit_Data;
      Upper  : Gela.Compilation_Units.Package_Unit_Access;
      Decl   : Gela.Compilation_Units.Library_Unit_Declaration_Access;
      Parent : Gela.Compilation_Units.Body_Unit_Access;
      Sub    : Gela.Compilation_Units.Subunit_Access;
      pragma Unreferenced (Sub);
   begin
      while Unit_Data_Lists.Has_Element (Pos) loop
         Item := Unit_Data_Lists.Element (Pos);

         case Item.Kind is
            when Unit_Declaration =>
               Upper := Packages.Element (Prefix (Item.Unit));

               Decl := Factory.Create_Library_Unit_Declaration
                 (Parent => Upper,
                  Name   => Item.Name,
                  Node   => Item.Unit);

               Specs.Insert (Item.Name, Decl);

               case Item.Unit.Unit_Kind is
                  when Gela.Semantic_Types.A_Package |
                       Gela.Semantic_Types.A_Generic_Package =>
                     Upper :=
                       Gela.Compilation_Units.Package_Unit_Access (Decl);
                     Packages.Insert (Item.Name, Upper);
                  when others =>
                     null;
               end case;

            when Unit_Body =>
               if Specs.Contains (Item.Name) then
                  Decl := Specs.Element (Item.Name);

                  Parent := Factory.Create_Body_Unit
                    (Declaration => Decl,
                     Name   => Item.Name,
                     Node   => Item.Unit);
               else
                  Upper := Packages.Element (Prefix (Item.Unit));

                  Parent := Factory.Create_Body_Unit_Without_Declaration
                    (Parent => Upper,
                     Name   => Item.Name,
                     Node   => Item.Unit);
               end if;

               Bodies.Insert (Item.Name, Parent);

            when Subunit =>
               Parent := Bodies.Element (Item.Parent);

               Sub := Factory.Create_Subunit
                 (Parent => Parent,
                  Name   => Item.Name,
                  Node   => Item.Unit);
         end case;

         Unit_Data_Lists.Next (Pos);
      end loop;
   end Create_Units;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Unit_Index) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Hash (Item.Name) * 3 + Unit_Kinds'Pos (Item.Kind);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Item);
   end Hash;

   ------------------------
   -- Next_Required_Unit --
   ------------------------

   overriding procedure Next_Required_Unit
     (Self         : in out Dependency_List;
      Name         : out Gela.Lexical_Types.Symbol;
      Declartion   : out Boolean)
   is
      use type Gela.Lexical_Types.Symbol;
      use type Gela.Lexical_Types.Symbol_List;
      Set    : Gela.Symbol_Sets.Symbol_Set_Access;
      Pos    : Unit_Data_Lists.Cursor;
      Item   : Unit_Data;
      Index  : Unit_Index;
      Withed : Gela.Lexical_Types.Symbol_List;
      Again  : Boolean := True;
   begin
      if not Self.Order.Is_Empty then
         declare
            Comp : constant Gela.Compilations.Compilation_Access :=
              Self.Order.First_Element.Unit.Enclosing_Compilation;
         begin
            Set := Comp.Context.Symbols;
         end;
      end if;

      while Again loop
         Again := False;

         Pos := Self.Queue.First;

         --  Iterate over Self.Queue
         while Unit_Data_Lists.Has_Element (Pos) loop
            Item   := Unit_Data_Lists.Element (Pos);
            Withed := Item.Withed;

            --  Check if withed units are ordered
            while Withed /= Gela.Lexical_Types.Empty_Symbol_List loop
               Name := Set.Tail (Withed);
               Index := (Unit_Declaration, Name);

               if Self.Ordered.Contains (Index) then
                  null;
               elsif Self.Queued.Contains (Index) then
                  goto Skip_This_Item;
               else
                  Declartion := True;
                  return;
               end if;

               Withed := Set.Head (Withed);
            end loop;

            case Item.Kind is
               when Unit_Declaration =>
                  --  Check if parent unit is ordered
                  Name := Prefix (Item.Unit);
                  Index := (Unit_Declaration, Name);

                  if Name = Gela.Lexical_Types.No_Symbol or else
                    Self.Ordered.Contains (Index)
                  then
                     null;
                  elsif Self.Queued.Contains (Index) then
                     goto Skip_This_Item;
                  else
                     Declartion := True;
                     return;
                  end if;

               when Unit_Body =>
                  --  Check if declaration is ordered
                  Name  := Item.Name;
                  Index := (Unit_Declaration, Name);

                  if Self.Ordered.Contains (Index) then
                     null;
                  elsif Self.Queued.Contains (Index) then
                     goto Skip_This_Item;
                  elsif Item.No_Spec then
                     null;
                  else
                     --  Ask for spec just once
                     Item.No_Spec := True;
                     Self.Order.Replace_Element (Pos, Item);
                     Declartion := True;
                     return;
                  end if;

               when Subunit =>
                  --  Check if enclosing body is ordered
                  Name := Item.Parent;
                  Index := (Unit_Body, Name);
                  if Self.Ordered.Contains (Index) then
                     null;
                  elsif Self.Queued.Contains (Index) then
                     goto Skip_This_Item;
                  else
                     Declartion := True;
                     return;
                  end if;

            end case;

            --  All dependencies are ordered. Append Item to Order
            Self.Order.Append (Item);
            Self.Ordered.Insert ((Item.Kind, Item.Name));
            Self.Queue.Delete (Pos);
            Pos := Self.Queue.First;
            Self.Queued.Delete ((Item.Kind, Item.Name));
            Again := True;

            <<Skip_This_Item>>
            Unit_Data_Lists.Next (Pos);
         end loop;
      end loop;

      if not Self.Queue.Is_Empty then
         raise Constraint_Error;
      end if;

      Name := Gela.Lexical_Types.No_Symbol;
      Declartion := False;
   end Next_Required_Unit;

   ------------
   -- Prefix --
   ------------

   function Prefix
     (Unit : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
         return Gela.Lexical_Types.Symbol
   is
      Comp : constant Gela.Compilations.Compilation_Access :=
        Unit.Enclosing_Compilation;
      Set : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Comp.Context.Symbols;
   begin
      return Set.Prefix (Unit.Full_Name);
   end Prefix;

end Gela.Plain_Dependency_Lists;

