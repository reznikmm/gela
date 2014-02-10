with Ada.Containers.Hashed_Maps;

with Gela.Symbol_Sets;
with Gela.Compilation_Units;
with Gela.Elements.Library_Unit_Bodies;
with Gela.Elements.Library_Unit_Declarations;
with Gela.Semantic_Types;

package body Gela.Plain_Dependency_Lists is

   function Hash
     (Item : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type;
   --  Support hashing of symbols

   -------------------
   -- Add_Body_Unit --
   -------------------

   overriding procedure Add_Body_Unit
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
   is
   begin
      if Self.Ordered.Contains ((Unit_Body, Name)) then
         --  Double append not allowed
         raise Constraint_Error;
      elsif Self.Pending.Contains ((Unit_Body, Name)) then
         Self.Pending.Delete ((Unit_Body, Name));
      end if;

      Self.Queued.Insert ((Unit_Body, Name));
      Self.Queue.Prepend
        ((Unit_Body, Name, Withed, Limited_With, Unit));
   end Add_Body_Unit;

   ----------------------------------
   -- Add_Library_Unit_Declaration --
   ----------------------------------

   overriding procedure Add_Library_Unit_Declaration
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access)
   is
   begin
      if Self.Ordered.Contains ((Unit_Declaration, Name)) then
         --  Double append not allowed
         raise Constraint_Error;
      elsif Self.Pending.Contains ((Unit_Declaration, Name)) then
         Self.Pending.Delete ((Unit_Declaration, Name));
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
      Unit         : Gela.Elements.Subunits.Subunit_Access)
   is
      Set : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Self.Context.Symbols;
      Full_Name : Gela.Lexical_Types.Symbol;
   begin
      Set.Join
        (Left  => Parent,
         Right => Name,
         Value => Full_Name);

      if Self.Ordered.Contains ((Subunit, Full_Name)) then
         --  Double append not allowed
         raise Constraint_Error;
      elsif Self.Pending.Contains ((Subunit, Full_Name)) then
         Self.Pending.Delete ((Subunit, Full_Name));
      end if;

      Self.Queued.Insert ((Subunit, Full_Name));
      Self.Queue.Prepend
        ((Subunit, Full_Name, Withed, Limited_With, Parent, Unit));
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

      Lib    : Gela.Elements.Library_Unit_Declarations.
        Library_Unit_Declaration_Access;
      Set    : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Self.Context.Symbols;
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
               Upper := Packages.Element (Set.Prefix (Item.Name));

               Decl := Factory.Create_Library_Unit_Declaration
                 (Parent => Upper,
                  Name   => Item.Name,
                  Node   => Item.Unit_Declaration);

               Specs.Insert (Item.Name, Decl);

               Lib := Item.Unit_Declaration.Unit_Declaration;

               case Lib.Unit_Kind is
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
                     Node   => Item.Unit_Body);
               else
                  Upper := Packages.Element (Set.Prefix (Item.Name));

                  Parent := Factory.Create_Body_Unit_Without_Declaration
                    (Parent => Upper,
                     Name   => Item.Name,
                     Node   => Item.Unit_Body);
               end if;

               Bodies.Insert (Item.Name, Parent);

            when Subunit =>
               Parent := Bodies.Element (Item.Parent);

               Sub := Factory.Create_Subunit
                 (Parent => Parent,
                  Name   => Item.Name,
                  Node   => Item.Subunit);
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

      Set     : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Self.Context.Symbols;
      Pos     : Unit_Data_Lists.Cursor;
      Lib     : Gela.Elements.Library_Unit_Bodies.Library_Unit_Body_Access;
      Item    : Unit_Data;
      Index   : Unit_Index;
      Index_S : Unit_Index;
      Withed  : Gela.Lexical_Types.Symbol_List;
      Again   : Boolean := True;
      Pending : Boolean;
   begin
      while Again loop
         Again := False;
         Pending := False;

         Pos := Self.Queue.First;

         --  Iterate over Self.Queue
         while Unit_Data_Lists.Has_Element (Pos) loop
            Item   := Unit_Data_Lists.Element (Pos);
            Withed := Item.Withed;

            --  Check if withed units are ordered
            while Withed /= Gela.Lexical_Types.Empty_Symbol_List loop
               Name := Set.Tail (Withed);
               Index := (Unit_Declaration, Name);

               if Self.No_Spec.Contains (Index) then
                  Index := (Unit_Body, Name);
               end if;

               if Self.Ordered.Contains (Index) then
                  null;  --  Continue with this unit
               elsif Self.Queued.Contains (Index) then
                  goto Skip_This_Item;
               elsif Self.Pending.Contains (Index) then
                  Pending := True;
               else
                  Self.Pending.Insert (Index);
                  Declartion := Index.Kind = Unit_Declaration;
                  return;
               end if;

               Withed := Set.Head (Withed);
            end loop;

            case Item.Kind is
               when Unit_Declaration =>
                  --  Check if parent unit is ordered
                  Name := Set.Prefix (Item.Name);
                  Index := (Unit_Declaration, Name);

                  if Name = Gela.Lexical_Types.No_Symbol or else
                    Self.Ordered.Contains (Index)
                  then
                     null;  --  Continue with this unit
                  elsif Self.Queued.Contains (Index) then
                     goto Skip_This_Item;
                  elsif Self.Pending.Contains (Index) then
                     Pending := True;
                  else
                     Self.Pending.Insert (Index);
                     Declartion := True;
                     return;
                  end if;

               when Unit_Body =>
                  --  Check if declaration is ordered
                  Name  := Item.Name;
                  Index := (Unit_Declaration, Name);

                  if Self.No_Spec.Contains (Index) then
                     Lib := Item.Unit_Body.Unit_Declaration;

                     if Lib.Unit_Kind in Gela.Semantic_Types.A_Function_Body
                       | Gela.Semantic_Types.A_Procedure_Body
                     then
                        null;  --  Continue with this unit
                     else
                        raise Constraint_Error;
                     end if;

                  elsif Self.Ordered.Contains (Index) then
                     null;  --  Continue with this unit
                  elsif Self.Queued.Contains (Index) then
                     goto Skip_This_Item;
                  elsif Self.Pending.Contains (Index) then
                     Pending := True;
                  else
                     Self.Pending.Insert (Index);
                     Declartion := True;
                     return;
                  end if;

               when Subunit =>
                  --  Check if enclosing body is ordered
                  Name := Item.Parent;

                  Index_S := (Subunit, Name);
                  Index := (Unit_Body, Name);
                  --  Check parent in existing subunits
                  if Self.Ordered.Contains (Index_S) then
                     null;  --  Continue with this unit
                  elsif Self.Queued.Contains (Index_S) then
                     goto Skip_This_Item;
                  --  Then check parent in existing bodies
                  elsif Self.Ordered.Contains (Index) then
                     null;  --  Continue with this unit
                  elsif Self.Queued.Contains (Index) then
                     goto Skip_This_Item;
                  elsif Self.Pending.Contains (Index) then
                     Pending := True;
                  else
                     Self.Pending.Insert (Index);
                     Declartion := True;
                     return;
                  end if;

            end case;

            if not Pending then
               --  All dependencies are ordered. Append Item to Order
               --  and try again
               Self.Order.Append (Item);
               Self.Ordered.Insert ((Item.Kind, Item.Name));
               Self.Queue.Delete (Pos);
               Pos := Self.Queue.First;
               Self.Queued.Delete ((Item.Kind, Item.Name));
               Again := True;
            end if;

            <<Skip_This_Item>>
            Unit_Data_Lists.Next (Pos);
         end loop;
      end loop;

      if not Self.Queue.Is_Empty then
         raise Constraint_Error
           with Set.Image (Self.Queue.First_Element.Name).To_UTF_8_String;
      end if;

      Name := Gela.Lexical_Types.No_Symbol;
      Declartion := False;
   end Next_Required_Unit;

   ---------------------------------
   -- No_Library_Unit_Declaration --
   ---------------------------------

   overriding procedure No_Library_Unit_Declaration
     (Self : in out Dependency_List;
      Name : Gela.Lexical_Types.Symbol) is
   begin
      if Self.Pending.Contains ((Unit_Declaration, Name)) then
         Self.Pending.Delete ((Unit_Declaration, Name));
         Self.No_Spec.Insert ((Unit_Declaration, Name));
      end if;
   end No_Library_Unit_Declaration;

end Gela.Plain_Dependency_Lists;

