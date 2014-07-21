with Gela.Symbol_Sets;
with Gela.Elements.Library_Unit_Bodies;
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

   -----------------
   -- Next_Action --
   -----------------

   overriding procedure Next_Action
     (Self   : in out Dependency_List;
      Action : out Gela.Dependency_Lists.Action)
   is
      use type Gela.Lexical_Types.Symbol;
      use type Gela.Lexical_Types.Symbol_List;
      use all type Gela.Dependency_Lists.Action_Kinds;

      Set     : constant Gela.Symbol_Sets.Symbol_Set_Access :=
        Self.Context.Symbols;
      Pos     : Unit_Data_Lists.Cursor := Self.Queue.First;
      Lib     : Gela.Elements.Library_Unit_Bodies.Library_Unit_Body_Access;
      Item    : Gela.Dependency_Lists.Unit_Data;
      Index   : Unit_Index;
      Index_S : Unit_Index;
      Withed  : Gela.Lexical_Types.Symbol_List;
      Pending : Boolean := False;
   begin
      --  Iterate over Self.Queue
      while Unit_Data_Lists.Has_Element (Pos) loop
         Item   := Unit_Data_Lists.Element (Pos);
         Withed := Item.Withed;

         --  Check if withed units are ordered
         while Withed /= Gela.Lexical_Types.Empty_Symbol_List loop
            Index := (Unit_Declaration, Set.Tail (Withed));

            if Self.No_Spec.Contains (Index) then
               Index := (Unit_Body, Index.Name);
            end if;

            if Self.Ordered.Contains (Index) then
               null;  --  Continue with this unit
            elsif Self.Queued.Contains (Index) then
               goto Skip_This_Item;
            elsif Self.Pending.Contains (Index) then
               Pending := True;
            else
               Self.Pending.Insert (Index);
               Action := (Unit_Required, Index.Name, Index.Kind);
               return;
            end if;

            Withed := Set.Head (Withed);
         end loop;

         case Item.Kind is
            when Unit_Declaration =>
               --  Check if parent unit is ordered
               Index := (Unit_Declaration, Set.Parent (Item.Name));

               if Index.Name = Gela.Lexical_Types.No_Symbol or else
                 Self.Ordered.Contains (Index)
               then
                  null;  --  Continue with this unit
               elsif Self.Queued.Contains (Index) then
                  goto Skip_This_Item;
               elsif Self.Pending.Contains (Index) then
                  Pending := True;
               else
                  Self.Pending.Insert (Index);
                  Action := (Unit_Required, Index.Name, Index.Kind);
                  return;
               end if;

            when Unit_Body =>
               --  Check if declaration is ordered
               Index := (Unit_Declaration, Item.Name);

               if Self.No_Spec.Contains (Index) then
                  --  Check if parent unit is ordered
                  Index := (Unit_Declaration, Set.Parent (Item.Name));
                  Lib := Item.Unit_Body.Unit_Declaration;

                  if Lib.Unit_Kind not in Gela.Semantic_Types.A_Function_Body
                    | Gela.Semantic_Types.A_Procedure_Body
                  then
                     raise Constraint_Error;
                  elsif Index.Name = Gela.Lexical_Types.No_Symbol or else
                    Self.Ordered.Contains (Index)
                  then
                     null;  --  Continue with this unit
                  elsif Self.Queued.Contains (Index) then
                     goto Skip_This_Item;
                  elsif Self.Pending.Contains (Index) then
                     Pending := True;
                  else
                     Self.Pending.Insert (Index);
                     Action := (Unit_Required, Index.Name, Index.Kind);
                     return;
                  end if;

               elsif Self.Ordered.Contains (Index) then
                  null;  --  Continue with this unit
               elsif Self.Queued.Contains (Index) then
                  goto Skip_This_Item;
               elsif Self.Pending.Contains (Index) then
                  Pending := True;
               else
                  Self.Pending.Insert (Index);
                  Action := (Unit_Required, Item.Name, Index.Kind);
                  return;
               end if;

            when Subunit =>
               --  Check if enclosing body is ordered
               Index_S := (Subunit, Item.Parent);
               Index := (Unit_Body, Item.Parent);
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
                  Action := (Unit_Required, Item.Parent, Index.Kind);
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
            Action := (Unit_Ready, Item);
            return;
         end if;

         <<Skip_This_Item>>
         Unit_Data_Lists.Next (Pos);
      end loop;

      if not Self.Queue.Is_Empty then
         raise Constraint_Error
           with Set.Image (Self.Queue.First_Element.Name).To_UTF_8_String;
      end if;

      Action := (Action_Kind => Complete);
   end Next_Action;

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

