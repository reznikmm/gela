--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

package body Program.Visibility is

   type Allocated_Snapshot is access all Snapshot;

   procedure Append_Item
     (Self  : in out Context'Class;
      Value : Item);

   function Get_View
     (Env   : access constant Context;
      Index : Item_Offset_Positive) return View;

   function To_Vector (List : View_Array) return Item_Offset_Vectors.Vector;

   function Immediate_Visible
     (Self   : access constant Context;
      Region : Item_Offset;
      Symbol : Program.Visibility.Symbol) return View_Array;

   -----------------
   -- Append_Item --
   -----------------

   procedure Append_Item
     (Self  : in out Context'Class;
      Value : Item) is
   begin
      Self.Last_Entity := Self.Last_Entity + 1;
      Self.Data.Append (Value);

      if not Self.Stack.Is_Empty then
         Self.Data (Self.Stack.Last_Element.Enclosing_Item).Region.Append
           (Self.Data.Last_Index);
      end if;
   end Append_Item;

   ---------------
   -- Component --
   ---------------

   function Component (Self : View) return View is
      Type_Item : Item renames Self.Env.Data (Self.Index);
   begin
      return Self.Env.Get_View (Type_Item.Component);
   end Component;

   -----------------------
   -- Create_Array_Type --
   -----------------------

   not overriding procedure Create_Array_Type
     (Self      : in out Context;
      Symbol    : Program.Visibility.Symbol;
      Name      : Defining_Name;
      Indexes   : View_Array;
      Component : View)
   is
      Value : constant Item :=
        (Kind      => Array_Type_View,
         Symbol    => Symbol,
         Name      => Name,
         Entity_Id => Self.Last_Entity + 1,
         Indexes   => To_Vector (Indexes),
         Component => Component.Index);

   begin
      Self.Append_Item (Value);
   end Create_Array_Type;

   ------------------------------
   -- Create_Character_Literal --
   ------------------------------

   not overriding procedure Create_Character_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Enumeration_Type : View)
   is
      Value : constant Item :=
        (Kind           => Character_Literal_View,
         Symbol         => Symbol,
         Name           => Name,
         Entity_Id      => Self.Last_Entity + 1,
         Character_Type => Enumeration_Type.Index);

   begin
      Self.Append_Item (Value);

      declare
         Type_Item : Item renames Self.Data (Enumeration_Type.Index);
      begin
         Type_Item.Enumeration_Literals.Append (Self.Data.Last_Index);
      end;
   end Create_Character_Literal;

   ------------------------------
   -- Create_Character_Literal --
   ------------------------------

   not overriding procedure Create_Character_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Meta_Character   : Meta_Character_Literal_Kind;
      Enumeration_Type : View)
   is
      pragma Unreferenced (Meta_Character);

      Value : constant Item :=
        (Kind           => Character_Literal_View,
         Symbol         => Symbol,
         Name           => null,   --  FIXME
         Entity_Id      => Self.Last_Entity + 1,
         Character_Type => Enumeration_Type.Index);

   begin
      Self.Append_Item (Value);

      declare
         Type_Item : Item renames Self.Data (Enumeration_Type.Index);
      begin
         Type_Item.Enumeration_Literals.Append (Self.Data.Last_Index);
      end;
   end Create_Character_Literal;

   --------------------------
   -- Create_Empty_Context --
   --------------------------

   not overriding procedure Create_Empty_Context
     (Self : aliased in out Context) is
   begin
      Self.Last_Entity := 0;
      Self.Data := Item_Vectors.Empty_Vector;
      Self.Stack := Region_Vectors.Empty_Vector;
   end Create_Empty_Context;

   --------------------------------
   -- Create_Enumeration_Literal --
   --------------------------------

   not overriding procedure Create_Enumeration_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Enumeration_Type : View)
   is
      Value : constant Item :=
        (Kind             => Enumeration_Literal_View,
         Symbol           => Symbol,
         Name             => Name,
         Entity_Id        => Self.Last_Entity + 1,
         Enumeration_Type => Enumeration_Type.Index);

   begin
      Self.Append_Item (Value);

      declare
         Type_Item : Item renames Self.Data (Enumeration_Type.Index);
      begin
         Type_Item.Enumeration_Literals.Append (Self.Data.Last_Index);
      end;
   end Create_Enumeration_Literal;

   -----------------------------
   -- Create_Enumeration_Type --
   -----------------------------

   not overriding procedure Create_Enumeration_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Item :=
        (Kind                 => Enumeration_Type_View,
         Symbol               => Symbol,
         Name                 => Name,
         Entity_Id            => Self.Last_Entity + 1,
         Enumeration_Literals => Item_Offset_Vectors.Empty_Vector,
         Is_Character_Type    => False);
   begin
      Self.Append_Item (Value);
   end Create_Enumeration_Type;

   ----------------------
   -- Create_Exception --
   ----------------------

   not overriding procedure Create_Exception
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Item :=
        (Kind      => Exception_View,
         Symbol    => Symbol,
         Name      => Name,
         Entity_Id => Self.Last_Entity + 1);
   begin
      Self.Append_Item (Value);
   end Create_Exception;

   -----------------------------
   -- Create_Float_Point_Type --
   -----------------------------

   not overriding procedure Create_Float_Point_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Item :=
        (Kind      => Float_Point_Type_View,
         Symbol    => Symbol,
         Name      => Name,
         Entity_Id => Self.Last_Entity + 1);
   begin
      Self.Append_Item (Value);
   end Create_Float_Point_Type;

   --------------------------
   -- Create_Implicit_Type --
   --------------------------

   not overriding procedure Create_Implicit_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Item :=
        (Kind      => Implicit_Type_View,
         Symbol    => Symbol,
         Name      => Name,
         Entity_Id => Self.Last_Entity + 1);
   begin
      Self.Append_Item (Value);
   end Create_Implicit_Type;

   -------------------------
   -- Create_Modular_Type --
   -------------------------

   not overriding procedure Create_Modular_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Item :=
        (Kind      => Modular_Type_View,
         Symbol    => Symbol,
         Name      => Name,
         Entity_Id => Self.Last_Entity + 1);
   begin
      Self.Append_Item (Value);
   end Create_Modular_Type;

   --------------------
   -- Create_Package --
   --------------------

   not overriding procedure Create_Package
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Item :=
        (Kind      => Package_View,
         Symbol    => Symbol,
         Name      => Name,
         Entity_Id => Self.Last_Entity + 1,
         Region    => Item_Offset_Vectors.Empty_Vector);
   begin
      Self.Append_Item (Value);
      Self.Stack.Append ((Enclosing_Item => Self.Data.Last_Index));
   end Create_Package;

   --------------------------------
   -- Create_Signed_Integer_Type --
   --------------------------------

   not overriding procedure Create_Signed_Integer_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Item :=
        (Kind      => Signed_Integer_Type_View,
         Symbol    => Symbol,
         Name      => Name,
         Entity_Id => Self.Last_Entity + 1);
   begin
      Self.Append_Item (Value);
   end Create_Signed_Integer_Type;

   ---------------------
   -- Create_Snapshot --
   ---------------------

   not overriding function Create_Snapshot
     (Self : aliased in out Context) return Snapshot_Access
   is
      Result : constant Allocated_Snapshot :=
        new Snapshot'(Stack => Self.Stack,
                      Data  => Self.Data);
   begin
      return Snapshot_Access (Result);
   end Create_Snapshot;

   --------------------
   -- Create_Subtype --
   --------------------

   not overriding procedure Create_Subtype
     (Self           : in out Context;
      Symbol         : Program.Visibility.Symbol;
      Name           : Defining_Name;
      Subtype_Mark   : View;
      Has_Constraint : Boolean)
   is
      Value : constant Item :=
        (Kind           => Subtype_View,
         Symbol         => Symbol,
         Name           => Name,
         Entity_Id      => Self.Last_Entity + 1,
         Subtype_Mark   => Subtype_Mark.Index,
         Has_Constraint => Has_Constraint);
   begin
      Self.Append_Item (Value);
   end Create_Subtype;

   --------------------
   -- Enter_Snapshot --
   --------------------

   not overriding procedure Enter_Snapshot
     (Self : in out Context; Snapshot : not null Snapshot_Access)
   is

      --  We create 3 maps:
      --  Keep_Map - elements to keep in context (above subtree)
      --  Update_Map - elements of subtree to be replaced by Snapshot elements
      --  Added_Map - new elements to be added from Snapshot

      function Hash
        (Value : Entity_Identifier) return Ada.Containers.Hash_Type is
          (Ada.Containers.Hash_Type (Value));

      package Entity_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Entity_Identifier,
         Element_Type    => Item_Offset_Positive,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => "=");

      procedure Append
        (Data   : Item_Vectors.Vector;
         Region : Item_Offset_Positive;
         Map    : in out Entity_Maps.Map);

      function Update_Index
        (Index : Item_Offset_Positive)  --  In Snapshot
         return Item_Offset_Positive;   --  In Env

      procedure Update_Index (Index : in out Item_Offset_Positive);
      procedure Update_Vector (Region : in out Item_Offset_Vectors.Vector);

      ------------
      -- Append --
      ------------

      procedure Append
        (Data   : Item_Vectors.Vector;
         Region : Item_Offset_Positive;
         Map    : in out Entity_Maps.Map)
      is
         Region_Item : Item renames Data (Region);
      begin
         Map.Insert (Region_Item.Entity_Id, Region);

         if Region_Item.Kind not in Has_Region_Kind then
            return;
         end if;

         for Index of Region_Item.Region loop
            declare
               Value : constant Item := Data (Index);
            begin
               if not Map.Contains (Value.Entity_Id) then
                  Append (Data, Index, Map);
               end if;
            end;
         end loop;
      end Append;

      Replaced_Items  : Entity_Maps.Map;  --  In Env
      Replacing_Items : Entity_Maps.Map;  --  In Snapshot
      Added_Items     : Entity_Maps.Map;  --  In Env

      ------------------
      -- Update_Index --
      ------------------

      function Update_Index
        (Index : Item_Offset_Positive)  --  In Snapshot
         return Item_Offset_Positive    --  In Env
      is
         Id : constant Entity_Identifier := Snapshot.Data (Index).Entity_Id;
      begin
         if Replaced_Items.Contains (Id) then
            return Replaced_Items (Id);
         else
            return Added_Items.Element (Id);
         end if;
      end Update_Index;

      ------------------
      -- Update_Index --
      ------------------

      procedure Update_Index (Index : in out Item_Offset_Positive) is
      begin
         Index := Update_Index (Index);
      end Update_Index;

      -------------------
      -- Update_Vector --
      -------------------

      procedure Update_Vector (Region : in out Item_Offset_Vectors.Vector) is
      begin
         for Ref of Region loop
            Ref := Update_Index (Ref);
         end loop;
      end Update_Vector;

      Env_Map : Entity_Maps.Map;
      Top_In_Snapshot : constant Item_Offset_Positive :=  --  In Snapshot
        Snapshot.Stack.Last_Element.Enclosing_Item;
      Top_Item_In_Snapshot : Item renames Snapshot.Data (Top_In_Snapshot);
      Replaced_Item : Item_Offset_Positive;  --  In Env
   begin
      for Region of reverse Self.Stack loop
         Append (Self.Data, Region.Enclosing_Item, Env_Map);
      end loop;

      Replaced_Item := Env_Map.Element (Top_Item_In_Snapshot.Entity_Id);
      Append (Self.Data, Replaced_Item, Replaced_Items);
      Append (Snapshot.Data, Top_In_Snapshot, Replacing_Items);

      --  Assign indexes to added items
      declare
         Key  : Entity_Identifier;
         Last : Item_Offset := Self.Data.Last_Index;
      begin
         for Cursor in Replacing_Items.Iterate loop
            Key := Entity_Maps.Key (Cursor);

            if not Replaced_Items.Contains (Key) then
               Last := Last + 1;
               Added_Items.Insert (Key, Last);
            end if;
         end loop;

         Self.Data.Set_Length (Ada.Containers.Count_Type (Last));
      end;

      --  Copy items from snapshot to env
      for Index of Replacing_Items loop
         declare
            Value : Item := Snapshot.Data (Index);
            To    : constant Item_Offset_Positive := Update_Index (Index);
         begin
            case Value.Kind is
               when Has_Region_Kind =>
                  Update_Vector (Value.Region);
               when others =>
                  case Value.Kind is
                     when Enumeration_Type_View =>
                        Update_Vector (Value.Enumeration_Literals);
                     when Enumeration_Literal_View =>
                        Update_Index (Value.Enumeration_Type);
                     when Subtype_View =>
                        Update_Index (Value.Subtype_Mark);
                     when Array_Type_View =>
                        Update_Vector (Value.Indexes);
                        Update_Index (Value.Component);
                     when others =>
                        null;
                  end case;
            end case;

            Replaced_Items.Exclude (Value.Entity_Id);
            Self.Data.Replace_Element (To, Value);
         end;
      end loop;

      pragma Assert (Replaced_Items.Is_Empty);

      Self.Stack.Append ((Enclosing_Item => Replaced_Item));
   end Enter_Snapshot;

   --------------------------
   -- Enumeration_Literals --
   --------------------------

   function Enumeration_Literals (Self : View) return View_Array is
      Type_Item : Item renames Self.Env.Data (Self.Index);
      Last : Positive := 1;
   begin
      return Result : View_Array
        (1 .. Type_Item.Enumeration_Literals.Last_Index)
      do
         for J of Type_Item.Enumeration_Literals loop
            Result (Last) := Self.Env.Get_View (J);
            Last := Last + 1;
         end loop;
      end return;
   end Enumeration_Literals;

   ----------------------
   -- Enumeration_Type --
   ----------------------

   function Enumeration_Type (Self : View) return View is
      Literal_Item : Item renames Self.Env.Data (Self.Index);
   begin
      return Self.Env.Get_View (Literal_Item.Enumeration_Type);
   end Enumeration_Type;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Env   : access constant Context;
      Index : Item_Offset_Positive) return View
   is
      Value : Item renames  Env.Data (Index);
   begin
      return (Kind => Value.Kind, Env => Env, Index => Index);
   end Get_View;

   --------------------
   -- Has_Constraint --
   --------------------

   function Has_Constraint (Self : View) return Boolean is
      Subtype_Item : Item renames Self.Env.Data (Self.Index);
   begin
      return Subtype_Item.Has_Constraint;
   end Has_Constraint;

   ----------------
   -- Has_Region --
   ----------------

   function Has_Region (Self : View) return Boolean is
   begin
      return Self.Kind in Package_View;
   end Has_Region;

   -----------------------
   -- Immediate_Visible --
   -----------------------

   function Immediate_Visible
     (Self   : access constant Context;
      Region : Item_Offset;
      Symbol : Program.Visibility.Symbol) return View_Array
   is
      Result : View_Array (1 .. 10);
      Last   : Natural := 0;
      Value  : Item renames Self.Data (Region);
   begin
      if Value.Kind in  Has_Region_Kind then
         for Index of Value.Region loop
            declare
               Value : constant Item := Self.Data (Index);
            begin
               if Value.Symbol = Symbol then
                  Last := Last + 1;
                  Result (Last) := (Value.Kind, Self, Index);
               end if;
            end;
         end loop;
      end if;

      return Result (1 .. Last);
   end Immediate_Visible;

   -----------------------
   -- Immediate_Visible --
   -----------------------

   function Immediate_Visible
     (Self   : View;
      Symbol : Program.Visibility.Symbol) return View_Array is
   begin
      return Immediate_Visible (Self.Env, Self.Index, Symbol);
   end Immediate_Visible;

   -----------------------
   -- Immediate_Visible --
   -----------------------

   not overriding function Immediate_Visible
     (Self : aliased Context; Symbol : Program.Visibility.Symbol)
      return View_Array
   is
      procedure Append (List : View_Array);
      Result : View_Array (1 .. 10);
      Last   : Natural := 0;

      procedure Append (List : View_Array) is
      begin
         Result (Last + 1 .. Last + List'Length) := List;
         Last := Last + List'Length;
      end Append;

   begin
      for J of reverse Self.Stack loop
         Append (Immediate_Visible (Self'Access, J.Enclosing_Item, Symbol));
      end loop;

      if Symbol = Standard and then not Self.Stack.Is_Empty then
         declare
            Top : constant View :=
              (Package_View,
               Self'Access,
               Self.Stack.First_Element.Enclosing_Item);
         begin
            Append ((1 => Top));
         end;
      end if;

      return Result (1 .. Last);
   end Immediate_Visible;

   -------------
   -- Indexes --
   -------------

   function Indexes (Self : View) return View_Array is
      Type_Item : Item renames Self.Env.Data (Self.Index);
      Last : Positive := 1;
   begin
      return Result : View_Array (1 .. Type_Item.Indexes.Last_Index) do
         for J of Type_Item.Indexes loop
            Result (Last) := Self.Env.Get_View (J);
            Last := Last + 1;
         end loop;
      end return;
   end Indexes;

   -----------------------
   -- Is_Character_Type --
   -----------------------

   function Is_Character_Type (Self : View) return Boolean is
      Type_Item : Item renames Self.Env.Data (Self.Index);
   begin
      return Type_Item.Is_Character_Type;
   end Is_Character_Type;

   ------------------------------
   -- Leave_Declarative_Region --
   ------------------------------

   not overriding procedure Leave_Declarative_Region (Self : in out Context) is
   begin
      Self.Stack.Delete_Last;
   end Leave_Declarative_Region;

   ----------
   -- Name --
   ----------

   function Name (Self : View) return Defining_Name is
   begin
      return Self.Env.Data (Self.Index).Name;
   end Name;

   ----------------------
   -- Restore_Snapshot --
   ----------------------

   not overriding procedure Restore_Snapshot
     (Self     : in out Context;
      Snapshot : not null Snapshot_Access)
   is
   begin
      Self.Stack := Snapshot.Stack;
      Self.Data := Snapshot.Data;
   end Restore_Snapshot;

   ------------------
   -- Subtype_Mark --
   ------------------

   function Subtype_Mark (Self : View) return View is
      Subtype_Item : Item renames Self.Env.Data (Self.Index);
   begin
      return Self.Env.Get_View (Subtype_Item.Subtype_Mark);
   end Subtype_Mark;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (List : View_Array) return Item_Offset_Vectors.Vector is
   begin
      return Result : Item_Offset_Vectors.Vector do
         Result.Reserve_Capacity (List'Length);
         for View of List loop
            Result.Append (View.Index);
         end loop;
      end return;
   end To_Vector;

end Program.Visibility;
