--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with System.Storage_Elements;

with Program.Elements.Defining_Identifiers;
with Program.Elements.Identifiers;
with Program.Safe_Element_Visitors;

package body Program.Visibility is

   procedure Step
     (Self   : Region_Immediate_Visible_Iterator'Class;
      Cursor : in out View_Cursor);

   procedure Step_Region
     (Self   : Context_Immediate_Visible_Iterator'Class;
      Cursor : in out View_Cursor);

   procedure Step_Use
     (Self   : Use_Visible_Iterator'Class;
      Cursor : in out View_Cursor);

   type Allocated_Snapshot is access all Snapshot;

   procedure Append_Item
     (Self   : in out Context'Class;
      Value  : Entity;
      Region : Boolean := True);

   function Get_View
     (Env   : not null Constant_Context_Access;
      Index : Entity_Reference) return View;

   function To_Vector (List : View_Array) return Entity_References.Vector;

   package Getters is

      type Visitor
        (Env    : not null Constant_Context_Access)
      is new Program.Safe_Element_Visitors.Safe_Element_Visitor with record
         Result : View;
      end record;

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

   end Getters;

   package body Getters is

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access)
      is
         Name : constant Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access :=
             Element.Corresponding_Defining_Identifier;
         Cursor : constant Defining_Name_Maps.Cursor := Self.Env.Xref.Find
           (Name.To_Defining_Name);
      begin
         if Defining_Name_Maps.Has_Element (Cursor) then
            Self.Result := Get_View
              (Self.Env, Defining_Name_Maps.Element (Cursor));
         end if;
      end Identifier;

   end Getters;

   ---------------------
   -- Add_Use_Package --
   ---------------------

   not overriding procedure Add_Use_Package
     (Self : in out Context'Class;
      Pkg  : View)
   is
      Item : Entity renames
        Self.Data (Pkg.Index.Region).Entities (Pkg.Index.Entity_Id);
      Reg  : constant Region_Identifier := Item.Region;
   begin
      if not Self.Data (Self.Top).Uses.Contains (Reg) then
         Self.Data (Self.Top).Uses.Append (Reg);
      end if;
   end Add_Use_Package;

   -----------------
   -- Append_Item --
   -----------------

   procedure Append_Item
     (Self   : in out Context'Class;
      Value  : Entity;
      Region : Boolean := True) is
   begin
      Self.Data (Self.Top).Entities.Append (Value);
      Self.Xref.Insert
        (Value.Name, (Self.Top, Self.Data (Self.Top).Entities.Last_Index));

      if Region then
         Self.Data.Append
           ((Enclosing => Self.Top,
             Entities  => Entity_Vectors.Empty_Vector,
             Uses      => Region_Id_Vectors.Empty_Vector));

         Self.Top := Self.Data.Last_Index;
      end if;
   end Append_Item;

   ---------------
   -- Component --
   ---------------

   function Component (Self : View) return View is
      Type_Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Get_View (Self.Env, Type_Item.Component);
   end Component;

   -----------------------
   -- Create_Array_Type --
   -----------------------

   not overriding procedure Create_Array_Type
     (Self      : in out Context'Class;
      Symbol    : Program.Visibility.Symbol;
      Name      : Defining_Name;
      Indexes   : View_Array;
      Component : View)
   is
      Value : constant Entity :=
        (Kind      => Array_Type_View,
         Symbol    => Symbol,
         Name      => Name,
         Indexes   => To_Vector (Indexes),
         Component => Component.Index);
   begin
      Self.Append_Item (Value);
   end Create_Array_Type;

   ------------------------------
   -- Create_Character_Literal --
   ------------------------------

   not overriding procedure Create_Character_Literal
     (Self             : in out Context'Class;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Enumeration_Type : View)
   is
      Value : constant Entity :=
        (Kind           => Character_Literal_View,
         Symbol         => Symbol,
         Name           => Name,
         Character_Type => Enumeration_Type.Index.Entity_Id);
   begin
      pragma Assert (Self.Top = Enumeration_Type.Index.Region);
      Append_Item (Self, Value, Region => False);

      declare
         Type_Item : Entity renames
           Self.Data (Enumeration_Type.Index.Region).Entities
             (Enumeration_Type.Index.Entity_Id);
      begin
         Type_Item.Last_Literal := Self.Data (Self.Top).Entities.Last_Index;
         Type_Item.Is_Character_Type := True;
      end;
   end Create_Character_Literal;

   ------------------------------
   -- Create_Character_Literal --
   ------------------------------

   not overriding procedure Create_Character_Literal
     (Self             : in out Context'Class;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Meta_Character   : Meta_Character_Literal_Kind;
      Enumeration_Type : View)
   is
      pragma Unreferenced (Meta_Character);
   begin
      Self.Create_Character_Literal (Symbol, Name, Enumeration_Type);
   end Create_Character_Literal;

   --------------------------
   -- Create_Empty_Context --
   --------------------------

   procedure Create_Empty_Context (Self : in out Context'Class) is
   begin
      Self.Data.Clear;
      Self.Data.Append
        ((Enclosing => Self.Data.Last_Index,
          Entities  => Entity_Vectors.Empty_Vector,
          Uses      => Region_Id_Vectors.Empty_Vector));
      Self.Top := Self.Data.Last_Index;
   end Create_Empty_Context;

   --------------------------------
   -- Create_Enumeration_Literal --
   --------------------------------

   not overriding procedure Create_Enumeration_Literal
     (Self             : in out Context'Class;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Enumeration_Type : View)
   is
      Value : constant Entity :=
        (Kind             => Enumeration_Literal_View,
         Symbol           => Symbol,
         Name             => Name,
         Enumeration_Type => Enumeration_Type.Index.Entity_Id);
   begin
      pragma Assert (Self.Top = Enumeration_Type.Index.Region);
      Append_Item (Self, Value, Region => False);

      declare
         Type_Item : Entity renames
           Self.Data (Enumeration_Type.Index.Region).Entities
             (Enumeration_Type.Index.Entity_Id);
      begin
         Type_Item.Last_Literal := Self.Data (Self.Top).Entities.Last_Index;
      end;
   end Create_Enumeration_Literal;

   -----------------------------
   -- Create_Enumeration_Type --
   -----------------------------

   procedure Create_Enumeration_Type
     (Self : in out Context'Class; Symbol : Program.Visibility.Symbol;
      Name :        Defining_Name)
   is
      Last : constant Entity_Identifier'Base :=
        Self.Data (Self.Top).Entities.Last_Index;

      Value : constant Entity :=
        (Kind              => Enumeration_Type_View,
         Symbol            => Symbol,
         Name              => Name,
         Is_Character_Type => False,
         First_Literal     => Last + 2,
         Last_Literal      => Last + 2);
   begin
      Append_Item (Self, Value, Region => False);
   end Create_Enumeration_Type;

   ----------------------
   -- Create_Exception --
   ----------------------

   procedure Create_Exception
     (Self : in out Context'Class; Symbol : Program.Visibility.Symbol;
      Name :        Defining_Name)
   is
      Value : constant Entity :=
        (Kind   => Exception_View,
         Symbol => Symbol,
         Name   => Name);
   begin
      Append_Item (Self, Value, Region => False);
   end Create_Exception;

   -----------------------------
   -- Create_Float_Point_Type --
   -----------------------------

   procedure Create_Float_Point_Type
     (Self   : in out Context'Class;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Entity :=
        (Kind   => Float_Point_Type_View,
         Symbol => Symbol,
         Name   => Name);
   begin
      Self.Append_Item (Value);
   end Create_Float_Point_Type;

   ---------------------
   -- Create_Function --
   ---------------------

   procedure Create_Function
     (Self   : in out Context'Class;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Entity :=
        (Kind       => Function_View,
         Symbol     => Symbol,
         Name       => Name,
         Region     => Self.Data.Last_Index + 1,
         Result_Def => (1, 1));
   begin
      Self.Append_Item (Value);
   end Create_Function;

   --------------------------
   -- Create_Implicit_Type --
   --------------------------

   procedure Create_Implicit_Type
     (Self   : in out Context'Class;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Entity :=
        (Kind   => Implicit_Type_View,
         Symbol => Symbol,
         Name   => Name);
   begin
      Self.Append_Item (Value);
   end Create_Implicit_Type;

   -------------------------
   -- Create_Modular_Type --
   -------------------------

   procedure Create_Modular_Type
     (Self   : in out Context'Class;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Entity :=
        (Kind   => Modular_Type_View,
         Symbol => Symbol,
         Name   => Name);
   begin
      Self.Append_Item (Value);
   end Create_Modular_Type;

   --------------------
   -- Create_Package --
   --------------------

   procedure Create_Package
     (Self   : in out Context'Class;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Entity :=
        (Kind      => Package_View,
         Symbol    => Symbol,
         Name      => Name,
         Region    => Self.Data.Last_Index + 1);
   begin
      Self.Append_Item (Value);
   end Create_Package;

   ----------------------
   -- Create_Parameter --
   ----------------------

   not overriding procedure Create_Parameter
     (Self        : in out Context'Class;
      Symbol      : Program.Visibility.Symbol;
      Name        : Defining_Name;
      Mode        : Parameter_Mode;
      Has_Default : Boolean)
   is
      Value : constant Entity :=
        (Kind        => Parameter_View,
         Symbol      => Symbol,
         Name        => Name,
         Param_Def   => (1, 1),
         Mode        => Mode,
         Has_Default => Has_Default);
   begin
      Self.Append_Item (Value);
   end Create_Parameter;

   ----------------------
   -- Create_Procedure --
   ----------------------

   procedure Create_Procedure
     (Self   : in out Context'Class;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Entity :=
        (Kind   => Procedure_View,
         Symbol => Symbol,
         Name   => Name,
         Region => Self.Data.Last_Index + 1);
   begin
      Self.Append_Item (Value);
   end Create_Procedure;

   --------------------------------
   -- Create_Signed_Integer_Type --
   --------------------------------

   procedure Create_Signed_Integer_Type
     (Self   : in out Context'Class;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name)
   is
      Value : constant Entity :=
        (Kind   => Signed_Integer_Type_View,
         Symbol => Symbol,
         Name   => Name);
   begin
      Self.Append_Item (Value);
   end Create_Signed_Integer_Type;

   ---------------------
   -- Create_Snapshot --
   ---------------------

   function Create_Snapshot
     (Self : in out Context'Class) return Snapshot_Access
   is
      Top : Region renames Self.Data (Self.Top);
      Result : constant Allocated_Snapshot :=
        new Snapshot'(Region_Id => Self.Top,
                      Entities  => Top.Entities,
                      Uses      => Top.Uses);
   begin
      return Snapshot_Access (Result);
   end Create_Snapshot;

   --------------------
   -- Create_Subtype --
   --------------------

   not overriding procedure Create_Subtype
     (Self           : in out Context'Class;
      Symbol         : Program.Visibility.Symbol;
      Name           : Defining_Name;
      Subtype_Mark   : View;
      Has_Constraint : Boolean)
   is
      Value : constant Entity :=
        (Kind           => Subtype_View,
         Symbol         => Symbol,
         Name           => Name,
         Subtype_Mark   => Subtype_Mark.Index,
         Has_Constraint => Has_Constraint);
   begin
      Self.Append_Item (Value);
   end Create_Subtype;

   --------------------
   -- Enter_Snapshot --
   --------------------

   not overriding procedure Enter_Snapshot
     (Self     : in out Context'Class;
      Snapshot : not null Snapshot_Access)
   is
   begin
      if Snapshot.Entities.Is_Empty then
         Self.Data.Append
           ((Enclosing => Self.Data.Last_Index,
             Entities  => Entity_Vectors.Empty_Vector,
             Uses      => Region_Id_Vectors.Empty_Vector));

         Self.Top := Self.Data.Last_Index;
      else
         Self.Top := Snapshot.Region_Id;
      end if;

      Self.Restore_Snapshot (Snapshot);
   end Enter_Snapshot;

   --------------------------
   -- Enumeration_Literals --
   --------------------------

   function Enumeration_Literals (Self : View) return View_Array is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
      Index : Entity_Identifier := Item.First_Literal;
      Count : constant Natural := Natural (Item.Last_Literal - Index) + 1;
   begin
      return Result : View_Array (1 .. Count) do
         for X of Result loop
            X := Get_View (Self.Env, (Self.Index.Region, Index));
            Index := Index + 1;
         end loop;
      end return;
   end Enumeration_Literals;

   ----------------------
   -- Enumeration_Type --
   ----------------------

   function Enumeration_Type (Self : View) return View is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Get_View (Self.Env, (Self.Index.Region, Item.Enumeration_Type));
   end Enumeration_Type;

   --------------------
   -- Direct_Visible --
   --------------------

   function Direct_Visible
     (Self   : Context'Class;
      Symbol : Program.Visibility.Symbol)
      return Direct_Visible_Name_Iterator is
   begin
      return (Use_Visible_Iterator (Self.Use_Visible (Symbol))
                with null record);
   end Direct_Visible;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Region_Immediate_Visible_Iterator) return View_Cursor is
   begin
      return Result : View_Cursor := (Self.Region, 1, others => <>) do
         Self.Step (Result);
      end return;
   end First;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Context_Immediate_Visible_Iterator) return View_Cursor is
   begin
      return Result : View_Cursor := (Self.Region, 1, others => <>) do
         Self.Step_Region (Result);
      end return;
   end First;

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Use_Visible_Iterator) return View_Cursor is
   begin
      return Result : View_Cursor := (Self.Region, 1, 1, View => <>) do
         Self.Step_Use (Result);
      end return;
   end First;

   -------------------
   -- Get_Name_View --
   -------------------

   function Get_Name_View
     (Self : Context'Class;
      Name : not null Program.Elements.Element_Access) return View
   is
      Visitor : Getters.Visitor (Self'Unchecked_Access);
   begin
      Visitor.Visit (Name);

      return Visitor.Result;
   end Get_Name_View;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Env   : not null Constant_Context_Access;
      Index : Entity_Reference) return View
   is
      Value : Entity renames
        Env.Data (Index.Region).Entities (Index.Entity_Id);
   begin
      return (Value.Kind, Env, Index);
   end Get_View;

   --------------
   -- Get_View --
   --------------

   function Get_View (Self : View_Cursor) return View is
   begin
      return Self.View;
   end Get_View;

   --------------------
   -- Has_Constraint --
   --------------------

   function Has_Constraint (Self : View) return Boolean is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Item.Has_Constraint;
   end Has_Constraint;

   -----------------
   -- Has_Default --
   -----------------

   function Has_Default (Self : View) return Boolean is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Item.Has_Default;
   end Has_Default;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : View_Cursor) return Boolean is
   begin
      return Self.Entity > 0;
   end Has_Element;

   ----------------
   -- Has_Region --
   ----------------

   function Has_Region (Self : View) return Boolean is
   begin
      return Self.Kind in Has_Region_Kind;
   end Has_Region;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : Program.Elements.Defining_Names.Defining_Name_Access)
      return Ada.Containers.Hash_Type
   is
      Addr : constant System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (Value.all'Address);
   begin
      return Ada.Containers.Hash_Type'Mod (Addr);
   end Hash;

   -----------------------
   -- Immediate_Visible --
   -----------------------

   function Immediate_Visible
     (Self   : View;
      Symbol : Program.Visibility.Symbol) return View_Iterator
   is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Region_Immediate_Visible_Iterator'
        (Context => Self.Env,
         Region  => Item.Region,
         Symbol  => Symbol);
   end Immediate_Visible;

   -----------------------
   -- Immediate_Visible --
   -----------------------

   function Immediate_Visible
     (Self   : Context'Class;
      Symbol : Program.Visibility.Symbol) return View_Iterator is
   begin
      return Context_Immediate_Visible_Iterator'
        (Context => Self'Unchecked_Access,
         Region  => Self.Top,
         Symbol  => Symbol);
   end Immediate_Visible;

   -------------
   -- Indexes --
   -------------

   function Indexes (Self : View) return View_Array is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
      Last : Positive := 1;
   begin
      return Result : View_Array (1 .. Item.Indexes.Last_Index) do
         for J of Item.Indexes loop
            Result (Last) := Get_View (Self.Env, J);
            Last := Last + 1;
         end loop;
      end return;
   end Indexes;

   -----------------------
   -- Is_Character_Type --
   -----------------------

   function Is_Character_Type (Self : View) return Boolean is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Item.Is_Character_Type;
   end Is_Character_Type;

   -----------------
   -- Latest_View --
   -----------------

   function Latest_View (Self : Context'Class) return View is
      Top   : Region renames Self.Data (Self.Top);
      Index : Entity_Reference;
   begin
      if Top.Entities.Is_Empty then
         Index := (Top.Enclosing,
                   Self.Data (Top.Enclosing).Entities.Last_Index);

      else
         Index := (Self.Top, Top.Entities.Last_Index);

      end if;

      return Get_View (Self'Unchecked_Access, Index);
   end Latest_View;

   ------------------------------
   -- Leave_Declarative_Region --
   ------------------------------

   procedure Leave_Declarative_Region (Self : in out Context'Class) is
      Enclosing : constant Region_Identifier'Base :=
        Self.Data (Self.Top).Enclosing;
   begin
      if Self.Data.Last_Index = Self.Top
        and then Self.Data (Self.Top).Entities.Is_Empty
      then
         Self.Data.Delete_Last;
      end if;

      Self.Top := Enclosing;
   end Leave_Declarative_Region;

   ----------
   -- Mode --
   ----------

   function Mode (Self : View) return Parameter_Mode is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Item.Mode;
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (Self : View) return Defining_Name is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Item.Name;
   end Name;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Region_Immediate_Visible_Iterator;
      Position : View_Cursor) return View_Cursor is
   begin
      return Result : View_Cursor :=
        (Position.Region, Position.Entity + 1, others => <>)
      do
         Self.Step (Result);
      end return;
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Context_Immediate_Visible_Iterator;
      Position : View_Cursor) return View_Cursor is
   begin
      return Result : View_Cursor :=
        (Position.Region, Position.Entity + 1, others => <>)
      do
         Self.Step_Region (Result);
      end return;
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Use_Visible_Iterator;
      Position : View_Cursor) return View_Cursor is
   begin
      return Result : View_Cursor :=
        (Position.Region, Position.Entity + 1, Position.Use_Id, View => <>)
      do
         Self.Step_Use (Result);
      end return;
   end Next;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (Self : View) return View_Array is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
      Reg  : Region renames Self.Env.Data (Item.Region);
      Last : Natural := 0;
   begin
      for J in 1 .. Reg.Entities.Last_Index loop
         if Reg.Entities (J).Kind = Parameter_View then
            Last := Last + 1;
         else
            exit;
         end if;
      end loop;

      return Result : View_Array (1 .. Last) do
         for J in Result'Range loop
            Result (Last) := Get_View
              (Self.Env, (Item.Region, Entity_Identifier (J)));
         end loop;
      end return;
   end Parameters;

   ------------------
   -- Region_Items --
   ------------------

   function Region_Items (Self : View) return View_Array is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
      Reg  : Region renames Self.Env.Data (Item.Region);
   begin
      return Result : View_Array (1 .. Natural (Reg.Entities.Last_Index)) do
         for J in Result'Range loop
            Result (J) := Get_View
              (Self.Env, (Item.Region, Entity_Identifier (J)));
         end loop;
      end return;
   end Region_Items;

   ----------------------
   -- Restore_Snapshot --
   ----------------------

   not overriding procedure Restore_Snapshot
     (Self     : in out Context'Class;
      Snapshot : not null Snapshot_Access)
   is
      Top : Region renames Self.Data (Self.Top);
--      Length : constant Ada.Containers.Count_Type := Top.Entities.Length;
   begin
      --  Ignore Snapshot.Region_Id if empty
      pragma Assert
        (Snapshot.Entities.Is_Empty or Self.Top = Snapshot.Region_Id);

      Top.Entities := Snapshot.Entities;
--      Top.Entities.Set_Length (Length);
      Top.Uses := Snapshot.Uses;

      for J in 1 .. Snapshot.Entities.Last_Index loop
         Self.Xref.Include (Snapshot.Entities (J).Name, (Self.Top, J));
      end loop;
   end Restore_Snapshot;

   ------------
   -- Result --
   ------------

   function Result (Self : View) return View is
      Item : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      return Get_View (Self.Env, Item.Result_Def);
   end Result;

   ------------------------
   -- Set_Parameter_Type --
   ------------------------

   not overriding procedure Set_Parameter_Type
     (Self       : in out Context'Class;
      Definition : View)
   is
      Top : Region renames Self.Data (Self.Top);
      Last : Entity renames Top.Entities (Top.Entities.Last_Index);
   begin
      pragma Assert (Last.Kind = Parameter_View);
      Last.Param_Def := Definition.Index;
   end Set_Parameter_Type;

   ---------------------
   -- Set_Result_Type --
   ---------------------

   procedure Set_Result_Type
     (Self       : in out Context'Class;
      Definition : View)
   is
      Top  : Region renames Self.Data (Self.Top);
      Last : Entity renames Top.Entities (Top.Entities.Last_Index);
   begin
      pragma Assert (Last.Kind = Function_View);
      Last.Result_Def := Definition.Index;
   end Set_Result_Type;

   ----------
   -- Step --
   ----------

   procedure Step
     (Self   : Region_Immediate_Visible_Iterator'Class;
      Cursor : in out View_Cursor)
   is
      use type Program.Symbols.Symbol;

      Value : Program.Visibility.Region renames
        Self.Context.Data (Cursor.Region);
   begin
      for Index in Cursor.Entity .. Value.Entities.Last_Index loop
         if Value.Entities (Index).Symbol = Self.Symbol then
            Cursor := (Cursor.Region,
                       Index,
                       1,
                       Get_View (Self.Context, (Cursor.Region, Index)));

            return;
         end if;
      end loop;

      Cursor := (Cursor.Region, Entity => 0, others => <>);
   end Step;

   -----------------
   -- Step_Region --
   -----------------

   procedure Step_Region
     (Self   : Context_Immediate_Visible_Iterator'Class;
      Cursor : in out View_Cursor)
   is
      Next : Region_Identifier'Base := Cursor.Region;
   begin
      loop
         Self.Step (Cursor);

         if Has_Element (Cursor) then
            return;
         end if;

         Next := Self.Context.Data (Next).Enclosing;

         exit when Next = 0;

         Cursor.Region := Next;
         Cursor.Entity := 1;
      end loop;

      Cursor := (Self.Region, Entity => 0, others => <>);
   end Step_Region;

   --------------
   -- Step_Use --
   --------------

   procedure Step_Use
     (Self   : Use_Visible_Iterator'Class;
      Cursor : in out View_Cursor)
   is
      Next : Region_Identifier'Base := Cursor.Region;
   begin
      loop  --  Over each nested region
         declare
            Top : Region renames Self.Context.Data (Next);
         begin
            if Cursor.Use_Id <= Top.Uses.Last_Index then  --  have use_cl
               Cursor.Region := Top.Uses (Cursor.Use_Id);  --  rewrite reg
               Self.Step (Cursor);
            else
               Cursor.Entity := 0;  --  clear cursor
            end if;

            if Has_Element (Cursor) then
               Cursor.Region := Next;  --  restore region

               return;
            elsif Cursor.Use_Id >= Top.Uses.Last_Index then
               Next := Self.Context.Data (Next).Enclosing;

               exit when Next = 0;

               Cursor.Use_Id := 1;
            else
               Cursor.Use_Id := Cursor.Use_Id + 1;
            end if;

            Cursor.Entity := 1;
         end;
      end loop;

      Cursor := (Self.Region, Entity => 0, others => <>);
   end Step_Use;
   ------------------
   -- Subtype_Mark --
   ------------------

   function Subtype_Mark (Self : View) return View is
      Value : Entity renames
        Self.Env.Data (Self.Index.Region).Entities (Self.Index.Entity_Id);
   begin
      case Value.Kind is
         when Subtype_View =>
            return Get_View (Self.Env, Value.Subtype_Mark);
         when Parameter_View =>
            return Get_View (Self.Env, Value.Param_Def);
         when others =>
            raise Constraint_Error;
      end case;
   end Subtype_Mark;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (List : View_Array) return Entity_References.Vector is
   begin
      return Result : Entity_References.Vector do
         Result.Reserve_Capacity (List'Length);

         for View of List loop
            Result.Append (View.Index);
         end loop;
      end return;
   end To_Vector;

   -----------------
   -- Use_Visible --
   -----------------

   function Use_Visible
     (Self   : Context'Class;
      Symbol : Program.Visibility.Symbol) return View_Iterator is
   begin
      return Use_Visible_Iterator'
        (Context => Self'Unchecked_Access,
         Region  => Self.Top,
         Symbol  => Symbol);
   end Use_Visible;

end Program.Visibility;
