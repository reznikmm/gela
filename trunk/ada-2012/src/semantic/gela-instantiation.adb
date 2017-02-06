with Ada.Containers.Hashed_Maps;

with Gela.Element_Cloners;
with Gela.Element_Visiters;
with Gela.Lexical_Types;
with Gela.Property_Setters;
with Gela.Property_Visiters;

with Gela.Elements.Basic_Declarative_Items;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Defining_Names;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Generic_Package_Declarations;
with Gela.Elements.Object_Declarations;
with Gela.Elements.Subtype_Declarations;
with Gela.Interpretations;
with Gela.Environments;

package body Gela.Instantiation is

   function Hash (Self : Gela.Elements.Defining_Names.Defining_Name_Access)
     return Ada.Containers.Hash_Type;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Gela.Elements.Defining_Names.Defining_Name_Access)
                  return Ada.Containers.Hash_Type is
   begin
      return Self.Hash;
   end Hash;

   package Name_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Elements.Defining_Names.Defining_Name_Access,
      Element_Type    => Gela.Elements.Defining_Names.Defining_Name_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Elements.Defining_Names."=",
      "="             => Gela.Elements.Defining_Names."=");

   package Cloners is
      type Cloner is new Gela.Element_Cloners.Cloner with record
         Map              : Name_Maps.Map;
         Instance_Name    : Gela.Elements.Defining_Names.Defining_Name_Access;
         Template         : access Gela.Elements.Element'Class;
      end record;

      overriding function Clone
        (Self    : in out Cloner;
         Element : access Gela.Elements.Element'Class)
         return Gela.Elements.Element_Access;

      overriding procedure Defining_Identifier
        (Self : in out Cloner;
         Node : not null Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access);

   end Cloners;

   package Setters is
      type Property_Setter
        (Source : Gela.Elements.Element_Access;
         Cloner : access Cloners.Cloner)
      is new Gela.Property_Setters.Property_Setter with record
         Corresponding_Generic_Element : Gela.Elements.Element_Access;
      end record;

      overriding procedure On_Index
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Token_Count);

      overriding procedure On_Env_In
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Env_Index);

      overriding procedure On_Env_Out
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Env_Index);

      overriding procedure On_Down
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.Interpretation_Index);

      overriding procedure On_Errors
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Error_Set_Index);

      overriding procedure On_Up
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.
           Interpretation_Tuple_Index);

      overriding procedure On_Up
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.
           Interpretation_Tuple_List_Index);

      overriding procedure On_Name_List
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol_List);

      overriding procedure On_Limited_With_List
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol_List);

      overriding procedure On_With_List
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol_List);

      overriding procedure On_Up
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.
           Interpretation_Set_Index);

      overriding procedure On_Static_Value
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Value_Index);

      overriding procedure On_Defining_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Defining_Names.
           Defining_Name_Access);

      overriding procedure On_Full_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol);

      overriding procedure On_Declarative_Region
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Env_Index);

      overriding procedure On_Type_Index
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Type_Index);

      overriding procedure On_Corresponding_Type
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Element_Access);

      overriding procedure On_Corresponding_Generic_Element
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Element_Access);

      overriding procedure On_Corresponding_View
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Element_Access);

      overriding procedure On_Expanded
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Element_Access);

   end Setters;

   package Update_Env is
      type Visiter is new Gela.Element_Visiters.Visiter with record
         Set  : Gela.Environments.Environment_Set_Access;
         Env  : Gela.Semantic_Types.Env_Index;
         Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      end record;

      overriding procedure Full_Type_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Full_Type_Declarations.
           Full_Type_Declaration_Access);

      overriding procedure Generic_Package_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Package_Declarations.
           Generic_Package_Declaration_Access);

      overriding procedure Object_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Object_Declarations.
           Object_Declaration_Access);

      overriding procedure Subtype_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Subtype_Declarations.
           Subtype_Declaration_Access);

   end Update_Env;

   -------------
   -- Setters --
   -------------

   package body Setters is

      overriding procedure On_Index
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Token_Count)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Index;

      overriding procedure On_Env_In
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Env_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Env_In;

      overriding procedure On_Env_Out
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Env_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Env_Out;

      overriding procedure On_Down
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.Interpretation_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Down;

      overriding procedure On_Errors
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Error_Set_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Errors;

      overriding procedure On_Up
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.
           Interpretation_Tuple_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Up;

      overriding procedure On_Up
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.
           Interpretation_Tuple_List_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Up;

      overriding procedure On_Name_List
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol_List)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Name_List;

      overriding procedure On_Limited_With_List
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol_List)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Limited_With_List;

      overriding procedure On_With_List
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol_List)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_With_List;

      overriding procedure On_Up
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.
           Interpretation_Set_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Up;

      overriding procedure On_Static_Value
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Value_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Static_Value;

      ------------------
      -- On_Full_Name --
      ------------------

      overriding procedure On_Full_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol)
      is
         pragma Unreferenced (Element);
         type Property_Visiter is new Gela.Property_Visiters.Property_Visiter
           with null record;

         overriding procedure On_Full_Name
           (Self    : in out Property_Visiter;
            Element : Gela.Elements.Element_Access;
            Value   : Gela.Lexical_Types.Symbol);

         overriding procedure On_Full_Name
           (Self    : in out Property_Visiter;
            Element : Gela.Elements.Element_Access;
            Value   : Gela.Lexical_Types.Symbol)
         is
            pragma Unreferenced (Self, Element);
         begin
            Setters.On_Full_Name.Value := Value;
         end On_Full_Name;

         Getter  : aliased Property_Visiter;
         Visiter : Gela.Property_Visiters.Visiter (Getter'Access);
      begin
         Self.Source.Visit (Visiter);
      end On_Full_Name;

      ----------------------
      -- On_Defining_Name --
      ----------------------

      overriding procedure On_Defining_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Defining_Names.Defining_Name_Access)
      is
         pragma Unreferenced (Element);
         type Property_Visiter is new Gela.Property_Visiters.Property_Visiter
           with null record;

         overriding procedure On_Defining_Name
           (Self    : in out Property_Visiter;
            Element : Gela.Elements.Element_Access;
            Value   : Gela.Elements.Defining_Names.Defining_Name_Access);

         overriding procedure On_Defining_Name
           (Self    : in out Property_Visiter;
            Element : Gela.Elements.Element_Access;
            Value   : Gela.Elements.Defining_Names.Defining_Name_Access)
         is
            pragma Unreferenced (Self, Element);
         begin
            Setters.On_Defining_Name.Value := Value;
         end On_Defining_Name;

         Getter  : aliased Property_Visiter;
         Visiter : Gela.Property_Visiters.Visiter (Getter'Access);
         Cursor  : Name_Maps.Cursor;
      begin
         Self.Source.Visit (Visiter);

         if Value.Assigned then
            Cursor := Self.Cloner.Map.Find (Value);

            if Name_Maps.Has_Element (Cursor) then
               Value := Name_Maps.Element (Cursor);
            end if;
         end if;
      end On_Defining_Name;

      overriding procedure On_Declarative_Region
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Env_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Declarative_Region;

      overriding procedure On_Type_Index
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Semantic_Types.Type_Index)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := 0;
      end On_Type_Index;

      overriding procedure On_Corresponding_Type
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Element_Access)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := null;
      end On_Corresponding_Type;

      overriding procedure On_Corresponding_Generic_Element
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Element_Access)
      is
         pragma Unreferenced (Element);
      begin
         Value := Self.Corresponding_Generic_Element;
      end On_Corresponding_Generic_Element;

      overriding procedure On_Corresponding_View
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Element_Access)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := null;
      end On_Corresponding_View;

      overriding procedure On_Expanded
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Element_Access)
      is
         pragma Unreferenced (Self, Element);
      begin
         Value := null;
      end On_Expanded;

   end Setters;


   -------------
   -- Cloners --
   -------------

   package body Cloners is

      -----------
      -- Clone --
      -----------

      overriding function Clone
        (Self    : in out Cloner;
         Element : access Gela.Elements.Element'Class)
         return Gela.Elements.Element_Access
      is
         Result  : Gela.Elements.Element_Access;
         Setter  : aliased Setters.Property_Setter
           (Element, Self'Unchecked_Access);
         Visiter : Gela.Property_Setters.Visiter (Setter'Access);
      begin
         if Element.Assigned then
            Setter.Corresponding_Generic_Element := Element;
            Result := Gela.Element_Cloners.Cloner (Self).Clone (Element);
            Result.Set_Part_Of_Instance;
            Result.Visit (Visiter);
         end if;

         return Result;
      end Clone;

      -------------------------
      -- Defining_Identifier --
      -------------------------

      overriding procedure Defining_Identifier
        (Self : in out Cloner;
         Node : not null Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access)
      is
         Source : constant Gela.Elements.Defining_Names.Defining_Name_Access :=
           Gela.Elements.Defining_Names.Defining_Name_Access (Node);
         Result : Gela.Elements.Defining_Names.Defining_Name_Access;
      begin
         Gela.Element_Cloners.Cloner (Self).Defining_Identifier (Node);
         Result :=
           Gela.Elements.Defining_Names.Defining_Name_Access (Self.Result);

         Self.Map.Insert (Source, Result);
      end Defining_Identifier;

   end Cloners;

   ----------------
   -- Update_Env --
   ----------------

   package body Update_Env is

      overriding procedure Full_Type_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Full_Type_Declarations.
           Full_Type_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Names.Defining_Name_Access :=
           Gela.Elements.Defining_Names.Defining_Name_Access (Node.Names);
      begin
         Self.Env := Self.Set.Add_Defining_Name
           (Self.Env, Name.Full_Name, Name);
      end Full_Type_Declaration;

      overriding procedure Generic_Package_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Generic_Package_Declarations.
           Generic_Package_Declaration_Access)
      is
         Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      begin
         if Self.Name.Assigned then
            Name := Self.Name;
            Self.Name := null;
         else
            Name :=
              Gela.Elements.Defining_Names.Defining_Name_Access (Node.Names);
         end if;

         Self.Env := Self.Set.Add_Defining_Name
           (Self.Env, Name.Full_Name, Name);

         Self.Env := Self.Set.Enter_Declarative_Region (Self.Env, Name);

         declare
            Item   : Gela.Elements.Basic_Declarative_Items
              .Basic_Declarative_Item_Access;
            Cursor : Gela.Elements.Basic_Declarative_Items
              .Basic_Declarative_Item_Sequence_Cursor :=
                Node.Visible_Part_Declarative_Items.First;
         begin
            while Cursor.Has_Element loop
               Item := Cursor.Element;
               Item.Visit (Self);
               Cursor.Next;
            end loop;
         end;

         Self.Env := Self.Set.Leave_Declarative_Region (Self.Env);
      end Generic_Package_Declaration;

      overriding procedure Object_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Object_Declarations.
           Object_Declaration_Access)
      is
         Item   : Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access;
         Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
         Cursor : Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Sequence_Cursor := Node.Names.First;
      begin
         while Cursor.Has_Element loop
            Item := Cursor.Element;
            Name := Gela.Elements.Defining_Names.Defining_Name_Access (Item);
            Self.Env := Self.Set.Add_Defining_Name
              (Self.Env, Name.Full_Name, Name);
            Cursor.Next;
         end loop;
      end Object_Declaration;

      overriding procedure Subtype_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Subtype_Declarations.
           Subtype_Declaration_Access)
      is
         Name : constant Gela.Elements.Defining_Names.Defining_Name_Access :=
           Gela.Elements.Defining_Names.Defining_Name_Access (Node.Names);
      begin
         Self.Env := Self.Set.Add_Defining_Name
           (Self.Env, Name.Full_Name, Name);
      end Subtype_Declaration;

   end Update_Env;

   ------------
   -- Expand --
   ------------

   procedure Expand
     (Comp         : Gela.Compilations.Compilation_Access;
      Node         : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access;
      Expanded     : out Gela.Elements.Element_Access)
   is
      pragma Unreferenced (Comp);

      Defining_Name : constant Gela.Elements.Defining_Names
        .Defining_Name_Access := Node.Generic_Unit_Name.Defining_Name;

   begin
      Expanded := Node.Expanded;

      if not Expanded.Assigned and Defining_Name.Assigned then
         declare
            Cloner : Cloners.Cloner
              (Defining_Name.Enclosing_Compilation.Factory);
         begin
            Cloner.Template := Defining_Name.Enclosing_Element;
            Cloner.Instance_Name := Defining_Name;
            Expanded := Cloner.Clone (Cloner.Template);
            Node.Set_Expanded (Expanded);
         end;
      end if;
   end Expand;

   -----------------
   -- Environment --
   -----------------

   procedure Environment
     (Comp         : Gela.Compilations.Compilation_Access;
      Node         : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access;
      Env_In       : Gela.Semantic_Types.Env_Index;
      Env_Out      : out Gela.Semantic_Types.Env_Index)
   is
      Visiter  : Update_Env.Visiter;
      Expanded : Gela.Elements.Element_Access := Node.Expanded;
      Name     : constant Gela.Elements.Defining_Names.Defining_Name_Access :=
        Gela.Elements.Defining_Names.Defining_Name_Access (Node.Names);
   begin
      if not Expanded.Assigned then
         Expand (Comp, Node, Expanded);
         Node.Set_Expanded (Expanded);
      end if;

      if Expanded.Assigned then
         Visiter.Set := Comp.Context.Environment_Set;
         Visiter.Name := Name;
         Visiter.Env := Env_In;
         Expanded.Visit (Visiter);
         Env_Out := Visiter.Env;
      else
         Env_Out := Env_In;
      end if;
   end Environment;

end Gela.Instantiation;
