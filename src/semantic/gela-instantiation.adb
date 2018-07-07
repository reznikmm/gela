with Ada.Containers.Hashed_Maps;

with Gela.Element_Cloners;
with Gela.Element_Visiters;
with Gela.Lexical_Types;
with Gela.Property_Getters;
with Gela.Property_Resets;
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
      type Property_Getter is limited new Gela.Property_Getters.Getter with
      record
         Visiter : Gela.Property_Visiters.Visiter
           (Property_Getter'Unchecked_Access);
      end record;

      type Cloner is new Gela.Element_Cloners.Cloner with record
         Map              : Name_Maps.Map;
         Instance_Name    : Gela.Elements.Defining_Names.Defining_Name_Access;
         Template         : access Gela.Elements.Element'Class;
         Getter           : Property_Getter;
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
      is new Gela.Property_Resets.Property_Reset with null record;

      overriding procedure On_Defining_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Defining_Names.
           Defining_Name_Access);

      overriding procedure On_Full_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol);

      overriding procedure On_Chosen_Interpretation
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.Interpretation_Kinds);

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

      ------------------
      -- On_Full_Name --
      ------------------

      overriding procedure On_Full_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Lexical_Types.Symbol)
      is
         pragma Unreferenced (Element);
      begin
         Self.Source.Visit (Self.Cloner.Getter.Visiter);
         Value := Self.Cloner.Getter.Full_Name;
         Self.Cloner.Getter.Full_Name := Self.Full_Name;
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

         Cursor  : Name_Maps.Cursor;
      begin
         Self.Source.Visit (Self.Cloner.Getter.Visiter);
         Value := Self.Cloner.Getter.Defining_Name;
         Self.Cloner.Getter.Defining_Name := null;

         if Value.Assigned then
            Cursor := Self.Cloner.Map.Find (Value);

            if Name_Maps.Has_Element (Cursor) then
               Value := Name_Maps.Element (Cursor);
            end if;
         end if;
      end On_Defining_Name;

      overriding procedure On_Chosen_Interpretation
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Interpretations.Interpretation_Kinds)
      is
         pragma Unreferenced (Element);
      begin
         Self.Source.Visit (Self.Cloner.Getter.Visiter);
         Value := Self.Cloner.Getter.Chosen_Interpretation;
         Self.Cloner.Getter.Chosen_Interpretation :=
           Self.Chosen_Interpretation;
      end On_Chosen_Interpretation;
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
