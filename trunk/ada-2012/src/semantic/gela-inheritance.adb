with Ada.Containers.Hashed_Maps;
with Gela.Element_Cloners;
with Gela.Element_Visiters;
with Gela.Elements.Component_Declarations;
with Gela.Elements.Component_Items;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Defining_Names;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Identifiers;
with Gela.Elements.Record_Definitions;
with Gela.Elements.Record_Type_Definitions;
with Gela.Elements.Selected_Components;
with Gela.Elements.Subtype_Indications;
with Gela.Elements.Subtype_Marks;
with Gela.Environments;
with Gela.Property_Getters;
with Gela.Property_Resets;
with Gela.Property_Setters;
with Gela.Property_Visiters;
with Gela.Lexical_Types;
with Gela.Interpretations;
--  with Gela.Type_Managers;

package body Gela.Inheritance is

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
         Map    : Name_Maps.Map;
         Getter : Property_Getter;
      end record;

      overriding function Clone
        (Self    : in out Cloner;
         Element : access Gela.Elements.Element'Class)
         return Gela.Elements.Element_Access;

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

   package body Cloners is

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
            Result := Gela.Element_Cloners.Cloner (Self).Clone (Element);
            Result.Set_Part_Of_Inherited;
            Result.Visit (Visiter);
         end if;

         return Result;
      end Clone;

   end Cloners;

   package body Setters is

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

      overriding procedure On_Defining_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : out Gela.Elements.Defining_Names.
           Defining_Name_Access)
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

   end Setters;

   package Update_Env is
      type Visiter is new Gela.Element_Visiters.Visiter with record
         Set  : Gela.Environments.Environment_Set_Access;
         Env  : Gela.Semantic_Types.Env_Index;
      end record;

      overriding procedure Component_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Component_Declarations.
           Component_Declaration_Access);

      overriding procedure Defining_Identifier
        (Self : in out Visiter;
         Node : not null Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access);
   end Update_Env;

   package body Update_Env is

      overriding procedure Component_Declaration
        (Self : in out Visiter;
         Node : not null Gela.Elements.Component_Declarations.
           Component_Declaration_Access)
      is
         Cursor : Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Sequence_Cursor := Node.Names.First;
      begin
         while Cursor.Has_Element loop
            Cursor.Element.Visit (Self);
            Cursor.Next;
         end loop;
      end Component_Declaration;

      overriding procedure Defining_Identifier
        (Self : in out Visiter;
         Node : not null Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access) is
      begin
         Self.Env := Self.Set.Add_Defining_Name
           (Self.Env, Node.Full_Name, Node.all'Access);
      end Defining_Identifier;

   end Update_Env;

   -----------------------
   -- Copy_Declarations --
   -----------------------

   procedure Copy_Declarations
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      Node         : not null Gela.Elements.Derived_Type_Definitions.
        Derived_Type_Definition_Access;
      Inherited    : out Gela.Elements.Element_Sequence_Access)
   is
      pragma Unreferenced (Env);
      --  Put in the map (parent type name -> derived type name) to
      --  replace possible T'Unchecked_Access.
      --  If there is known discriminants put in the map
      --  (old discriminant -> new discriminant) for each discriminant
      --  according to constraint in subtype_indication.
      --  Else clone known discriminants from parent if any.
      --  Then clone components, entries and protected subprograms.

      Parent : constant
        Gela.Elements.Subtype_Indications.Subtype_Indication_Access :=
          Node.Parent_Subtype_Indication;
      Subtype_Mark : constant
        Gela.Elements.Subtype_Marks.Subtype_Mark_Access :=
          Parent.Subtype_Mark;

      package Each is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Parent_Name : Gela.Elements.Defining_Names.Defining_Name_Access;
            Parent_Declaration : Gela.Elements.Element_Access;
            Components : Gela.Elements.Component_Items.
              Component_Item_Sequence_Access;
         end record;

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.
              Identifier_Access);

         overriding procedure Selected_Component
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Components.
              Selected_Component_Access);

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access);

         overriding procedure Record_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Definitions.
              Record_Definition_Access);

         overriding procedure Record_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Type_Definitions.
              Record_Type_Definition_Access);

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access);

      end Each;

      package body Each is

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access)
         is
            Cloner : Cloners.Cloner (Node.Enclosing_Compilation.Factory);
            Item : Gela.Elements.Component_Items.Component_Item_Access;
         begin
            Node.Visit (Cloner);
            Item := Gela.Elements.Component_Items.Component_Item_Access
              (Cloner.Result);
            Self.Components.Append (Item);
         end Component_Declaration;

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access) is
         begin
            Node.Type_Declaration_View.Visit (Self);
         end Full_Type_Declaration;

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.
              Identifier_Access) is
         begin
            Self.Parent_Name := Node.Defining_Name;

            if Self.Parent_Name.Assigned then
               Self.Parent_Declaration := Self.Parent_Name.Enclosing_Element;
               Self.Parent_Declaration.Visit (Self);
            end if;
         end Identifier;

         overriding procedure Record_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Definitions.
              Record_Definition_Access) is
         begin
            Self.Components := Comp.Factory.Component_Item_Sequence;

            for J in Node.Record_Components.Each_Element loop
               J.Element.Visit (Self);
            end loop;
         end Record_Definition;

         overriding procedure Record_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Type_Definitions.
              Record_Type_Definition_Access) is
         begin
            Node.Record_Definition.Visit (Self);
         end Record_Type_Definition;

         overriding procedure Selected_Component
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Components.
              Selected_Component_Access) is
         begin
            Node.Selector.Visit (Self);
         end Selected_Component;

      end Each;

      V : Each.Visiter;
   begin
      Inherited := Node.Inh_List;

      if Inherited not in null then
         return;
      end if;
      Subtype_Mark.Visit (V);
      Inherited := Gela.Elements.Element_Sequence_Access (V.Components);
   end Copy_Declarations;

   -----------------
   -- Environment --
   -----------------

   procedure Environment
     (Comp         : Gela.Compilations.Compilation_Access;
      Node         : not null Gela.Elements.Derived_Type_Definitions.
        Derived_Type_Definition_Access;
      Env_In       : Gela.Semantic_Types.Env_Index;
      Env_Out      : out Gela.Semantic_Types.Env_Index)
   is
      Inherited    : Gela.Elements.Element_Sequence_Access := Node.Inh_List;
   begin
      if Inherited in null then
         Copy_Declarations (Comp, Env_In, Node, Inherited);
         Node.Set_Inh_List (Inherited);
      end if;

      if Inherited not in null then
         declare
            Visiter  : Update_Env.Visiter;
         begin
            Visiter.Set := Comp.Context.Environment_Set;
            Visiter.Env := Env_In;

            for Cursor in Inherited.Each_Element loop
               Cursor.Element.Visit (Visiter);
            end loop;

            Env_Out := Visiter.Env;
         end;
      else
         Env_Out := Env_In;
      end if;
   end Environment;

end Gela.Inheritance;
