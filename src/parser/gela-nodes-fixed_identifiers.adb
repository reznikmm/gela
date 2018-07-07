with Gela.Element_Factories;
with Gela.Elements.Associations;
with Gela.Elements.Identifiers;
with Gela.Property_Resets;
with Gela.Property_Setters;

package body Gela.Nodes.Fixed_Identifiers is

   ---------------------------
   -- Chosen_Interpretation --
   ---------------------------

   overriding function Chosen_Interpretation
     (Self : Identifier)
      return Gela.Interpretations.Interpretation_Kinds
   is
   begin
      if Self.Prefix.Assigned then
         return Gela.Interpretations.Function_Call;
      else
         return Gela.Interpretations.Identifier;
      end if;
   end Chosen_Interpretation;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Comp             : Gela.Compilations.Compilation_Access;
      Identifier_Token : Gela.Lexical_Types.Token_Count)
      return Identifier is
   begin
      return (Gela.Nodes.Identifiers.Create (Comp, Identifier_Token)
              with Prefix => null, Parameters => null);
   end Create;

   ------------------------------
   -- Function_Call_Parameters --
   ------------------------------

   overriding function Function_Call_Parameters
     (Self : Identifier)
      return Gela.Elements.Association_Lists.Association_List_Access is
   begin
      return Self.Parameters;
   end Function_Call_Parameters;

   ------------------
   -- Nested_Items --
   ------------------

   overriding function Nested_Items
     (Self  : Identifier) return Gela.Elements.Nested_Array is
   begin
      if Self.Prefix.Assigned then
         return ((Gela.Elements.Nested_Element,
                 Gela.Elements.Element_Access (Self.Prefix)),
                 (Gela.Elements.Nested_Element,
                  Gela.Elements.Element_Access (Self.Parameters)));
      else
         return Gela.Nodes.Identifiers.Identifier (Self).Nested_Items;
      end if;
   end Nested_Items;

   ------------
   -- Prefix --
   ------------

   overriding function Prefix (Self : Identifier)
      return Gela.Elements.Prefixes.Prefix_Access is
   begin
      return Self.Prefix;
   end Prefix;

   -------------------------------
   -- Set_Chosen_Interpretation --
   -------------------------------

   overriding procedure Set_Chosen_Interpretation
     (Self    : in out Identifier;
      Value   : Gela.Interpretations.Interpretation_Kinds)
   is
   begin
      case Value is
         when Gela.Interpretations.Function_Call =>
            declare
               IM  : constant Gela.Interpretations.
                 Interpretation_Manager_Access :=
                   Self.Enclosing_Compilation.Context.Interpretation_Manager;
               Factory : Gela.Element_Factories.Element_Factory_Access;
               Identifier : access Gela.Elements.Identifiers.Identifier'Class;
               Set : aliased Gela.Property_Resets.Property_Reset;
               Visiter : Gela.Property_Setters.Visiter (Set'Access);
               Sequence : Gela.Elements.Associations.
                 Association_Sequence_Access;
            begin
               IM.Get_Down_Interpretation
                 (Value  => Self.Down,
                  Index  => 1,
                  Result => Set.Down);
               IM.Get_Defining_Name (Set.Down, Set.Defining_Name);
               Set.Errors := Self.Errors;
               Set.Up_Set := Self.Up;
               Set.Env_In := Self.Env_In;
               Set.Full_Name := Self.Full_Name;
               Set.Static_Value := Self.Static_Value;
               Set.Chosen_Interpretation := Gela.Interpretations.Identifier;

               Factory := Self.Enclosing_Compilation.Factory;
               Identifier := Factory.Identifier (Self.Identifier_Token);
               Identifier.Visit (Visiter);
               Self.Prefix := Identifier;
               Sequence := Factory.Association_Sequence;
               Self.Parameters := Factory.Association_List (0, Sequence, 0);

               Gela.Elements.Set_Enclosing.Element_Access (Self.Parameters)
                 .Set_Enclosing_Element (Self'Unchecked_Access);
               Gela.Elements.Set_Enclosing.Element_Access (Identifier)
                 .Set_Enclosing_Element (Self'Unchecked_Access);
            end;
         when Gela.Interpretations.Identifier =>
            Self.Prefix := null;
         when others =>
            raise Constraint_Error;
      end case;
   end Set_Chosen_Interpretation;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : access Identifier;
      Visiter : in out Gela.Element_Visiters.Visiter'Class)
   is
   begin
      if Self.Prefix.Assigned then
         Visiter.Function_Call (Self);
      else
         Visiter.Identifier (Self);
      end if;
   end Visit;

end Gela.Nodes.Fixed_Identifiers;
