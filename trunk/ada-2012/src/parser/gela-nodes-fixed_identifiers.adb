with Gela.Element_Factories;
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
              with Prefix => null);
   end Create;

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
               Factory : Gela.Element_Factories.Element_Factory_Access;
               Identifier : access Gela.Elements.Identifiers.Identifier'Class;
               Set : aliased Gela.Property_Resets.Property_Reset;
               Visiter : Gela.Property_Setters.Visiter (Set'Access);
            begin
               Set.Defining_Name := Self.Defining_Name;
               Set.Down := Self.Down;
               Set.Errors := Self.Errors;
               Set.Up_Set := Self.Up;
               Set.Env_In := Self.Env_In;
               Set.Full_Name := Self.Full_Name;
               Set.Static_Value := Self.Static_Value;

               Factory := Self.Enclosing_Compilation.Factory;
               Identifier := Factory.Identifier (Self.Identifier_Token);
               Identifier.Visit (Visiter);
               Self.Prefix := Identifier;
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
