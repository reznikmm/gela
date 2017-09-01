--  This package provides special version of Identifier element.
--  This element works as Identifier or Function_Call depending on
--  its state.


with Gela.Nodes.Identifiers;
with Gela.Element_Visiters;
with Gela.Elements.Association_Lists;
with Gela.Elements.Function_Calls;
with Gela.Elements.Prefixes;
with Gela.Interpretations;

package Gela.Nodes.Fixed_Identifiers is
   pragma Preelaborate;

   type Identifier is limited new
     Gela.Nodes.Identifiers.Identifier
     and Gela.Elements.Function_Calls.Function_Call with private;

   type Identifier_Access is access all Identifier;

   overriding function Create
     (Comp             : Gela.Compilations.Compilation_Access;
      Identifier_Token : Gela.Lexical_Types.Token_Count)
      return Identifier;

private

   type Identifier is limited new
     Gela.Nodes.Identifiers.Identifier
     and Gela.Elements.Function_Calls.Function_Call with
   record
      Prefix : Gela.Elements.Prefixes.Prefix_Access;
   end record;

   overriding function Prefix
     (Self : Identifier)
      return Gela.Elements.Prefixes.Prefix_Access;

   overriding function Function_Call_Parameters
     (Self : Identifier)
      return Gela.Elements.Association_Lists.Association_List_Access is
       (null);

   overriding function Chosen_Interpretation
     (Self : Identifier) return Gela.Interpretations.Interpretation_Kinds;

   overriding procedure Set_Chosen_Interpretation
     (Self    : in out Identifier;
      Value   : Gela.Interpretations.Interpretation_Kinds);

   overriding procedure Visit
     (Self    : access Identifier;
      Visiter : in out Gela.Element_Visiters.Visiter'Class);

end Gela.Nodes.Fixed_Identifiers;
