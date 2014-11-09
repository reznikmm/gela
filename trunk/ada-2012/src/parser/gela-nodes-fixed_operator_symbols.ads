--  This package provides special version of Operator_Symbol element.
--  This element works as String_Literal or Operator_Symbol depending on
--  its state.


with Gela.Nodes.Operator_Symbols;
with Gela.Elements.String_Literals;
with Gela.Element_Visiters;
with Gela.Lexical_Types;

package Gela.Nodes.Fixed_Operator_Symbols is
   pragma Preelaborate;

   type Operator_Symbol is limited new
     Gela.Nodes.Operator_Symbols.Operator_Symbol
     and Gela.Elements.String_Literals.String_Literal with private;

   type Operator_Symbol_Access is
     access all Operator_Symbol;

private

   type Operator_Symbol is limited new
     Gela.Nodes.Operator_Symbols.Operator_Symbol
     and Gela.Elements.String_Literals.String_Literal with null record;

   overriding function String_Literal_Token
     (Self    : Operator_Symbol)
      return Gela.Lexical_Types.Token_Count;

   overriding procedure Visit
     (Self    : access Operator_Symbol;
      Visiter : in out Gela.Element_Visiters.Visiter'Class);

end Gela.Nodes.Fixed_Operator_Symbols;
