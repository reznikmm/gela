with Gela.Compilations;
with Gela.Elements.Operator_Symbols;
with Gela.Lexical_Types;
with Gela.Node_Factories;

package Gela.Fix_Node_Factories is
   pragma Preelaborate;

   type Element_Factory (Comp : Gela.Compilations.Compilation_Access) is
      limited new Gela.Node_Factories.Element_Factory (Comp) with null record;
   type Element_Factory_Access is access all Element_Factory'Class;

   overriding function Operator_Symbol
     (Self : in out Element_Factory;
      Operator_Symbol_Token : Gela.Lexical_Types.Token_Count)
      return Gela.Elements.Operator_Symbols.Operator_Symbol_Access;

end Gela.Fix_Node_Factories;
