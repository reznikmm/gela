--  This package provides special Element_Factory to create elements from
--  Gela.Nodes.Fixed_* packages.

with Gela.Compilations;
with Gela.Elements.Expressions;
with Gela.Elements.Names;
with Gela.Elements.Operator_Symbols;
with Gela.Elements.Procedure_Call_Statements;
with Gela.Elements.Qualified_Expressions;
with Gela.Elements.Subtype_Marks;
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

   overriding function Procedure_Call_Statement
     (Self : in out Element_Factory;
      Function_Call : Gela.Elements.Names.Name_Access;
      Semicolon_Token : Gela.Lexical_Types.Token_Count)
      return Gela.Elements.Procedure_Call_Statements.
        Procedure_Call_Statement_Access;

   overriding function Qualified_Expression
     (Self : in out Element_Factory;
      Converted_Or_Qualified_Subtype_Mark : Gela.Elements.Subtype_Marks.
        Subtype_Mark_Access;
      Apostrophe_Token : Gela.Lexical_Types.Token_Count;
      Left_Parenthesis_Token : Gela.Lexical_Types.Token_Count;
      Converted_Or_Qualified_Expression : Gela.Elements.Expressions.
        Expression_Access;
      Right_Parenthesis_Token : Gela.Lexical_Types.Token_Count)
      return Gela.Elements.Qualified_Expressions.Qualified_Expression_Access;

end Gela.Fix_Node_Factories;
