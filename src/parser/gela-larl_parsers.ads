with Gela.Contexts;
with Gela.Element_Factories;
with Gela.Elements.Association_Lists;
with Gela.Elements.Compilations;
with Gela.Elements.Defining_Program_Unit_Names;
with Gela.Elements.Expressions;
with Gela.Elements.Function_Calls;
with Gela.Elements.Scalar_Constraints;
with Gela.Elements.Selected_Identifiers;
with Gela.Elements.Subtype_Indications;
with Gela.Lexical_Types;
with Gela.Parsers;

package Gela.LARL_Parsers is
   pragma Preelaborate;

   type Parser (Context : access Gela.Contexts.Context'Class) is
     limited new Gela.Parsers.Parser with private;

   overriding procedure Parse
     (Self       : in out Parser;
      Input      : not null access Gela.Parsers.Parser_Input'Class;
      Factory    : not null Gela.Element_Factories.Element_Factory_Access;
      Root       : out Gela.Elements.Compilations.Compilation_Access;
      Last_Token : out Gela.Lexical_Types.Token_Index);

private

   type Parser (Context : access Gela.Contexts.Context'Class) is
     limited new Gela.Parsers.Parser with null record;

   type Parser_Context is tagged record
      Factory : not null Gela.Element_Factories.Element_Factory_Access;
   end record;

   function Infix_Call
     (Self   : access Parser_Context;
      Prefix : Gela.Lexical_Types.Token_Count;
      Left   : Gela.Elements.Expressions.Expression_Access;
      Right  : Gela.Elements.Expressions.Expression_Access := null)
      return Gela.Elements.Function_Calls.Function_Call_Access;

   function To_Defining_Program_Unit_Name
     (Self  : access Parser_Context;
      Value : Gela.Elements.Selected_Identifiers.Selected_Identifier_Access)
      return Gela.Elements.Defining_Program_Unit_Names.
               Defining_Program_Unit_Name_Access;

   function To_Subtype_Indication
     (Self       : access Parser_Context;
      Not_Token  : Gela.Lexical_Types.Token_Count;
      Null_Token : Gela.Lexical_Types.Token_Count;
      Mark       : Gela.Elements.Element_Access;
      Constraint : Gela.Elements.Scalar_Constraints.Scalar_Constraint_Access)
      return Gela.Elements.Subtype_Indications.Subtype_Indication_Access;

   function To_Aggregate_Or_Expression
     (Self  : access Parser_Context;
      Value : Gela.Elements.Association_Lists.Association_List_Access)
      return Gela.Elements.Expressions.Expression_Access;
   --  If Value is (X) return Parenthesized_Expression else
   --  return Record_Aggregate

end Gela.LARL_Parsers;