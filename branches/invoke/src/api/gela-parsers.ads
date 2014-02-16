--  This package provides Parser and Parser_Input interfaces and methods.

with Gela.Lexical_Types;
with Gela.Element_Factories;
with Gela.Elements.Compilations;

package Gela.Parsers is
   pragma Preelaborate;

   type Parser_Input is limited interface;
   --  Source of syntax analysis
   type Parser_Input_Access is access all Parser_Input'Class;
   for Parser_Input_Access'Storage_Size use 0;

   not overriding procedure Next_Token
     (Self  : in out Parser_Input;
      Token : out Gela.Lexical_Types.Token_Kind;
      Index : out Gela.Lexical_Types.Token_Index) is abstract;
   --  Provide next token tosyntax analysis

   type Parser is limited interface;
   --  Type to run syntax analysis
   type Parser_Access is access all Parser'Class;
   for Parser_Access'Storage_Size use 0;

   not overriding procedure Parse
     (Self       : in out Parser;  --  TODO: Drop 'in out'
      Input      : not null access Parser_Input'Class;
      Factory    : not null Gela.Element_Factories.Element_Factory_Access;
      Root       : out Gela.Elements.Compilations.Compilation_Access;
      Last_Token : out Gela.Lexical_Types.Token_Index) is abstract;
   --  Run syntax analysis of given Input. Use Factory to create AST elements.
   --  Return Root of AST ans last token index

end Gela.Parsers;
