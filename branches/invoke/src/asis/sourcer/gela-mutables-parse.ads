with Gela.Nodes;
with Gela.Grammars.LR_Parsers;
with Gela.Mutables.Lexers;
with Gela.Mutables.Parser_Data;
with Gela.Mutables.Parsers;
with Gela.Mutables.On_Reduce;

procedure Gela.Mutables.Parse is new Gela.Grammars.LR_Parsers.Parse
  (Node        => Gela.Nodes.Element,
   Node_Array  => Gela.Mutables.Parsers.Node_Array,
   Lexer       => Gela.Mutables.Lexers.Lexer,
   Parser      => Gela.Mutables.Parsers.Parser,
   Next_Token  => Gela.Mutables.Lexers.Next_Token,
   Next_Action => Gela.Mutables.Parser_Data.Next_Action,
   Go_To       => Gela.Mutables.Parser_Data.Go_To,
   On_Reduce   => Gela.Mutables.On_Reduce);

pragma Preelaborate (Gela.Mutables.Parse);
