with Anagram.Grammars;
with Anagram.Grammars.LR_Parsers;

package Program.Parsers.Data is
   pragma Preelaborate;

   procedure Next_Action
     (State : Anagram.Grammars.LR_Parsers.State_Index;
      Token : Anagram.Grammars.Terminal_Count;
      Value : out Anagram.Grammars.LR_Parsers.Action);

   function Go_To
     (State : Anagram.Grammars.LR_Parsers.State_Index;
      NT    : Anagram.Grammars.Non_Terminal_Index)
      return Anagram.Grammars.LR_Parsers.State_Index;

end Program.Parsers.Data;
