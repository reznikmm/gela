with Gela.Grammars;

package AG_Tools.Check_Ordered is

   type NT_Map is array
     (Gela.Grammars.Non_Terminal_Index range <>,
      Gela.Grammars.Non_Terminal_Index range <>) of Boolean;

   type NT_List is array
     (Gela.Grammars.Non_Terminal_Index range <>) of Boolean;

   procedure Check
     (G : Gela.Grammars.Grammar;
      Implement : NT_Map;
      Is_Concrete : NT_List);

end AG_Tools.Check_Ordered;
