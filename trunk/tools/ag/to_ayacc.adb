with Ada.Command_Line;
with Gela.Grammars.AYACC;
with Gela.Grammars.Reader;
with Gela.Grammars_Convertors;

procedure To_AYACC is
   File  : constant String := Ada.Command_Line.Argument (1);
   G     : constant Gela.Grammars.Grammar := Gela.Grammars.Reader.Read (File);
   Plain : constant Gela.Grammars.Grammar :=
     Gela.Grammars_Convertors.Convert (G, Left => False);
begin
   Gela.Grammars.AYACC.Write (Plain);
end To_AYACC;
