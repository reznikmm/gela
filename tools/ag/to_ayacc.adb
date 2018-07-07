with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;
with Anagram.Grammars.AYACC;
with Anagram.Grammars.Reader;
with Anagram.Grammars_Convertors;
--  with Anagram.Grammars_Debug;

with Anagram.Grammars.Tools;

procedure To_AYACC is
   File  : constant String := Ada.Command_Line.Argument (1);
   G     : constant Anagram.Grammars.Grammar :=
     Anagram.Grammars.Reader.Read (File);
   Plain : constant Anagram.Grammars.Grammar :=
     Anagram.Grammars_Convertors.Convert (G, Left => False);
begin
--   Anagram.Grammars_Debug.Print (G);
   Anagram.Grammars.AYACC.Write (Plain);

   if Ada.Command_Line.Argument_Count = 1 then
      return;
   end if;

   declare
      use Anagram.Grammars.Tools;
      First  : Terminal_Set_Per_Non_Terminal
        (Plain.Last_Terminal, Plain.Last_Non_Terminal);
      Follow : Terminal_Set_Per_Non_Terminal
        (Plain.Last_Terminal, Plain.Last_Non_Terminal);
   begin
      Anagram.Grammars.Tools.Get_First (Plain, First);
      Anagram.Grammars.Tools.Get_Follow (Plain, First, Follow);

      for NT of Plain.Non_Terminal loop
         Ada.Wide_Wide_Text_IO.Put (NT.Name.To_Wide_Wide_String);
         Ada.Wide_Wide_Text_IO.Put (": ");
         for T in Plain.Terminal'Range loop
            if Follow.Map (NT.Index, T) then
               Ada.Wide_Wide_Text_IO.Put
                 (Plain.Terminal (T).Image.To_Wide_Wide_String);
               Ada.Wide_Wide_Text_IO.Put (" ");
            end if;
         end loop;
         Ada.Wide_Wide_Text_IO.New_Line;
      end loop;
   end;
end To_AYACC;
