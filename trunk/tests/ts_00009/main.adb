--  Check LL(1) parser generator

with Ada.Text_IO;

with Gela.Grammars.Reader;
with Gela.Grammars.Attributed.Extended;
with Gela.Grammars_Recursive_Descent;
with Gela.Grammars_Convertors;

procedure Main is
   G : aliased Gela.Grammars.Attributed.Extended.Grammar :=
     Gela.Grammars.Reader.Read ("test.ag");

   Plain : aliased Gela.Grammars.Attributed.Grammar :=
     Gela.Grammars_Convertors.Convert (G'Access);

   Ok : Boolean;
begin
   Gela.Grammars_Recursive_Descent.Bbb (Plain'Access, Ok);
   Ada.Text_IO.Put_Line (Boolean'Image (Ok));
end Main;
