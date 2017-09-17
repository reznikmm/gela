
with League.Application;
with League.Strings;
with League.String_Vectors;

with Gela.Contexts;
with Gela.Context_Factories;

procedure Parser_Test is

   Hash    : League.Hash_Type;
   pragma Unreferenced (Hash);
   Env     : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("GELA_INCLUDE_PATH");
   Args    : League.String_Vectors.Universal_String_Vector;
   Context : Gela.Contexts.Context_Access;

   pragma Unreferenced (Context);
begin
   --  Command line: "-IDIR1" "-IDIR2" "FILE" "HASH"
   for J in 1 .. League.Application.Arguments.Length - 1 loop
      Args.Append (League.Application.Arguments.Element (J));
   end loop;

   Hash := League.Hash_Type'Wide_Wide_Value
     (League.Application.Arguments.Element
        (League.Application.Arguments.Length).To_Wide_Wide_String);

   Context := Gela.Context_Factories.Create_Context
     (Args, League.Application.Environment.Value (Env));
end Parser_Test;
