
with League.Application;
with League.Strings;
with League.String_Vectors;

with Gela.Contexts;
with Gela.Context_Factories;
with Gela.Elements.Compilations;

procedure Parser_Test is

   use type League.Hash_Type;
   use type League.Strings.Universal_String;
   use type Gela.Elements.Compilations.Compilation_Access;

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
   Args.Append ("-I" & League.Application.Environment.Value (Env));

   Hash := League.Hash_Type'Wide_Wide_Value
     (League.Application.Arguments.Element
        (League.Application.Arguments.Length).To_Wide_Wide_String);

   Context := Gela.Context_Factories.Create_Context (Args);
end Parser_Test;
