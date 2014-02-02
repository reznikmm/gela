with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;

with Gela.Contexts;
with Gela.Context_Factories;
with Gela.Compilations;
with Gela.Lexers;
with Gela.Lexical_Handler;
with Gela.Plain_Compilations;
with Gela.LARL_Parsers;
with Gela.Lexical_Types;
with Gela.Element_Factories;
with Gela.Elements.Compilations;

with Gela.Node_Factories;
with Gela.Element_Visiters;
with Gela.Pass_List;
with Gela.Pass_1;
with Gela.Pass_2;

procedure Parser_Test is

   use type League.Hash_Type;
   use type Gela.Elements.Compilations.Compilation_Access;

   Hash    : League.Hash_Type;
   pragma Unreferenced (Hash);
   Path    : League.Strings.Universal_String;
   File    : Ada.Wide_Wide_Text_IO.File_Type;
   Input   : League.Strings.Universal_String;
   Context : constant Gela.Contexts.Context_Access :=
     Gela.Context_Factories.Create_Context (League.Application.Arguments);
   Lexer   : constant Gela.Lexers.Lexer_Access := Context.Lexer;
   Comp    : constant Gela.Plain_Compilations.Compilation_Access :=
     new Gela.Plain_Compilations.Compilation (Context);
   Parser  : Gela.LARL_Parsers.Parser (Context);
   Root    : Gela.Elements.Compilations.Compilation_Access;
   Last    : Gela.Lexical_Types.Token_Index;
   Factory  : constant Gela.Node_Factories.Element_Factory_Access :=
     new Gela.Node_Factories.Element_Factory
       (Gela.Compilations.Compilation_Access (Comp));
begin
   --  Command line: "-IDIR" "FILE" "HASH"
   Path := League.Application.Arguments.Element (1);
   Path.Slice (3, Path.Length);
   Path.Append ("/");
   Path.Append (League.Application.Arguments.Element (2));
   Hash := League.Hash_Type'Wide_Wide_Value
     (League.Application.Arguments.Element (3).To_Wide_Wide_String);
   Gela.Lexical_Handler.Initialize;
   Comp.Initialize;

   Ada.Wide_Wide_Text_IO.Open
     (File,
      Ada.Wide_Wide_Text_IO.In_File,
      Path.To_UTF_8_String,
      "WCEM=8");

   while not Ada.Wide_Wide_Text_IO.End_Of_File (File) loop
      Input.Append (Ada.Wide_Wide_Text_IO.Get_Line (File));
      Input.Append (Wide_Wide_Character'Val (10));
   end loop;

   Lexer.Scan (Input, Comp);

   Parser.Parse
     (Input      => Comp,
      Factory     => Gela.Element_Factories.Element_Factory_Access (Factory),
      Root       => Root,
      Last_Token => Last);

   if Root = null then
      raise Constraint_Error;
   end if;

   declare
      C : constant Gela.Compilations.Compilation_Access :=
        Gela.Compilations.Compilation_Access (Comp);
      PL : constant Gela.Pass_List.Visiter_Access :=
        new Gela.Pass_List.Visiter (C);
      P1 : constant Gela.Pass_1.Visiter_Access :=
        new Gela.Pass_1.Visiter (C, PL);
      P2 : constant Gela.Pass_2.Visiter_Access :=
        new Gela.Pass_2.Visiter (C, PL);
   begin
      PL.Parent := PL;
      PL.P1 := Gela.Element_Visiters.Visiter_Access (P1);
      PL.P2 := Gela.Element_Visiters.Visiter_Access (P2);
      Root.Visit (P1.all);
   end;
end Parser_Test;
