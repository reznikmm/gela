with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.Text_Codecs;

with Gela.Errors.Put_Lines;
with Gela.Mutables.Compilations;
with Gela.Mutables.To_XML;
with Gela.Lexical.Handler;
with Gela.Types;

with Gela.Grammars.Reader;
with Gela.Grammars.Conflicts;
with Gela.Grammars_Convertors;
with Gela.Grammars.Constructors;
with Gela.Grammars.LR_Tables;
with Gela.Grammars.LR.LALR;

procedure Lexer_Test is

   function Read_File
     (File_Name : String)
     return League.Strings.Universal_String;

   G     : constant Gela.Grammars.Grammar :=
     Gela.Grammars.Reader.Read ("ada-ast.ag");
   Resolver : Gela.Grammars.Conflicts.Resolver;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (File_Name : String)
     return League.Strings.Universal_String
   is
      Decoder : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec_For_Application_Locale;

      Size : constant Ada.Directories.File_Size :=
        Ada.Directories.Size (File_Name);

      Length : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Count (Size);

      File   : Ada.Streams.Stream_IO.File_Type;
      Data   : Ada.Streams.Stream_Element_Array (1 .. Length);
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, File_Name);
      Ada.Streams.Stream_IO.Read (File, Data, Last);
      Ada.Streams.Stream_IO.Close (File);

      return Decoder.Decode (Data (1 .. Last));
   end Read_File;

   Name    : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("aaa");
   Text    : constant League.Strings.Universal_String := Read_File ("aaa");

   Ada_AG : constant Gela.Grammars.Grammar :=
     Gela.Grammars.Reader.Read ("ada.ag");
   Ada_Plain : constant Gela.Grammars.Grammar_Access :=
     new Gela.Grammars.Grammar'
       (Gela.Grammars_Convertors.Convert (Ada_AG, Left => False));
   AG : constant Gela.Grammars.Grammar :=
     Gela.Grammars.Constructors.To_Augmented (Ada_Plain.all);
   Table : constant Gela.Grammars.LR_Tables.Table_Access :=
     new Gela.Grammars.LR_Tables.Table'
       (Gela.Grammars.LR.LALR.Build (AG, Right_Nulled => False));

   Comp    : constant Gela.Mutables.Mutable_Compilation_Access :=
     Gela.Mutables.Compilations.Create
       (Name, Text, new Gela.Errors.Put_Lines.Handler, Ada_Plain, Table);

   use type Gela.Types.Payload;
begin
   Resolver.Resolve (AG, Table.all);
   Gela.Lexical.Handler.Initialize;
   Comp.Start;

   if Comp.Root.Object /= null then
      Ada.Wide_Wide_Text_IO.Put_Line
        (Gela.Mutables.To_XML.Compilation (Comp, G).To_Wide_Wide_String);
   end if;
end Lexer_Test;
