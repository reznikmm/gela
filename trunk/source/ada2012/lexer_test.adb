with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.Text_Codecs;

with Gela.Mutables.Compilations;
with Gela.Lexical.Handler;

procedure Lexer_Test is

   function Read_File
     (File_Name : String)
     return League.Strings.Universal_String;

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
   Comp    : constant Gela.Mutables.Mutable_Compilation_Access :=
     Gela.Mutables.Compilations.Create (Name, Text);
begin
   Gela.Lexical.Handler.Initialize;
   Comp.Start;

   Ada.Wide_Wide_Text_IO.New_Line;
   Ada.Wide_Wide_Text_IO.Put_Line
     (Gela.Lexical.Line_Count'Wide_Wide_Image (Comp.Last_Line));
end Lexer_Test;
