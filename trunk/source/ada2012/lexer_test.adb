with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.Text_Codecs;

with Gela.Lexical.Handler;
with Gela.Lexical.Scanners;
with Gela.Lexical.Tokens;

with String_Sources;

with Gela.Compilations.Mutables.Symbol_Tables;
with Gela.Elements;

procedure Lexer_Test is

   function Read_File
     (File_Name : String)
     return League.Strings.Universal_String;

   procedure Test_RB_Tree;

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

   ------------------
   -- Test_RB_Tree --
   ------------------

   procedure Test_RB_Tree is
      use Gela.Compilations.Mutables.Symbol_Tables;
      T : Symbol_Table;
      D : Gela.Elements.Payload := 0;
   begin
      T.Append (D, 10, (null, 10));
      T.Append (D, 20, (null, 20));
      T.Append (D, 5, (null, 5));

      declare
         X  : Gela.Elements.Element;
         T2 : aliased Symbol_Table;
      begin
         X := (T2'Unchecked_Access, 0);
         T.Copy (D, X);
         T2.Append (D, 15, (null, 15));
         T2.Append (D, 17, (null, 17));
         D := X.Payload;

         Ada.Wide_Wide_Text_IO.Put_Line
           (Gela.Elements.Payload'Wide_Wide_Image
              (T2.Find (D, 15).Payload));
      end;
   end Test_RB_Tree;

   Text    : constant League.Strings.Universal_String := Read_File ("aaa");
   Scanner : aliased Gela.Lexical.Scanners.Scanner;
   Source  : aliased String_Sources.String_Source;
   Handler : aliased Gela.Lexical.Handler.Handler;
begin
   Test_RB_Tree;

   Gela.Lexical.Handler.Initialize;
   Scanner.Set_Source (Source'Unchecked_Access);
   Scanner.Set_Handler (Handler'Unchecked_Access);
   Source.Create (Text);

   loop
      declare
         Token : Gela.Lexical.Tokens.Token;
      begin
         Scanner.Get_Token (Token);

         Ada.Wide_Wide_Text_IO.Put
           (Gela.Lexical.Tokens.Token'Wide_Wide_Image (Token));

         exit when Token in Gela.Lexical.Tokens.End_Of_Input
           | Gela.Lexical.Tokens.Error;

         Ada.Wide_Wide_Text_IO.Put (": ");

         Ada.Wide_Wide_Text_IO.Put_Line
           (Scanner.Get_Text.To_Wide_Wide_String);
      end;
   end loop;

   Ada.Wide_Wide_Text_IO.New_Line;
end Lexer_Test;
