------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with League.Text_Codecs;

package body Gela.Test_Tools is

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (File_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String is
   begin
      return Read_File (File_Name.To_UTF_8_String);
   end Read_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (File_Name : String) return League.Strings.Universal_String
   is
      type Stream_Element_Array_Access is access
        Ada.Streams.Stream_Element_Array;

      UTF_8   : constant League.Strings.Universal_String :=
        League.Strings.To_Universal_String ("utf-8");
      Decoder : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (UTF_8);

      Size : constant Ada.Directories.File_Size :=
        Ada.Directories.Size (File_Name);

      Length : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Count (Size);

      Data   : constant Stream_Element_Array_Access :=
        new Ada.Streams.Stream_Element_Array (1 .. Length);
      File   : Ada.Streams.Stream_IO.File_Type;
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, File_Name);
      Ada.Streams.Stream_IO.Read (File, Data.all, Last);
      Ada.Streams.Stream_IO.Close (File);

      return Decoder.Decode (Data (1 .. Last));
   end Read_File;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File
     (File : League.Strings.Universal_String;
      Text : League.Strings.Universal_String)
   is
      Output : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Writing " & File.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Create
        (Output, Name => File.To_UTF_8_String, Form => "WCEM=8");
      Ada.Wide_Wide_Text_IO.Put_Line (Output, Text.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write_File;

end Gela.Test_Tools;
