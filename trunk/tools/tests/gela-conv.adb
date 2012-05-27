------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with League.Text_Codecs;

package body Gela.Conv is

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (File_Name : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      type Stream_Element_Array_Access is access
        Ada.Streams.Stream_Element_Array;

      UTF_8   : constant League.Strings.Universal_String :=
        League.Strings.To_Universal_String ("utf-8");
      Decoder : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (UTF_8);

      Name : constant String := Conv.To_String (File_Name);

      Size : constant Ada.Directories.File_Size :=
        Ada.Directories.Size (Name);

      Length : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Count (Size);

      Data   : constant Stream_Element_Array_Access :=
        new Ada.Streams.Stream_Element_Array (1 .. Length);
      File   : Ada.Streams.Stream_IO.File_Type;
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Name);
      Ada.Streams.Stream_IO.Read (File, Data.all, Last);
      Ada.Streams.Stream_IO.Close (File);

      return Decoder.Decode (Data (1 .. Last));
   end Read_File;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Text : League.Strings.Universal_String)
      return String
   renames League.Text_Codecs.To_Exception_Message;

   -------------------------
   -- To_Universal_String --
   -------------------------

   function To_Universal_String
     (Text : String)
      return League.Strings.Universal_String
   is
      Wide_Text : constant Wide_Wide_String :=
        Ada.Characters.Conversions.To_Wide_Wide_String (Text);
   begin
      return League.Strings.To_Universal_String (Wide_Text);
   end To_Universal_String;

end Gela.Conv;
