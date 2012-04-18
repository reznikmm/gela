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
with Gela.Conv;
with GNAT.OS_Lib;
with League.Application;
with League.Text_Codecs;

package body Gela.Host is

   use League.Strings;

   function "+"
     (Text : Wide_Wide_String)
     return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   Is_Windows : constant Boolean :=
     League.Application.Environment.Value (+"OS") = +"Windows_NT";

   Suffixes : constant array (Boolean) of League.Strings.Universal_String :=
     (False => League.Strings.Empty_Universal_String,
      True  => +".exe");

   Exe_Suffix : constant League.Strings.Universal_String :=
     Suffixes (Is_Windows);

   procedure Create_Output
     (Output_File : in out League.Strings.Universal_String;
      Result      : out GNAT.OS_Lib.File_Descriptor);

   function Is_Absolute_Path
     (File : League.Strings.Universal_String)
     return Boolean;

   function Temp_Directory return League.Strings.Universal_String;

   function Read_File
     (File_Name : League.Strings.Universal_String)
     return League.Strings.Universal_String;

   -------------------
   -- Create_Output --
   -------------------

   procedure Create_Output
     (Output_File : in out League.Strings.Universal_String;
      Result      : out GNAT.OS_Lib.File_Descriptor) is
   begin
      if Output_File.Is_Empty then
         Output_File := +"aaa1";
      end if;

      if not Is_Absolute_Path (Output_File) then
         Output_File := Temp_Directory & Output_File;
      end if;

      Result := GNAT.OS_Lib.Create_Output_Text_File
        (Conv.To_String (Output_File));
   end Create_Output;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Command     : League.Strings.Universal_String;
      Arguments   : League.String_Vectors.Universal_String_Vector;
      Exit_Code   : out Integer;
      Output      : out League.Strings.Universal_String;
      Output_File : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
   is
      Args : GNAT.OS_Lib.Argument_List (1 .. Arguments.Length);
      Log  : GNAT.OS_Lib.File_Descriptor;
      Exe  : constant League.Strings.Universal_String := Command & Exe_Suffix;
      Exec : GNAT.OS_Lib.String_Access;
      Name : League.Strings.Universal_String := Output_File;
      Text : League.Strings.Universal_String;
   begin
      Create_Output (Name, Log);

      if not Is_Absolute_Path (Exe) and not Is_Windows then
         Exec := GNAT.OS_Lib.Locate_Exec_On_Path (Conv.To_String (Exe));
      else
         Exec := new String'(Conv.To_String (Exe));
      end if;

      for J in Args'Range loop
         Args (J) := new String'(Conv.To_String (Arguments.Element (J)));
      end loop;

      Text := Conv.To_Universal_String (Exec.all);
      Text.Append (' ');
      Text.Append (Arguments.Join (' '));

      Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

      GNAT.OS_Lib.Spawn
        (Program_Name => Exec.all,
         Args         => Args,
         Output_File_Descriptor => Log,
         Return_Code  => Exit_Code);

      Output := Read_File (Name);
   end Execute;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path
     (File : League.Strings.Universal_String)
     return Boolean
   is
      use Ada.Directories;
      Name : constant String := Conv.To_String (File);
   begin
      return Name /= Simple_Name (Name);
   end Is_Absolute_Path;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (File_Name : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      Decoder : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");

      Name : constant String := Conv.To_String (File_Name);

      Size : constant Ada.Directories.File_Size :=
        Ada.Directories.Size (Name);

      Length : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Count (Size);

      File   : Ada.Streams.Stream_IO.File_Type;
      Data   : Ada.Streams.Stream_Element_Array (1 .. Length);
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Name);
      Ada.Streams.Stream_IO.Read (File, Data, Last);
      Ada.Streams.Stream_IO.Close (File);

      return Decoder.Decode (Data (1 .. Last));
   end Read_File;

   --------------------
   -- Temp_Directory --
   --------------------

   function Temp_Directory return League.Strings.Universal_String is
   begin
      if Is_Windows then
         return League.Application.Environment.Value (+"TMP", +".\");
      else
         return League.Application.Environment.Value (+"TMPDIR", +"/tmp/");
      end if;
   end Temp_Directory;

end Gela.Host;
