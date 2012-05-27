------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Directories;
with Ada.Wide_Wide_Text_IO;
with Gela.Conv;
with GNAT.Source_Info;
with GNAT.OS_Lib;
with League.Application;

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

   Build_Root_Value  : League.Strings.Universal_String;
   Source_Root_Value : League.Strings.Universal_String;

   procedure Create_Output
     (Output_File : in out League.Strings.Universal_String;
      Result      : out GNAT.OS_Lib.File_Descriptor);

   function Is_Absolute_Path
     (File : League.Strings.Universal_String)
     return Boolean;

   function Temp_Directory return League.Strings.Universal_String;

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
      Arguments   : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector;
      Exit_Code   : out Integer;
      Output      : out League.Strings.Universal_String;
      Output_File : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Directory   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
   is
      Args : GNAT.OS_Lib.Argument_List (1 .. Arguments.Length);
      Log  : GNAT.OS_Lib.File_Descriptor;
      Exe  : constant League.Strings.Universal_String := Command & Exe_Suffix;
      Exec : GNAT.OS_Lib.String_Access;
      Name : League.Strings.Universal_String := Output_File;
      Text : League.Strings.Universal_String;
      Dir  : constant String := Ada.Directories.Current_Directory;
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

      if not Directory.Is_Empty then
         Ada.Directories.Set_Directory (Conv.To_String (Directory));
         Text.Append ("[ in ");
         Text.Append (Directory);
         Text.Append ("]");
      end if;

      Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

      GNAT.OS_Lib.Spawn
        (Program_Name => Exec.all,
         Args         => Args,
         Output_File_Descriptor => Log,
         Return_Code  => Exit_Code);

      if not Directory.Is_Empty then
         Ada.Directories.Set_Directory (Dir);
      end if;

      Output := Conv.Read_File (Name);
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

   function Build_Root return League.Strings.Universal_String is
   begin
      if Build_Root_Value.Is_Empty then
         Build_Root_Value := Gela.Conv.To_Universal_String
           (Ada.Directories.Containing_Directory
              (Ada.Command_Line.Command_Name));
      end if;

      return Build_Root_Value;
   end Build_Root;

   function Source_Root return League.Strings.Universal_String is
      CR  : constant Wide_Wide_Character := Wide_Wide_Character'Val (13);
      LF  : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);
      TAB : constant Wide_Wide_String := (1 => Wide_Wide_Character'Val (09));

      Found     : League.Strings.Universal_String;
      Exe_File  : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector;
      Exit_Code : Integer;
      Output    : League.Strings.Universal_String;
      Pos       : Positive := 1;
   begin
      if Source_Root_Value.Is_Empty then
         Exe_File := Gela.Conv.To_Universal_String
           (Ada.Command_Line.Command_Name);

         Arguments.Append (+"--dwarf=info");
         Arguments.Append (Exe_File);

         Execute
           (Command     => +"objdump",
            Arguments   => Arguments,
            Exit_Code   => Exit_Code,
            Output      => Output);

         if Exit_Code /= 0 then
            raise Constraint_Error;
         end if;

         Search_Path :
         loop
            declare
               use League.String_Vectors;
               File_Name  : constant Universal_String :=
                 Gela.Conv.To_Universal_String (GNAT.Source_Info.File);
               Line       : Universal_String;
               Block_Size : constant := 1024;
               Lines : constant Universal_String_Vector :=
                 Output.Slice (Pos, Pos + Block_Size).Split (LF);
            begin
               Pos := Pos + Block_Size - Lines.Element (Lines.Length).Length;

               for J in 1 .. Lines.Length loop
                  Line := Lines.Element (J);
                  if Line.Ends_With (TAB & CR) then
                     Line := Line.Slice (1, Line.Length - 2);
                  elsif Line.Ends_With (TAB) then
                     Line := Line.Slice (1, Line.Length - 1);
                  end if;

                  if Line.Ends_With (File_Name) then
                     declare
                        Words : constant Universal_String_Vector :=
                          Line.Split (' ');
                     begin
                        Found := Words.Element (Words.Length);
                        exit Search_Path;
                     end;
                  end if;
               end loop;
            end;
         end loop Search_Path;

         declare
            function Up (X : String) return String renames
              Ada.Directories.Containing_Directory;
         begin
            Source_Root_Value := Gela.Conv.To_Universal_String
              (Up (Up (Up ((Gela.Conv.To_String (Found))))));
         end;
      end if;

      return Source_Root_Value;
   end Source_Root;

end Gela.Host;
