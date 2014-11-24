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
with Ada.Streams;
with Ada.Wide_Wide_Text_IO;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with Interfaces.C.Strings;

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

   Build_Root_Value  : League.Strings.Universal_String;
   Source_Root_Value : League.Strings.Universal_String;

   function Is_Absolute_Path
     (File : League.Strings.Universal_String)
     return Boolean;


   function To_C
     (Text : League.Strings.Universal_String)
      return Interfaces.C.Strings.chars_ptr;

   procedure Error_Exit
     (Output : GNAT.OS_Lib.File_Descriptor;
      Name   : String);
   --  Print errno code and exit

   ----------------
   -- Error_Exit --
   ----------------

   procedure Error_Exit
     (Output : GNAT.OS_Lib.File_Descriptor;
      Name   : String)
   is
      Ignore : Integer;
      pragma Unreferenced (Ignore);

      Error  : constant String := Name & " failed:"
        & Integer'Image (GNAT.OS_Lib.Errno)
        & ASCII.LF;
   begin
      Ignore := GNAT.OS_Lib.Write
        (Output, Error (1)'Address, Error'Length);
      GNAT.OS_Lib.OS_Exit (-1);
   end Error_Exit;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Command     : League.Strings.Universal_String;
      Arguments   : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector;
      Exit_Code   : out Integer;
      Output      : out League.Strings.Universal_String;
      Directory   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
   is
      use Interfaces;
      use type C.int;
      use type C.Strings.chars_ptr;

      type File_Discriptor_Pair is
        array (0 .. 1) of GNAT.OS_Lib.File_Descriptor;

      function pipe2
        (FD : out File_Discriptor_Pair; Flags : C.int) return C.int;
      pragma Import (C, pipe2, "pipe2");

      O_CLOEXEC : constant := 8#2000000#;

      procedure dup2
        (oldfd : GNAT.OS_Lib.File_Descriptor;
         newfd : GNAT.OS_Lib.File_Descriptor);

      pragma Import (C, dup2, "dup2");

      function fork return C.int;
      pragma Import (C, fork, "fork");

      function chdir (Path : C.Strings.chars_ptr) return C.int;
      pragma Import (C, chdir, "chdir");

      type char_prt_array is array (Natural range <>) of C.Strings.chars_ptr;

      procedure execvp
        (Path : C.Strings.chars_ptr;
         Args : char_prt_array);
      pragma Import (C, execvp, "execvp");

      procedure waitpid
        (PID    : C.int;
         Status : out C.int;
         Option : C.int);
      pragma Import (C, waitpid, "waitpid");

      Args   : char_prt_array (0 .. Arguments.Length + 1);
      Dir    : C.Strings.chars_ptr := To_C (Directory);
      PID    : C.int;
      Result : C.int;
      Input  : File_Discriptor_Pair;  --  Input of child process
      Output1 : File_Discriptor_Pair;  --  Output of child process


      Exe  : constant League.Strings.Universal_String := Command & Exe_Suffix;
      Exec : GNAT.OS_Lib.String_Access;
      Text : League.Strings.Universal_String;
   begin
      if not Is_Absolute_Path (Exe) and not Is_Windows then
         Exec := GNAT.OS_Lib.Locate_Exec_On_Path (Exe.To_UTF_8_String);
      else
         Exec := new String'(Exe.To_UTF_8_String);
      end if;

      Text := League.Strings.From_UTF_8_String (Exec.all);
      Text.Append (' ');
      Text.Append (Arguments.Join (' '));

      if not Directory.Is_Empty then
         Ada.Directories.Set_Directory (Directory.To_UTF_8_String);
         Text.Append (" [ in ");
         Text.Append (Directory);
         Text.Append ("]");
      end if;

      Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

      if pipe2 (FD => Output1, Flags => O_CLOEXEC) = -1 then
         raise Program_Error with "pipe (output) failed";
      end if;

      if pipe2 (FD => Input, Flags => O_CLOEXEC) = -1 then
         raise Program_Error with "pipe (input) failed";
      end if;

      Args (0) := To_C (Exe);

      for J in 1 .. Arguments.Length loop
         Args (J) := To_C (Arguments.Element (J));
      end loop;

      Args (Arguments.Length + 1) := C.Strings.Null_Ptr;

      PID := fork;

--      Child.stdin  := Input  (1);
--      Child.stdout := Output (0);

      if PID = 0 then
         --  Please no more tricks with controlled values after fork here
         if Dir = C.Strings.Null_Ptr or else chdir (Dir) /= -1 then
            GNAT.OS_Lib.Close (Input (1));
            GNAT.OS_Lib.Close (Output1 (0));
            dup2 (newfd => 0, oldfd => Input (0));
            dup2 (newfd => 1, oldfd => Output1 (1));
            dup2 (newfd => 2, oldfd => Output1 (1));
            execvp (Args (0), Args);

            --  We are here only if error occured. Print Errno and exit
            declare
               Name : constant String := "execvp ("
                 & C.Strings.Value (Args (0)) & ")";
            begin
               Error_Exit (Output1 (1), Name);
            end;
         else
            --  chdir failed. Print Errno and exit
            Error_Exit (Output1 (1), "chdir");
         end if;
      elsif PID = -1 then
         Error_Exit (2, "fork");
      else
         C.Strings.Free (Dir);

         for Arg of Args loop
            C.Strings.Free (Arg);
         end loop;

         GNAT.OS_Lib.Close (Input (0));
         GNAT.OS_Lib.Close (Output1 (1));

         declare
            Codec  : constant League.Text_Codecs.Text_Codec :=
              League.Text_Codecs.Codec_For_Application_Locale;
            Buffer : Ada.Streams.Stream_Element_Array (1 .. 1024);
            Count  : Integer;
         begin
            loop
               Count := GNAT.OS_Lib.Read
                 (Output1 (0), Buffer (1)'Address, Buffer'Length);

               exit when Count <= 0;

               Output.Append
                 (Codec.Decode
                    (Buffer (1 .. Ada.Streams.Stream_Element_Offset (Count))));
            end loop;

            GNAT.OS_Lib.Close (Input (1));
            GNAT.OS_Lib.Close (Output1 (0));
         end;

         waitpid (PID, Result, 0);
         Exit_Code := Integer (Result);
      end if;
   end Execute;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path
     (File : League.Strings.Universal_String)
     return Boolean
   is
      use Ada.Directories;
      Name : constant String := File.To_UTF_8_String;
   begin
      return Name /= Simple_Name (Name);
   end Is_Absolute_Path;

   --------------------
   -- Temp_Directory --
   --------------------


   function Build_Root return League.Strings.Universal_String is
   begin
      if Build_Root_Value.Is_Empty then
         Build_Root_Value := League.Strings.From_UTF_8_String
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
         Exe_File := League.Strings.From_UTF_8_String
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
                 League.Strings.From_UTF_8_String (GNAT.Source_Info.File);
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
            Source_Root_Value := League.Strings.From_UTF_8_String
              (Up (Up (Up (Found.To_UTF_8_String))));
         end;
      end if;

      return Source_Root_Value;
   end Source_Root;

   ----------
   -- To_C --
   ----------

   function To_C
     (Text : League.Strings.Universal_String)
      return Interfaces.C.Strings.chars_ptr is
   begin
      if Text.Is_Empty then
         return Interfaces.C.Strings.Null_Ptr;
      else
         return Interfaces.C.Strings.New_String (Text.To_UTF_8_String);
      end if;
   end To_C;


end Gela.Host;
