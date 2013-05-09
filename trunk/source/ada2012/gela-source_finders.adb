------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Directories;
with Ada.Streams.Stream_IO;

with League.String_Vectors;
with League.Text_Codecs;

package body Gela.Source_Finders is

   function Read_File (Name : String) return League.Strings.Universal_String;

   ------------
   -- Create --
   ------------

   function Create
     (Path   : League.Strings.Universal_String;
      Schema : Gela.Name_Schemas.Name_Schema_Access;
      Next   : Source_Finder_Access := null)
      return Source_Finder_Access is
   begin
      if Path.Index (':') > 0 then
         declare
            Result : Source_Finder_Access;
            List   : League.String_Vectors.Universal_String_Vector;
         begin
            List := Path.Split (':', League.Strings.Skip_Empty);

            for J in reverse 1 .. List.Length loop
               Result := Create (List.Element (J), Schema, Result);
            end loop;

            return Result;
         end;
      else
         return new Source_Finder'(Path, Schema, Next);
      end if;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Source_Finder_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Source_Finder'Class, Source_Finder_Access);
   begin
      if Self.Next /= null then
         Destroy (Self.Next);
      end if;

      Free (Self);
   end Destroy;

   -----------------
   -- Lookup_Body --
   -----------------

   procedure Lookup_Body
     (Self  : access Source_Finder;
      Unit  : League.Strings.Universal_String;
      Found : out Boolean;
      File  : out League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String)
   is
      Name : constant League.Strings.Universal_String :=
        Self.Schema.Body_Name (Unit);
   begin
      Self.Lookup_File (Name, Found, File, Text);
   end Lookup_Body;

   ------------------------
   -- Lookup_Declaration --
   ------------------------

   procedure Lookup_Declaration
     (Self  : access Source_Finder;
      Unit  : League.Strings.Universal_String;
      Found : out Boolean;
      File  : out League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String)
   is
      Name : constant League.Strings.Universal_String :=
        Self.Schema.Declaration_Name (Unit);
   begin
      Self.Lookup_File (Name, Found, File, Text);
   end Lookup_Declaration;

   -----------------
   -- Lookup_File --
   -----------------

   procedure Lookup_File
     (Self  : access Source_Finder;
      Name  : League.Strings.Universal_String;
      Found : out Boolean;
      File  : in out League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String)
   is
      Result : League.Strings.Universal_String := Self.Directory;
   begin
      if not Result.Is_Empty and then not Result.Ends_With ("/") then
         Result.Append ("/");
      end if;

      Result.Append (Name);

      declare
         --  Shell we use here Codec_For_Application_Locale?
         Full_Name : constant String := Result.To_UTF_8_String;
      begin
         if Ada.Directories.Exists (Full_Name) then
            File := Result;
            Text := Read_File (Full_Name);
            Found := True;
         elsif Self.Next /= null then
            Self.Next.Lookup_File (Name, Found, File, Text);
         else
            Found := False;
         end if;
      end;
   end Lookup_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Name : String) return League.Strings.Universal_String is
      Decoder : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec_For_Application_Locale;

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

end Gela.Source_Finders;
