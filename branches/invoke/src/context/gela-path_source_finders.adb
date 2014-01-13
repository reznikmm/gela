with Ada.Streams.Stream_IO;

with League.String_Vectors;

with Gela.Naming_Schemas;

package body Gela.Path_Source_Finders is

   procedure Read_File
     (Name  : String;
      Text  : out League.Strings.Universal_String;
      Found : out Boolean);

   type Path_Source_Finder_Access is access all Source_Finder'Class;

   ------------
   -- Create --
   ------------

   function Create
     (Path    : League.Strings.Universal_String;
      Context : access Gela.Contexts.Context'Class;
      Next    : Gela.Source_Finders.Source_Finder_Access := null)
      return Gela.Source_Finders.Source_Finder_Access
   is
   begin
      if Path.Index (':') > 0 then
         declare
            Result : Gela.Source_Finders.Source_Finder_Access;
            List   : League.String_Vectors.Universal_String_Vector;
         begin
            List := Path.Split (':', League.Strings.Skip_Empty);

            for J in reverse 1 .. List.Length loop
               Result := Create (List.Element (J), Context, Result);
            end loop;

            return Result;
         end;
      else
         declare
            Result : Path_Source_Finder_Access;
         begin
            Result := new Source_Finder'(Context, Path, Next);
            return Gela.Source_Finders.Source_Finder_Access (Result);
         end;
      end if;
   end Create;

   -----------------
   -- Lookup_Body --
   -----------------

   overriding procedure Lookup_Body
     (Self   : Source_Finder;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String)
   is
      Schema : constant Gela.Naming_Schemas.Naming_Schema_Access :=
        Self.Context.Naming_Schema;
      Name   : constant League.Strings.Universal_String :=
        Schema.Body_Name (Symbol);
   begin
      Self.Lookup_Compilation (Name, Found, File, Source);
   end Lookup_Body;

   ------------------------
   -- Lookup_Compilation --
   ------------------------

   overriding procedure Lookup_Compilation
     (Self   : Source_Finder;
      Name   : League.Strings.Universal_String;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String)
   is
      use type Gela.Source_Finders.Source_Finder_Access;
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
         Read_File (Full_Name, Source, Found);

         if Found then
            File := Result;
         elsif Self.Next /= null then
            Self.Next.Lookup_Compilation (Name, Found, File, Source);
         else
            Found := False;
         end if;
      end;
   end Lookup_Compilation;

   ------------------------
   -- Lookup_Declaration --
   ------------------------

   overriding procedure Lookup_Declaration
     (Self   : Source_Finder;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String)
   is
      Schema : constant Gela.Naming_Schemas.Naming_Schema_Access :=
        Self.Context.Naming_Schema;
      Name   : constant League.Strings.Universal_String :=
        Schema.Declaration_Name (Symbol);
   begin
      Self.Lookup_Compilation (Name, Found, File, Source);
   end Lookup_Declaration;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (Name  : String;
      Text  : out League.Strings.Universal_String;
      Found : out Boolean)
   is
      File   : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Name);

      declare
         Size : constant Ada.Streams.Stream_IO.Count :=
           Ada.Streams.Stream_IO.Size (File);

         Length : constant Ada.Streams.Stream_Element_Offset :=
           Ada.Streams.Stream_Element_Count (Size);

         Data   : Ada.Streams.Stream_Element_Array (1 .. Length);
         Aux    : String (1 .. Natural (Length));
         for Aux'Address use Data'Address;
         Last   : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (File, Data, Last);
         Ada.Streams.Stream_IO.Close (File);

         Text := League.Strings.From_UTF_8_String (Aux (1 .. Natural (Last)));
         Found := True;
      end;
   exception
      when Ada.Streams.Stream_IO.Name_Error =>
         Found := False;
   end Read_File;

end Gela.Path_Source_Finders;
