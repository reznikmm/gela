------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with League.String_Vectors;
with XML.SAX.Attributes;
with XML.SAX.Content_Handlers;
with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Simple_Readers;

package body Gela.Test_Cases.Valgrind is

   package Report_Readers is

      package Maps is new Ada.Containers.Ordered_Maps
        (League.Strings.Universal_String, Natural);

      type Report_Reader is limited new
        XML.SAX.Content_Handlers.SAX_Content_Handler with
         record
            Error  : League.Strings.Universal_String;
            Errors : Maps.Map;
         end record;

      overriding function Error_String
        (Self : Report_Reader)
         return League.Strings.Universal_String;

      overriding procedure Start_Element
        (Self           : in out Report_Reader;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Attributes     : XML.SAX.Attributes.SAX_Attributes;
         Success        : in out Boolean);

      overriding procedure End_Element
        (Self           : in out Report_Reader;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Success        : in out Boolean);

      overriding procedure Characters
        (Self           : in out Report_Reader;
         Text    : League.Strings.Universal_String;
         Success : in out Boolean);

   end Report_Readers;

   package body Report_Readers is

      Kind : constant League.Strings.Universal_String :=
        League.Strings.To_Universal_String ("kind");

      ----------------
      -- Characters --
      ----------------

      overriding procedure Characters
        (Self           : in out Report_Reader;
         Text    : League.Strings.Universal_String;
         Success : in out Boolean) is
         pragma Unreferenced (Success);
      begin
         Self.Error.Append (Text);
      end Characters;

      -----------------
      -- End_Element --
      -----------------

      overriding procedure End_Element
        (Self           : in out Report_Reader;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Success        : in out Boolean) is
         pragma Unreferenced (Namespace_URI, Qualified_Name, Success);
      begin
         if Local_Name = Kind then
            declare
               Cursor : constant Maps.Cursor := Self.Errors.Find (Self.Error);
            begin
               if Maps.Has_Element (Cursor) then
                  Self.Errors.Replace_Element
                    (Cursor, Maps.Element (Cursor) + 1);
               else
                  Self.Errors.Insert (Self.Error, 1);
               end if;
            end;
         end if;
      end End_Element;

      -----------------
      -- End_Element --
      -----------------

      overriding function Error_String
        (Self : Report_Reader)
         return League.Strings.Universal_String
      is
         pragma Unreferenced (Self);
      begin
         return League.Strings.Empty_Universal_String;
      end Error_String;

      -------------------
      -- Start_Element --
      -------------------

      overriding procedure Start_Element
        (Self           : in out Report_Reader;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Attributes     : XML.SAX.Attributes.SAX_Attributes;
         Success        : in out Boolean)
      is
         pragma Unreferenced (Namespace_URI);
         pragma Unreferenced (Local_Name, Qualified_Name, Attributes, Success);
      begin
         Self.Error.Clear;
      end Start_Element;

   end Report_Readers;

   ------------
   -- Create --
   ------------

   function Create
     (Run_Test  : Gela.Test_Cases.Execute.Test_Case_Access)
      return Test_Cases.Test_Case_Access
   is
      Prefix    : League.String_Vectors.Universal_String_Vector;
      Arguments : League.String_Vectors.Universal_String_Vector :=
        Run_Test.Arguments;
      Result    : constant Test_Case :=
        (Run_Test  => Run_Test,
         Errors    => False,
         Output    => League.Strings.Empty_Universal_String);
   begin
      Prefix.Append (To_Universal_String ("--xml=yes"));
      Prefix.Append ("--xml-file=" & Result.Valgrind_Report);
      Prefix.Append (To_Universal_String ("--suppressions=../valgrind.supp"));
      Prefix.Append (Run_Test.Command);
      Arguments.Prepend (Prefix);
      Run_Test.Set_Command
        (Command   => To_Universal_String ("valgrind"),
         Arguments => Arguments);
      return new Test_Case'(Result);
   end Create;

   --------------
   -- Duration --
   --------------

   function Duration (Self : Test_Case) return League.Calendars.Time is
   begin
      return Self.Run_Test.Duration;
   end Duration;

   ----------
   -- File --
   ----------

   function File (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Run_Test.File;
   end File;

   -------------
   -- Fixture --
   -------------

   function Fixture
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Run_Test.Fixture;
   end Fixture;

   ----------
   -- Name --
   ----------

   function Name (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return "valgrind " & Self.Run_Test.Name;
   end Name;

   ------------
   -- Output --
   ------------

   function Output
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      if Self.Output.Is_Empty then
         return Self.Run_Test.Output;
      else
         return Self.Output;
      end if;
   end Output;

   --------------------------
   -- Read_Valgrind_Report --
   --------------------------

   not overriding procedure Read_Valgrind_Report (Self : in out Test_Case) is
      Source  : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
      Reader  : aliased XML.SAX.Simple_Readers.SAX_Simple_Reader;
      Handler : aliased Report_Readers.Report_Reader;
   begin
      Reader.Set_Content_Handler (Handler'Unchecked_Access);

      Source.Open_By_File_Name (Self.Valgrind_Report);
      Reader.Parse (Source'Access);

      declare
         use Report_Readers;

         Leak_Definitely_Lost : constant League.Strings.Universal_String :=
           League.Strings.To_Universal_String ("Leak_DefinitelyLost");

         Cursor : Maps.Cursor := Handler.Errors.First;
      begin
         while Maps.Has_Element (Cursor) loop
            Self.Output.Append (Maps.Key (Cursor));
            Self.Output.Append (" = ");
            Self.Output.Append
              (Natural'Wide_Wide_Image (Maps.Element (Cursor)));
            Self.Output.Append (Wide_Wide_Character'Val (10));

            if Maps.Key (Cursor) /= Leak_Definitely_Lost then
               Self.Errors := True;
            end if;

            Cursor := Maps.Next (Cursor);
         end loop;
      end;
   end Read_Valgrind_Report;

   ---------
   -- Run --
   ---------

   procedure Run (Self : in out Test_Case) is
   begin
      Self.Run_Test.Run;
      if Self.Run_Test.Status = Success then
         Self.Read_Valgrind_Report;
      end if;
   end Run;

   ------------
   -- Status --
   ------------

   function Status (Self : Test_Case) return Test_Cases.Status_Kind is
   begin
      if Self.Errors then
         return Gela.Test_Cases.Failure;
      else
         return Self.Run_Test.Status;
      end if;
   end Status;

   ---------------
   -- Traceback --
   ---------------

   function Traceback
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Run_Test.Traceback;
   end Traceback;

   ---------------------
   -- Valgrind_Report --
   ---------------------

   not overriding
   function Valgrind_Report
     (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Run_Test.Build & "/valgrind.xml";
   end Valgrind_Report;
end Gela.Test_Cases.Valgrind;
