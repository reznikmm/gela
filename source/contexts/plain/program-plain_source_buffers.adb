--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams.Stream_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Interfaces;
with Program.Scanners;

package body Program.Plain_Source_Buffers is

   subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self : in out Source_Buffer;
      Name : Program.Text)
   is
      procedure Read_File (Text : in out UTF_8_String);

      Input : Ada.Streams.Stream_IO.File_Type;

      ---------------
      -- Read_File --
      ---------------

      procedure Read_File (Text : in out UTF_8_String) is
         Data : Ada.Streams.Stream_Element_Array (1 .. Text'Length)
           with Import, Convention => Ada, Address => Text'Address;
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (Input, Data, Last);
      end Read_File;

      File_Size : Natural;
      File_Name : constant UTF_8_String :=
        Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Name);

   begin
      Ada.Streams.Stream_IO.Open
        (Input, Ada.Streams.Stream_IO.In_File, File_Name);

      File_Size := Natural (Ada.Streams.Stream_IO.Size (Input));
      Self.Text := new UTF_8_String (1 .. File_Size);
      Read_File (Self.Text.all);
      Ada.Streams.Stream_IO.Close (Input);
      Self.Rewind;
   end Initialize;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Source_Buffer;
      Data : out Program.Source_Buffers.Character_Info_Array;
      Last : out Natural)
   is
      use all type Interfaces.Unsigned_32;

      procedure Add
        (Value        : in out Interfaces.Unsigned_32;
         Continuation : String);

      ---------
      -- Add --
      ---------

      procedure Add
        (Value        : in out Interfaces.Unsigned_32;
         Continuation : String)
      is
         Code : Interfaces.Unsigned_32 range 2#10_000000# .. 2#10_111111#;
      begin
         for Next of Continuation loop
            Code := Character'Pos (Next);
            Value := Shift_Left (Value, 6) or (Code and 2#00_111111#);
         end loop;
      end Add;

      Index : Positive := Data'First;
      Min   : constant Natural := Natural'Min
        (Data'Length, Self.Text'Last - Self.From + 1);
      Char  : Interfaces.Unsigned_32;
   begin
      Last := Data'First + Min - 1;

      while Index <= Last and Self.From <= Self.Text'Last loop
         Char := Character'Pos (Self.Text (Self.From));

         case Char is
            when 0 .. 16#7F# =>
               Data (Index).Length := 1;
            when 2#110_00000# .. 2#110_11111# =>
               Char := Char and 2#000_11111#;
               Data (Index).Length := 2;
            when 2#1110_0000# .. 2#1110_1111# =>
               Char := Char and 2#0000_1111#;
               Data (Index).Length := 3;
            when 2#11110_000# .. 2#11110_111# =>
               Char := Char and 2#00000_111#;
               Data (Index).Length := 4;
            when others =>
               raise Constraint_Error with "Wrong UTF-8 data";
         end case;

         Add
           (Char,
            Self.Text (Self.From + 1 .. Self.From + Data (Index).Length - 1));

         Data (Index).Class :=
           Program.Scanners.Tables.To_Class (Natural (Char));

         Self.From := Self.From + Data (Index).Length;
         Index := Index + 1;
      end loop;
   end Read;

   ------------
   -- Rewind --
   ------------

   overriding procedure Rewind (Self : in out Source_Buffer) is
   begin
      Self.From := 1;
   end Rewind;

   ----------
   -- Text --
   ----------

   overriding function Text
     (Self : Source_Buffer;
      Span : Program.Source_Buffers.Span) return Program.Text is
   begin
      return Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
        (Self.Text (Span.From .. Span.To));
   end Text;

end Program.Plain_Source_Buffers;
