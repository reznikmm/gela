------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

private with League.Calendars;

with Gela.Lexical.Fabrics;
with Gela.Lexical.Tokens;
with Gela.Relocatable_Arrays;
with Gela.Errors;

package Gela.Compilations.Mutables is

   type Mutable_Compilation is limited
     new Abstract_Compilation and Gela.Lexical.Fabrics.Fabric
     with private;

   type Mutable_Compilation_Access is access all Mutable_Compilation;

   not overriding function Create
     (Name   : League.Strings.Universal_String;
      Source : League.Strings.Universal_String)
      return Mutable_Compilation_Access;

   not overriding procedure Start
     (Self : not null access Mutable_Compilation);

private

   type Line_Offset_Array is array
     (Gela.Lexical.Line_Index range <>) of Line_Offset;
   type Line_Offset_Array_Access is access all Line_Offset_Array;

   type Internal_Data;
   type Internal_Access is access all Internal_Data;

   type Payload_Array_Access is access all Gela.Types.Payload_Array;

   type Mutable_Compilation is limited
     new Abstract_Compilation and Gela.Lexical.Fabrics.Fabric
     with record
        Name        : League.Strings.Universal_String;
        Object_Name : League.Strings.Universal_String;
        Options     : League.Strings.Universal_String;
        Text        : League.Strings.Universal_String;
        Tokens      : Payload_Array_Access;
        Last_Token  : Natural := 0;
        Store       : Gela.Relocatable_Arrays.Relocatable_Array;
        Lines       : Line_Offset_Array_Access;
        Last_Line   : Gela.Lexical.Line_Count;
        Errors      : access Gela.Errors.Error_Handler'Class;
        Internal    : Internal_Access;
        Updated     : League.Calendars.Date_Time;
        CPU_Spent   : Duration;
   end record;

   overriding function Text_Name
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String;

   overriding function Object_Name
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String;

   overriding function Compilation_Command_Line_Options
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String;

   overriding function Time_Of_Last_Update
     (Self : access Mutable_Compilation)
      return League.Calendars.Date_Time;

   overriding function Compilation_CPU_Duration
     (Self : access Mutable_Compilation)
      return Duration;

   overriding function Text
     (Self : access Mutable_Compilation)
      return League.Strings.Universal_String;

   overriding function Last_Line
     (Self : access Mutable_Compilation) return Gela.Lexical.Line_Count;

   overriding function Line
     (Self  : access Mutable_Compilation;
      Index : Gela.Lexical.Line_Index) return Line_Offset;

   overriding function First_Token
     (Self  : access Mutable_Compilation)
      return Gela.Types.Token;

   overriding function Folded_Set
     (Self  : access Mutable_Compilation)
      return Gela.Types.Folded_Set_Access;

   overriding procedure New_Line
     (Self    : access Mutable_Compilation;
      First   : Text_Index;
      Last    : Text_Index;
      Comment : Text_Index);

   overriding procedure New_Token
     (Self      : access Mutable_Compilation;
      Token     : Gela.Lexical.Tokens.Token;
      Line      : Positive;
      First     : Text_Index;
      Last      : Text_Index;
      Separator : Text_Index;
      Folded    : League.Strings.Universal_String);

end Gela.Compilations.Mutables;
