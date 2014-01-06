with League.Calendars;
with League.String_Vectors;
with League.Strings;

with Gela.Lexical_Types;

package Gela.Compilations is
   pragma Preelaborate;

   type Compilation is limited interface;
   type Compilation_Access is access all Compilation'Class;
   for Compilation_Access'Storage_Size use 0;

   not overriding function Text_Name
     (Self : Compilation) return League.Strings.Universal_String is abstract;

   not overriding function Object_Name
     (Self : Compilation) return League.Strings.Universal_String is abstract;

   not overriding function Compilation_Command_Line_Options
     (Self : Compilation)
      return League.String_Vectors.Universal_String_Vector is abstract;

   not overriding function Time_Of_Last_Update
     (Self : Compilation)
      return League.Calendars.Date_Time is abstract;

   not overriding function Compilation_CPU_Duration
     (Self : Compilation) return Duration is abstract;

   not overriding function Source
     (Self : Compilation) return League.Strings.Universal_String is abstract;

   not overriding function Line_Count
     (Self : Compilation) return Gela.Lexical_Types.Line_Count is abstract;

   not overriding function Get_Line_Span
     (Self  : Compilation;
      Index : Gela.Lexical_Types.Line_Index)
      return Gela.Lexical_Types.Line_Span is abstract;

   not overriding function Token_Count
     (Self : Compilation) return Gela.Lexical_Types.Token_Count is abstract;

   not overriding function Get_Token
     (Self  : Compilation;
      Index : Gela.Lexical_Types.Token_Index)
      return Gela.Lexical_Types.Token is abstract;

end Gela.Compilations;
