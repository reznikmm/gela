--  This package provides Compilation_Unit interfaces and their methods.

with League.Calendars;
with League.String_Vectors;
with League.Strings;

with Gela.Contexts;
with Gela.Element_Factories;
with Gela.Lexical_Types;

package Gela.Compilations is
   pragma Preelaborate;

   type Compilation is limited interface;
   --  This type represent single compilation from some context

   type Compilation_Access is access all Compilation'Class;
   for Compilation_Access'Storage_Size use 0;

   not overriding function Context
     (Self : Compilation) return Gela.Contexts.Context_Access is abstract;
   --  Return corresponding context

   not overriding function Text_Name
     (Self : Compilation) return League.Strings.Universal_String is abstract;
   --  Return name of compilation source

   not overriding function Object_Name
     (Self : Compilation) return League.Strings.Universal_String is abstract;
   --  Return name of compilation result

   not overriding function Compilation_Command_Line_Options
     (Self : Compilation)
      return League.String_Vectors.Universal_String_Vector is abstract;
   --  Return compilation options

   not overriding function Time_Of_Last_Update
     (Self : Compilation)
      return League.Calendars.Date_Time is abstract;
   --  Return time of compilation

   not overriding function Compilation_CPU_Duration
     (Self : Compilation) return Duration is abstract;
   --  Return duration of compilation

   not overriding function Source
     (Self : Compilation) return League.Strings.Universal_String is abstract;
   --  Return source test of compilation

   not overriding function Line_Count
     (Self : Compilation) return Gela.Lexical_Types.Line_Count is abstract;
   --  Return line count of compilation source

   not overriding function Get_Line_Span
     (Self  : Compilation;
      Index : Gela.Lexical_Types.Line_Index)
      return Gela.Lexical_Types.Line_Span is abstract;
   --  Return indexes in source of begin, end and comment for given line

   not overriding function Token_Count
     (Self : Compilation) return Gela.Lexical_Types.Token_Count is abstract;
   --  Return token count of compilation source

   not overriding function Get_Token
     (Self  : Compilation;
      Index : Gela.Lexical_Types.Token_Index)
      return Gela.Lexical_Types.Token is abstract;
   --  Return token information for given token

   not overriding function Factory
     (Self : Compilation) return Gela.Element_Factories.Element_Factory_Access
        is abstract;

end Gela.Compilations;
