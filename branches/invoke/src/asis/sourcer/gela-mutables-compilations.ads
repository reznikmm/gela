------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with League.Calendars;

with Gela.Compilations;
with Gela.Errors;
with Gela.Lexical;
with Gela.Types;

with Gela.Mutables.Lexers;
with Gela.Mutables.Symbol_Tables;
with Gela.Nodes;

with Gela.Stores;
with Gela.Stores.Fabrics;
with Gela.Simple_Contexts;

package Gela.Mutables.Compilations is
   pragma Preelaborate;

   type Store is new Gela.Stores.Store with record
      Fabric : aliased Stores.Fabrics.Fabric (Store'Unchecked_Access);
   end record;

   type Compilation is limited new Gela.Compilations.Abstract_Compilation
   with record
      --  Properties
      Name        : League.Strings.Universal_String;
      Object_Name : League.Strings.Universal_String;
      Options     : League.Strings.Universal_String;
      Text        : League.Strings.Universal_String;
      Updated     : League.Calendars.Date_Time;
      CPU_Spent   : Duration;
      Origin      : Gela.Types.Unit_Origins;
      --  Components
      Context : Gela.Simple_Contexts.Context_Access;
      Store   : Mutables.Compilations.Store (Compilation'Unchecked_Access);
      Root    : Gela.Nodes.Element;
      Errors  : Gela.Errors.Error_Handler_Access;
      Lexer   : aliased Mutables.Lexers.Lexer (Compilation'Unchecked_Access);
      Symbols : Gela.Types.Symbol_Set_Access;
      Env     : aliased Mutables.Symbol_Tables.Symbol_Table
                          (Compilation'Unchecked_Access);
   end record;

   function Create
     (Name    : League.Strings.Universal_String;
      Context : Gela.Simple_Contexts.Context_Access;
      Source  : League.Strings.Universal_String;
      Errors  : Gela.Errors.Error_Handler_Access;
      Symbols : Gela.Types.Symbol_Set_Access;
      Origin  : Gela.Types.Unit_Origins)
      return Mutable_Compilation_Access;

   not overriding procedure Start
     (Self : not null access Compilation);

private

   overriding function Text_Name
     (Self : access Compilation)
      return League.Strings.Universal_String;

   overriding function Object_Name
     (Self : access Compilation)
      return League.Strings.Universal_String;

   overriding function Compilation_Command_Line_Options
     (Self : access Compilation)
      return League.Strings.Universal_String;

   overriding function Time_Of_Last_Update
     (Self : access Compilation)
      return League.Calendars.Date_Time;

   overriding function Compilation_CPU_Duration
     (Self : access Compilation)
      return Duration;

   overriding function Text
     (Self : access Compilation)
      return League.Strings.Universal_String;

   overriding function Last_Line
     (Self : access Compilation) return Gela.Lexical.Line_Count;

   overriding function Line
     (Self  : access Compilation;
      Index : Gela.Lexical.Line_Index) return Gela.Lexical.Line_Offset;

   overriding function Get_Token
     (Self  : access Compilation;
      Index : Positive)
      return Gela.Types.Token;

   overriding function Symbols
     (Self  : access Compilation) return Gela.Types.Symbol_Set_Access;

   overriding function Origin
     (Self  : access Compilation)
      return Gela.Types.Unit_Origins;

end Gela.Mutables.Compilations;
