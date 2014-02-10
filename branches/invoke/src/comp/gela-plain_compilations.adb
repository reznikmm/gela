package body Gela.Plain_Compilations is

   --------------------------------------
   -- Compilation_Command_Line_Options --
   --------------------------------------

   overriding function Compilation_Command_Line_Options
     (Self : Compilation)
      return League.String_Vectors.Universal_String_Vector
   is
      pragma Unreferenced (Self);
   begin
      return League.String_Vectors.Empty_Universal_String_Vector;
   end Compilation_Command_Line_Options;

   ------------------------------
   -- Compilation_CPU_Duration --
   ------------------------------

   overriding function Compilation_CPU_Duration
     (Self : Compilation)
      return Duration
   is
      pragma Unreferenced (Self);
   begin
      return 0.0;
   end Compilation_CPU_Duration;

   -------------
   -- Context --
   -------------

   overriding function Context
     (Self : Compilation) return Gela.Contexts.Context_Access is
   begin
      return Self.Context;
   end Context;

   -------------------
   -- Get_Line_Span --
   -------------------

   overriding function Get_Line_Span
     (Self  : Compilation;
      Index : Gela.Lexical_Types.Line_Index)
      return Gela.Lexical_Types.Line_Span
   is
   begin
      return Self.Lines.Element (Index);
   end Get_Line_Span;

   ---------------
   -- Get_Token --
   ---------------

   overriding function Get_Token
     (Self  : Compilation;
      Index : Gela.Lexical_Types.Token_Index)
      return Gela.Lexical_Types.Token
   is
   begin
      return Self.Tokens.Element (Index);
   end Get_Token;

   ----------------
   -- Line_Count --
   ----------------

   overriding function Line_Count
     (Self : Compilation)
      return Gela.Lexical_Types.Line_Count
   is
   begin
      return Self.Lines.Last_Index;
   end Line_Count;

   ---------------
   -- New_Token --
   ---------------

   overriding procedure New_Token
     (Self  : in out Compilation;
      Token : Gela.Lexical_Types.Token) is
   begin
      Self.Tokens.Append (Token);
   end New_Token;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self  : in out Compilation;
      Line  : Gela.Lexical_Types.Line_Span) is
   begin
      Self.Lines.Append (Line);
   end New_Line;

   ----------------
   -- Next_Token --
   ----------------

   overriding procedure Next_Token
     (Self  : in out Compilation;
      Token : out Gela.Lexical_Types.Token_Kind;
      Index : out Gela.Lexical_Types.Token_Index)
   is
      use type Gela.Lexical_Types.Token_Index;
   begin
      if Self.Index <= Self.Tokens.Last_Index then
         Token := Self.Tokens.Element (Self.Index).Kind;
         Index := Self.Index;
         Self.Index := Self.Index + 1;
      else
         Token := Gela.Lexical_Types.End_Of_Input;
         Index := Self.Index;
      end if;
   end Next_Token;

   -----------------
   -- Object_Name --
   -----------------

   overriding function Object_Name
     (Self : Compilation)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.Empty_Universal_String;
   end Object_Name;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self      : in out Compilation;
      Text_Name : League.Strings.Universal_String;
      Source    : League.Strings.Universal_String) is
   begin
      Self.Update := League.Calendars.Clock;
      Self.Text_Name := Text_Name;
      Self.Source := Source;
   end Initialize;

   ------------
   -- Source --
   ------------

   overriding function Source
     (Self : Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Source;
   end Source;

   ---------------
   -- Text_Name --
   ---------------

   overriding function Text_Name
     (Self : Compilation)
      return League.Strings.Universal_String is
   begin
      return Self.Text_Name;
   end Text_Name;

   -------------------------
   -- Time_Of_Last_Update --
   -------------------------

   overriding function Time_Of_Last_Update
     (Self : Compilation)
      return League.Calendars.Date_Time is
   begin
      return Self.Update;
   end Time_Of_Last_Update;

   -----------------
   -- Token_Count --
   -----------------

   overriding function Token_Count
     (Self : Compilation)
      return Gela.Lexical_Types.Token_Count
   is
   begin
      return Self.Tokens.Last_Index;
   end Token_Count;

end Gela.Plain_Compilations;
