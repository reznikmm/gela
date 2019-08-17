--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Scanner_Destinations;

package body Program.Scanners is

   type First_Stage_Index is mod 16#1100#;
   type Second_Stage_Index is mod 16#100#;

   generic
      type Element_Type is private;
      type Second_Stage_Array is
        array (Second_Stage_Index) of Element_Type;
      type Second_Stage_Array_Access is
        not null access constant Second_Stage_Array;
      type First_Stage_Array is
        array (First_Stage_Index) of Second_Stage_Array_Access;
   function Generic_Element (Data : First_Stage_Array; Code : Natural)
     return Element_Type;

   ---------------------
   -- Generic_Element --
   ---------------------

   function Generic_Element
     (Data : First_Stage_Array;
      Code : Natural) return Element_Type is
   begin
      return Data (First_Stage_Index (Code / Second_Stage_Index'Modulus))
        (Second_Stage_Index (Code mod Second_Stage_Index'Modulus));
   end Generic_Element;

   package body Tables is separate;

   procedure On_Accept
     (Self    : not null access Program.Scanned_Rule_Handlers.Handler'Class;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Lexical_Elements.Lexical_Element_Kind;
      Skip    : in out Boolean);

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Self : Scanner'Class)
     return not null Program.Source_Buffers.Source_Buffer_Access is
   begin
      return Self.Source;
   end Get_Source;

   --------------
   -- Get_Span --
   --------------

   function Get_Span
     (Self : Scanner'Class) return Program.Source_Buffers.Span is
   begin
      return (Self.From, Self.To);
   end Get_Span;

   -------------------------
   -- Get_Start_Condition --
   -------------------------

   function Get_Start_Condition
     (Self : Scanner'Class)
      return Start_Condition
   is
   begin
      return Self.Start;
   end Get_Start_Condition;

   ---------------
   -- Get_Token --
   ---------------

   procedure Get_Token
     (Self  : access Scanner'Class;
      Value : out Program.Lexical_Elements.Lexical_Element_Kind)
   is
      procedure Next;

      procedure Next is
      begin
         Self.Offset :=
           Self.Offset + Positive (Self.Classes (Self.Next).Length);

         if Self.Next = Self.Classes'Last then
            Self.Next := 1;
         else
            Self.Next := Self.Next + 1;
         end if;
      end Next;

      use type Program.Scanner_States.Character_Class;
      use type Program.Source_Buffers.Character_Length;

      Current_State : Program.Scanner_States.State;
      Char          : Program.Scanner_States.Character_Class;
      Skip          : Boolean := True;
      Valid_Rule    : Program.Scanner_States.Rule_Index;
      Valid_Next    : Buffer_Index;
      Valid_From    : Positive;
      Valid_To      : Positive;  --  Offset for Valid_Next
   begin
      if Self.EOF = Self.Next then
         Value := Program.Lexical_Elements.End_Of_Input;
         return;
      end if;

      loop
         Current_State := Self.Start;
         Valid_Rule := 0;
         Valid_From := Self.Offset;  --  Begin of any token
         Valid_Next := Self.Next;
         Valid_To   := Self.Offset;  --  End of token in case of Error

         loop
            Char := Self.Classes (Self.Next).Class;

            if Char /= Error_Character then
               Current_State := Tables.Switch (Current_State, Char);

               if Current_State not in Scanner_States.Looping_State then
                  if Current_State in Scanner_States.Final_State then
                     Valid_Rule := Tables.Rule (Current_State);
                     Valid_Next := Self.Next;
                     Valid_To := Self.Offset;
                  end if;

                  exit;
               elsif Current_State in Scanner_States.Final_State then
                  Valid_Rule := Tables.Rule (Current_State);
                  Valid_Next := Self.Next;
                  Valid_To := Self.Offset;
               end if;

               Next;
            elsif Self.Classes (Self.Next).Length = End_Of_Buffer then
               Self.Read_Buffer;

               if Self.EOF = Self.Next then
                  if Valid_Next = Self.Next then
                     Value := Program.Lexical_Elements.End_Of_Input;
                     return;
                  else
                     exit;
                  end if;
               end if;
            else
               exit;
            end if;
         end loop;

         Self.Next   := Valid_Next;
         Self.Offset := Valid_To;
         Self.From   := Valid_From;
         Self.To     := Valid_To;
         Next;

         if Valid_Rule in 0 then
            Value := Program.Lexical_Elements.Error;

            return;
         else
            On_Accept (Self.Handler, Self, Valid_Rule, Value, Skip);

            if not Skip then
               return;
            end if;
         end if;
      end loop;
   end Get_Token;

   procedure On_Accept
     (Self    : not null access Program.Scanned_Rule_Handlers.Handler'Class;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Lexical_Elements.Lexical_Element_Kind;
      Skip    : in out Boolean) is separate;

   -----------------
   -- Read_Buffer --
   -----------------

   procedure Read_Buffer (Self : in out Scanner'Class) is
      Last : Natural := Buffer_Half_Size;
   begin
      if Self.Next > Buffer_Half_Size then
         Last := Buffer_Index'Last;
      end if;

      Self.Source.Read (Self.Classes (Self.Next .. Last), Last);

      if Last < Self.Next then
         Self.EOF := Self.Next;
         return;
      elsif Last = Buffer_Index'Last then
         Last := 1;
      else
         Last := Last + 1;
      end if;

      Self.Classes (Last) :=
        (Class => Error_Character, Length => End_Of_Buffer);
   end Read_Buffer;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Self    : in out Scanner'Class;
      Handler : not null Program.Scanned_Rule_Handlers.Handler_Access) is
   begin
      Self.Handler := Handler;
   end Set_Handler;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source
     (Self   : in out Scanner'Class;
      Source : not null Program.Source_Buffers.Source_Buffer_Access)
   is
   begin
      Self.Source := Source;
      Self.Source.Rewind;
   end Set_Source;

   -------------------------
   -- Set_Start_Condition --
   -------------------------

   procedure Set_Start_Condition
     (Self : in out Scanner'Class;
      Condition : Start_Condition)
   is
   begin
      Self.Start := Condition;
   end Set_Start_Condition;

end Program.Scanners;
