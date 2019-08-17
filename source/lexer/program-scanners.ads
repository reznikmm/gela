--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Scanned_Rule_Handlers;
with Program.Scanner_States;
with Program.Source_Buffers;

package Program.Scanners is
   pragma Pure;

   type Scanner is tagged limited private;
   --  Scaner of source code

   procedure Set_Source
     (Self   : in out Scanner'Class;
      Source : not null Program.Source_Buffers.Source_Buffer_Access);
   --  Assign source buffer to the scanner

   function Get_Source (Self : Scanner'Class)
     return not null Program.Source_Buffers.Source_Buffer_Access;
   --  Get assigned source buffer

   procedure Set_Handler
     (Self    : in out Scanner'Class;
      Handler : not null Program.Scanned_Rule_Handlers.Handler_Access);
   --  Assign rule handler to the scanner

   subtype Start_Condition is Program.Scanner_States.State;

   procedure Set_Start_Condition
    (Self      : in out Scanner'Class;
     Condition : Start_Condition);
   --  Set new start condition to the scanner

   function Get_Start_Condition (Self : Scanner'Class) return Start_Condition;
   --  Get current start condition

   procedure Get_Token
     (Self  : access Scanner'Class;
      Value : out Program.Lexical_Elements.Lexical_Element_Kind);
   --  Scan for next token. The scanner searches for text matching regexps
   --  and calls corresponding routine of rule handler. Rule handler decodes
   --  token kind and return Skip flag. Get_Token returns when Skip = False.

   function Get_Span (Self : Scanner'Class) return Program.Source_Buffers.Span;
   --  Get Span of last token

   package Tables is
      use Program.Scanner_States;

      subtype Code_Point is Natural;

      function To_Class (Value : Code_Point) return Character_Class
        with Inline;

      function Switch (S : State; Class : Character_Class) return State
        with Inline;

      function Rule (S : State) return Rule_Index;
   end Tables;

private

   Buffer_Half_Size : constant := 1024;
   End_Of_Buffer : constant Program.Source_Buffers.Character_Length := 0;

   subtype Buffer_Index is Positive range 1 .. 2 * Buffer_Half_Size;

   Error_Character : constant Program.Scanner_States.Character_Class := 0;
   Error_State     : constant Program.Scanner_States.State :=
     Program.Scanner_States.Error_State;

   type Scanner is tagged limited record
      Handler : Program.Scanned_Rule_Handlers.Handler_Access;
      Source  : Program.Source_Buffers.Source_Buffer_Access;
      Start   : Program.Scanner_States.State := Program.Scanner_States.INITIAL;
      Next    : Buffer_Index := 1;  --  Where we scan Classes buffer
      Offset  : Positive := 1;      --  Corresponding offset in Source_Buffer
      From    : Positive := 1;      --  Start of token in Source_Buffer
      To      : Natural;            --  End of token in Source_Buffer
      EOF     : Natural := 0;
      Classes : Program.Source_Buffers.Character_Info_Array (Buffer_Index) :=
        (1 => (Class => Error_Character, Length => End_Of_Buffer),
         others => <>);
   end record;

   procedure Read_Buffer (Self : in out Scanner'Class);

end Program.Scanners;
