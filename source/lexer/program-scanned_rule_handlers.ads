--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

limited with Program.Scanners;
with Program.Scanner_Destinations;
with Program.Scanner_States;

package Program.Scanned_Rule_Handlers is
   pragma Pure;

   type Handler is abstract tagged limited null record;

   procedure Delimiter
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Identifier
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Obsolescent_Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Character_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Obsolescent_String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Comment
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Space
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure New_Line
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Error
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean) is abstract;

   type Handler_Access is access all Handler'Class with Storage_Size => 0;

end Program.Scanned_Rule_Handlers;
