--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Scanned_Rule_Handlers;
with Program.Scanner_States;
with Program.Scanners;
with Program.Scanner_Destinations;

package Program.Lexical_Handlers is
   pragma Pure;

   type Handler
     (Output  : not null access
        Program.Scanner_Destinations.Scanner_Destination'Class)
   is new Program.Scanned_Rule_Handlers.Handler with private;

   overriding procedure Identifier
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Obsolescent_Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Character_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Obsolescent_String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Comment
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Space
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure New_Line
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Error
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Delimiter
     (Self    : not null access Handler;
      Scanner : not null access Program.Scanners.Scanner'Class;
      Rule    : Program.Scanner_States.Rule_Index;
      Token   : out Program.Scanner_Destinations.Token_Kind;
      Skip    : in out Boolean);

private

   type Handler
     (Output  : not null access
        Program.Scanner_Destinations.Scanner_Destination'Class)
     is new Program.Scanned_Rule_Handlers.Handler with
   record
      Last_Token : Program.Scanner_Destinations.Token_Kind :=
        Program.Scanner_Destinations.Token_Kind'First;
      Line       : Positive := 1;
   end record;

end Program.Lexical_Handlers;
