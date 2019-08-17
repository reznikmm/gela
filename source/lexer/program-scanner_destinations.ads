--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Source_Buffers;

package Program.Scanner_Destinations is
   pragma Pure;

   subtype Token_Kind is Program.Lexical_Elements.Lexical_Element_Kind;

   type Token is record
      Span : Program.Source_Buffers.Span;
      Kind : Token_Kind;
   end record;

   type Scanner_Destination is limited interface;
   --  Type to report result of lexic analysis

   not overriding procedure New_Token
     (Self  : in out Scanner_Destination;
      Token : Program.Scanner_Destinations.Token) is abstract;
   --  Called when new token found during lexic analysis

   not overriding procedure New_Line
     (Self  : in out Scanner_Destination;
      Line  : Positive) is abstract;
   --  Called when new end of line found during lexic analysis

end Program.Scanner_Destinations;
