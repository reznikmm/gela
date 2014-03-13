------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with Gela.Grammars;

package AG_Tools is
   pragma Preelaborate;

   function To_Ada
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;
   --  Convert Text into an Ada identifier

   function Plural
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;
   --  Get Plural form of noun Text

   function Return_Type
     (G    : Gela.Grammars.Grammar;
      Part : Gela.Grammars.Part)
      return League.Strings.Universal_String;
   --  Get identifier of type of given Part

   function Return_Type
     (G  : Gela.Grammars.Grammar;
      NT : Gela.Grammars.Non_Terminal)
      return League.Strings.Universal_String;
   --  Get identifier of type of given Part

   function Package_Name
     (Name : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Is_Converted_List
     (G  : Gela.Grammars.Grammar;
      NT : Gela.Grammars.Non_Terminal) return Boolean;
   --  Detect if NT was a list before converting to plain AG

   function List_Item
     (G : Gela.Grammars.Grammar;
      NT : Gela.Grammars.Non_Terminal)
      return Gela.Grammars.Non_Terminal_Index;
   --  Return items non-terminal for list, like NT={item}

end AG_Tools;
