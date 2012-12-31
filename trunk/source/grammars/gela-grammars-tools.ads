------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Grammars.Tools is

   ε : constant Terminal_Count := 0;

   type Terminal_Set_Indexed_By_Production is
     array (Production_Index range <>,
            Terminal_Count   range <>)
     of Boolean;

   type Terminal_Set_Per_Production
     (Last_Terminal   : Terminal_Count;
      Last_Production : Production_Count) is
   record
      Map : Terminal_Set_Indexed_By_Production
        (1 .. Last_Production, 0 .. Last_Terminal);
   end record;

   type Terminal_Set_Indexed_By_Non_Terminal is
     array (Non_Terminal_Index range <>,
            Terminal_Count     range <>)
     of Boolean;

   type Terminal_Set_Per_Non_Terminal
     (Last_Terminal     : Terminal_Count;
      Last_Non_Terminal : Non_Terminal_Count) is
   record
      Map : Terminal_Set_Indexed_By_Non_Terminal
        (1 .. Last_Non_Terminal, 0 .. Last_Terminal);
   end record;

   procedure Get_First
     (Input : Grammar;
      Value : out Terminal_Set_Per_Production);

   procedure Get_First
     (Input : Grammar;
      Value : out Terminal_Set_Per_Non_Terminal);

   procedure Get_Follow
     (Input : Grammar;
      First : Terminal_Set_Per_Non_Terminal;
      Value : out Terminal_Set_Per_Non_Terminal);

end Gela.Grammars.Tools;