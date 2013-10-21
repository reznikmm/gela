------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Grammars;

package AG_Tools.Input is

   procedure Initialize (File_Name : String);
   --  Read and preprocess attributed grammar from given file.
   --  Grammar should be in speccial form:
   --  * if production contains several parts it should be only production in
   --    non-terminal. Such production named "concrete" here, others are
   --    "abstract"
   --  * abstract productions can not have attribution rules
   --  Preprocessing inlcudes
   --  * turn each optional part into non-optional, keep parts' optionality
   --    outside of resulting grammar (see Is_Option frunction)
   --  * create attribution rules for abstract productions as trivial copy-rule

   function Grammar return Gela.Grammars.Grammar_Access;

   function Is_Concrete (NT : Gela.Grammars.Non_Terminal_Index) return Boolean;
   --  Return true if NT has production with several parts, in other words
   --  there is production in form NT ::= child_1 child_2 ..;
   --  Such NT has just single production

   function Implement (X, Y : Gela.Grammars.Non_Terminal_Index) return Boolean;
   --  If Implement (X, Y) then X implement Y, in other words there is
   --  production  Y := X | ..;

   function Macro_Reference
     (Prod : Gela.Grammars.Production)
      return Gela.Grammars.Non_Terminal_Count;
   --  If Prod is just reference to another NT, return NT.Index.
   --  Return 0 otherwise.

   function Has_List (NT : Gela.Grammars.Non_Terminal_Index) return Boolean;
   --  If there are references to {NT} then Has_List (NT) = True

   function Is_Option
     (G    : Gela.Grammars.Grammar;
      Part : Gela.Grammars.Part) return Boolean;
   --  Return True if Part was option in original grammar

end AG_Tools.Input;
