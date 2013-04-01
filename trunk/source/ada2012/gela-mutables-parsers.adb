------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Grammars.Constructors;
with Gela.Grammars.LR.LALR;
with Gela.Grammars.LR_Tables;
with Gela.Grammars.RNGLR;
with Gela.Grammars.Reader;
with Gela.Grammars_Convertors;

with Gela.Mutables.Compilations;
pragma Unreferenced (Gela.Mutables.Compilations);

package body Gela.Mutables.Parsers is

   function New_Token (Self : access Parser) return Gela.Types.Payload;

   function New_Node
     (Self       : access Parser;
      Production : Gela.Grammars.Production_Index) return Gela.Types.Payload;

   function New_Alternative
     (Self : access Parser;
      NT   : Gela.Grammars.Non_Terminal_Index) return Gela.Types.Payload;

   procedure Set_Child
     (Self   : access Parser;
      Object : Gela.Types.Payload;
      Index  : Positive;
      Value  : Gela.Types.Payload);

   procedure Reference
     (Self   : access Parser;
      Object : Gela.Types.Payload);

   procedure Dereference
     (Self   : access Parser;
      Object : in out Gela.Types.Payload);

   package RNGLR is new Gela.Grammars.RNGLR
     (Node_Access => Gela.Types.Payload,
      Null_Node   => 0,
      Node_Fabric => Parser);

   type Grammar_Access is access all Gela.Grammars.Grammar;
   type Table_Access is access all Gela.Grammars.LR_Tables.Table;

   G : Grammar_Access;
   T : Table_Access;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference
     (Self   : access Parser;
      Object : in out Gela.Types.Payload) is
   begin
      Self.Compilation.Fabric.Token.Dereference (Object);
   end Dereference;

   ---------------------
   -- New_Alternative --
   ---------------------

   function New_Alternative
     (Self : access Parser;
      NT   : Gela.Grammars.Non_Terminal_Index) return Gela.Types.Payload
   is
   begin
      raise Program_Error with "Unimplemented function New_Alternative";
      return New_Alternative (Self, NT);
   end New_Alternative;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Self       : access Parser;
      Production : Gela.Grammars.Production_Index) return Gela.Types.Payload is
   begin
      return Self.Compilation.Fabric.Create_Production (Production);
   end New_Node;

   ---------------
   -- New_Token --
   ---------------

   function New_Token (Self : access Parser) return Gela.Types.Payload is
   begin
      return Self.Compilation.Lexer.Last_Token;
   end New_Token;

   -----------
   -- Parse --
   -----------

   procedure Parse (Self : access Parser) is
      Root : Gela.Types.Payload;
   begin
      if G = null then
         declare
            Ada_AG : constant Gela.Grammars.Grammar :=
              Gela.Grammars.Reader.Read ("ada.ag");
            Plain : constant Gela.Grammars.Grammar :=
              Gela.Grammars_Convertors.Convert_With_Empty (Ada_AG);
         begin
            G := new Gela.Grammars.Grammar'
              (Gela.Grammars.Constructors.To_Augmented (Plain));
            T := new Gela.Grammars.LR_Tables.Table'
              (Gela.Grammars.LR.LALR.Build (G.all, Right_Nulled => True));
         end;
      end if;

      Self.Free_Lists := (others => 0);

      RNGLR.Parse
        (G.all, T.all, Self, L => Self.Compilation.Lexer, Tree => Root);
   end Parse;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self   : access Parser;
      Object : Gela.Types.Payload) is
   begin
      Self.Compilation.Fabric.Token.Reference (Object);
   end Reference;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Self   : access Parser;
      Object : Gela.Types.Payload;
      Index  : Positive;
      Value  : Gela.Types.Payload) is
   begin
      Self.Compilation.Fabric.Token.Set_Child (Object, Index, Value);
   end Set_Child;

end Gela.Mutables.Parsers;
