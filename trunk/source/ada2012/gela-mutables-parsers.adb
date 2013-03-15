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
with Gela.Properties;
with Gela.Relocatable_Arrays;

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
      Object : Gela.Types.Payload;
      Step   : Integer := 1);

   package RNGLR is new Gela.Grammars.RNGLR
     (Node_Access => Gela.Types.Payload,
      Null_Node   => 0,
      Node_Fabric => Parser);

   type Grammar_Access is access all Gela.Grammars.Grammar;
   type Table_Access is access all Gela.Grammars.LR_Tables.Table;

   G : Grammar_Access;
   T : Table_Access;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Self   : access Parser;
      Object : Gela.Types.Payload)
      return access Gela.Mutables.Elements.Element'Class
   is
      Tag : constant Natural := Self.Production.Tag (Object);
   begin
      if Tag in 1 .. Natural (G.Last_Production) then
         return Self.Production'Access;
      else
         return Self.Compilation.Token'Access;
      end if;
   end Get_Element;

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
      Production : Gela.Grammars.Production_Index) return Gela.Types.Payload
   is
      use type Gela.Types.Payload;
      use type Gela.Relocatable_Arrays.Index;

      Size  : constant Gela.Relocatable_Arrays.Index :=
        Gela.Properties.Size (Production);

      Result : Gela.Types.Payload;
   begin
      Result := Self.Free_Lists (Positive (Size));

      if Result = 0 then
         Result := Gela.Types.Payload
           (Gela.Relocatable_Arrays.Allocate (Self.Compilation.Store, Size));
      else
         Self.Free_Lists (Positive (Size)) :=
           Self.Production.Free_List_Link (Result);
      end if;

      Self.Production.Set_Tag (Result, Natural (Production));
      Self.Production.Set_Count (Result, 1);

      return Result;
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
      Object : Gela.Types.Payload;
      Step   : Integer := 1)
   is
      use type Gela.Relocatable_Arrays.Index;
      use type Gela.Relocatable_Arrays.Element;

      Count : constant Natural := Self.Production.Count (Object);
   begin
      if Count = 1 and Step = -1 then
         declare
            use type Gela.Types.Payload;
            Element : constant access Gela.Mutables.Elements.Element'Class :=
              Self.Get_Element (Object);
            Child   : Gela.Types.Payload;
         begin
            for J in 1 .. Element.Last_Child (Object) loop
               Child := Element.Child (Object, J);

               if Child /= 0 then
                  Reference (Self, Child, -1);
               end if;
            end loop;

            Element.Set_Free_List_Link
              (Object, Self.Free_Lists (Element.Size (Object)));

            Self.Free_Lists (Element.Size (Object)) := Object;
         end;
      else
         Self.Production.Set_Count (Object, Count + Step);
      end if;
   end Reference;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Self   : access Parser;
      Object : Gela.Types.Payload;
      Index  : Positive;
      Value  : Gela.Types.Payload)
   is
      Element : constant access Gela.Mutables.Elements.Element'Class :=
        Self.Get_Element (Object);
   begin
      Element.Set_Child (Object, Index, Value);

      Reference (Self, Value);
   end Set_Child;

end Gela.Mutables.Parsers;
