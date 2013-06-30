------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with XML.SAX.Pretty_Writers;
with Gela.Mutables.Compilations;
pragma Unreferenced (Gela.Mutables.Compilations);
with XML.SAX.Attributes;
with Gela.Types;
with Gela.Stores.Nodes;
with Gela.Stores.Tokens;
with Gela.Lexical.Tokens;
with Gela.Stores.Productions;

package body Gela.Mutables.To_XML is

   Compilation_Image : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("compilation");
   Error_Image : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("ERROR");
   List_Image : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("list");
   Name_Image : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("name");
   Null_Image : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("null");
   Token_Image : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("token");

   procedure Print
     (W       : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer;
      Comp    : Gela.Mutables.Mutable_Compilation_Access;
      AST     : Gela.Grammars.Grammar;
      Item    : Gela.Stores.Element_Access;
      Payload : Gela.Types.Payload);

   function Compilation
     (C   : Gela.Mutables.Mutable_Compilation_Access;
      AST : Gela.Grammars.Grammar)
      return League.Strings.Universal_String
   is
      W    : XML.SAX.Pretty_Writers.SAX_Pretty_Writer;
      List : XML.SAX.Attributes.SAX_Attributes;
   begin
      W.Set_Offset (2);
      List.Set_Value (Name_Image, C.Text_Name);
      W.Start_Element
        (Qualified_Name => Compilation_Image,
         Attributes     => List);
      Print (W, C, AST,
             Gela.Stores.Element_Access (C.Root.Object), C.Root.Payload);
      W.End_Element (Qualified_Name => Compilation_Image);
      return W.Text;
   end Compilation;

   procedure Print
     (W       : in out XML.SAX.Pretty_Writers.SAX_Pretty_Writer;
      Comp    : Gela.Mutables.Mutable_Compilation_Access;
      AST     : Gela.Grammars.Grammar;
      Item    : Gela.Stores.Element_Access;
      Payload : Gela.Types.Payload)
   is
      use type Gela.Types.Payload;
   begin
      if Payload = 0 then
         W.Start_Element (Qualified_Name => Null_Image);
         W.End_Element (Qualified_Name => Null_Image);
      elsif Item.all in Gela.Stores.Tokens.Token then
         declare
            List  : XML.SAX.Attributes.SAX_Attributes;
            Token : Gela.Stores.Tokens.Token renames
              Gela.Stores.Tokens.Token (Item.all);
         begin
            List.Set_Value
              (Token_Image,
               League.Strings.To_Universal_String
                 (Gela.Lexical.Tokens.Token'Wide_Wide_Image
                    (Token.Value (Payload))));
            W.Start_Element (Token_Image, List);
            W.End_Element (Qualified_Name => Token_Image);
         end;
      elsif Item.all in Gela.Stores.Nodes.Node'Class then
         declare
            Name : League.Strings.Universal_String;
            Node : Gela.Stores.Nodes.Node'Class renames
              Gela.Stores.Nodes.Node'Class (Item.all);
         begin
            if Item.all in Gela.Stores.Productions.Production'Class then
               declare
                  Prod : Gela.Stores.Productions.Production'Class renames
                    Gela.Stores.Productions.Production'Class (Item.all);
               begin
                  Name := AST.Non_Terminal
                    (AST.Production (Prod.Production_Index (Payload))
                     .Parent).Name;
                  Name.Append (".");
                  Name.Append
                    (AST.Production (Prod.Production_Index (Payload)).Name);
               end;
            else
               Name := List_Image;
            end if;

            W.Start_Element (Name);

            for K in 1 .. Node.Last_Child (Payload) loop
               Print (W, Comp, AST,
                      Comp.Store.Fabric.To_Element (Node.Child (Payload, K)),
                      Node.Child (Payload, K));
            end loop;

            W.End_Element (Name);
         end;
      else
         W.Start_Element (Error_Image);
         W.End_Element (Error_Image);
      end if;
   end Print;

end Gela.Mutables.To_XML;
