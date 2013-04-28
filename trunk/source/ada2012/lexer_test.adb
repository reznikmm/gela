with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with League.Text_Codecs;

with Gela.Errors.Put_Lines;
with Gela.Mutables.Compilations;
with Gela.Lexical.Handler;
with Gela.Types;
with Gela.Stores.Nodes;
with Gela.Stores.Tokens;
with Gela.Lexical.Tokens;
with Gela.Stores.Productions;

with Gela.Grammars.Reader;
with Gela.Grammars_Convertors;
with Gela.Grammars.Constructors;
with Gela.Grammars.LR_Tables;
with Gela.Grammars.LR.LALR;

procedure Lexer_Test is

   function Read_File
     (File_Name : String)
     return League.Strings.Universal_String;

   procedure Print
     (Comp    : Gela.Mutables.Mutable_Compilation_Access;
      Item    : Gela.Stores.Element_Access;
      Payload : Gela.Types.Payload);

   G     : constant Gela.Grammars.Grammar :=
     Gela.Grammars.Reader.Read ("ada-ast.ag");
   Plain : constant Gela.Grammars.Grammar := G;

   -----------
   -- Print --
   -----------

   procedure Print
     (Comp    : Gela.Mutables.Mutable_Compilation_Access;
      Item    : Gela.Stores.Element_Access;
      Payload : Gela.Types.Payload)
   is
      use type Gela.Types.Payload;
   begin
      if Payload = 0 then
         Ada.Wide_Wide_Text_IO.Put ("null");
      elsif Item.all in Gela.Stores.Tokens.Token then
         declare
            Token : Gela.Stores.Tokens.Token renames
              Gela.Stores.Tokens.Token (Item.all);
         begin
            Ada.Wide_Wide_Text_IO.Put ("Token:");
            Ada.Wide_Wide_Text_IO.Put
              (Gela.Lexical.Tokens.Token'Wide_Wide_Image
                 (Token.Value (Payload)));
         end;
      elsif Item.all in Gela.Stores.Nodes.Node'Class then
         declare
            Node : Gela.Stores.Nodes.Node'Class renames
              Gela.Stores.Nodes.Node'Class (Item.all);
         begin
            if Item.all in Gela.Stores.Productions.Production'Class then
               declare
                  Prod : Gela.Stores.Productions.Production'Class renames
                    Gela.Stores.Productions.Production'Class (Item.all);
               begin
                  Ada.Wide_Wide_Text_IO.Put
                    (Plain.Non_Terminal
                       (Plain.Production
                          (Prod.Production_Index (Payload))
                         .Parent).Name.To_Wide_Wide_String);
                  Ada.Wide_Wide_Text_IO.Put (".");
                  Ada.Wide_Wide_Text_IO.Put
                    (Plain.Production
                       (Prod.Production_Index (Payload))
                         .Name.To_Wide_Wide_String);
               end;
            end if;

            Ada.Wide_Wide_Text_IO.Put ("(");
            for K in 1 .. Node.Last_Child (Payload) loop
               Print (Comp,
                      Comp.Store.Fabric.To_Element (Node.Child (Payload, K)),
                      Node.Child (Payload, K));
            end loop;
            Ada.Wide_Wide_Text_IO.Put (")");
         end;
      else
         Ada.Wide_Wide_Text_IO.Put ("???");
      end if;
   end Print;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (File_Name : String)
     return League.Strings.Universal_String
   is
      Decoder : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec_For_Application_Locale;

      Size : constant Ada.Directories.File_Size :=
        Ada.Directories.Size (File_Name);

      Length : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Count (Size);

      File   : Ada.Streams.Stream_IO.File_Type;
      Data   : Ada.Streams.Stream_Element_Array (1 .. Length);
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, File_Name);
      Ada.Streams.Stream_IO.Read (File, Data, Last);
      Ada.Streams.Stream_IO.Close (File);

      return Decoder.Decode (Data (1 .. Last));
   end Read_File;

   Name    : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("aaa");
   Text    : constant League.Strings.Universal_String := Read_File ("aaa");

   Ada_AG : constant Gela.Grammars.Grammar :=
     Gela.Grammars.Reader.Read ("ada.ag");
   Ada_Plain : constant Gela.Grammars.Grammar_Access :=
     new Gela.Grammars.Grammar'
       (Gela.Grammars_Convertors.Convert (Ada_AG, Left => False));
   Table : constant Gela.Grammars.LR_Tables.Table_Access :=
     new Gela.Grammars.LR_Tables.Table'
       (Gela.Grammars.LR.LALR.Build
            (Gela.Grammars.Constructors.To_Augmented (Ada_Plain.all),
             Right_Nulled => False));

   Comp    : constant Gela.Mutables.Mutable_Compilation_Access :=
     Gela.Mutables.Compilations.Create
       (Name, Text, new Gela.Errors.Put_Lines.Handler, Ada_Plain, Table);

   use type Gela.Types.Payload;
begin
   Gela.Lexical.Handler.Initialize;
   Comp.Start;

   Ada.Wide_Wide_Text_IO.New_Line;
   Ada.Wide_Wide_Text_IO.Put_Line
     (Gela.Lexical.Line_Count'Wide_Wide_Image (Comp.Last_Line));

   if Comp.Root.Payload /= 0 then
      Print
        (Comp,
         Comp.Store.Fabric.To_Element (Comp.Root.Payload),
         Comp.Root.Payload);
   end if;
end Lexer_Test;
