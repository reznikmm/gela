with Qt4.Colors;
with Ada_Syntax_Highlighters.Moc;
with Ada.Wide_Text_IO;

with Asis.Extensions.Syntax_Highlight;

package body Ada_Syntax_Highlighters is

   ---------------------
   -- Highlight_Block --
   ---------------------

   procedure Highlight_Block
     (Self : not null access Ada_Syntax_Highlighter;
      Text : in              Qt4.Strings.Q_String)
   is
      use type Qt4.Q_Integer;
      use Asis.Extensions.Syntax_Highlight;

--      Length : constant Qt4.Q_Integer := Qt4.Strings.Length (Text);

      Colors : constant array (Token_Kinds) of Qt4.Colors.Q_Color :=
        (Comment     => Qt4.Colors.Create (0, 128, 128),
         Identifier  => Qt4.Colors.Create (0, 0, 128),
         Literal     => Qt4.Colors.Create (0, 128, 0),
         Delimiter   => Qt4.Colors.Create (128, 128, 0),
         Keyword     => Qt4.Colors.Create (128, 0, 128),
         Error       => Qt4.Colors.Create (255, 0, 0));

      procedure Set_Token
        (From : Positive;
         Count : Positive;
         Kind : Token_Kinds) is
      begin
         Set_Format (Self,
                     Qt4.Q_Integer (From) - 1,  -- zero based index
                     Qt4.Q_Integer (Count),
                     Colors (Kind));
      end Set_Token;

      procedure Go is new Parse (Set_Token);

      State : Integer := Integer (Previous_Block_State (Self));
   begin
      if State = -1 then
         State := Default_State;
      end if;

      Go (Qt4.Strings.To_Utf_16 (Text), State);

      Set_Current_Block_State (Self, Qt4.Q_Integer (State));
      Ada.Wide_Text_IO.Put_Line (">" & Qt4.Strings.To_Utf_16 (Text));
   end Highlight_Block;

   ------------
   -- Create --
   ------------

   function Create
     (Parent : access Qt4.Text_Documents.Q_Text_Document'Class := null)
      return not null Ada_Syntax_Highlighter_Access
   is
      Self : constant not null Ada_Syntax_Highlighter_Access
        := new Ada_Syntax_Highlighter;

   begin
      Qt4.Syntax_Highlighters.Impl.Constructors.Initialize (Self, Parent);

      return Self;
   end Create;

end Ada_Syntax_Highlighters;
