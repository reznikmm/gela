with Qt4.Objects;
with Qt4.Strings;
with Qt4.Syntax_Highlighters.Impl;
with Qt4.Text_Documents;

package Ada_Syntax_Highlighters is

   type Ada_Syntax_Highlighter is new
     Qt4.Syntax_Highlighters.Impl.Q_Syntax_Highlighter_Impl with null record;

   overriding
   procedure Highlight_Block
    (Self : not null access Ada_Syntax_Highlighter;
     Text : in              Qt4.Strings.Q_String);

   type Ada_Syntax_Highlighter_Access is access Ada_Syntax_Highlighter;

   function Create
     (Parent : access Qt4.Text_Documents.Q_Text_Document'Class := null)
      return not null Ada_Syntax_Highlighter_Access;

--   pragma Q_Object
--     (Ada_Syntax_Highlighter,
--      Ada_Syntax_Highlighter,
--      "Ada_Syntax_Highlighter");
end;
