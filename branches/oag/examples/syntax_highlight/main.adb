------------------------------------------------------------------------------
--                                                                          --
--                An Ada2005 binding to the Qt C++ framework                --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2008 Vadim Godunko <vgodunko@gmail.com>                      --
--                                                                          --
-- QtAda is free software;  you can  redistribute it and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. QtAda is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with QtAda;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------
--  $Revision: 2784 $ $Date: 2008-08-10 21:36:09 +0400 (Sun, 10 Aug 2008) $
------------------------------------------------------------------------------
with Qt_Ada.Application;
with Qt4.Core_Applications;
with Qt4.Fonts;
with Qt4.Objects;
with Qt4.Plain_Text_Edits.Constructors;
with Qt4.Strings;

with Ada_Syntax_Highlighters;

procedure Main is
   Editor      : Qt4.Plain_Text_Edits.Q_Plain_Text_Edit_Access;
   Highlighter : Ada_Syntax_Highlighters.Ada_Syntax_Highlighter_Access;

begin
   Qt_Ada.Application.Initialize;

   Editor :=
     Qt4.Plain_Text_Edits.Constructors.Create
      (Qt4.Strings.From_Utf_16 ("Quit"));
   Highlighter := Ada_Syntax_Highlighters.Create (Editor.Document);

   Editor.Resize (800, 600);
   Editor.Show;

   Qt_Ada.Application.Execute;
end Main;
