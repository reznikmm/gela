------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-wide_wide_text_io-unbounded_io.ads 2651 2008-05-29 08:12:10Z maxr $

with Ada.Strings.Wide_Wide_Unbounded;

package Ada.Wide_Wide_Text_IO.Unbounded_IO is

   procedure Put
     (File : in File_Type;
      Item : in Strings.Wide_Wide_Unbounded.Wide_Wide_Unbounded_String);

   procedure Put
     (Item : in Strings.Wide_Wide_Unbounded.Wide_Wide_Unbounded_String);

   procedure Put_Line
     (File : in File_Type;
      Item : in Strings.Wide_Wide_Unbounded.Wide_Wide_Unbounded_String);

   procedure Put_Line
     (Item : in Strings.Wide_Wide_Unbounded.Wide_Wide_Unbounded_String);

   function Get_Line
     (File : in File_Type)
     return Strings.Wide_Wide_Unbounded.Wide_Wide_Unbounded_String;

   function Get_Line
     return Strings.Wide_Wide_Unbounded.Wide_Wide_Unbounded_String;

   procedure Get_Line
     (File : in File_Type;
      Item : out Strings.Wide_Wide_Unbounded.Wide_Wide_Unbounded_String);

   procedure Get_Line
     (Item : out Strings.Wide_Wide_Unbounded.Wide_Wide_Unbounded_String);

end Ada.Wide_Wide_Text_IO.Unbounded_IO;

