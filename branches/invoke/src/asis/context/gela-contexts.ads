------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Types;
with Gela.Unit_Containers;

package Gela.Contexts is
   pragma Preelaborate;

   type Context is limited interface
     and Gela.Unit_Containers.Unit_Container;

   procedure Associate
     (Self       : access Context;
      Name       : League.Strings.Universal_String;
      Parameters : League.Strings.Universal_String) is abstract;

   procedure Open (Self : access Context) is abstract;

   function Is_Open (Self : access Context) return Boolean is abstract;

   procedure Close (Self : access Context) is abstract;

   procedure Dissociate (Self : access Context) is abstract;

   function Name
     (Self : access Context)
      return League.Strings.Universal_String is abstract;

   function Parameters
     (Self : access Context)
      return League.Strings.Universal_String is abstract;

   function Length (Self : access Context) return Natural is abstract;

   function Container
     (Self  : access Context;
      Index : Positive)
      return Gela.Types.Container_Access is abstract;

   function Debug_Image
     (Self : access Context)
      return League.Strings.Universal_String is abstract;

   function Corresponding_Children
     (Self   : access Context;
      Parent : Gela.Types.Compilation_Unit)
      return Gela.Types.Compilation_Unit_List is abstract;

end Gela.Contexts;
