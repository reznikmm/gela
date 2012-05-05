with Asis;
with Asis.Elements;
with Ada.Containers.Vectors;

with Glib.Values;
with Gtk.Tree_Model.Abstract_Store;
with Ada.Containers.Hashed_Maps;

package Gtk.Tree_Model.Asis_Models is

   type Gtk_Asis_Model_Record is
     new Abstract_Store.Gtk_Abstract_Model_Record with private;

   type Gtk_Asis_Model is
      access all Gtk_Asis_Model_Record'Class;

   procedure Gtk_New (Model : out Gtk_Asis_Model);

   procedure Add
     (Model   : access Gtk_Asis_Model_Record;
      Element : in     Asis.Element);

   procedure Add
     (Model   : access Gtk_Asis_Model_Record;
      Unit    : in     Asis.Compilation_Unit);

   function Get
     (Model : access Gtk_Asis_Model_Record;
      Path  : in     Gtk_Tree_Path)
      return Asis.Element;

   function Get
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in     Gtk_Tree_Iter)
     return Asis.Element;

   function Get
     (Model   : access Gtk_Asis_Model_Record;
      Element : in     Asis.Element)
     return Gtk_Tree_Iter;

   function Children
     (Model  : access Gtk_Asis_Model_Record;
      Parent : in     Gtk_Tree_Iter)
     return Gtk_Tree_Iter;

   function Get_Column_Type
     (Model : access Gtk_Asis_Model_Record;
      Index : in     Gint)
     return GType;

   function Get_Flags
     (Model : access Gtk_Asis_Model_Record)
     return Tree_Model_Flags;

   function Get_Iter
     (Model : access Gtk_Asis_Model_Record;
      Path  : in     Gtk_Tree_Path)
     return Gtk_Tree_Iter;

   function Get_N_Columns
     (Model : access Gtk_Asis_Model_Record)
     return Gint;

   function Get_Path
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in     Gtk_Tree_Iter)
     return Gtk_Tree_Path;

   procedure Get_Value
     (Model  : access Gtk_Asis_Model_Record;
      Iter   : in     Gtk_Tree_Iter;
      Column : in     Gint;
      Value  :    out Glib.Values.GValue);

   function Has_Child
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in     Gtk_Tree_Iter)
     return Boolean;

   procedure Next
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in out Gtk_Tree_Iter);

   function Nth_Child
     (Model  : access Gtk_Asis_Model_Record;
      Parent : in     Gtk_Tree_Iter;
      N      : in     Gint)
     return  Gtk_Tree_Iter;

   function N_Children
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in     Gtk_Tree_Iter := Null_Iter)
     return Gint;

   function Parent
     (Model : access Gtk_Asis_Model_Record;
      Child : in     Gtk_Tree_Iter)
     return Gtk_Tree_Iter;
private

   type Asis_Element;

   type Asis_Element_Access is access all Asis_Element;

   package Vectors is
      new Ada.Containers.Vectors (Positive, Asis_Element_Access);

   type Asis_Element (Is_Unit : Boolean := False) Is record
      Level    : Natural;
      Index    : Positive;
      Parent   : Asis_Element_Access;
      Children : Vectors.Vector;
      case Is_Unit is
         when True =>
            Unit     : Asis.Compilation_Unit;
         when False =>
            Element  : Asis.Element;
      end case;
   end record;

   function Hash (Item : Asis.Element) return Ada.Containers.Hash_Type;

   package Hashed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type     => Asis.Element,
      Element_Type => Asis_Element_Access,
      Hash         => Hash,
      Equivalent_Keys => Asis.Elements.Is_Equal);

   type Gtk_Asis_Model_Record is
     new Abstract_Store.Gtk_Abstract_Model_Record with record
        Root : Asis_Element_Access;
        Map  : Hashed_Maps.Map;
     end record;

end Gtk.Tree_Model.Asis_Models;
