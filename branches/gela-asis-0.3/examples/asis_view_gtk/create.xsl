<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="text"/>

<xsl:template match="/">
with Gtk.Box;
with Gtk.Enums;
with Gtk.Label;
with Gtk.GEntry;
with Gtk.Combo_Box;
with Gtk.Event_Box;
with Gtk.Image;
with Gtk.Button;

separate (GTK_Asis_Elements)
procedure Create_Frames
  (Root   : in  GTK_Asis_Element)
is
   Frames : GTK_Asis_Element_Frames.GTK_Asis_Element_Frame
     renames Root.Frames;
   Box    : Gtk.Box.Gtk_Box;
   HBox   : Gtk.Box.Gtk_Box;
   Label  : Gtk.Label.Gtk_Label;
   Img    : Gtk.Image.Gtk_Image;
   Button : Gtk.Button.Gtk_Button;
   Event  : Gtk.Event_Box.Gtk_Event_Box;
begin
<xsl:apply-templates select= "//node[@name!='Element_Node']"
         mode="gtk-new"/>
   --
<xsl:apply-templates select= "//node"
         mode="fill"/>
   null;
end;

</xsl:template>
  
<xsl:template match="node" mode="gtk-new"
>   Gtk.Frame.Gtk_New (Frames.<xsl:value-of select="@name"
      />, "<xsl:value-of select="substring-before(@name,'_Node')"/>");
</xsl:template>

<xsl:template match="node" mode="fill"
>   Gtk.Box.Gtk_New_Vbox (Box);
   Gtk.Box.Show (Box);
<xsl:if test="@name!='Element_Node'">
   Gtk.Frame.Add (Frames.<xsl:value-of select="@name"/>, Box);
</xsl:if>
<xsl:if test="@name='Element_Node'">
   Add (Root, Box);
</xsl:if>
<xsl:for-each select="attr"
>      Gtk.Box.Gtk_New_Hbox (HBox);
      Gtk.Label.Gtk_New (Label, "<xsl:value-of select="@name"/>");
      Gtk.Label.Set_Alignment (Label, 0.0, 0.5);
      Gtk.Box.Pack_Start (HBox, Label, False);
      Gtk.Size_Group.Add_Widget (Root.Size, Label);

  <xsl:choose>
    <xsl:when test="substring-before(@type,'_Lists') !=''">
      Gtk.Combo_Box.Gtk_New_Text (Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>);
--      Gtk.Combo_Box.Show (Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>);
      Vector_Data.Set (Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>,
                       Vectors.To_Vector (0));
      Tips_Callback.Connect (Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>, "changed", Combo_Changed'Access, Root.Tips);
      Gtk.Event_Box.Gtk_New (Event);
      Gtk.Event_Box.Add (Event, Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>);
--      Gtk.Event_Box.Show (Event);
      Gtk.Box.Pack_Start (HBox, Event, False);
    </xsl:when>

    <xsl:otherwise>
      Gtk.GEntry.Gtk_New (Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>);
--      Gtk.GEntry.Show (Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>);
      Gtk.Box.Pack_Start (HBox, Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>, False);
    </xsl:otherwise>
  </xsl:choose>

      Gtk.Size_Group.Add_Widget (Root.Size_2, Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>);
      Gtk.Box.Pack_Start (Box, HBox, False);
  <xsl:choose>
    <xsl:when test="@name = 'Is_Name_Repeated'
                    or @name = 'Is_Private_Present'" />

    <xsl:when test="substring-before(@type,'_Kinds') !=''
         or @type = 'Boolean'
         or @type='Asis.ASIS_Integer'
         or @type='Asis.Declaration_Origins' "/>
    <xsl:when test="@type = 'Unbounded_Wide_String'
         or @type ='Asis.Compilation_Unit'" />

    <xsl:when test="substring-before(@type,'_Lists') !=''">
      Gtk.Button.Gtk_New (Button);
      Gtk.Image.Gtk_New (Img,
                         Stock_Id => "gtk-jump-to",
--                         Stock_Id => "gtk-go-forward",
                         Size => Gtk.Enums.Icon_Size_Menu);
      Gtk.Button.Set_Image (Button, Img);
      Combo_Callback.Connect (Button, "clicked", Go_To_List_Element'Access, Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>);
      Gtk.Box.Pack_Start (HBox, Button, False, False);
--      Gtk.Button.Show (Button);
    </xsl:when>

    <xsl:when test="@type ='Asis.Text.Span'" />

    <xsl:otherwise>
      Gtk.Button.Gtk_New (Button);
      Gtk.Image.Gtk_New (Img,
                         Stock_Id => "gtk-jump-to",
--                         Stock_Id => "gtk-go-forward",
                         Size => Gtk.Enums.Icon_Size_Menu);
      Gtk.Button.Set_Image (Button, Img);
      Entry_Callback.Connect (Button, "clicked", Go_To_Element'Access, Frames.<xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>);
      Gtk.Box.Pack_Start (HBox, Button, False, False);
--      Gtk.Button.Show (Button);
    </xsl:otherwise>
  </xsl:choose>

  <xsl:if test="@name!='References'">
    Gtk.Box.Show_All (HBox);
  </xsl:if>

</xsl:for-each>
<xsl:for-each select="node"
>      Gtk.Box.Pack_Start (Box, Frames.<xsl:value-of select="@name"/>, False);
</xsl:for-each>
</xsl:template>

</xsl:stylesheet>
