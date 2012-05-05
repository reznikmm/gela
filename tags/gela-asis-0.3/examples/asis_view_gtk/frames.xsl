<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="text"/>

<xsl:template match="/">
with Gtk.Frame;
with Gtk.GEntry;
with Gtk.Combo_Box;

package GTK_Asis_Element_Frames is

   type GTK_Asis_Element_Frame is record
      <xsl:apply-templates select="//node[@name!='Element_Node']"/>
      <xsl:apply-templates select="//attr"/>
   end record;

end;

</xsl:template>
  
<xsl:template match="node" >
<xsl:value-of select="@name"/> : Gtk.Frame.Gtk_Frame;
      </xsl:template>

<xsl:template match="attr" >
  <xsl:value-of select="../@name"/>_<xsl:value-of select="@name"/>
  <xsl:text> : </xsl:text>
  <xsl:choose>
    <xsl:when test="substring-before(@type,'_Lists') !=''"
      >Gtk.Combo_Box.Gtk_Combo_Box;
</xsl:when>
    <xsl:otherwise>Gtk.GEntry.Gtk_Entry;
</xsl:otherwise>
  </xsl:choose>

</xsl:template>

</xsl:stylesheet>
