<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="html" indent="yes" encoding="utf-8"/>

<xsl:template match="/">
<html>
<head/>
    <xsl:apply-templates select="*"/>
</html>
</xsl:template>

<xsl:template match="report">
  <h3>Report. Category: <xsl:value-of select="@category"/></h3>
  <table border='1'>
    <xsl:apply-templates select="*">
      <xsl:sort select="@name"/>
    </xsl:apply-templates>
  </table>
</xsl:template>

<xsl:template match="test">
  <tr>
    <td><xsl:value-of select="@name"/></td>
    <td><xsl:value-of select="@status"/></td>
    <td><xsl:value-of select="@duration"/> s.</td>
    <td><xsl:value-of select="@fixture"/></td>
    <td><xsl:value-of select="@file"/></td>
    <td><pre><xsl:apply-templates select="stdout/text()"/></pre></td>
  </tr>
</xsl:template>

</xsl:stylesheet>
