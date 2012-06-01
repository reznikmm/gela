<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="html" indent="yes" encoding="utf-8"/>

<xsl:template match="/">
<html>
  <head>
    <style type="text/css">
tr.out {
display: none;
background-color: #F0FFF0;
}
tr.success {
background-color: #F0FFF0;
}
tr.failure {
background-color: #FFF0F0;
}
tr.error {
background-color: #FFE0E0;
}
tr.ignore {
background-color: #F0F0F0;
}
    </style>
<script>
  function popup(name) {
    if (document.getElementById(name).style.display != 'block')
       document.getElementById(name).style.display = 'block';
    else
       document.getElementById(name).style.display = 'none';
  }
</script> 
  </head>
  <body>
    <xsl:apply-templates select="*"/>
  </body>
</html>
</xsl:template>

<xsl:template match="report">
  <h3>Report. Category: <xsl:value-of select="@category"/></h3>

  <h4>Totals:</h4>
  <table border='1'>
    <tr class="success"><td>Success</td><td>
      <xsl:value-of select="count(test[@status='success'])"/>
    </td></tr>
    <tr class="failure"><td>Failure</td><td>
      <xsl:value-of select="count(test[@status='failure'])"/>
    </td></tr>
    <tr class="error"><td>Error</td><td>
      <xsl:value-of select="count(test[@status='error'])"/>
    </td></tr>
    <tr class="ignore"><td>Ignore</td><td>
      <xsl:value-of select="count(test[@status='ignore'])"/>
    </td></tr>
  </table>

  <h4>List:</h4>
  <table border='1'>
    <tr>
      <th>Name</th>
      <th>Status</th>
      <th>Duration</th>
      <th>Fixture</th>
      <th>File</th>
    </tr>
    <xsl:apply-templates select="*">
      <xsl:sort select="@name"/>
    </xsl:apply-templates>
  </table>
</xsl:template>

<xsl:template match="test">
  <tr id="{@name}" class="{@status}" onclick="popup('{@name}-out')">
    <td><xsl:value-of select="@name"/></td>
    <td><xsl:value-of select="@status"/></td>
    <td align="right">
    <xsl:value-of select="format-number(@duration,'#.##')"/> s.</td>
    <td><xsl:value-of select="@fixture"/></td>
    <td><xsl:value-of select="@file"/></td>
  </tr>
  <tr id="{concat(@name,'-out')}" class="out">
    <td colspan="5">
    <pre><xsl:apply-templates select="stdout/text()"/></pre>
    </td>
  </tr>
</xsl:template>

</xsl:stylesheet>
