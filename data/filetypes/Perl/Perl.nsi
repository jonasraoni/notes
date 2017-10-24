<?xml version="1.0" encoding="ISO-8859-1"?>

<NotesSmartIndent skip="^[ \t]*/[\*/]">

  <triggers>
      <trigger>}</trigger>
      <trigger>{</trigger>
  </triggers>

  <!-- } finally {, } else {, } else if {  -->
  <rule match="^[ \t]*\}[ \t]*(finally|catch|else)" nextline="inc" curline="dec" />
  <!-- finally {, else {, else if {  -->
  <rule match="^[ \t]*(finally|catch|else)" nextline="inc" curline="keep" />
  <!-- } while ();  -->
  <rule match="^[ \t]*\}[ \t]*(while).*\(.*\).*;" nextline="keep" curline="dec" />
  <!-- if () {, try, etc.  -->
  <rule match="\{[ \t]*$" nextline="inc" curline="keep" />
  <!-- case .. : -->
  <rule match="(case).*:[ \t]*$" nextline="inc" curline="keep" />
  <!-- expr;} -->
  <rule match="^[ \t]*[a-zA-Z]+.*\}[ \t]*$" nextline="dec" curline="keep" />
  <!-- break; -->
  <rule match="^[ \t]*(break;)[ \t]*$" nextline="dec" curline="keep" />
  <!-- } -->
  <rule match="^[ \t]*\}[ \t]*$" nextline="keep" curline="dec" />

</NotesSmartIndent>