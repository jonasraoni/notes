<?xml version="1.0" encoding="ISO-8859-1"?>

<NotesSmartIndent skip="^[ \t]*/[\*/]">

  <triggers>
      <trigger>}</trigger>
      <trigger>{</trigger>
  </triggers>

  <!-- if () {, try, etc.  -->
  <rule match="\{[ \t]*$" nextline="inc" curline="keep" />
  <!-- expr;} -->
  <rule match="^[ \t]*[a-zA-Z]+.*\}[ \t]*$" nextline="dec" curline="keep" />
  <!-- } -->
  <rule match="^[ \t]*\}[ \t]*$" nextline="keep" curline="dec" />

</NotesSmartIndent>