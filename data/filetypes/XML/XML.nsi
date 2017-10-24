<?xml version="1.0" encoding="ISO-8859-1"?>

<NotesSmartIndent>

  <triggers>
      <trigger>&lt;</trigger>
      <trigger>&gt;</trigger>
  </triggers>

  <rule match="^[ \t]*&lt;[^/][^&lt;]*[^\\][ \t]*&gt;[ \t]*$" nextline="inc" curline="keep" />
  
  <rule match="^[ \t]*&lt;/[^&lt;]*[^\\][ \t]*&gt;[ \t]*$" nextline="keep" curline="dec" />

</NotesSmartIndent>