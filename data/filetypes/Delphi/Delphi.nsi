<?xml version="1.0" encoding="ISO-8859-1"?>

<NotesSmartIndent skip="^[ \t]*(//|\{|\(\*)" casesensitive="false">

  <triggers>
      <trigger>begin</trigger>
      <trigger>end</trigger>
      <trigger>end;</trigger>
      <trigger>end.</trigger>
      <trigger>finally</trigger>
      <trigger>except</trigger>
      <trigger>else</trigger>
      <trigger>until</trigger>
      <trigger>record</trigger>
      <trigger>private</trigger>
      <trigger>public</trigger>
      <trigger>protected</trigger>
      <trigger>published</trigger>
      <trigger>class</trigger>
      <trigger>interface</trigger>
  </triggers>

  <!-- if ... then -->
  <rule match="^[ \t]*(.*if|try|repeat)" nextline="inc" curline="keep" />
  <!-- end else, else, begin e finally  -->
  <rule match="^[ \t]*(end[ \t]*)?(begin|finally|else|except|asm)" nextline="inc" curline="dec" />
  <!-- for .. do, while..do -->
  <rule match="^[ \t]*(for|while|with).+do" nextline="inc" curline="keep" />
  <!-- uses, program, unit, end., interface, type, implementation, initialization, finalization -->
  <rule match="^[ \t]*(end\.|var|const|interface|type|implementation|initialization|finalization|uses|unit|program)" nextline="inc" curline="clear" />
  <!-- end; e end -->
  <rule match="^[ \t]*(end|until)" nextline="keep" curline="dec" />
  <!-- class declarations, they should be at two spaces from the left -->
  <rule match="(private|record|public|protected|published|class|interface)" nextline="inc" curline="one" />
</NotesSmartIndent>