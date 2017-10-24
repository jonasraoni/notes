(*

  SmallTidyWraper - wraper para a tidylib baseado
  no TidyPas, só quem sem o monte de partes inúteis
  e arquivos de includes que ele tem :)
  
  Por Anderson R. Barbieri, 2004.

  Arquivo baseado na biblioteca TidyPas (c) 2002 by Jeffrey Pohlmeyer.
  HTML-Tidy (c) 1998-2004 World Wide Web Consortium.

  COPYRIGHT NOTICE:

    This software and documentation is provided "as is," and
    the copyright holders and contributing author(s) make no
    representations or warranties, express or implied, including
    but not limited to, warranties of merchantability or fitness
    for any particular purpose or that the use of the software or
    documentation will not infringe any third party patents,
    copyrights, trademarks or other rights.
    
    The copyright holders and contributing author(s) will not be held
    liable for any direct, indirect, special or consequential damages
    arising out of any use of the software or documentation, even if
    advised of the possibility of such damage.
    
    Permission is hereby granted to use, copy, modify, and distribute
    this source code, or portions hereof, documentation and executables,
    for any purpose, without fee, subject to the following restrictions:
    
    1. The origin of this source code must not be misrepresented.
    2. Altered versions must be plainly marked as such and must
       not be misrepresented as being the original source.
    3. This Copyright notice may not be removed or altered from any
       source or altered source distribution.
    
    The copyright holders and contributing author(s) specifically
    permit, without fee, and encourage the use of this source code
    as a component for supporting the Hypertext Markup Language in
    commercial products. If you use this source code in a product,
    acknowledgment is not required but would be appreciated.
    
*)

unit SmallTidyWraper;

interface
{$IFDEF FPC}
  {$MODE OBJFPC}
  {$M+}
  {$H+}
  uses strings;
{$ELSE}
  uses SysUtils, Classes;
{$ENDIF}


{$IFDEF FPC}
  {$PACKRECORDS C}
  {$IFDEF LINUX}
    {$LINKLIB tidy}
  {$ENDIF}
{$ENDIF}

{$IFDEF WIN32}
  const
    TIDY_LIB = 'libtidy.dll';
    TIDY_NULL_FILE = 'NUL';
    LIB_C    = 'msvcrt.dll';
{$ELSE}
  const
    TIDY_LIB = 'libtidy.so';
    TIDY_NULL_FILE = '/dev/null';
    LIB_C    = 'libc.so.6';
{$ENDIF}

// DE tidyenum.inc

type TidyReportLevel = ( TidyInfo, TidyWarning, TidyConfig, TidyAccess, TidyError, TidyBadDocument, TidyFatal );

type  TidyNodeType = (
  TidyNode_Root,    TidyNode_DocType,  TidyNode_Comment,
  TidyNode_ProcIns, TidyNode_Text,     TidyNode_Start,
  TidyNode_End,     TidyNode_StartEnd, TidyNode_CDATA,
  TidyNode_Section, TidyNode_Asp,      TidyNode_Jste,
  TidyNode_Php,     TidyNode_XmlDecl  );

type TidyTagId = (
  TidyTag_UNKNOWN,    TidyTag_A,        TidyTag_ABBR,
  TidyTag_ACRONYM,    TidyTag_ADDRESS,  TidyTag_ALIGN,
  TidyTag_APPLET,     TidyTag_AREA,     TidyTag_B,
  TidyTag_BASE,       TidyTag_BASEFONT, TidyTag_BDO,
  TidyTag_BGSOUND,    TidyTag_BIG,      TidyTag_BLINK,
  TidyTag_BLOCKQUOTE, TidyTag_BODY,     TidyTag_BR,
  TidyTag_BUTTON,     TidyTag_CAPTION,  TidyTag_CENTER,
  TidyTag_CITE,       TidyTag_CODE,     TidyTag_COL,
  TidyTag_COLGROUP,   TidyTag_COMMENT,  TidyTag_DD,
  TidyTag_DEL,        TidyTag_DFN,      TidyTag_DIR,
  TidyTag_DIV,        TidyTag_DL,       TidyTag_DT,
  TidyTag_EM,         TidyTag_EMBED,    TidyTag_FIELDSET,
  TidyTag_FONT,       TidyTag_FORM,     TidyTag_FRAME,
  TidyTag_FRAMESET,   TidyTag_H1,       TidyTag_H2,
  TidyTag_H3,         TidyTag_H4,       TidyTag_H5,
  TidyTag_H6,         TidyTag_HEAD,     TidyTag_HR,
  TidyTag_HTML,       TidyTag_I,        TidyTag_IFRAME,
  TidyTag_ILAYER,     TidyTag_IMG,      TidyTag_INPUT,
  TidyTag_INS,        TidyTag_ISINDEX,  TidyTag_KBD,
  TidyTag_KEYGEN,     TidyTag_LABEL,    TidyTag_LAYER,
  TidyTag_LEGEND,     TidyTag_LI,       TidyTag_LINK,
  TidyTag_LISTING,    TidyTag_MAP,      TidyTag_MARQUEE,
  TidyTag_MENU,       TidyTag_META,     TidyTag_MULTICOL,
  TidyTag_NOBR,       TidyTag_NOEMBED,  TidyTag_NOFRAMES,
  TidyTag_NOLAYER,    TidyTag_NOSAVE,   TidyTag_NOSCRIPT,
  TidyTag_OBJECT,     TidyTag_OL,       TidyTag_OPTGROUP,
  TidyTag_OPTION,     TidyTag_P,        TidyTag_PARAM,
  TidyTag_PLAINTEXT,  TidyTag_PRE,      TidyTag_Q,
  TidyTag_RB,         TidyTag_RBC,      TidyTag_RP,
  TidyTag_RT,         TidyTag_RTC,      TidyTag_RUBY,
  TidyTag_S,          TidyTag_SAMP,     TidyTag_SCRIPT,
  TidyTag_SELECT,     TidyTag_SERVER,   TidyTag_SERVLET,
  TidyTag_SMALL,      TidyTag_SPACER,   TidyTag_SPAN,
  TidyTag_STRIKE,     TidyTag_STRONG,   TidyTag_STYLE,
  TidyTag_SUB,        TidyTag_SUP,      TidyTag_TABLE,
  TidyTag_TBODY,      TidyTag_TD,       TidyTag_TEXTAREA,
  TidyTag_TFOOT,      TidyTag_TH,       TidyTag_THEAD,
  TidyTag_TITLE,      TidyTag_TR,       TidyTag_TT,
  TidyTag_U,          TidyTag_UL,       TidyTag_VAR,
  TidyTag_WBR,        TidyTag_XMP,      N_TIDY_TAGS);

type   TidyAttrId = (
TidyAttr_UNKNOWN,           TidyAttr_ABBR,            TidyAttr_ACCEPT,
TidyAttr_ACCEPT_CHARSET,    TidyAttr_ACCESSKEY,       TidyAttr_ACTION,
TidyAttr_ADD_DATE,          TidyAttr_ALIGN,           TidyAttr_ALINK,
TidyAttr_ALT,               TidyAttr_ARCHIVE,         TidyAttr_AXIS,
TidyAttr_BACKGROUND,        TidyAttr_BGCOLOR,         TidyAttr_BGPROPERTIES,
TidyAttr_BORDER,            TidyAttr_BORDERCOLOR,     TidyAttr_BOTTOMMARGIN,
TidyAttr_CELLPADDING,       TidyAttr_CELLSPACING,     TidyAttr_CHAR,
TidyAttr_CHAROFF,           TidyAttr_CHARSET,         TidyAttr_CHECKED,
TidyAttr_CITE,              TidyAttr_CLASS,           TidyAttr_CLASSID,
TidyAttr_CLEAR,             TidyAttr_CODE,            TidyAttr_CODEBASE,
TidyAttr_CODETYPE,          TidyAttr_COLOR,           TidyAttr_COLS,
TidyAttr_COLSPAN,           TidyAttr_COMPACT,         TidyAttr_CONTENT,
TidyAttr_COORDS,            TidyAttr_DATA,            TidyAttr_DATAFLD,
TidyAttr_DATAFORMATAS,      TidyAttr_DATAPAGESIZE,    TidyAttr_DATASRC,
TidyAttr_DATETIME,          TidyAttr_DECLARE,         TidyAttr_DEFER,
TidyAttr_DIR,               TidyAttr_DISABLED,        TidyAttr_ENCODING,
TidyAttr_ENCTYPE,           TidyAttr_FACE,            TidyAttr_FOR,
TidyAttr_FRAME,             TidyAttr_FRAMEBORDER,     TidyAttr_FRAMESPACING,
TidyAttr_GRIDX,             TidyAttr_GRIDY,           TidyAttr_HEADERS,
TidyAttr_HEIGHT,            TidyAttr_HREF,            TidyAttr_HREFLANG,
TidyAttr_HSPACE,            TidyAttr_HTTP_EQUIV,      TidyAttr_ID,
TidyAttr_ISMAP,             TidyAttr_LABEL,           TidyAttr_LANG,
TidyAttr_LANGUAGE,          TidyAttr_LAST_MODIFIED,   TidyAttr_LAST_VISIT,
TidyAttr_LEFTMARGIN,        TidyAttr_LINK,            TidyAttr_LONGDESC,
TidyAttr_LOWSRC,            TidyAttr_MARGINHEIGHT,    TidyAttr_MARGINWIDTH,
TidyAttr_MAXLENGTH,         TidyAttr_MEDIA,           TidyAttr_METHOD,
TidyAttr_MULTIPLE,          TidyAttr_NAME,            TidyAttr_NOHREF,
TidyAttr_NORESIZE,          TidyAttr_NOSHADE,         TidyAttr_NOWRAP,
TidyAttr_OBJECT,            TidyAttr_OnAFTERUPDATE,   TidyAttr_OnBEFOREUNLOAD,
TidyAttr_OnBEFOREUPDATE,    TidyAttr_OnBLUR,          TidyAttr_OnCHANGE,
TidyAttr_OnCLICK,           TidyAttr_OnDATAAVAILABLE, TidyAttr_OnDATASETCHANGED,
TidyAttr_OnDATASETCOMPLETE, TidyAttr_OnDBLCLICK,      TidyAttr_OnERRORUPDATE,
TidyAttr_OnFOCUS,           TidyAttr_OnKEYDOWN,       TidyAttr_OnKEYPRESS,
TidyAttr_OnKEYUP,           TidyAttr_OnLOAD,          TidyAttr_OnMOUSEDOWN,
TidyAttr_OnMOUSEMOVE,       TidyAttr_OnMOUSEOUT,      TidyAttr_OnMOUSEOVER,
TidyAttr_OnMOUSEUP,         TidyAttr_OnRESET,         TidyAttr_OnROWENTER,
TidyAttr_OnROWEXIT,         TidyAttr_OnSELECT,        TidyAttr_OnSUBMIT,
TidyAttr_OnUNLOAD,          TidyAttr_PROFILE,         TidyAttr_PROMPT,
TidyAttr_RBSPAN,            TidyAttr_READONLY,        TidyAttr_REL,
TidyAttr_REV,               TidyAttr_RIGHTMARGIN,     TidyAttr_ROWS,
TidyAttr_ROWSPAN,           TidyAttr_RULES,           TidyAttr_SCHEME,
TidyAttr_SCOPE,             TidyAttr_SCROLLING,       TidyAttr_SELECTED,
TidyAttr_SHAPE,             TidyAttr_SHOWGRID,        TidyAttr_SHOWGRIDX,
TidyAttr_SHOWGRIDY,         TidyAttr_SIZE,            TidyAttr_SPAN,
TidyAttr_SRC,               TidyAttr_STANDBY,         TidyAttr_START,
TidyAttr_STYLE,             TidyAttr_SUMMARY,         TidyAttr_TABINDEX,
TidyAttr_TARGET,            TidyAttr_TEXT,            TidyAttr_TITLE,
TidyAttr_TOPMARGIN,         TidyAttr_TYPE,            TidyAttr_USEMAP,
TidyAttr_VALIGN,            TidyAttr_VALUE,           TidyAttr_VALUETYPE,
TidyAttr_VERSION,           TidyAttr_VLINK,           TidyAttr_VSPACE,
TidyAttr_WIDTH,             TidyAttr_WRAP,            TidyAttr_XML_LANG,
TidyAttr_XML_SPACE,         TidyAttr_XMLNS,           N_TIDY_ATTRIBS
);
// END tidyenum.inc

// DE tidytype.inc

{$IFNDEF FPC}
  type DWORD = cardinal;
{$ENDIF}

type
  uint    =  DWORD;
  ulong   =  DWORD;
  pUint   =  ^uint;
  ctmbstr =  pChar;
  tmbstr  =  pChar;
  Bool    =  boolean;
  CFILE   =  LongInt;
  pCFILE  =  ^CFILE;
  void    =  pointer;

  opaque = pointer; { a pointer to something you probably don't want to see }

  TidyOption = opaque;
  TidyIterator = opaque;
  pTidyIterator = ^TidyIterator;
  pDict=^tDict;
  pTidyNode = ^tTidyNode;
  pTidyAttr = ^tTidyAttr;
  pTidyBuffer = ^TidyBuffer;


{ If you feel like shooting yourself in the foot for no good reason, then
  uncomment the TIDY_DOC_INTERNALS define.  See "tidydoc.inc" for more info... }

{. $DEFINE TIDY_DOC_INTERNALS }

{$IFDEF TIDY_DOC_INTERNALS }
  TidyDoc = ^tTidyDocImpl; // TidyDoc in all its glory
{$ELSE}                    // else
  TidyDoc=opaque;          // Keep it simple
{$ENDIF}

  TidyReportFilter = function ( tdoc:TidyDoc; lvl:TidyReportLevel;
                                  line:uint; col:uint; mssg:ctmbstr ):Bool; cdecl;

  pTidyReportFilter = ^TidyReportFilter;


{$IFDEF TIDY_DOC_INTERNALS }
  {$INCLUDE tidydoc.inc}
{$ENDIF}

  tDict = record
    id: TidyTagId;
    name: tmbstr;
    versions: uint;
    model:uint;
    parser:opaque;
    chkattrs:opaque;
    next:pDict;
  end;

  tTidyAttr = record
    next:   pTidyAttr;
    AttDef: pDict;
    asp:    pTidyNode;
    php:    pTidyNode;
    delim:  LongInt;
    name:   tmbstr;
    value:  tmbstr;
  end;

  tTidyNode =  record
    parent: pTidyNode;    { tree structure }
    prev:pTidyNode;       { previous sibling }
    next:pTidyNode;       { next sibling }
    content:pTidyNode;    { first child }
    last:pTidyNode;       { last node in the document }
    attributes:pTidyAttr; { linked list of attributes }
    was:pDict;            { old tag when it was changed }
    tag:pDict;            { tag's dictionary definition }
    element:tmbstr;       { name (null for text nodes) }
    start: uint;          { start of span onto text array }
    _end: uint;           { end of span onto text array }
    _type: uint;          { TextNode, StartTag, EndTag etc. }
    line: uint;           { current line of document }
    column: uint;         { current column of document }
    closed:Bool;          { true if closed by explicit end tag }
    implicit:Bool;        { true if inferred }
    linebreak:Bool;       { true if followed by a line break }
  end;

  TidyBuffer = record
    bp: pChar;
    size : uint;
    allocated : uint;
    next : uint;
  end;


const
  yes = true;
  no = false;
  ENOMEM = 12;

// END tidytype.inc


// DE tidyio.inc

const
  EndOfStream =  not (0);

type
  TidyInputType = ( TidyContentInput, TidyConfigInput );
  TidyOutputType = ( TidyContentOutput, TidyDiagnosticOutput );
  TidyPutByteFunc = procedure (sinkData:uint; bt:byte); cdecl;
  TidyGetByteFunc = function ( sourceData:uint ):LongInt; cdecl;
  TidyUngetByteFunc = procedure ( sourceData:uint; bt:byte ); cdecl;
  TidyEOFFunc = function (sourceData:uint):Bool; cdecl;

  TidyInputSource = record
    sourceData : uint;
    getByte : TidyGetByteFunc;
    ungetByte : TidyUngetByteFunc;
    eof : TidyEOFFunc;
  end;
  pTidyInputSource = ^TidyInputSource;

  TidyOutputSink = record
     sinkData : uint;
     putByte : TidyPutByteFunc;
  end;
  pTidyOutputSink = ^TidyOutputSink;

procedure tidyBufInit  ( buf:PTidyBuffer ); cdecl; external TIDY_LIB;
procedure tidyBufAlloc ( buf:PTidyBuffer; allocSize:uint ); cdecl; external TIDY_LIB;
procedure tidyBufCheckAlloc ( buf:PTidyBuffer; allocSize:uint; chunkSize:uint ); cdecl; external TIDY_LIB;
procedure tidyBufFree ( buf:PTidyBuffer ); cdecl; external TIDY_LIB;
procedure tidyBufClear ( buf:PTidyBuffer ); cdecl; external TIDY_LIB;
procedure tidyBufAttach ( buf:PTidyBuffer; bp:pointer; size:uint ); cdecl; external TIDY_LIB;
procedure tidyBufDetach ( buf:PTidyBuffer ); cdecl; external TIDY_LIB;
procedure tidyBufAppend ( buf:PTidyBuffer; vp:pointer; size:uint ); cdecl; external TIDY_LIB;
procedure tidyBufPutByte ( buf:PTidyBuffer; bv:byte ); cdecl; external TIDY_LIB;
function tidyBufPopByte ( buf:PTidyBuffer ):LongInt; cdecl; external TIDY_LIB;
function tidyBufGetByte ( buf:PTidyBuffer ):LongInt; cdecl; external TIDY_LIB;
function tidyBufEndOfInput ( buf:PTidyBuffer ):Bool; cdecl; external TIDY_LIB;
procedure tidyBufUngetByte ( buf:PTidyBuffer; bv:byte ); cdecl; external TIDY_LIB;

function tidyInitSource( source:PTidyInputSource; srcData:pointer; gbFunc:TidyGetByteFunc; ugbFunc:TidyUngetByteFunc; endFunc:TidyEOFFunc):Bool; cdecl; external TIDY_LIB;

procedure initInputBuffer ( inp:PTidyInputSource; buf:PTidyBuffer ); cdecl; external TIDY_LIB;
function tidyGetByte ( source:PTidyInputSource ):uint; cdecl; external TIDY_LIB;
procedure tidyUngetByte ( source:PTidyInputSource; byteValue:uint ); cdecl; external TIDY_LIB;
function tidyIsEOF ( source:PTidyInputSource ):Bool; cdecl; external TIDY_LIB;
function tidyInitSink ( sink:PTidyOutputSink; snkData:pointer; pbFunc:TidyPutByteFunc ):Bool; cdecl; external TIDY_LIB;

procedure tidyPutByte ( sink:PTidyOutputSink; byteValue:uint ); cdecl; external TIDY_LIB;
procedure initOutputBuffer ( outp:PTidyOutputSink; buf:PTidyBuffer ); cdecl; external TIDY_LIB;
function tidyParseSource ( tdoc:TidyDoc; source:PTidyInputSource ):LongInt; cdecl; external TIDY_LIB;

function tidySetErrorSink ( tdoc:TidyDoc; sink:PTidyOutputSink ):LongInt; cdecl; external TIDY_LIB;
function tidySaveSink ( tdoc:TidyDoc; sink:PTidyOutputSink ):LongInt; cdecl; external TIDY_LIB;
function tidyOptSaveSink ( tdoc:TidyDoc; sink:PTidyOutputSink ):LongInt; cdecl; external TIDY_LIB;
 
// END tidyio.inc

function tidyGetRoot(tdoc:TidyDoc):pTidyNode; cdecl; external TIDY_LIB;

// DE tidymain.inc

function tidySetReportFilter(tdoc:TidyDoc; filtCallback:TidyReportFilter):Bool; cdecl; external TIDY_LIB;

function tidyParseFile(tdoc:TidyDoc; filename:ctmbstr):longint; cdecl; external TIDY_LIB;
function tidyParseStdin(tdoc:TidyDoc):longint; cdecl; external TIDY_LIB;
function tidyParseString(tdoc:TidyDoc; content:ctmbstr):longint; cdecl; external TIDY_LIB;
function tidyParseBuffer(tdoc:TidyDoc; buffer:pTidyBuffer):longint; cdecl; external TIDY_LIB;
function tidyCleanAndRepair(tdoc:TidyDoc):longint; cdecl; external TIDY_LIB;
function tidyRunDiagnostics(tdoc:TidyDoc):longint; cdecl; external TIDY_LIB;

function tidySaveFile(tdoc:TidyDoc; filename:ctmbstr):longint; cdecl; external TIDY_LIB;
function tidySaveStdout(tdoc:TidyDoc):longint; cdecl; external TIDY_LIB;
function tidySaveBuffer(tdoc:TidyDoc; buffer:pTidyBuffer):longint; cdecl; external TIDY_LIB;
function tidySaveString(tdoc:TidyDoc; buffer:tmbstr; buflen:Puint):longint; cdecl; external TIDY_LIB;

procedure tidyErrorSummary(tdoc:TidyDoc); cdecl; external TIDY_LIB;
procedure tidyGeneralInfo(tdoc:TidyDoc); cdecl; external TIDY_LIB;

function tidyCreate:TidyDoc; cdecl; external TIDY_LIB;
procedure tidyRelease(tdoc:TidyDoc); cdecl; external TIDY_LIB;

procedure tidySetAppData(tdoc:TidyDoc; appData:pointer); cdecl; external TIDY_LIB;
function tidyGetAppData(tdoc:TidyDoc):pointer; cdecl; external TIDY_LIB;

function tidyReleaseDate:ctmbstr; cdecl; external TIDY_LIB;

function tidyDetectedHtmlVersion(tdoc:TidyDoc):longint; cdecl; external TIDY_LIB;
function tidyDetectedXhtml(tdoc:TidyDoc):Bool; cdecl; external TIDY_LIB;
function tidyDetectedGenericXml(tdoc:TidyDoc):Bool; cdecl; external TIDY_LIB;

function tidyStatus(tdoc:TidyDoc):longint; cdecl; external TIDY_LIB;
function tidyErrorCount(tdoc:TidyDoc):uint; cdecl; external TIDY_LIB;
function tidyWarningCount(tdoc:TidyDoc):uint; cdecl; external TIDY_LIB;
function tidyAccessWarningCount(tdoc:TidyDoc):uint; cdecl; external TIDY_LIB;

function tidySetErrorBuffer(tdoc:TidyDoc; errbuf:pTidyBuffer):LongInt; cdecl;  external TIDY_LIB;

// END tidymain.inc

// DE tidycfg.inc

function tidyConfigErrorCount(tdoc:TidyDoc):uint; cdecl; external TIDY_LIB;
function tidyLoadConfig(tdoc:TidyDoc; configFile:ctmbstr):longint; cdecl; external TIDY_LIB;
function tidyLoadConfigEnc(tdoc:TidyDoc; configFile:ctmbstr; charenc:ctmbstr):longint; cdecl; external TIDY_LIB;
function tidySetCharEncoding(tdoc:TidyDoc; encnam:ctmbstr):longint; cdecl; external TIDY_LIB;
function tidySetErrorFile(tdoc:TidyDoc; errfilnam:ctmbstr):pCFILE; cdecl; external TIDY_LIB;
function tidyOptSaveFile(tdoc:TidyDoc; cfgfil:ctmbstr):longint; cdecl; external TIDY_LIB;


type  TidyConfigCategory = ( TidyMarkup, TidyDiagnostics, TidyPrettyPrint, TidyEncoding, TidyMiscellaneous );

{$define SUPPORT_ASIAN_ENCODINGS}
{$define SUPPORT_UTF16_ENCODINGS}


type TidyOptionId = (
   TidyUnknownOption,   TidyIndentSpaces,    TidyWrapLen,          TidyTabSize,
   TidyCharEncoding,    TidyInCharEncoding,  TidyOutCharEncoding,
                                                         TidyNewline, // <- NEW !
                                                                   TidyDoctypeMode,
   TidyDoctype,         TidyDuplicateAttrs,  TidyAltText,          TidySlideStyle,
   TidyErrFile,
             TidyOutFile, // <- NEW !
                        TidyWriteBack,        TidyShowMarkup,   TidyShowWarnings,
   TidyQuiet,           TidyIndentContent,   TidyHideEndTags,      TidyXmlTags,
   TidyXmlOut,          TidyXhtmlOut,        TidyHtmlOut,          TidyXmlDecl,
   TidyUpperCaseTags,   TidyUpperCaseAttrs,  TidyMakeBare,         TidyMakeClean,
   TidyLogicalEmphasis, TidyDropPropAttrs,   TidyDropFontTags,     TidyDropEmptyParas,
   TidyFixComments,     TidyBreakBeforeBR,   TidyBurstSlides,      TidyNumEntities,
   TidyQuoteMarks,      TidyQuoteNbsp,       TidyQuoteAmpersand,   TidyWrapAttVals,
   TidyWrapScriptlets,  TidyWrapSection,     TidyWrapAsp,          TidyWrapJste,
   TidyWrapPhp,         TidyFixBackslash,    TidyIndentAttributes, TidyXmlPIs,
   TidyXmlSpace,        TidyEncloseBodyText, TidyEncloseBlockText, TidyKeepFileTimes,
   TidyWord2000,        TidyMark,            TidyEmacs,            TidyEmacsFile,
   TidyLiteralAttribs,  TidyBodyOnly,        TidyFixUri,           TidyLowerLiterals,
   TidyHideComments,    TidyIndentCdata,     TidyForceOutput,      TidyShowErrors,
   TidyAsciiChars,      TidyJoinClasses,     TidyJoinStyles,       TidyEscapeCdata,
   {$ifdef SUPPORT_ASIAN_ENCODINGS}
   TidyLanguage, TidyNCR,
   {$endif}
   {$ifdef SUPPORT_UTF16_ENCODINGS}
   TidyOutputBOM,
   {$endif}
   TidyReplaceColor,    TidyCSSPrefix,       TidyInlineTags,       TidyBlockTags,
   TidyEmptyTags,       TidyPreTags,         TidyAccessibilityCheckLevel,
   N_TIDY_OPTIONS);

type TidyOptionType = (TidyString,TidyInteger,TidyBoolean);

{$IFDEF FPC}
type TidyTriState = (TidyNoState := no, TidyYesState := yes, TidyAutoState);
const
  TidyFalse =  TidyNoState;
  TidyTrue  =  TidyYesState;
  TidyAuto  =  TidyAutoState;
{$ELSE}
type TidyTriState = (TidyFalse, TidyTrue, TidyAuto);
const
  TidyNoState   = TidyFalse;
  TidyYesState  = TidyTrue;
  TidyAutoState = TidyAuto;
{$ENDIF}

(*
  The encoding ID's are hidden constants (integers) in the libtidy sources,
  but jeff thinks they are cooler to be an enumerated type ...
*)
type TidyEncodingID = (
  TidyRaw, TidyASCII, TidyLatin1, TidyUTF8, TidyISO2022, TidyMacRoman, TidyWin1252,
{$IFDEF SUPPORT_UTF16_ENCODINGS}
  TidyUTF16le,  TidyUTF16be,  TidyUTF16,
{$ENDIF}
{$IFDEF SUPPORT_ASIAN_ENCODINGS}
  TidyBig5, TidyShiftJIS
{$ENDIF}
);



type TidyDoctypeModes = (TidyDoctypeOmit, TidyDoctypeAuto, TidyDoctypeStrict, TidyDoctypeLoose, TidyDoctypeUser);
type TidyDupAttrModes = (TidyKeepFirst, TidyKeepLast);

function tidyOptGetIdForName(optnam:ctmbstr):TidyOptionId; cdecl; external TIDY_LIB;
function tidyGetOptionList(tdoc:TidyDoc):TidyIterator; cdecl; external TIDY_LIB;
function tidyGetNextOption(tdoc:TidyDoc; pos:PTidyIterator):TidyOption; cdecl; external TIDY_LIB;
function tidyGetOption(tdoc:TidyDoc; optId:TidyOptionId):TidyOption; cdecl; external TIDY_LIB;
function tidyGetOptionByName(tdoc:TidyDoc; optnam:ctmbstr):TidyOption; cdecl; external TIDY_LIB;
function tidyOptGetId(opt:TidyOption):TidyOptionId; cdecl; external TIDY_LIB;
function tidyOptGetName(opt:TidyOption):ctmbstr; cdecl; external TIDY_LIB;
function tidyOptGetType(opt:TidyOption):TidyOptionType; cdecl; external TIDY_LIB;
function tidyOptIsReadOnly(opt:TidyOption):Bool; cdecl; external TIDY_LIB;
function tidyOptGetCategory(opt:TidyOption):TidyConfigCategory; cdecl; external TIDY_LIB;
function tidyOptGetDefault(opt:TidyOption):ctmbstr; cdecl; external TIDY_LIB;
function tidyOptGetDefaultInt(opt:TidyOption):uint; cdecl; external TIDY_LIB;
function tidyOptGetDefaultBool(opt:TidyOption):Bool; cdecl; external TIDY_LIB;
function tidyOptGetPickList(opt:TidyOption):TidyIterator; cdecl; external TIDY_LIB;
function tidyOptGetNextPick(opt:TidyOption; pos:PTidyIterator):ctmbstr; cdecl; external TIDY_LIB;
function tidyOptGetValue(tdoc:TidyDoc; optId:TidyOptionId):ctmbstr; cdecl; external TIDY_LIB;
function tidyOptSetValue(tdoc:TidyDoc; optId:TidyOptionId; val:ctmbstr):Bool; cdecl; external TIDY_LIB;
function tidyOptParseValue(tdoc:TidyDoc; optnam:ctmbstr; val:ctmbstr):Bool; cdecl; external TIDY_LIB;
function tidyOptGetInt(tdoc:TidyDoc; optId:TidyOptionId):uint; cdecl; external TIDY_LIB;
function tidyOptSetInt(tdoc:TidyDoc; optId:TidyOptionId; val:uint):Bool; cdecl; external TIDY_LIB;
function tidyOptGetBool(tdoc:TidyDoc; optId:TidyOptionId):Bool; cdecl; external TIDY_LIB;
function tidyOptSetBool(tdoc:TidyDoc; optId:TidyOptionId; val:Bool):Bool; cdecl; external TIDY_LIB;
function tidyOptResetToDefault(tdoc:TidyDoc; opt:TidyOptionId):Bool; cdecl; external TIDY_LIB;
function tidyOptResetAllToDefault(tdoc:TidyDoc):Bool; cdecl; external TIDY_LIB;
function tidyOptSnapshot(tdoc:TidyDoc):Bool; cdecl; external TIDY_LIB;
function tidyOptResetToSnapshot(tdoc:TidyDoc):Bool; cdecl; external TIDY_LIB;
function tidyOptDiffThanDefault(tdoc:TidyDoc):Bool; cdecl; external TIDY_LIB;
function tidyOptDiffThanSnapshot(tdoc:TidyDoc):Bool; cdecl; external TIDY_LIB;
function tidyOptCopyConfig(copy_to:TidyDoc; from:TidyDoc):Bool; cdecl; external TIDY_LIB;
function tidyOptGetEncName(tdoc:TidyDoc; optId:TidyOptionId):ctmbstr; cdecl; external TIDY_LIB;
function tidyOptGetCurrPick(tdoc:TidyDoc; optId:TidyOptionId):ctmbstr; cdecl; external TIDY_LIB;
function tidyOptGetDeclTagList(tdoc:TidyDoc):TidyIterator; cdecl; external TIDY_LIB;
function tidyOptGetNextDeclTag(tdoc:TidyDoc; optId:TidyOptionId; iter:PTidyIterator):ctmbstr; cdecl; external TIDY_LIB;

// END tidycfg.inc


// DE tidy.pas

type tTidyReportEvent = procedure( sender:tObject;  level:TidyReportLevel;  line,col:cardinal;
                                     msg:ansistring; var bWriteOut:Boolean  ) of object;

type pTidyReportEvent = ^tTidyReportEvent;


type TTidy = class (TObject)
  private
    fTidyDoc:TidyDoc;
    fOnReport:tTidyReportEvent;
    fReportFilter:TidyReportFilter;
    fConfigOK:Bool;
    fInputFile:string;
    fInputString:pChar;
    fHaveStdIn:Boolean;
    fCongruent:Boolean;
  protected
    // DE tidyprot.inc
      function GetDoctype:string;
      procedure SetDoctype(aValue: string);
      function GetAltText:string;
      procedure SetAltText(aValue: string);
      function GetSlideStyle:string;
      procedure SetSlideStyle(aValue: string);
      function GetErrFile:string;
      procedure SetErrFile(aValue: string);
      function GetEmacsFile:string;
      procedure SetEmacsFile(aValue: string);
      function GetReplaceColor:bool;
      procedure SetReplaceColor(aValue: bool);
      function GetEmptyTags:string;
      procedure SetEmptyTags(aValue: string);
      function GetPreTags:string;
      procedure SetPreTags(aValue: string);
      function GetAccessibilityCheckLevel:uint;
      procedure SetAccessibilityCheckLevel(aValue: uint);
      function GetIndentSpaces:uint;
      procedure SetIndentSpaces(aValue: uint);
      function GetWrapLen:uint;
      procedure SetWrapLen(aValue: uint);
      function GetTabSize:uint;
      procedure SetTabSize(aValue: uint);
    
      function GetCharEncoding:TidyEncodingID;
      procedure SetCharEncoding(aValue: TidyEncodingID);
      function GetInCharEncoding:TidyEncodingID;
      procedure SetInCharEncoding(aValue: TidyEncodingID);
      function GetOutCharEncoding:TidyEncodingID;
      procedure SetOutCharEncoding(aValue: TidyEncodingID);
    
    
    
      function GetDoctypeMode:uint;
      function GetDuplicateAttrs:TidyDupAttrModes;
      procedure SetDuplicateAttrs(aValue: TidyDupAttrModes);
      function GetIndentContent:TidyTriState;
      procedure SetIndentContent(aValue: TidyTriState);
      function GetShowErrors:uint;
      procedure SetShowErrors(aValue: uint);
      function GetInlineTags:string;
      procedure SetInlineTags(aValue: string);
      function GetWriteBack:bool;
      procedure SetWriteBack(aValue: bool);
      function GetShowMarkup:bool;
      procedure SetShowMarkup(aValue: bool);
      function GetShowWarnings:bool;
      procedure SetShowWarnings(aValue: bool);
      function GetQuiet:bool;
      procedure SetQuiet(aValue: bool);
      function GetHideEndTags:bool;
      procedure SetHideEndTags(aValue: bool);
      function GetXmlTags:bool;
      procedure SetXmlTags(aValue: bool);
      function GetXmlOut:bool;
      procedure SetXmlOut(aValue: bool);
      function GetXhtmlOut:bool;
      procedure SetXhtmlOut(aValue: bool);
      function GetHtmlOut:bool;
      procedure SetHtmlOut(aValue: bool);
      function GetXmlDecl:bool;
      procedure SetXmlDecl(aValue: bool);
      function GetUpperCaseTags:bool;
      procedure SetUpperCaseTags(aValue: bool);
      function GetUpperCaseAttrs:bool;
      procedure SetUpperCaseAttrs(aValue: bool);
      function GetMakeBare:bool;
      procedure SetMakeBare(aValue: bool);
      function GetMakeClean:bool;
      procedure SetMakeClean(aValue: bool);
      function GetLogicalEmphasis:bool;
      procedure SetLogicalEmphasis(aValue: bool);
      function GetDropPropAttrs:bool;
      procedure SetDropPropAttrs(aValue: bool);
      function GetDropFontTags:bool;
      procedure SetDropFontTags(aValue: bool);
      function GetDropEmptyParas:bool;
      procedure SetDropEmptyParas(aValue: bool);
      function GetFixComments:bool;
      procedure SetFixComments(aValue: bool);
      function GetBreakBeforeBR:bool;
      procedure SetBreakBeforeBR(aValue: bool);
      function GetBurstSlides:bool;
      procedure SetBurstSlides(aValue: bool);
      function GetNumEntities:bool;
      procedure SetNumEntities(aValue: bool);
      function GetQuoteMarks:bool;
      procedure SetQuoteMarks(aValue: bool);
      function GetQuoteNbsp:bool;
      procedure SetQuoteNbsp(aValue: bool);
      function GetQuoteAmpersand:bool;
      procedure SetQuoteAmpersand(aValue: bool);
      function GetWrapAttVals:bool;
      procedure SetWrapAttVals(aValue: bool);
      function GetWrapScriptlets:bool;
      procedure SetWrapScriptlets(aValue: bool);
      function GetWrapSection:bool;
      procedure SetWrapSection(aValue: bool);
      function GetWrapAsp:bool;
      procedure SetWrapAsp(aValue: bool);
      function GetWrapJste:bool;
      procedure SetWrapJste(aValue: bool);
      function GetWrapPhp:bool;
      procedure SetWrapPhp(aValue: bool);
      function GetFixBackslash:bool;
      procedure SetFixBackslash(aValue: bool);
      function GetIndentAttributes:bool;
      procedure SetIndentAttributes(aValue: bool);
      function GetXmlPIs:bool;
      procedure SetXmlPIs(aValue: bool);
      function GetXmlSpace:bool;
      procedure SetXmlSpace(aValue: bool);
      function GetEncloseBodyText:bool;
      procedure SetEncloseBodyText(aValue: bool);
      function GetEncloseBlockText:bool;
      procedure SetEncloseBlockText(aValue: bool);
      function GetKeepFileTimes:bool;
      procedure SetKeepFileTimes(aValue: bool);
      function GetWord2000:bool;
      procedure SetWord2000(aValue: bool);
      function GetMark:bool;
      procedure SetMark(aValue: bool);
      function GetEmacs:bool;
      procedure SetEmacs(aValue: bool);
      function GetLiteralAttribs:bool;
      procedure SetLiteralAttribs(aValue: bool);
      function GetBodyOnly:bool;
      procedure SetBodyOnly(aValue: bool);
      function GetFixUri:bool;
      procedure SetFixUri(aValue: bool);
      function GetLowerLiterals:bool;
      procedure SetLowerLiterals(aValue: bool);
      function GetHideComments:bool;
      procedure SetHideComments(aValue: bool);
      function GetIndentCdata:bool;
      procedure SetIndentCdata(aValue: bool);
      function GetForceOutput:bool;
      procedure SetForceOutput(aValue: bool);
      function GetAsciiChars:bool;
      procedure SetAsciiChars(aValue: bool);
      function GetJoinClasses:bool;
      procedure SetJoinClasses(aValue: bool);
      function GetJoinStyles:bool;
      procedure SetJoinStyles(aValue: bool);
      function GetEscapeCdata:bool;
      procedure SetEscapeCdata(aValue: bool);
      function GetCSSPrefix:string;
      procedure SetCSSPrefix(aValue: string);
      function GetBlockTags:string;
      procedure SetBlockTags(aValue: string);
  {$IFDEF SUPPORT_ASIAN_ENCODINGS}
    function GetNCR:bool;
    procedure SetNCR(aValue: bool);
  {$ENDIF SUPPORT_ASIAN_ENCODINGS}
  {$IFDEF SUPPORT_UTF16_ENCODINGS}
    function GetOutputBOM:TidyTriState;
    procedure SetOutputBOM(aValue: TidyTriState);
  {$ENDIF SUPPORT_UTF16_ENCODINGS}

    // END tidyprot.inc
    procedure SetOnReport(aReportEvent:tTidyReportEvent);
    procedure SetReportFilter(aReportFilter:TidyReportFilter);
    function DoOutputString:string;
    function GetOutputString:string;
    function GetHtml:string;
    function GetXhtml:string;
    function GetXml:string;
    function GetErrorCount:uint;
    function GetWarningCount:uint;
    function GetAccessWarningCount:uint;
    function GetConfigErrorCount:uint;
    function GetStatus:LongInt;
    function GetRootNode:pTidyNode;

  public

    constructor Create;
    destructor Destroy; override;
     { Read input from a file }
    procedure ParseFile ( const aFilename:string );
     { Use an AnsiString as input }
    procedure ParseString ( const aInputString:string );
     { Clears previous errors/warnings }
    procedure Reset;

   function LoadConfigFile( const aFilename:string): bool;
   function SaveConfigFile( const aFilename:string): bool;

{
  The ParseFile,  ParseString,  ParseStdin routines
  above just tell the library where to get its input.
  The real "grunt" work happens when you read the output
  from the HTML, XHTML, or XML string properties...
}
    property HTML:  string read getHtml;
    property XHTML: string read getXhtml;
    property XML:   string read getXml;


{
  "Handle" exposes the library's "tidydoc" instance,
  so you can call library functions directly.
  DO NOT use TidySetAppData() on this !!!
  TidySetAppData() and TidyGetAppData() are used
  internally by TTidy, and should NOT be modified !!!
}

    property Handle: TidyDoc read fTidyDoc;
    property DoctypeMode:uint read GetDoctypeMode;
    property ErrorCount:uint read GetErrorCount;
    property WarningCount:uint read GetWarningCount;
    property AccessWarningCount:uint read GetAccessWarningCount;
    property ConfigErrorCount:uint read GetConfigErrorCount;
    property Status:LongInt read GetStatus;
    property RootNode:pTidyNode read GetRootNode;
    class function ReleaseDate:string;


{
  "ReportCallback" and "OnReport" essentially perform the same
  task, although the implementation is slightly different.
  ReportCallback is a procedural "c-style" callback...
    ReportCallback:=@MyReport;
  OnReport references a method inside another object...
    OnReport:=Form1.Tidy1Report;
  Generally speaking, OnReport is for component/form based
  applications, and ReportCallback is for console apps.
  Choose one or the other, NOT both!
}
  public    property ReportCallback:TidyReportFilter read fReportFilter write SetReportFilter;
  published property OnReport: tTidyReportEvent read fOnReport write SetOnReport;


{
  Setting Congruent to TRUE will cause TTidy to re-parse the input,
  in an attempt to make the output text consistent with the node tree.
  This is needed to accurately reference the line and column index of
  the output text while traversing the node tree.
  In the unlikely event that this fails, Congruent will be reset to FALSE.
  Note that setting Congruent:=TRUE automatically sets IndentContent:=0.
}

  published property Congruent:Boolean read fCongruent write fCongruent default False;

  // DE tidyprop.inc
    public property Doctype:string read GetDoctype write SetDoctype;
    published property AltText:string read GetAltText write SetAltText;
    
    published property ErrorFile:string read GetErrFile write SetErrFile;
    
    //public property ErrorStrings:string read GetErrorStrings;
    
    published property Quiet:bool read GetQuiet write SetQuiet;
    published property ShowErrors:uint read GetShowErrors write SetShowErrors;
    published property ShowWarnings:bool read GetShowWarnings write SetShowWarnings;
    
    published property AccessibilityCheckLevel:uint read GetAccessibilityCheckLevel write SetAccessibilityCheckLevel;
    published property ForceOutput:bool read GetForceOutput write SetForceOutput;
    
    published property ReplaceColor:bool read GetReplaceColor write SetReplaceColor;
    
    public property EmptyTags:string read GetEmptyTags write SetEmptyTags;
    public property PreTags:string read GetPreTags write SetPreTags;
    public property InlineTags:string read GetInlineTags write SetInlineTags;
    public property BlockTags:string read GetBlockTags write SetBlockTags;
    
    published property IndentSpaces:uint read GetIndentSpaces write SetIndentSpaces;
    published property WrapLen:uint read GetWrapLen write SetWrapLen;
    published property TabSize:uint read GetTabSize write SetTabSize;
    
    published property CharEncoding:TidyEncodingID read GetCharEncoding write SetCharEncoding;
    published property InCharEncoding:TidyEncodingID read GetInCharEncoding write SetInCharEncoding;
    published property OutCharEncoding:TidyEncodingID read GetOutCharEncoding write SetOutCharEncoding;
    
    published property DuplicateAttrs:TidyDupAttrModes read GetDuplicateAttrs write SetDuplicateAttrs;
    published property IndentContent:TidyTriState read GetIndentContent write SetIndentContent;
    
    
    published property HideEndTags:bool read GetHideEndTags write SetHideEndTags;
    published property XmlTags:bool read GetXmlTags write SetXmlTags;
    published property XmlDecl:bool read GetXmlDecl write SetXmlDecl;
    published property UpperCaseTags:bool read GetUpperCaseTags write SetUpperCaseTags;
    published property UpperCaseAttrs:bool read GetUpperCaseAttrs write SetUpperCaseAttrs;
    published property MakeBare:bool read GetMakeBare write SetMakeBare;
    published property MakeClean:bool read GetMakeClean write SetMakeClean;
    published property LogicalEmphasis:bool read GetLogicalEmphasis write SetLogicalEmphasis;
    published property DropPropAttrs:bool read GetDropPropAttrs write SetDropPropAttrs;
    published property DropFontTags:bool read GetDropFontTags write SetDropFontTags;
    published property DropEmptyParas:bool read GetDropEmptyParas write SetDropEmptyParas;
    published property FixComments:bool read GetFixComments write SetFixComments;
    published property BreakBeforeBR:bool read GetBreakBeforeBR write SetBreakBeforeBR;
    
    //published property SlideStyle:string read GetSlideStyle write SetSlideStyle;
    //published property BurstSlides:bool read GetBurstSlides write SetBurstSlides;
    
    published property NumEntities:bool read GetNumEntities write SetNumEntities;
    published property QuoteMarks:bool read GetQuoteMarks write SetQuoteMarks;
    published property QuoteNbsp:bool read GetQuoteNbsp write SetQuoteNbsp;
    published property QuoteAmpersand:bool read GetQuoteAmpersand write SetQuoteAmpersand;
    published property WrapAttVals:bool read GetWrapAttVals write SetWrapAttVals;
    published property WrapScriptlets:bool read GetWrapScriptlets write SetWrapScriptlets;
    published property WrapSection:bool read GetWrapSection write SetWrapSection;
    published property WrapAsp:bool read GetWrapAsp write SetWrapAsp;
    published property WrapJste:bool read GetWrapJste write SetWrapJste;
    published property WrapPhp:bool read GetWrapPhp write SetWrapPhp;
    
    published property IndentAttributes:bool read GetIndentAttributes write SetIndentAttributes;
    published property XmlPIs:bool read GetXmlPIs write SetXmlPIs;
    published property XmlSpace:bool read GetXmlSpace write SetXmlSpace;
    published property EncloseBodyText:bool read GetEncloseBodyText write SetEncloseBodyText;
    published property EncloseBlockText:bool read GetEncloseBlockText write SetEncloseBlockText;
    published property Word2000:bool read GetWord2000 write SetWord2000;
    published property Mark:bool read GetMark write SetMark;
    
    published property LiteralAttribs:bool read GetLiteralAttribs write SetLiteralAttribs;
    published property BodyOnly:bool read GetBodyOnly write SetBodyOnly;
    
    published property FixBackslash:bool read GetFixBackslash write SetFixBackslash;
    published property FixUri:bool read GetFixUri write SetFixUri;
    
    published property LowerLiterals:bool read GetLowerLiterals write SetLowerLiterals;
    published property HideComments:bool read GetHideComments write SetHideComments;
    published property IndentCdata:bool read GetIndentCdata write SetIndentCdata;
    
    published property AsciiChars:bool read GetAsciiChars write SetAsciiChars;
    published property JoinClasses:bool read GetJoinClasses write SetJoinClasses;
    published property JoinStyles:bool read GetJoinStyles write SetJoinStyles;
    published property EscapeCdata:bool read GetEscapeCdata write SetEscapeCdata;
    public property CSSPrefix:string read GetCSSPrefix write SetCSSPrefix;
    

{$IFDEF SUPPORT_ASIAN_ENCODINGS}
   public property NCR:bool read GetNCR write SetNCR;
{$ENDIF SUPPORT_ASIAN_ENCODINGS}

{$IFDEF SUPPORT_UTF16_ENCODINGS}
  public property OutputBom:TidyTriState read GetOutputBOM write SetOutputBOM;
{$ENDIF SUPPORT_UTF16_ENCODINGS}
  // END tidyprop.inc
end;


// END tidy.pas


implementation

{
  Delphi brings Windows to its knees if DocType
  is incorrectly set at design time -
  so we save the value in a private field,
  and then attempt to set it at run time.
}
function TTidy.GetDoctype:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyDoctype);
end;

procedure TTidy.SetDoctype(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyDoctype, pChar(aValue)) and fConfigOK;
end;

function TTidy.GetAltText:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyAltText);
end;

procedure TTidy.SetAltText(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyAltText, pChar(aValue)) and fConfigOK;
end;

function TTidy.GetSlideStyle:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidySlideStyle);
end;

procedure TTidy.SetSlideStyle(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidySlideStyle, pChar(aValue)) and fConfigOK;
end;

function TTidy.GetErrFile:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyErrFile);
end;

procedure TTidy.SetErrFile(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyErrFile, pChar(aValue))
  and ( tidySetErrorFile(fTidyDoc, pChar(aValue)) <> nil ) and
  fConfigOK;
end;

function TTidy.GetEmacsFile:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyEmacsFile);
end;

procedure TTidy.SetEmacsFile(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyEmacsFile, pChar(aValue)) and fConfigOK;
end;

function TTidy.GetReplaceColor:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyReplaceColor);
end;

procedure TTidy.SetReplaceColor(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyReplaceColor, aValue) and fConfigOK;
end;

function TTidy.GetEmptyTags:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyEmptyTags);
end;

procedure TTidy.SetEmptyTags(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyEmptyTags, pChar(aValue)) and fConfigOK;
end;

function TTidy.GetPreTags:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyPreTags);
end;

procedure TTidy.SetPreTags(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyPreTags, pChar(aValue)) and fConfigOK;
end;

function TTidy.GetAccessibilityCheckLevel:uint;
begin
  Result := tidyOptGetInt(fTidyDoc, TidyAccessibilityCheckLevel);
end;

procedure TTidy.SetAccessibilityCheckLevel(aValue: uint);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyAccessibilityCheckLevel, aValue) and fConfigOK;
end;

function TTidy.GetIndentSpaces:uint;
begin
  Result := tidyOptGetInt(fTidyDoc, TidyIndentSpaces);
end;

procedure TTidy.SetIndentSpaces(aValue: uint);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyIndentSpaces, aValue) and fConfigOK;
end;

function TTidy.GetWrapLen:uint;
begin
  Result := tidyOptGetInt(fTidyDoc, TidyWrapLen);
end;

procedure TTidy.SetWrapLen(aValue: uint);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyWrapLen, aValue) and fConfigOK;
end;

function TTidy.GetTabSize:uint;
begin
  Result := tidyOptGetInt(fTidyDoc, TidyTabSize);
end;

procedure TTidy.SetTabSize(aValue: uint);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyTabSize, aValue) and fConfigOK;
end;





function TTidy.GetCharEncoding:TidyEncodingID;
begin
  Result := TidyEncodingID(tidyOptGetInt(fTidyDoc, TidyCharEncoding));
end;

procedure TTidy.SetCharEncoding(aValue: TidyEncodingID);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyCharEncoding, uint(aValue)) and fConfigOK;
end;

function TTidy.GetInCharEncoding:TidyEncodingID;
begin
  Result := TidyEncodingID(tidyOptGetInt(fTidyDoc, TidyInCharEncoding));
end;

procedure TTidy.SetInCharEncoding(aValue: TidyEncodingID);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyInCharEncoding, uint(aValue)) and fConfigOK;
end;

function TTidy.GetOutCharEncoding:TidyEncodingID;
begin
  Result := TidyEncodingID(tidyOptGetInt(fTidyDoc, TidyOutCharEncoding));
end;

procedure TTidy.SetOutCharEncoding(aValue: TidyEncodingID);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyOutCharEncoding, uint(aValue)) and fConfigOK;
end;






function TTidy.GetDoctypeMode:uint;
begin
  Result := tidyOptGetInt(fTidyDoc, TidyDoctypeMode);
end;


function TTidy.GetDuplicateAttrs:TidyDupAttrModes;
begin
  Result := TidyDupAttrModes(tidyOptGetInt(fTidyDoc, TidyDuplicateAttrs));
end;

procedure TTidy.SetDuplicateAttrs(aValue: TidyDupAttrModes);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyDuplicateAttrs, uint(aValue)) and fConfigOK;
end;

function TTidy.GetIndentContent:TidyTriState;
begin
  Result := TidyTriState(tidyOptGetInt(fTidyDoc, TidyIndentContent));
end;

procedure TTidy.SetIndentContent(aValue: TidyTriState);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyIndentContent, uint(aValue)) and fConfigOK;
end;

function TTidy.GetShowErrors:uint;
begin
  Result := tidyOptGetInt(fTidyDoc, TidyShowErrors);
end;

procedure TTidy.SetShowErrors(aValue: uint);
begin
  fConfigOK := tidyOptSetInt(fTidyDoc, TidyShowErrors, aValue) and fConfigOK;
end;

function TTidy.GetInlineTags:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyInlineTags);
end;

procedure TTidy.SetInlineTags(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyInlineTags, pChar(aValue)) and fConfigOK;
end;

function TTidy.GetWriteBack:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyWriteBack);
end;

procedure TTidy.SetWriteBack(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyWriteBack, aValue) and fConfigOK;
end;

function TTidy.GetShowMarkup:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyShowMarkup);
end;

procedure TTidy.SetShowMarkup(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyShowMarkup, aValue) and fConfigOK;
end;

function TTidy.GetShowWarnings:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyShowWarnings);
end;

procedure TTidy.SetShowWarnings(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyShowWarnings, aValue) and fConfigOK;
end;

function TTidy.GetQuiet:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyQuiet);
end;

procedure TTidy.SetQuiet(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyQuiet, aValue) and fConfigOK;
end;

function TTidy.GetHideEndTags:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyHideEndTags);
end;

procedure TTidy.SetHideEndTags(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyHideEndTags, aValue) and fConfigOK;
end;

function TTidy.GetXmlTags:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyXmlTags);
end;

procedure TTidy.SetXmlTags(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyXmlTags, aValue) and fConfigOK;
end;

function TTidy.GetXmlOut:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyXmlOut);
end;

procedure TTidy.SetXmlOut(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyXmlOut, aValue) and fConfigOK;
end;

function TTidy.GetXhtmlOut:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyXhtmlOut);
end;

procedure TTidy.SetXhtmlOut(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyXhtmlOut, aValue) and fConfigOK;
end;

function TTidy.GetHtmlOut:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyHtmlOut);
end;

procedure TTidy.SetHtmlOut(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyHtmlOut, aValue) and fConfigOK;
end;

function TTidy.GetXmlDecl:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyXmlDecl);
end;

procedure TTidy.SetXmlDecl(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyXmlDecl, aValue) and fConfigOK;
end;

function TTidy.GetUpperCaseTags:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyUpperCaseTags);
end;

procedure TTidy.SetUpperCaseTags(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyUpperCaseTags, aValue) and fConfigOK;
end;

function TTidy.GetUpperCaseAttrs:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyUpperCaseAttrs);
end;

procedure TTidy.SetUpperCaseAttrs(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyUpperCaseAttrs, aValue) and fConfigOK;
end;

function TTidy.GetMakeBare:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyMakeBare);
end;

procedure TTidy.SetMakeBare(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyMakeBare, aValue) and fConfigOK;
end;

function TTidy.GetMakeClean:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyMakeClean);
end;

procedure TTidy.SetMakeClean(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyMakeClean, aValue) and fConfigOK;
end;

function TTidy.GetLogicalEmphasis:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyLogicalEmphasis);
end;

procedure TTidy.SetLogicalEmphasis(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyLogicalEmphasis, aValue) and fConfigOK;
end;

function TTidy.GetDropPropAttrs:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyDropPropAttrs);
end;

procedure TTidy.SetDropPropAttrs(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyDropPropAttrs, aValue) and fConfigOK;
end;

function TTidy.GetDropFontTags:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyDropFontTags);
end;

procedure TTidy.SetDropFontTags(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyDropFontTags, aValue) and fConfigOK;
end;

function TTidy.GetDropEmptyParas:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyDropEmptyParas);
end;

procedure TTidy.SetDropEmptyParas(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyDropEmptyParas, aValue) and fConfigOK;
end;

function TTidy.GetFixComments:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyFixComments);
end;

procedure TTidy.SetFixComments(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyFixComments, aValue) and fConfigOK;
end;

function TTidy.GetBreakBeforeBR:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyBreakBeforeBR);
end;

procedure TTidy.SetBreakBeforeBR(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyBreakBeforeBR, aValue) and fConfigOK;
end;

function TTidy.GetBurstSlides:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyBurstSlides);
end;

procedure TTidy.SetBurstSlides(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyBurstSlides, aValue) and fConfigOK;
end;

function TTidy.GetNumEntities:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyNumEntities);
end;

procedure TTidy.SetNumEntities(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyNumEntities, aValue) and fConfigOK;
end;

function TTidy.GetQuoteMarks:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyQuoteMarks);
end;

procedure TTidy.SetQuoteMarks(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyQuoteMarks, aValue) and fConfigOK;
end;

function TTidy.GetQuoteNbsp:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyQuoteNbsp);
end;

procedure TTidy.SetQuoteNbsp(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyQuoteNbsp, aValue) and fConfigOK;
end;

function TTidy.GetQuoteAmpersand:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyQuoteAmpersand);
end;

procedure TTidy.SetQuoteAmpersand(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyQuoteAmpersand, aValue) and fConfigOK;
end;

function TTidy.GetWrapAttVals:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyWrapAttVals);
end;

procedure TTidy.SetWrapAttVals(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyWrapAttVals, aValue) and fConfigOK;
end;

function TTidy.GetWrapScriptlets:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyWrapScriptlets);
end;

procedure TTidy.SetWrapScriptlets(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyWrapScriptlets, aValue) and fConfigOK;
end;

function TTidy.GetWrapSection:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyWrapSection);
end;

procedure TTidy.SetWrapSection(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyWrapSection, aValue) and fConfigOK;
end;

function TTidy.GetWrapAsp:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyWrapAsp);
end;

procedure TTidy.SetWrapAsp(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyWrapAsp, aValue) and fConfigOK;
end;

function TTidy.GetWrapJste:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyWrapJste);
end;

procedure TTidy.SetWrapJste(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyWrapJste, aValue) and fConfigOK;
end;

function TTidy.GetWrapPhp:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyWrapPhp);
end;

procedure TTidy.SetWrapPhp(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyWrapPhp, aValue) and fConfigOK;
end;

function TTidy.GetFixBackslash:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyFixBackslash);
end;

procedure TTidy.SetFixBackslash(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyFixBackslash, aValue) and fConfigOK;
end;

function TTidy.GetIndentAttributes:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyIndentAttributes);
end;

procedure TTidy.SetIndentAttributes(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyIndentAttributes, aValue) and fConfigOK;
end;

function TTidy.GetXmlPIs:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyXmlPIs);
end;

procedure TTidy.SetXmlPIs(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyXmlPIs, aValue) and fConfigOK;
end;

function TTidy.GetXmlSpace:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyXmlSpace);
end;

procedure TTidy.SetXmlSpace(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyXmlSpace, aValue) and fConfigOK;
end;

function TTidy.GetEncloseBodyText:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyEncloseBodyText);
end;

procedure TTidy.SetEncloseBodyText(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyEncloseBodyText, aValue) and fConfigOK;
end;

function TTidy.GetEncloseBlockText:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyEncloseBlockText);
end;

procedure TTidy.SetEncloseBlockText(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyEncloseBlockText, aValue) and fConfigOK;
end;

function TTidy.GetKeepFileTimes:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyKeepFileTimes);
end;

procedure TTidy.SetKeepFileTimes(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyKeepFileTimes, aValue) and fConfigOK;
end;

function TTidy.GetWord2000:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyWord2000);
end;

procedure TTidy.SetWord2000(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyWord2000, aValue) and fConfigOK;
end;

function TTidy.GetMark:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyMark);
end;

procedure TTidy.SetMark(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyMark, aValue) and fConfigOK;
end;

function TTidy.GetEmacs:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyEmacs);
end;

procedure TTidy.SetEmacs(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyEmacs, aValue) and fConfigOK;
end;

function TTidy.GetLiteralAttribs:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyLiteralAttribs);
end;

procedure TTidy.SetLiteralAttribs(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyLiteralAttribs, aValue) and fConfigOK;
end;

function TTidy.GetBodyOnly:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyBodyOnly);
end;

procedure TTidy.SetBodyOnly(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyBodyOnly, aValue) and fConfigOK;
end;

function TTidy.GetFixUri:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyFixUri);
end;

procedure TTidy.SetFixUri(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyFixUri, aValue) and fConfigOK;
end;

function TTidy.GetLowerLiterals:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyLowerLiterals);
end;

procedure TTidy.SetLowerLiterals(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyLowerLiterals, aValue) and fConfigOK;
end;

function TTidy.GetHideComments:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyHideComments);
end;

procedure TTidy.SetHideComments(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyHideComments, aValue) and fConfigOK;
end;

function TTidy.GetIndentCdata:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyIndentCdata);
end;

procedure TTidy.SetIndentCdata(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyIndentCdata, aValue) and fConfigOK;
end;

function TTidy.GetForceOutput:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyForceOutput);
end;

procedure TTidy.SetForceOutput(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyForceOutput, aValue) and fConfigOK;
end;

function TTidy.GetAsciiChars:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyAsciiChars);
end;

procedure TTidy.SetAsciiChars(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyAsciiChars, aValue) and fConfigOK;
end;

function TTidy.GetJoinClasses:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyJoinClasses);
end;

procedure TTidy.SetJoinClasses(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyJoinClasses, aValue) and fConfigOK;
end;

function TTidy.GetJoinStyles:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyJoinStyles);
end;

procedure TTidy.SetJoinStyles(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyJoinStyles, aValue) and fConfigOK;
end;

function TTidy.GetEscapeCdata:bool;
begin
  Result := tidyOptGetBool(fTidyDoc, TidyEscapeCdata);
end;

procedure TTidy.SetEscapeCdata(aValue: bool);
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyEscapeCdata, aValue) and fConfigOK;
end;

function TTidy.GetCSSPrefix:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyCSSPrefix);
end;

procedure TTidy.SetCSSPrefix(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyCSSPrefix, pChar(aValue)) and fConfigOK;
end;

function TTidy.GetBlockTags:string;
begin
  Result := tidyOptGetValue(fTidyDoc, TidyBlockTags);
end;

procedure TTidy.SetBlockTags(aValue: string);
begin
  fConfigOK := tidyOptSetValue(fTidyDoc, TidyBlockTags, pchar(aValue)) and fConfigOK;
end;


{$IFDEF SUPPORT_ASIAN_ENCODINGS}
  function TTidy.GetNCR:bool;
  begin
    Result := tidyOptGetBool(fTidyDoc, TidyNCR);
  end;

  procedure TTidy.SetNCR(aValue: bool);
  begin
    fConfigOK := tidyOptSetBool(fTidyDoc, TidyNCR, aValue) and fConfigOK;
  end;
{$ENDIF SUPPORT_ASIAN_ENCODINGS}


{$IFDEF SUPPORT_UTF16_ENCODINGS}
  function TTidy.GetOutputBOM:TidyTriState;
  begin
    Result := TidyTriState(tidyOptGetInt(fTidyDoc, TidyOutputBOM));
  end;
  procedure TTidy.SetOutputBOM(aValue: TidyTriState);
  begin
    fConfigOK := tidyOptSetInt(fTidyDoc, TidyOutputBOM, uint(aValue)) and fConfigOK;
  end;
{$ENDIF SUPPORT_UTF16_ENCODINGS}


function DoReport( tdoc: TidyDoc;  lvl: TidyReportLevel;  line: uint; col: uint;  mssg: ctmbstr ): Boolean; cdecl;
var
  bWriteOut:boolean;
  ThisTidy:tTidy;
begin
  result:=yes;
  bWriteOut:=true;
  ThisTidy:=tTidy(TidyGetAppData(tDoc));
  if ( ThisTidy <> nil ) then with ThisTidy do begin
    if Assigned({$IFNDEF FPC}@{$ENDIF}OnReport)
    and ({$IFNDEF FPC}@{$ENDIF}OnReport <> nil )
    then begin
      OnReport(ThisTidy, lvl, line, col, ansistring(mssg), bWriteOut);
      Result:= bWriteOut;
    end;
  end;
end;


procedure tTidy.SetOnReport(aReportEvent:tTidyReportEvent);
begin
  fReportFilter:=nil;
  fOnReport:=aReportEvent;
  if ( {$IFNDEF FPC}@{$ENDIF}fOnReport = nil )
  then tidySetReportFilter(fTidyDoc, nil)
  else tidySetReportFilter(fTidyDoc, @DoReport);
end;

procedure tTidy.SetReportFilter(aReportFilter:TidyReportFilter);
begin
  fReportFilter:=aReportFilter;
  fOnReport:=nil;
  tidySetReportFilter(fTidyDoc, fReportFilter);
end;



constructor tTidy.Create;

begin
  inherited Create;
  fTidyDoc:=tidyCreate;
  TidySetAppData(fTidyDoc, self);
  fInputFile:='';
  fInputString:=nil;
  fHaveStdIn:=False;
  fCongruent:=False;
  fConfigOK:=True;
end;

procedure tTidy.ParseFile ( const aFilename:string );
begin
  fInputFile:=aFilename;
  fInputString:=nil;
  fHaveStdIn:=False;
end;


{
  It seems much more efficient to simply get a
  pointer to the input string, but Kylix has
  trouble remembering where it puts things -
  so it needs its very own copy...
}

{$IFDEF FPC}
  procedure  tTidy.ParseString ( const aInputString:string );
  begin
    fInputFile:='';
    if (aInputString <> '' )
    then fInputString:=@aInputString[1]
    else fInputString:=nil;
    fHaveStdIn:=False;
  end;
{$ELSE}
  procedure  tTidy.ParseString ( const aInputString:string );
  var L:Uint;
  begin
    fInputFile:='';
    if (aInputString <> '' )
    then begin
      L:=Length(aInputString);
      GetMem(fInputString, L+1);
      FillChar(fInputString[0], L+1, #0);
      StrLCopy(fInputString, @aInputString[1], L);
    end else fInputString:=nil;
    fHaveStdIn:=False;
  end;
{$ENDIF}

function tTidy.DoOutputString:string;
var
  I, L:ulong;
  Src, Trg:string;
  MaxAttempts:ulong;
  IsCongruent:Boolean;
  Err:LongInt;
begin
  Result:='';
  Src:='';
  I:=1;
  if fCongruent then begin
    MaxAttempts:=10;
    IsCongruent:=False;
  end else begin
    MaxAttempts:=1;
    IsCongruent:=False;
  end;
  while ( I <= MaxAttempts) and ( not IsCongruent) do begin
    Err:=TidyCleanAndRepair(fTidyDoc);
    if ( Err < 0 ) then  { Should we handle the error here ? };
    Err:=TidyRunDiagnostics(fTidyDoc);
    if ( Err < 0 ) then  { Should we handle the error here ? };
    L:=1;
    SetLength(Trg, L);
    Err:=tidySaveString(fTidyDoc, @Trg[1], @L);
    if Err = -(ENOMEM) then begin
      SetLength(Trg, L);
      tidySaveString(fTidyDoc, @Trg[1], @L);
    end;
    IsCongruent:= IsCongruent or ( Src = Trg );
    if not IsCongruent then begin
      Src:=Trg;
      tidyParseString (fTidyDoc, pChar(Src));
    end;
    inc(I);
  end;
  fCongruent:=IsCongruent;
  Src:='';
  Result:=Trg;
end;


function tTidy.GetOutputString:string;
var
  Err:LongInt;
begin
  Result:='';
  if fCongruent then IndentContent:=TidyNoState;
  if ( fInputFile <> '' ) then begin
    Err:=tidyParseFile(fTidyDoc, pChar(fInputFile));
    if ( Err < 0 ) then EXIT;
  end else if ( fInputString <> nil ) then begin
    Err:=tidyParseString (fTidyDoc, pChar(fInputString));
    if ( Err < 0 ) then { Should we handle the error here ? };
    {$IFNDEF FPC}
    FreeMem(fInputString); {MAYBE NOT???}
    {$ENDIF}
  end else if fHaveStdIn then begin
    Err:=tidyParseStdin(fTidyDoc);
    if ( Err < 0 ) then  { Should we handle the error here ? };
  end else EXIT; { We must have some kind of input }
  Result:=DoOutputString;
  if ( Result <> '' ) and ( length(Result) < 3 ) and ( Result[1] in [#10, #13] ) then Result:='';
end;


function tTidy.GetHtml:string;
begin
  fConfigOK:= tidyOptSetBool(fTidyDoc, TidyHTMLOut, True) and fConfigOK;
  Result:=GetOutputString;
end;


function tTidy.GetXhtml:string;
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyXHTMLOut, True) and fConfigOK;
  Result:=GetOutputString;
end;

function tTidy.GetXml:string;
begin
  fConfigOK := tidyOptSetBool(fTidyDoc, TidyXMLOut, True) and fConfigOK;
  Result:=GetOutputString;
end;

function tTidy.GetErrorCount:uint;
begin
  Result:=tidyErrorCount(fTidyDoc);
end;

function tTidy.GetWarningCount:uint;
begin
  Result:=tidyWarningCount(fTidyDoc);
end;

function tTidy.GetAccessWarningCount:uint;
begin
  Result:=tidyAccessWarningCount(fTidyDoc);
end;

function tTidy.GetConfigErrorCount:uint;
begin
  Result:=tidyConfigErrorCount(fTidyDoc);
end;

function tTidy.GetStatus:LongInt;
begin
  Result:=tidyStatus(fTidyDoc)
end;

function tTidy.LoadConfigFile( const aFilename:string): bool;
begin
  Result:=tidyLoadConfig(fTidyDoc, pChar(aFilename)) >= 0;
end;

function tTidy.SaveConfigFile( const aFilename:string): bool;
begin
  Result:=tidyOptSaveFile(fTidyDoc, pChar(aFilename)) >= 0;
end;

function tTidy.GetRootNode:pTidyNode;
begin
  Result:=TidyGetRoot(fTidyDoc);
end;

class function tTidy.ReleaseDate:string;
begin
  Result:=string(tidyReleaseDate);
end;

procedure tTidy.Reset;
var
  TmpDoc:TidyDoc;
  TmpErrFile:string;
begin
  TmpErrFile:=ErrorFile;
  TmpDoc:=TidyCreate;
  tidyOptCopyConfig(TmpDoc, fTidyDoc);
  tidyRelease(fTidyDoc);
  fTidyDoc:=nil;
  fTidyDoc:=TidyCreate;
  tidyOptCopyConfig(fTidyDoc, TmpDoc);
  tidyRelease(TmpDoc);
  if ( TmpErrFile <> '' ) then ErrorFile:=TmpErrFile;
  TidySetAppData(fTidyDoc, self);
  if ( {$IFNDEF FPC}@{$ENDIF}fOnReport <> nil )
  then SetOnReport(fOnReport)
  else if ( {$IFNDEF FPC}@{$ENDIF}fReportFilter <> nil )
       then SetReportFilter( fReportFilter );
end;

destructor tTidy.Destroy;
begin
  TidyRelease(fTidyDoc);
  inherited Destroy;
end;

type
  TidyMalloc  = function ( len: LongWord ): pointer; cdecl;
  TidyRealloc = function ( buf: pointer; len: LongWord ): pointer; cdecl;
  TidyFree  = procedure ( buf: pointer ); cdecl;
  TidyPanic = procedure (mssg: pChar); cdecl;

function tidySetMallocCall  ( fmalloc: TidyMalloc ):   boolean; cdecl; external TIDY_LIB;
function tidySetReallocCall ( frealloc: TidyRealloc ): boolean; cdecl; external TIDY_LIB;
function tidySetFreeCall    ( ffree: TidyFree ):       boolean; cdecl; external TIDY_LIB;
function tidySetPanicCall   ( fpanic: TidyPanic ):     boolean; cdecl; external TIDY_LIB;

function malloc ( Size: LongWord ): Pointer; cdecl; external LIB_C;
procedure free ( P: pointer ); cdecl; external LIB_C;
function realloc ( P: Pointer; Size: LongWord ): pointer; cdecl; external LIB_C;

procedure Panic(mssg:pChar); cdecl;
begin
{ Not sure exactly how to handle this -
    just cross your fingers and hope that Panic() never gets called !!!
}
end;


initialization
  tidySetMallocCall  ( {$IFDEF FPC}@{$ENDIF}malloc  );
  tidySetReallocCall ( {$IFDEF FPC}@{$ENDIF}realloc );
  tidySetFreeCall    ( {$IFDEF FPC}@{$ENDIF}free    );
  tidySetPanicCall   ( {$IFDEF FPC}@{$ENDIF}panic   );

finalization


end.