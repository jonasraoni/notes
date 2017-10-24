//
//    TFrmPrinc - form principal do Notes.
//
//    Notes, https://github.com/jonasraoni/notes
//    Copyright (C) 2003-2004, Equipe do Notes.
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

{.$DEFINE GXDEBUG}

(*
@abstract(frm_Princ - form principal do Notes.)
@author(Anderson R. Barbieri <notesnr@ig.com.br>)
@author(Jonas Raoni Soares da Silva <jonasraoni@gmail.com>)
@author(Josimar Silva <josimar_me@yahoo.com.br>)
@author(André Muraro <bug_do_notes@yahoo.com.br>, correção de bugs.)
@author(Tiago Schubert <tiagosch@q3arena.com>, correção de bugs.)
Esta é a Unit principal do Notes - nela todos os componentes se integram
e é dela que a maior parte dos diálogos são chamados.
<BR><BR>

<B>O Editor</B><BR>
O componente de edição do notes é um TSynEdit (http://synedit.sf.net) com
alguns comportamentos extras. Estes comportamentos estão na classe
@link(TNotesEditorTab)(vide sessão abaixo). Praticamente todos os comandos de
edição são do SynEdit ou TNotesEditorTab, então é nestas classes que
o código dos comandos de edição deve ser procurado.
<BR><BR>

<B>TNotesEditorTab</B><BR>
Ao invés de modificar o componente SynEdit ou derivar um componente dele, optou-se
por criar uma classe que se "acopla", se une ao SynEdit para fazê-lo comportar-se
do jeito que a Equipe do Notes queria. Esta classe é a @link(TNotesEditorTab).
Além de modificar o comportmento padrão do SynEdit, um TNotesEditorTab também
agrupa em si mesmo um SynEdit, uma tab (TTabSheet) e um colorador de código
(NotesHighlighter). Ela permite acessar estes três através de propriedades:
"TNotesEditorTab.Editor", "TNotesEditorTab.Tab" e "TNotesEditorTab.Highlighter".
A unit NotesTabsManager traz diversos métodos que possibilitam manipular
as tabs do Notes. Ela é que cria, destrói e permite acessar os editores.

<B>Os painéis</B><BR>
Os painéis que você vê na interface do Notes são classes derivadas
de TNotesPanel. Para criar um novo painel, crie uma unit chamada
NotesXXXXXPanel na pasta onde ficam as units dos painéis. Você
deverá criar uma classe derivando de TNotesPanel e implementado
os métodos desta classe. Depois, adicione a unit no uses desta
unit e crie-o junto com os outros painéis no FormCreate.

<BR><BR>

<B>Ações, teclas de atalho, menus</B><BR>
O Notes usa uma TActionList para gerenciar todas as suas ações, inclusive
todas as ações (desde um movimento do cursor até um comando de copiar)
do editor. A classe TNotesKeymaps é capaz de gerenciar as teclas
de atalho de TActionLists - é assim que as teclas de atalho podem ser
configuráveis. Quando for criar qualquer item no menu, crie uma ação
antes e depois apenas set ela na propriedade "Action" do novo item
do menu. É importante que cada ação que você crie tenha uma tag
diferente de todas as outras (o TNotesKeymaps usa a propriedade tag
da TAction para diferenciar uma TAction de outra).

<BR><BR>

<B>Menus de arquivos recentes, menus criados através de diretórios.</B><BR>
Os menus "arquivos recentes" e "favoritos" são criados usando a classe
@link(TNotesMRUMenu). Menus que são criados a partir de arquivos de uma pasta
usam a rotina BuildMenuFromFolderEx (veja os comentários desta rotina
para mais informações).
<BR><BR>

<B>Profiles e configurações</B>
O Notes guarda todos os dados que podem ser configurados pelo usuário
em um profile. Um profile é basicamente uma pasta cheia de arquivos e
diretórios, cada um com uma função importante no Notes. Um usuário pode
ter inúmeros profile.

Um profile fica na pasta de "dados de aplicativos" do windows. Um exemplo
de localizalização de profile poderia ser o seguinte:<BR>
@code(C:\WINDOWS\Application Data\Notes\profiles\default)
<BR><BR>
Onde:<BR>
@code(C:\WINDOWS\Application Data\) - pasta 'AppData' do windows. <BR>
@code(C:\WINDOWS\Application Data\Notes} - pasta do Notes dentro de AppData.<BR>
@code(C:\WINDOWS\Application Data\Notes\profiles) - pasta dos profiles.<BR>
@code(C:\WINDOWS\Application Data\Notes\profiles\default) - pasta do profile chamado "default".<BR>
<BR><BR>

O nome do profile ativo (aquele que está sendo usado pelo usuário) fica
em um arquivo chamado "ActiveProfile.dat" dentro da pasta raiz dos profiles.
Dentro da pasta do profile temos os seguintes arquivos:<BR>
@code(extlang.dat) - associações entre filetypes e extensões.<BR>
@code(fav.dat) - lista dos arquivos favoritos.<BR>
@code(mru.dat) - lista dos arquivos recentes.<BR>
@code(nconfig.xml) - configurações do usuário. Vide @(TNotesConfig).<BR>
@code(nopts.xml) - opções que permitem restaurar o estado do Notes. vide @link(TNotesOptions).<BR>
@code(npanels.xml) - opções dos painéis.
<BR>
Os profiles são gerenciados pela unit @link(NotesProfile). Use o objeto
"nProfile" para acessar todo tipo de configuração do Notes. Para usar este
objeto, acrescente a unit NotesGlobals no uses. Dentro dos profiles
existe um subdiretório chamado "run" onde ficam guardados os itens do menu
"executar". Outro subdiretório é o "FileTypes", onde ficam guardados os
arquivos de configuração específico para cada tipo de arquivo que o Notes
suporta. Cada tipo de arquivo tem o seu próprio diretório e é dentro do
subdiretório de um FileType (p. ex. "javaScript") que ficam a biblioteca
e os templates para aquele tipo de arquivo.<BR><BR>

Para aprender a cria suporte a novos tipos de arquivo, veja a documentação
do Notes. Lá é explicado também a estrutura dos diretórios.
*)
unit frm_princ;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  Dialogs, Menus, ImgList, ToolWin, StdCtrls, Printers, ExtCtrls, CommCtrl,
  Buttons, {$IFDEF GXDEBUG} DbugIntf, {$ENDIF} NotesMRU, SynEdit,
  SynEditKeyCmds, bLink, NotesConfig, NotesEditorTab, NotesOptions,
  ShellAPI, ShlObj, NotesGlobals, SynEditPrint, NotesProfile, NotesPanels,
  NotesEditorTabPosList, NotesStartPage, NotesTabsManager, ActnList,
  NotesKeymaps, NotesTranslation, NotesMacros;

type
  {Form principal do Notes.}
  TfrmMain = class(TForm)
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    icosXP: TImageList;
    MainMenu: TMainMenu;
    ActionList: TActionList;
    actCommandsCompressHtml: TAction;
    actCommandsConvertSpecialHtmlChars: TAction;
    actCommandsFixHTML: TAction;
    actCommandsFixlinebreaks: TAction;
    actCommandsHTMLtoXHTML: TAction;
    actCommandsInsertDateTime: TAction;
    actCommandsInsertFile: TAction;
    actCommandsInsertImage: TAction;
    actCommandsUndo: TAction;
    actCommandsVerifyHTML: TAction;
    actCommandsVerifyXML: TAction;
    actCommandsWrap: TAction;
    actEditAddToClipboard: TAction;
    actEditComment: TAction;
    actEditCopy: TAction;
    actEditCut: TAction;
    actEditDel: TAction;
    actEditFormatLowercase: TAction;
    actEditFormatTitlecase: TAction;
    actEditFormatToggleCase: TAction;
    actEditFormatUppercase: TAction;
    actEditIndent: TAction;
    actEditLineDel: TAction;
    actEditLineDuplicate: TAction;
    actEditLineSelect: TAction;
    actEditorBreakLine: TAction;
    actEditorBreakLineFromEOL: TAction;
    actEditorBreakLineNoMove: TAction;
    actEditorClearAll: TAction;
    actEditorDeleteBOL: TAction;
    actEditorDeleteEOL: TAction;
    actEditorDeleteLastChar: TAction;
    actEditorDeleteLastWord: TAction;
    actEditorDeleteWord: TAction;
    actEditorDown: TAction;
    actEditorEditorBottom: TAction;
    actEditorEditorTop: TAction;
    actEditorGotoMatchBracket: TAction;
    actEditorLeft: TAction;
    actEditorLineEnd: TAction;
    actEditorLineStart: TAction;
    actEditorPageBottom: TAction;
    actEditorPageDown: TAction;
    actEditorPageLeft: TAction;
    actEditorPageRight: TAction;
    actEditorPageTop: TAction;
    actEditorPageUp: TAction;
    actEditorRight: TAction;
    actEditorScrollDown: TAction;
    actEditorScrollLeft: TAction;
    actEditorScrollRight: TAction;
    actEditorScrollUp: TAction;
    actEditorSelDown: TAction;
    actEditorSelEditorBottom: TAction;
    actEditorSelEditorTop: TAction;
    actEditorSelLeft: TAction;
    actEditorSelLineEnd: TAction;
    actEditorSelLinestart: TAction;
    actEditorSelPageBottom: TAction;
    actEditorSelPageDown: TAction;
    actEditorSelPageLeft: TAction;
    actEditorSelPageRight: TAction;
    actEditorSelPageTop: TAction;
    actEditorSelPageUp: TAction;
    actEditorSelRight: TAction;
    actEditorSelUp: TAction;
    actEditorSelWord: TAction;
    actEditorSelWordLeft: TAction;
    actEditorSelWordRigth: TAction;
    actEditorShiftTab: TAction;
    actEditorTab: TAction;
    actEditorTogleMode: TAction;
    actEditorUp: TAction;
    actEditorWordLeft: TAction;
    actEditorWordRight: TAction;
    actEditPaste: TAction;
    actEditRedo: TAction;
    actEditSelectAll: TAction;
    actEditSmartPaste: TAction;
    actEditUncomment: TAction;
    actEditUndo: TAction;
    actEditUnindent: TAction;
    actFileBookmakersAdd: TAction;
    actFileBookmakersEdit: TAction;
    actFileClose: TAction;
    actFileCloseAll: TAction;
    actFileExit: TAction;
    actFileMruClear: TAction;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileOpenAtCursor: TAction;
    actFilePrint: TAction;
    actFileRevert: TAction;
    actFileSave: TAction;
    actFileSaveAll: TAction;
    actFileSaveAs: TAction;
    actFileSaveSelection: TAction;
    actFileStatistics: TAction;
    actHelpAbout: TAction;
    actHelpBugreport: TAction;
    actHelpCopy: TAction;
    actHelpIndex: TAction;
    actHelpReadme: TAction;
    actHelpTellAFriend: TAction;
    actHelpWebsite: TAction;
    actOptionsPreferences: TAction;
    actOptionsProfiles: TAction;
    actOptionsReadonly: TAction;
    actOptionsTemplates: TAction;
    actOptionsTopmost: TAction;
    actProjectAdd: TAction;
    actProjectCompile: TAction;
    actProjectNew: TAction;
    actProjectOpen: TAction;
    actProjectOpenAll: TAction;
    actProjectOptions: TAction;
    actProjectPublish: TAction;
    actProjectRemove: TAction;
    actProjectRun: TAction;
    actRunConfigure: TAction;
    actRunFile: TAction;
    actRunLast: TAction;
    actSearchBack: TAction;
    actSearchClearMarks: TAction;
    actSearchClearProblems: TAction;
    actSearchFind: TAction;
    actSearchFindNext: TAction;
    actSearchFindPrevius: TAction;
    actSearchForward: TAction;
    actSearchGotocol: TAction;
    actSearchGotoline: TAction;
    actSearchGotoMark: TAction;
    actSearchNextMark: TAction;
    actSearchNextProblem: TAction;
    actSearchPreviusMark: TAction;
    actSearchPreviusProblem: TAction;
    actSearchReplace: TAction;
    actSearchToggleMark: TAction;
    actViewNexttab: TAction;
    actViewPreviustab: TAction;
    actViewStatusBar: TAction;
    actViewToolbar: TAction;
    mnCommands: TMenuItem;
    mnCommandsCompressHTML: TMenuItem;
    mnCommandsConvertSpecialHtmlChars: TMenuItem;
    mnCommandsFixHTML: TMenuItem;
    mnCommandsFixlinebreaks: TMenuItem;
    mnCommandsHTMLtoXHTML: TMenuItem;
    mnCommandsInsertDateTime: TMenuItem;
    mnCommandsInsertfile: TMenuItem;
    mnCommandsInsertImage: TMenuItem;
    mnCommandsVerifyHTML: TMenuItem;
    mnCommandsVerifyXML: TMenuItem;
    mnCommandsWrap: TMenuItem;
    mnCommandUndo: TMenuItem;
    mnEdit: TMenuItem;
    mnEditAddToClipboard: TMenuItem;
    mnEditComment: TMenuItem;
    mnEditCopy: TMenuItem;
    mnEditCut: TMenuItem;
    mnEditDel: TMenuItem;
    mnEditFormat: TMenuItem;
    mnEditFormatLowercase: TMenuItem;
    mnEditFormatTitlecase: TMenuItem;
    mnEditFormatTogglecase: TMenuItem;
    mnEditFormatUppercase: TMenuItem;
    mnEditIndent: TMenuItem;
    mnEditLine: TMenuItem;
    mnEditLineDel: TMenuItem;
    mnEditLineDuplicate: TMenuItem;
    mnEditLineSelect: TMenuItem;
    mnEditPaste: TMenuItem;
    mnEditRedo: TMenuItem;
    mnEditSelectAll: TMenuItem;
    mnEditUncomment: TMenuItem;
    mnEditUndo: TMenuItem;
    mnEditUnindent: TMenuItem;
    mnFile: TMenuItem;
    mnFileBookmakers: TMenuItem;
    mnFileBookmakersAdd: TMenuItem;
    mnFileBookmakersEdit: TMenuItem;
    mnFileClose: TMenuItem;
    mnFileCloseAll: TMenuItem;
    mnFileExit: TMenuItem;
    mnFileMru: TMenuItem;
    mnFileMruClear: TMenuItem;
    mnFileNew: TMenuItem;
    mnFileOpen: TMenuItem;
    mnFilePrint: TMenuItem;
    mnFileRevert: TMenuItem;
    mnFileSave: TMenuItem;
    mnFileSaveAll: TMenuItem;
    mnFileSaveAs: TMenuItem;
    mnFileSaveSelection: TMenuItem;
    mnFileStatistics: TMenuItem;
    mnHelp: TMenuItem;
    mnHelpAbout: TMenuItem;
    mnHelpCopy: TMenuItem;
    mnHelpIndex: TMenuItem;
    mnHelpReadme: TMenuItem;
    mnHelpSugestion: TMenuItem;
    mnHelpTellAFriend: TMenuItem;
    mnHelpWebsite: TMenuItem;
    mnOptions: TMenuItem;
    mnOptionsEOL: TMenuItem;
    mnOptionsEolLinux: TMenuItem;
    mnOptionsEolMac: TMenuItem;
    mnOptionsEOLWin: TMenuItem;
    mnOptionsFileType: TMenuItem;
    mnOptionsPreferences: TMenuItem;
    mnOptionsProfiles: TMenuItem;
    mnOptionsReadonly: TMenuItem;
    mnOptionsShortcuts: TMenuItem;
    mnOptionsTemplates: TMenuItem;
    mnOptionsTopmost: TMenuItem;
    mnProject: TMenuItem;
    mnProjectAdd: TMenuItem;
    mnProjectCompile: TMenuItem;
    mnProjectMru: TMenuItem;
    mnProjectNew: TMenuItem;
    mnProjectOpen: TMenuItem;
    mnProjectOpenall: TMenuItem;
    mnProjectOptions: TMenuItem;
    mnProjectPublish: TMenuItem;
    mnProjectRemove: TMenuItem;
    mnProjectRun: TMenuItem;
    mnRun: TMenuItem;
    mnRunConfigure: TMenuItem;
    mnRunFile: TMenuItem;
    mnRunLast: TMenuItem;
    mnRunOWNER: TMenuItem;
    mnSearch: TMenuItem;
    mnSearchBack: TMenuItem;
    mnSearchClearProblems: TMenuItem;
    mnSearchFind: TMenuItem;
    mnSearchFindNext: TMenuItem;
    mnSearchFindPrevius: TMenuItem;
    mnSearchForward: TMenuItem;
    mnSearchGotocol: TMenuItem;
    mnSearchGotoline: TMenuItem;
    mnSearchGotoMark: TMenuItem;
    mnSearchNextMark: TMenuItem;
    mnSearchNextProblem: TMenuItem;
    mnSearchPreviusMark: TMenuItem;
    mnSearchPreviusProblem: TMenuItem;
    mnSearchReplace: TMenuItem;
    mnSearchToggleMark: TMenuItem;
    mnuFileTypesOwner: TMenuItem;
    mnuPopDel: TMenuItem;
    mnView: TMenuItem;
    mnViewNexttab: TMenuItem;
    mnViewPreviustab: TMenuItem;
    mnViewStatusbar: TMenuItem;
    mnViewToolbar: TMenuItem;
    N1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N2: TMenuItem;
    N20: TMenuItem;
    N22: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N26: TMenuItem;
    N3: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N33: TMenuItem;
    N35: TMenuItem;
    N37: TMenuItem;
    N38: TMenuItem;
    N39: TMenuItem;
    N4: TMenuItem;
    N40: TMenuItem;
    N41: TMenuItem;
    N42: TMenuItem;
    N43: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    paBottom: TPanel;
    paCenter: TPanel;
    paLeft: TPanel;
    paRight: TPanel;
    pgEditor: TPageControl;
    poNextMark: TMenuItem;
    popColar: TMenuItem;
    popCopiar: TMenuItem;
    popDesfazer: TMenuItem;
    popFormat: TMenuItem;
    popIndent: TMenuItem;
    popMenu: TPopupMenu;
    popOpenatCursor: TMenuItem;
    popOutdent: TMenuItem;
    popRecortar: TMenuItem;
    popRefazer: TMenuItem;
    poPreviusMark: TMenuItem;
    popSelAll: TMenuItem;
    popSelFirstUpCase: TMenuItem;
    popSelInvertCase: TMenuItem;
    popSelLowerCase: TMenuItem;
    popSelUpcase: TMenuItem;
    poTabClose: TMenuItem;
    poTabFav: TMenuItem;
    poTabs: TPopupMenu;
    poTabSave: TMenuItem;
    poTabSaveAll: TMenuItem;
    poTabsCloseAll: TMenuItem;
    poTogleMark: TMenuItem;
    sbStatusBar: TStatusBar;
    tbBack: TToolButton;
    tbComment: TToolButton;
    tbCopy: TToolButton;
    tbCut: TToolButton;
    tbFind: TToolButton;
    tbForward: TToolButton;
    tbGotoline: TToolButton;
    tbIndent: TToolButton;
    tbNew: TToolButton;
    tbNextMark: TToolButton;
    tbOpen: TToolButton;
    tbPaste: TToolButton;
    tbPreviusMark: TToolButton;
    tbPrint: TToolButton;
    tbRedo: TToolButton;
    tbReplace: TToolButton;
    tbSave: TToolButton;
    tbSaveAll: TToolButton;
    tbToogleMark: TToolButton;
    tbUnComment: TToolButton;
    tbUndo: TToolButton;
    tbUnIndent: TToolButton;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    actViewHidePanels: TAction;
    mnViewHidePanels: TMenuItem;
    actOptionsKeymap: TAction;
    actOptionsWordWrap: TAction;
    mnOptionsWordWrap: TMenuItem;
    actEditTogleColumSel: TAction;
    mnEditTogleColumSel: TMenuItem;
    N21: TMenuItem;
    actMacrosRun: TAction;
    actMacrosRecord: TAction;
    actMacrosSave: TAction;
    mnMacros: TMenuItem;
    mnMacrosRecord: TMenuItem;
    mnMacrosRun: TMenuItem;
    mnMacrosSave: TMenuItem;
    mnMacrosOwner: TMenuItem;
    actMacrosConfigure: TAction;
    N23: TMenuItem;
    mnMacrosConfig: TMenuItem;
    mnSeachClearMarks: TMenuItem;
    actEditReindent: TAction;
    mnEditReindent: TMenuItem;
    actEditFormatTabToSpaces: TAction;
    actEditFormatSpacesToTabs: TAction;
    N27: TMenuItem;
    mnEditFormatTabsToSpaces: TMenuItem;
    mnEditFormatSpacesToTabs: TMenuItem;

    { Procedimentos ligado as ações do Form }

    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);

    // Vigia o tamanho dos painéis do Notes
    procedure CanNotesPanelResize(Sender: TObject; var NewWidth,
       NewHeight: Integer; var Resize: Boolean);
    // Atualiza a inteerface toda vez que a tab ativa é modificada
    procedure pgEditorChange(Sender: TObject);
    // Interpreta os cliques do usuário na StatusBar
    procedure sbStatusBarClick(Sender: TObject);
    // Chamado quando um intem da lista de arquivos recentes/favoritos é clicado
    procedure MruOrFavClick(Sender: TObject; const FileName: String);
    // Chamado quando um item do menu executar é clicado
    procedure NotesRunFile(sender: TObject);
    // roda uma macro
    procedure NotesRunMacro(sender: TObject);

    { Procedimentos chamados quando um menu vai ser mostrado
     para atulizar o status de certos items do menu }

    procedure popMenuPopup(Sender: TObject);
    procedure mnOptionsClick(Sender: TObject);
    procedure poTabsPopup(Sender: TObject);

    procedure doChangeFiletype(Sender: TObject);
    // Chamado quando o usuário quer mudar o tipo de final de linha do arquivo
    procedure mnuChangeBreakType(Sender: TObject);

    { Ações do Notes. Vide os Hints de cada uma para uma
      descrição.  }

    procedure actCommandsCompressHtmlExecute(Sender: TObject);
    procedure actCommandsConvertSpecialHtmlCharsExecute(Sender: TObject);
    procedure actCommandsFixHTMLExecute(Sender: TObject);
    procedure actCommandsFixlinebreaksExecute(Sender: TObject);
    procedure actCommandsHTMLtoXHTMLExecute(Sender: TObject);
    procedure actCommandsInsertDateTimeExecute(Sender: TObject);
    procedure actCommandsInsertFileExecute(Sender: TObject);
    procedure actCommandsInsertImageExecute(Sender: TObject);
    procedure actCommandsUndoExecute(Sender: TObject);
    procedure actCommandsVerifyHTMLExecute(Sender: TObject);
    procedure actCommandsVerifyXMLExecute(Sender: TObject);
    procedure actCommandsWrapExecute(Sender: TObject);
    procedure actFileCloseAllExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileRevertExecute(Sender: TObject);
    procedure actFileSaveAllExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveSelectionExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actHelpBugreportExecute(Sender: TObject);
    procedure actHelpCopyExecute(Sender: TObject);
    procedure actHelpIndexExecute(Sender: TObject);
    procedure actHelpReadmeExecute(Sender: TObject);
    procedure actHelpTellAFriendExecute(Sender: TObject);
    procedure actHelpWebsiteExecute(Sender: TObject);
    procedure actOptionsPreferencesExecute(Sender: TObject);
    procedure actOptionsProfilesExecute(Sender: TObject);
    procedure actOptionsReadonlyExecute(Sender: TObject);
    procedure actOptionsTemplatesExecute(Sender: TObject);
    procedure actOptionsTopmostExecute(Sender: TObject);
    procedure actProjectAddExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectOpenAllExecute(Sender: TObject);
    procedure actProjectOpenExecute(Sender: TObject);
    procedure actProjectOptionsExecute(Sender: TObject);
    procedure actProjectRemoveExecute(Sender: TObject);
    procedure actRunConfigureExecute(Sender: TObject);
    procedure actRunFileExecute(Sender: TObject);
    procedure actRunLastExecute(Sender: TObject);
    procedure actSearchBackExecute(Sender: TObject);
    procedure actSearchClearProblemsExecute(Sender: TObject);
    procedure actSearchFindExecute(Sender: TObject);
    procedure actSearchFindNextExecute(Sender: TObject);
    procedure actSearchFindPreviusExecute(Sender: TObject);
    procedure actSearchForwardExecute(Sender: TObject);
    procedure actSearchGotocolExecute(Sender: TObject);
    procedure actSearchGotolineExecute(Sender: TObject);
    procedure actSearchNextMarkExecute(Sender: TObject);
    procedure actSearchNextProblemExecute(Sender: TObject);
    procedure actSearchPreviusMarkExecute(Sender: TObject);
    procedure actSearchPreviusProblemExecute(Sender: TObject);
    procedure actSearchReplaceExecute(Sender: TObject);
    procedure actSearchToggleMarkExecute(Sender: TObject);
    procedure actViewNexttabExecute(Sender: TObject);
    procedure actViewPreviustabExecute(Sender: TObject);
    procedure actViewStatusBarExecute(Sender: TObject);
    procedure actViewToolbarExecute(Sender: TObject);
    procedure actFileMruClearExecute(Sender: TObject);
    procedure actFileStatisticsExecute(Sender: TObject);
    procedure actFileBookmakersAddExecute(Sender: TObject);
    procedure actFileBookmakersEditExecute(Sender: TObject);
    procedure actFileOpenAtCursorExecute(Sender: TObject);
    procedure actSearchGotoMarkExecute(Sender: TObject);
    procedure actSearchClearMarksExecute(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure pgEditorEnter(Sender: TObject);
    procedure pgEditorExit(Sender: TObject);
    procedure actViewHidePanelsExecute(Sender: TObject);
    procedure mnFileClick(Sender: TObject);
    procedure actOptionsKeymapExecute(Sender: TObject);
    procedure actOptionsWordWrapExecute(Sender: TObject);
    procedure actEditTogleColumSelExecute(Sender: TObject);
    procedure ActionListExecute(Action: TBasicAction;
      var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actMacrosRunExecute(Sender: TObject);
    procedure actMacrosRecordExecute(Sender: TObject);
    procedure actMacrosSaveExecute(Sender: TObject);
    procedure actMacrosConfigureExecute(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

  private
    fKeymap: TNotesKeymap;
    fMacros: TNotesMacros;
    OutType: Integer;
    fStopResize: boolean;
    fLastRunItem: TNotesRunItem;
    fLastRunActiveFile: string;
    PanelsManager: TNotesPanelManager;

    fTranslation: TNotesTranslation;

    // strings localizadas
    l_Insert: string;
    l_Overwrite: string;

    procedure DoStopNotesShortcuts(Sender: TObject);
    procedure DoRestoreNotesShortcuts(Sender: TObject);

    procedure SetStatusBarMessage(const msg: string);

    // Constrói os menus baseados em pastas (macros e executar)
    procedure BuildFolderBasedMenus;
    // Adiciona um item ao menu
    function AddMenuItem(AOwner: TComponent; ACaption: TCaption; AOnClick: TNotifyEvent;
       AHint: String; AMenuItem: TMenuItem): TMenuItem;
    // Chamado pelo dlg de configuração de macros
    procedure DoEditMacro(const FileName: string; var ReloadItems: boolean);
    // Chamado pelo dlg de configuração dos items do menu executar
    procedure DoEditRunItem(const FileName: string; var ReloadItems: boolean);
    // Chamado pelo dlg de configuração dos items do menu executar
    procedure DoCreateRunItem(const FileName: string; var ReloadItems: boolean);

    // implementação do procedimento NotesExec
    procedure doNotesExec(const ActionName: string);

    procedure WMDrawClipboard(var Msg:TMessage); message WM_DRAWCLIPBOARD;
   { Por este procedure o Notes é avisado que ouve uma mudança na cadeia
    de clipboardviewers, ele repassa a mensagem para todos os aplicativos
    da cadeia.}
    procedure WMChangeCBChain(var Msg: TMessage); message WM_CHANGECBCHAIN;
    // Pega arquivos arrastados do Win. Explorer para o Notes
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

    procedure HandleHint(Sender: TObject);

    { Interceptador de ações do editor }
    procedure EditorOnStatusChangeHandler(Sender: TObject; Changes: TSynStatusChanges);
    { Handler do evento DropFiles no editor }
    procedure EditorOnDropFilesHandler(Sender: TObject; X, Y: Integer; AFiles: TStrings);
    { Aplica as configurações }
    procedure ApplyConfig;
    { Atualiza a propriedade caption do form com informações úteis. }
    procedure UpdateCaption;
    { Atualiza a barra de status com a linha/coluna atual, etc.}
    procedure UpdateStatusBar;
    { Habilita/Desabilita itens - menus, itens da toolbar, etc. }
    procedure UpdateGUI;

    { Vai para o @link(TNotesEditorTabPos) passado em @code(etpos).
      Se o arquivo referido em @code(etpos.FileName) não estiver
      aberto, a função irá abri-lo. Se você quiser mostrar a linha
      para a qual o editor irá pular como como uma linha de erro,
      passe @code(True) em @code(AsError). A função retornará
      @code(True) se conseguir ir para a posição passada. }
    function DoGoToEditorTabPos(const etpos: TNotesEditorTabPos; AsError: boolean): boolean;

    procedure OpenMruFromStartPage(FileToOpen: string; const IsProject: boolean);
  public
    procedure ExecuteCmdLine;
    procedure NotesInitialize;
    procedure HandleNewInstance;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

// incluí o XPmanifest no Notes para que no winXP
// ele apareça com os novos controles deste OS.
{$IFDEF MSWINDOWS}
  {$R WindowsXP.res}
{$ENDIF}

uses
  frm_addItem, frm_About, NotesXML, NotesImgSize, Clipbrd, FastStrings,
  NotesHTML, NotesUtils, InstanceManager, frm_ConfigPrint, NotesOutputParser,
  NotesTimers, NotesListEditor, frm_NewFile, frm_ConfigTemplates,
  NotesTemplates, frm_ProfileMan, NotesSearchReplace, frm_Search,
  frm_Shortcuts, NotesActions, NotesHighlighter, frm_Config, SynEditTypes,
  NotesRememberEditorState, frm_FolderMenuEditor, NotesFolderMenus,
  frm_editRunItem, frm_ListDlg, NotesEventServer, frm_statistics;

var
  NMRU, NFav: TNotesMRUMenu;
  // Objeto que faz a limpeza do cache de arquivos antigos
  EdStateCleanUp: TNotesEditorStateCleanUp;

const
  SB_MACRO = 0;
  SB_COL = 2;
  SB_LIN = 1;
  SB_INS = 3;
  SB_EOL = 4;
  SB_HINT = 5;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  setStatusMsg:= SetStatusBarMessage;

  EdStateCleanUp:= nil;
  fTranslation := TNotesTranslation.Create;
  with fTranslation do
  begin
    TranslationFile:= nProfile.getTranslationFileForModule(self.ClassName);
    TranslateComponent(self);
  end;

  tabs:= TNotesTabsManager.Create;
  // StartPage
  tabs.OnStartPageOpenMruItem:= OpenMruFromStartPage;
  tabs.OnStartPageRequestNewFile:= actFileNew.OnExecute;
  tabs.OnStartPageRequestOpenFile:= actFileOpen.OnExecute;
  tabs.PageControl:= pgEditor;
  tabs.OnEditorStatusChange:= EditorOnStatusChangeHandler;
  tabs.OnEditorDropFiles:= EditorOnDropFilesHandler;
  tabs.EditorPopupMenu:= popMenu;

  ActiveEditorTab:= tabs.getActiveEditorTab;
  GoToEditorTabPos:= DoGoToEditorTabPos;
  NImages:= icosXP;

  folderMenusKeymap.Load;

  ActionMan:= TNotesActionManager.Create(ActionList, mnFile,
  mnEdit, mnSearch, mnView, mnProject, mnRun, mnCommands,
  mnOptions, mnHelp);

  {  AÇÕES QUE PRECISAM SER HABILITADAS / DESABILITADAS SEGUNDO
     CERTAS CONDIÇÕES. Se uma ação deve ficar sempre habilitada,
     não a coloque aqui!      }
  ActionMan.ControlAction(actCommandsCompressHtml, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actCommandsConvertSpecialHtmlChars, [ecHasSelection, ecNotReadonly]);
  ActionMan.ControlAction(actCommandsFixHTML, [ecHasContent, ecHasFileName, ecNotReadonly]);
  ActionMan.ControlAction(actCommandsFixlinebreaks, [ecHasSelection, ecNotReadonly]);
  ActionMan.ControlAction(actCommandsHTMLtoXHTML, [ecHasContent, ecHasFileName]);
  ActionMan.ControlAction(actCommandsInsertDateTime, [ecHasEditorTab, ecNotReadonly]);
  ActionMan.ControlAction(actCommandsInsertFile, [ecHasEditorTab, ecNotReadonly]);
  ActionMan.ControlAction(actCommandsInsertImage, [ecHasEditorTab, ecHasFileName, ecNotReadonly]);
  ActionMan.ControlAction(actCommandsUndo, [ecCanUndo, ecNotReadonly]);
  ActionMan.ControlAction(actCommandsVerifyHTML, [ecHasContent, ecHasFileName]);
  ActionMan.ControlAction(actCommandsVerifyXML, [ecHasContent, ecHasFileName]);
  ActionMan.ControlAction(actCommandsWrap, [ecHasSelection, ecNotReadonly]);
  ActionMan.ControlAction(actEditAddToClipboard, [ecHasSelection]);
  ActionMan.ControlAction(actEditComment, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditCopy, [ecHasContent]);
  ActionMan.ControlAction(actEditCut, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditDel, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditFormatTabToSpaces, [ecHasSelection, ecNotReadonly]);
  ActionMan.ControlAction(actEditFormatSpacesToTabs, [ecHasSelection, ecNotReadonly]);
  ActionMan.ControlAction(actEditFormatLowercase, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditFormatTitlecase, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditFormatToggleCase, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditFormatUppercase, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditIndent, [ecHasEditorTab, ecNotReadonly]);
  ActionMan.ControlAction(actEditLineDel, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditLineDuplicate, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditLineSelect, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditPaste, [ecCanPaste, ecNotReadonly]);
  ActionMan.ControlAction(actEditRedo, [ecHasEditorTab, ecCanRedo, ecNotReadonly]);
  ActionMan.ControlAction(actEditSelectAll, [ecHasContent]);
  ActionMan.ControlAction(actEditSmartPaste, [ecHasEditorTab, ecNotReadonly]);
  ActionMan.ControlAction(actEditReindent, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditUncomment, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actEditUndo, [ecHasEditorTab, ecCanUndo, ecNotReadonly]);
  ActionMan.ControlAction(actEditUnindent, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actFilePrint, [ecHasContent]);
  ActionMan.ControlAction(actFileRevert, [ecHasEditorTab, ecHasFileName, ecNotReadonly]);
  ActionMan.ControlAction(actFileSave, [ecHasEditorTab]);
  ActionMan.ControlAction(actFileSaveAs, [ecHasEditorTab]);
  ActionMan.ControlAction(actFileSaveSelection, [ecHasEditorTab, ecHasSelection]);
  ActionMan.ControlAction(actFileStatistics, [ecHasEditorTab]);
  ActionMan.ControlAction(actProjectAdd, [ecHasProject]);
  ActionMan.ControlAction(actProjectOpen, [ecHasProject]);
  ActionMan.ControlAction(actProjectOpenAll, [ecHasProject]);
  ActionMan.ControlAction(actProjectOptions, [ecHasProject]);
  ActionMan.ControlAction(actProjectRemove, [ecHasProject]);
  ActionMan.ControlAction(actRunFile, [ecHasFileName]);
  ActionMan.ControlAction(actRunLast, [ecHasFileName]);
  ActionMan.ControlAction(actSearchBack, [ecHasContent]);
  ActionMan.ControlAction(actSearchClearProblems, [ecHasContent]);
  ActionMan.ControlAction(actSearchFind, [ecHasContent]);
  ActionMan.ControlAction(actSearchFindNext, [ecHasContent]);
  ActionMan.ControlAction(actSearchFindPrevius, [ecHasContent]);
  ActionMan.ControlAction(actSearchForward, [ecHasContent]);
  ActionMan.ControlAction(actSearchGotocol, [ecHasContent]);
  ActionMan.ControlAction(actSearchGotoline, [ecHasContent]);
  ActionMan.ControlAction(actSearchNextMark, [ecHasContent]);
  ActionMan.ControlAction(actSearchPreviusMark, [ecHasContent]);
  ActionMan.ControlAction(actSearchNextProblem, [ecHasEditorTab]);
  ActionMan.ControlAction(actSearchPreviusProblem, [ecHasEditorTab]);
  ActionMan.ControlAction(actSearchReplace, [ecHasContent, ecNotReadonly]);
  ActionMan.ControlAction(actSearchToggleMark, [ecHasEditorTab]);
  ActionMan.ControlAction(actSearchClearMarks, [ecHasEditorTab]);
  ActionMan.ControlAction(actSearchGotoMark, [ecHasEditorTab]);
  ActionMan.ControlAction(actOptionsWordWrap, [ecHasContent]);
  ActionMan.ControlAction(actMacrosSave, [ecMacroLoaded]);

  sbStatusBar.Panels[SB_MACRO].Text:= '';
  sbStatusBar.Panels[SB_HINT].Text:= fTranslation.getMsgTranslation('Loading', 'Loading Notes...');
  fStopResize:= true;

  with NProfile.Options do
  begin
    actViewStatusbar.Checked := ShowStatusBar;
    if not ShowStatusbar then
      actViewStatusbar.Execute;
    actViewToolBar.Checked := ShowToolbar;
    if not ShowToolbar then
      actViewToolBar.Execute;
    paLeft.Width := IIF( ShowLeftPanel, 200, 8 );
    paRight.Width := IIF( ShowRightPanel, 200, 8 );
    paBottom.Height := IIF( ShowBottomPanel, 140, 8 );
    SetBounds( FormLeft, FormTop, FormWidth, FormHeight );
    if NProfile.Options.Maximized then
      WindowState := wsMaximized;
  end;

  ////////////////////////////////////

  PanelsManager:= TNotesPanelManager.Create;
  PanelsManager.addPanelHost('Direita', paRight, htRight);
  PanelsManager.addPanelHost('Esquerda', paLeft, htLeft);
  PanelsManager.addPanelHost('Em baixo', paBottom, htBottom);
  PanelsManager.DefaultHost:= 'Esquerda';

  PanelsManager.createPanels;
  PanelsManager.Load;

  fKeymap:= TNotesKeymap.Create;
  fMacros:= TNotesMacros.Create;

  NotesGlobals.NotesExec:= doNotesExec;
  NotesGlobals.PanelExec:= PanelsManager.PanelExecute;
  NotesGlobals.PanelSetProperty:= PanelsManager.setPanelProperty;
  NotesGlobals.PanelGetProperty:= PanelsManager.getPanelProperty;

  timers.setTimeOut(100, self.NotesInitialize);
end;

procedure TfrmMain.NotesInitialize;
begin
  screen.Cursor:= crHourGlass;

  OutType := -1;

  //Arquivos recentes
  NMRU := TNotesMRUMenu.Create( mnFileMruClear, mnFileMruClear, MruOrFavClick );
  NMRU.LoadFromFile(NotesGlobals.NProfile.Paths.MRUFile);

  //favoritos
  NFav := TNotesMRUMenu.Create( frmMain, mnFileBookmakersEdit, MruOrFavClick );
  NFav.MaxItens := 250;

  // pega o handle do próximo programa registrado como clipboard viewer
  NNextInClipboardChain := SetClipboardViewer(Handle);

  ApplyConfig;

  PanelsManager.Initialize;

  //Inicializa a forma como o Notes vai cuidar de Hints
  with Application do
  begin
    HintPause := 800;
    HintHidePause := 12000;
    OnHint := HandleHint;
  end;

  if NotesCmdLine.FilesToOpen.Count = 0 then
  begin
    // mostramos a startpage se não tiver arquivos a serem abertos
    tabs.ShowStartPage;
    tabs.ReloadStartPage;
  end;

  screen.Cursor:= crDefault;
  InstanceManager.triggerProc:= HandleNewInstance;

// ** inicia ações do editor **
  actEditAddToClipboard.OnExecute:= tabs.EditorCopyAppend;
  actEditComment.OnExecute:= tabs.EditorComment;
  actEditCopy.OnExecute:= tabs.EditorCopy;
  actEditCut.OnExecute:= tabs.EditorCut;
  actEditDel.OnExecute:= tabs.EditorDelete;
  actEditFormatLowercase.OnExecute:= tabs.EditorLowerCase;
  actEditFormatTitlecase.OnExecute:= tabs.EditorTitleCase;
  actEditFormatToggleCase.OnExecute:= tabs.EditorToggleCase;
  actEditFormatUppercase.OnExecute:= tabs.EditorUpperCase;
  actEditIndent.OnExecute:= tabs.EditorIndent;
  actEditLineDel.OnExecute:= tabs.EditorDelLine;
  actEditLineDuplicate.OnExecute:= tabs.EditorDuplicateLine;
  actEditLineSelect.OnExecute:= tabs.EditorSelLine;
  actEditorBreakLine.OnExecute:= tabs.EditorBreakLine;
  actEditorBreakLineFromEOL.OnExecute:= tabs.EditorBreakLineFromEOL;
  actEditorBreakLineNoMove.OnExecute:= tabs.EditorBreakLineNoMove;
  actEditorClearAll.OnExecute:= tabs.EditorClearAll;
  actEditorDeleteBOL.OnExecute:= tabs.EditorDeleteBOL;
  actEditorDeleteEOL.OnExecute:= tabs.EditorDeleteEOL;
  actEditorDeleteLastChar.OnExecute:= tabs.EditorDeleteLastChar;
  actEditorDeleteLastWord.OnExecute:= tabs.EditorDeleteLastWord;
  actEditorDeleteWord.OnExecute:= tabs.EditorDeleteWord;
  actEditorDown.OnExecute:= tabs.EditorDown;
  actEditorEditorBottom.OnExecute:= tabs.EditorEditorBottom;
  actEditorEditorTop.OnExecute:= tabs.EditorEditorTop;
  actEditorGotoMatchBracket.OnExecute:= tabs.EditorGotoMatchBracket;
  actEditorLeft.OnExecute:= tabs.EditorLeft;
  actEditorLineEnd.OnExecute:= tabs.EditorLineEnd;
  actEditorLineStart.OnExecute:= tabs.EditorLineStart;
  actEditorPageBottom.OnExecute:= tabs.EditorPageBottom;
  actEditorPageDown.OnExecute:= tabs.EditorPageDown;
  actEditorPageLeft.OnExecute:= tabs.EditorPageLeft;
  actEditorPageRight.OnExecute:= tabs.EditorPageRight;
  actEditorPageTop.OnExecute:= tabs.EditorPageTop;
  actEditorPageUp.OnExecute:= tabs.EditorPageUp;
  actEditorRight.OnExecute:= tabs.EditorRight;
  actEditorScrollDown.OnExecute:= tabs.EditorScrollDown;
  actEditorScrollLeft.OnExecute:= tabs.EditorScrollLeft;
  actEditorScrollRight.OnExecute:= tabs.EditorScrollRight;
  actEditorScrollUp.OnExecute:= tabs.EditorScrollUp;
  actEditorSelDown.OnExecute:= tabs.EditorSelDown;
  actEditorSelEditorBottom.OnExecute:= tabs.EditorSelEditorBottom;
  actEditorSelEditorTop.OnExecute:= tabs.EditorSelEditorTop;
  actEditorSelLeft.OnExecute:= tabs.EditorSelLeft;
  actEditorSelLineEnd.OnExecute:= tabs.EditorSelLineEnd;
  actEditorSelLinestart.OnExecute:= tabs.EditorSelLinestart;
  actEditorSelPageBottom.OnExecute:= tabs.EditorSelPageBottom;
  actEditorSelPageDown.OnExecute:= tabs.EditorSelPageDown;
  actEditorSelPageLeft.OnExecute:= tabs.EditorSelPageLeft;
  actEditorSelPageRight.OnExecute:= tabs.EditorSelPageRight;
  actEditorSelPageTop.OnExecute:= tabs.EditorSelPageTop;
  actEditorSelPageUp.OnExecute:= tabs.EditorSelPageUp;
  actEditorSelRight.OnExecute:= tabs.EditorSelRight;
  actEditorSelUp.OnExecute:= tabs.EditorSelUp;
  actEditorSelWordLeft.OnExecute:= tabs.EditorSelWordLeft;
  actEditorSelWordRigth.OnExecute:= tabs.EditorSelWordRight;
  actEditorShiftTab.OnExecute:= tabs.EditorShiftTab;
  actEditorTab.OnExecute:= tabs.EditorTab;
  actEditorTogleMode.OnExecute:= tabs.EditorToggleMode;
  actEditorUp.OnExecute:= tabs.EditorUp;
  actEditorWordLeft.OnExecute:= tabs.EditorWordLeft;
  actEditorWordRight.OnExecute:= tabs.EditorWordRight;
  actEditPaste.OnExecute:= tabs.EditorPaste;
  actEditRedo.OnExecute:= tabs.EditorRedo;
  actEditSelectAll.OnExecute:= tabs.EditorSelAll;
  actEditSmartPaste.OnExecute:= tabs.EditorSmartPaste;
  actEditUncomment.OnExecute:= tabs.EditorUcomment;
  actEditUndo.OnExecute:= tabs.EditorUndo;
  actEditUnindent.OnExecute:= tabs.EditorUnindent;
  actEditorSelWord.OnExecute:= tabs.EditorSelWordAtCursor;
  actEditReindent.OnExecute:= tabs.EditorReindent;
  actEditFormatTabToSpaces.OnExecute:= tabs.EditorTabsToSpaces;
  actEditFormatSpacesToTabs.OnExecute:= tabs.EditorSpacesToTabs;
// *** fim das ações **

  // Carregamos o Keymap
  fKeymap.LoadKeymap(nProfile.Paths.KeymapFile);
  fKeymap.AssignToActionList(ActionList);

  l_Insert:= fTranslation.getMsgTranslation('Insert', 'Insert');
  l_Overwrite:= fTranslation.getMsgTranslation('Overwrite', 'Overwrite');

  Toolbar.ShowHint:= True;
  nfav.CaptionWithPath:= false;
  nmru.CaptionWithPath:= false;

  // Avisa que o notes está terminando de carregar e vai executar as ações
  // da linha de comando
  eventServer.NotifyEvent(self, 'notesload', NotesCmdLine.Cmd);
  // Executa as ações da linha de comando
  ExecuteCmdLine;

  UpdateStatusbar;
  sbStatusBar.Panels[SB_HINT].Text := fTranslation.getMsgTranslation('Loaded', 'Press F1 for help.');
  // economizamos uns KBs :)
  fTranslation.releaseCache;

  // Isto vai limpar o cache de estado do editor para arquivos antigos
  EdStateCleanUp:= TNotesEditorStateCleanup.Create(true);
  EdStateCleanUp.DaysOld:= nProfile.Config.RemeberFileInfo;
  EdStateCleanUp.EditorStateDir:= nProfile.Paths.EditorStateDir;
  EdStateCleanUp.EditorStateExt:= NotesGlobals.NOTES_EDITOR_STATE_EXT;
  EdStateCleanUp.Execute;

  UpdateCaption;
  UpdateGui;

  // Constrói menu executar/menu de macros
  BuildFolderBasedMenus;

  // aceitar arquivos arrastados do explorer p/ o Notes
  DragAcceptFiles(Handle, TRUE);

  NotesGlobals.stopNotesShortcuts:= DoStopNotesShortcuts;
  NotesGlobals.restoreNotesShortcuts:= DoRestoreNotesShortcuts;
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
Var
  I, R: integer;
  S: string;
begin
  CanClose := false;
  // Tentamos fehcar todos os arquivos

  for I:= 0 to tabs.EditorsTabCount - 1 do
  begin
    if tabs.EditorsTab[I].Editor.Modified then
    begin
      pgEditor.ActivePage:= tabs.EditorsTab[I].Tab;
      S:= Copy(ActiveEditorTab.Tab.Caption, 1, length(ActiveEditorTab.Tab.Caption)-1);
      R:= MsgQuest(Format(fTranslation.getMsgTranslation('ConfirmSaveBeforeClose', 'Do you want to save "%s" before close?'), [S]), Handle);
      if R = IDYes then
      begin
        actFileSave.Execute;
        // Se continua modificado, é por que o usuário cancelou
        if (tabs.EditorsTab[I] <> nil) and (tabs.EditorsTab[I].Editor.Modified) then
          Exit;
      end else
      if R = IDCancel then
        Exit;
    end;
  end;

  fStopResize:= true;

  eventServer.NotifyEvent(self, 'notesunload', '');

  with NProfile.Options do
  begin
    ShowStatusBar := sbStatusBar.Visible;
    ShowToolBar := ToolBar.Visible;
    ShowBottomPanel := paBottom.Height > 8;
    ShowRightPanel := paRight.Width > 8;
    ShowLeftPanel := paLeft.Width > 8;
    Maximized := WindowState = wsMaximized;
    FormLeft := Left;
    FormTop := Top;
    FormWidth := Width;
    FormHeight := Height;
    Save;
  end;
  PanelsManager.Save;

  FreeAndNil(tabs);

  CanClose := True;

  Application.Terminate;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fStopResize:= true;
  OnResize:= nil;

  DragAcceptFiles(Handle, FALSE);
  ChangeClipboardChain( Handle, NNextInClipboardChain );

  LockwindowUpdate( handle );
  FreeAndNil(fTranslation);
  FreeAndNil(fKeymap);
  FreeAndNil(fMacros);
  FreeAndNil(PanelsManager);
  FreeAndNil(NMRU);
  FreeAndNil(NFav);
  FreeAndNil(ActionMan);
  if assigned(tabs) then
    FreeAndNil(tabs);

  LockWindowUpdate(0);

  if Assigned(EdStateCleanUp) then
  begin
    EdStateCleanUp.Terminate;
    FreeAndNil(EdStateCleanUp);
  end;

//  NotesMenu:= nil;
  NotesCmdLine:= nil;
  NImages:= nil;
end;


function TFrmMain.AddMenuItem(AOwner: TComponent; ACaption: TCaption; AOnClick: TNotifyEvent;
  AHint: String; AMenuItem: TMenuItem): TMenuItem;
begin
  Result := TMenuItem.Create(AOwner);
  with Result do
  begin
    OnClick := AOnClick;
    Caption := ACaption;
    Hint := AHint;
    AMenuItem.Insert( AMenuItem.Count, Result );
//    if NProfile.Config.OfficeXPTheme then
//      NotesMenu.ActivateMenuItem( Result, False );
  end;
end;


//////////// EVENTOS LIGADOS A INTERFACE ////////////////

function TfrmMain.DoGoToEditorTabPos(const etpos: TNotesEditorTabPos;
  AsError: boolean): boolean;
Var
  I: integer;
  // precisa abrir o arquivo?
  NeddToLoad: boolean;
begin
  NeddToLoad:= true;

  if FileExists(etpos.FileName) then
  begin
    // tentamos ir para a tab do arquivo passado
    for I:= 0 to tabs.EditorsTabCount - 1 do
    begin
      if SameText(tabs.EditorsTab[I].FullPath, etpos.FileName) then
      begin
        pgEditor.ActivePage:= tabs.EditorsTab[I].Tab;
        NeddToLoad:= false;
        break;
      end;
    end;

    if NeddToLoad then
    begin
      NMRU.LoadFromFile(NProfile.Paths.MRUFile);
      if tabs.LoadFile( etpos.FileName ) then
      begin
        pgEditor.ActivePageIndex:= pred( pgEditor.PageCount );
        ActiveEditorTab.Editor.InvalidateGutter;
        UpdateCaption;
        UpdateGUI;
        UpdateStatusBar;
        NMRU.Add( etpos.FileName);
        NMRU.SaveToFile( NProfile.Paths.MRUFile );
      end;
    end;
  end else
  begin
    // se não existe, a caption da tab, ex. "Documento 1", deve ter sido passada
    for I:= 0 to tabs.EditorsTabCount - 1 do
    begin
      if SameText(tabs.EditorsTab[I].Tab.Caption, etpos.FileName) then
      begin
        pgEditor.ActivePage:= tabs.EditorsTab[I].Tab;
        break;
      end;
    end;
  end;

  if etpos.Line > 0 then
    ActiveEditorTab.GoToLine(etpos.Line);
  if etpos.Col > 0 then
    ActiveEditorTab.GoToCol(etpos.Col);

  tabs.FocusActiveEditor;

  if AsError then
  begin
    ActiveEditorTab.ErrorLine:= ActiveEditorTab.Editor.CaretY;
    ActiveEditorTab.Editor.InvalidateLine(ActiveEditorTab.Editor.CaretY);
  end;

  Result:= true;
end;

procedure TfrmMain.EditorOnDropFilesHandler(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
  tabs.LoadMultipleFiles(AFiles);
  pgEditor.ActivePageIndex:= pred( pgEditor.PageCount );
  UpdateCaption;
  tabs.FocusActiveEditor;
  UpdateStatusBar;
  UpdateGui;
end;


procedure TfrmMain.EditorOnStatusChangeHandler(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  UpdateGUI;
  UpdateStatusBar;
  if scModified in Changes then
    UpdateCaption;

  if scSelection in Changes then
    eventServer.NotifyEvent(self, 'editorselchanged', '');
end;

procedure TfrmMain.UpdateGUI;
var
  Conds: TNotesActionEnableConditions;
begin
  if fStopResize then Exit;

  conds:= [];

  if fMacros.HasMacro then
    include(Conds, ecMacroLoaded);

  if tabs.IsEditorActivated then
  begin
    include(Conds, ecHasEditorTab);
    if ActiveEditorTab.FullPath <> '' then
      include(Conds, ecHasFileName);
    if length(ActiveEditorTab.Editor.Lines.Text) > 1 then
      include(Conds, ecHasContent);
    if ActiveEditorTab.Editor.ReadOnly = false then
      include(Conds, ecNotReadonly);
    if ActiveEditorTab.Editor.CanUndo then
      include(Conds, ecCanUndo);
    if ActiveEditorTab.Editor.CanRedo then
      include(conds, ecCanRedo);
    if ActiveEditorTab.Editor.CanPaste then
      include(conds, ecCanPaste);
    if ActiveEditorTab.Editor.SelAvail then
      include(Conds, ecHasSelection);
  end;

  ActionMan.CheckConditions(Conds);

  actOptionsReadOnly.Checked := (ActiveEditorTab <> nil) and (ActiveEditorTab.Editor.ReadOnly);
  actOptionsWordWrap.Checked:= (ActiveEditorTab <> nil) and (ActiveEditorTab.Editor.WordWrap);
  actEditTogleColumSel.Checked:= (ActiveEditorTab <> nil) and (ActiveEditorTab.Editor.SelectionMode <> smNormal);
end;

procedure TfrmMain.HandleHint(Sender: TObject);
var
  S: String;
begin
  if sbStatusBar.Visible then
  begin
    S := GetLongHint( Application.Hint );
    sbStatusBar.Panels[SB_HINT].Text:='';
    if ( Length( S ) > 2 ) and ( Copy( S, 2, 2 ) <> ':\' ) or ( Copy( S, 1, 2 ) <> '\\' ) or (S[1] <> '/') then
        sbStatusBar.Panels[SB_HINT].Text := S;
  end;
end;

procedure TfrmMain.doNotesExec(const ActionName: string);
Var
  I: integer;
begin
  for I:= pred( ActionList.ActionCount ) downto 0 do
    if SameText( (ActionList.Actions[I] As TAction).name, ActionName) then
    begin
      (ActionList.Actions[I] As TAction).execute;
      Exit;
    end;
end;

procedure TfrmMain.ApplyConfig;
var
  I: Integer;
  Tree: PNotesFolderTree;
  mi: TMenuItem;
begin

  mnuFileTypesOwner.DestroyComponents;

  NProfile.FileTypesMgr.ListTypes(Tree);
  for I:= 0 to High(Tree^) do
  begin
    mi:= AddMenuItem(mnuFileTypesOwner, Tree^[I].Path , doChangeFiletype, Tree^[I].Path, mnOptionsFileType);
    mi.GroupIndex:= 33;
    mi.RadioItem:= true;
  end;
  FreeFolderTree(Tree);

  //carregar as associações entre extensões de arquivos e linguagens
  NProfile.LoadAssociations;

  if tabs.EditorsTabCount = 0 then Exit;

  screen.Cursor:= crHourGlass;

  try
    for I := 0 to tabs.EditorsTabCount -1 do
      tabs.EditorsTab[I].ReloadConfig;

    if ActiveEditorTab <> nil then
    begin
      ActiveEditorTab.Editor.Update;
      ActiveEditorTab.Editor.UpdateCaret;
    end;
  finally
    screen.Cursor:= crDefault;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  //[:S:]->Verificar os editores abertos, comparando a data de alteração deles (no caso seria necessário criar + uma propriedade) com a contida no arquivo a q eles se referenciam, caso seja diferente, oferecer algo para recarregar o texto...
  fKeymap.AssignToActionList(ActionList);
  self.KeyPreview:= true;
  UpdateGUI;
end;

procedure TfrmMain.FormDeactivate(Sender: TObject);
begin
  // temos q remover as teclas de atalho por causa dos forms não modais
  // lembre-se de recoloca-los no lugar no activate!!!
  fKeymap.ClearActionListShortcuts(ActionList);
  self.KeyPreview:= false;  
end;

procedure TfrmMain.pgEditorEnter(Sender: TObject);
begin
  fKeymap.AssignToActionList(ActionList);
end;

procedure TfrmMain.pgEditorExit(Sender: TObject);
begin
  // Quando há um onExit no pgEditor, isto indica que
  // o usuário está mexendo em algum painel ou outro
  // form. Ou seja que ele não está usando o editor.
  // Como as shortcuts são executadas não importando
  // se o editor está ativo ou não, o jeito é desligar
  // todas as shortcuts de edição. Se não fizermos isto
  // teclas como as setas e outras não funcionam como
  // o esperado...
  fKeymap.ClearActionListEditingShortcuts(ActionList);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LockwindowUpdate( handle );

  {corrige problema da janela não estar sendo
  realinhada ao ser maximizada}

  fStopResize:= false;

  if WindowState = wsMaximized then
  begin
    WindowState:= wsNormal;
    WindowState:= wsMaximized;
  end;

  LockWindowUpdate(0);
end;

procedure TfrmMain.CanNotesPanelResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin

  if Sender is TPanel then
  begin
    if (Sender = paLeft) or (Sender = paRight) then
    begin
      if pgEditor.Width < 100 then
        if NewWidth > (Sender As TPanel).Width then
          Resize:= false;
    end else
    if Sender = paBottom then
    begin
      if NewHeight > (Height -200) then
        if NewHeight > (Sender As TPanel).Height then
          Resize:= false;
    end;
  end;
end;

procedure TfrmMain.pgEditorChange(Sender: TObject);
begin
  UpdateGui;
  UpdateCaption;
  UpdateStatusBar;

  if pgEditor.ActivePageIndex = 0 then
    tabs.ReloadStartPage;

  eventServer.NotifyEvent(self, 'tabchanged', '');
end;

procedure TfrmMain.ExecuteCmdLine;
begin

  // arquivos
  if NotesCmdLine.FilesToOpen.Count > 0 then
  begin
    tabs.LoadMultipleFiles(NotesCmdLine.FilesToOpen);
    pgEditor.ActivePageIndex:= pred( pgEditor.PageCount );
    tabs.FocusActiveEditor;
  end;
  // mostrar informações

  // abrir como template

  // imprimir
end;

procedure TfrmMain.poTabsPopup(Sender: TObject);
var
  MousePt : TPoint;
  ClickedTabIndex : integer;
begin
  // Tentamos ativar a tab clicada
  GetCursorPos(MousePt);
  MousePt := pgEditor.ScreenToClient(MousePt);
  ClickedTabIndex := pgEditor.IndexOfTabAt(MousePT.X, MousePT.Y);
  if ClickedTabIndex > -1 then
    pgEditor.ActivePageIndex:= ClickedTabIndex;
end;

procedure TfrmMain.OpenMruFromStartPage(FileToOpen: string;
  const IsProject: boolean);
begin
  if FileExists( FileToOpen ) then
  begin
    NMRU.LoadFromFile( NProfile.Paths.MRUFile );
    if tabs.LoadFile( FileToOpen ) then
    begin
      pgEditor.ActivePageIndex:= pred( pgEditor.PageCount );
      UpdateCaption;
      tabs.FocusActiveEditor;
      UpdateGui;
      UpdateStatusBar;
      NMRU.Add(FileToOpen);
      NMRU.SaveToFile( NProfile.Paths.MRUFile );
    end;
  end
end;

procedure TfrmMain.HandleNewInstance;
begin
  NotesCmdLine.Cmd:= InstanceManager.rcvStr;
  NotesCmdLine.Parse;
  ExecuteCmdLine;

  if IsIconic(Application.Handle) then
    Application.Restore
  else
  begin
      Application.Minimize;
      Application.Restore;
  end;

 // BUGzim! A JANELA DEVERIA APARECER COM O CÓDIGO ABAIXO
//  SetForegroundWindow(self.Handle);
//  Application.Restore;
//  Application.BringToFront;
end;

procedure TfrmMain.WMDrawClipboard(var Msg:TMessage);
begin
  // vemos se é preciso habilitar os botões colar...
  actEditPaste.Enabled:= tabs.IsEditorActivated and (Clipboard.HasFormat(CF_TEXT)) and not ActiveEditorTab.Editor.ReadOnly;
  actEditSmartPaste.Enabled:= actEditPaste.Enabled;

  // passamos a mensagem p/ o próximo cliboarviewer...
  if NNextInClipboardChain <> 0 then
    SendMessage(NNextInClipboardChain, WM_DrawClipboard, 0, 0)
end;


procedure TfrmMain.WMChangeCBChain(var Msg: TMessage);
// recebe mensagens sobre mudanças nos clipboarviewer...
var
  hRemove, hNext: THandle;
begin
  hRemove := Msg.WParam;
  hNext := Msg.LParam;

  with Msg do
  begin
    if NNextInClipboardChain = hRemove then
      NNextInClipboardChain:= hNext
     else if NNextInClipboardChain <> 0 then
      SendMessage(NNextInClipboardChain, WM_ChangeCBChain, hRemove, hNext);
  end;
end;


procedure TfrmMain.WMDropFiles(var Msg: TWMDropFiles);
var
  S : string;   // nome do arquivo - como string
  PC : PChar;   // nome do arquivo - como pchar
  intFilesCount : Integer; // guarda número de arquivos que foram jogados no notes
  I : Integer;
begin
  intFilesCount:= DragQueryFile(Msg.Drop, UINT(-1), nil, 0);

  try
    for I := 0 to intFilesCount - 1 do
    begin
      SetLength(S, DragQueryFile(Msg.Drop, 0, nil, 0)+1);
      PC:= PChar(S);
      DragQueryFile(Msg.Drop, I, PC, Length(S));
      S:= string(PC);
      if tabs.LoadFile(S) then
        NMRU.Add(S);
    end;
  finally
    DragFinish(Msg.Drop);
  end;

  pgEditor.ActivePageIndex:= pred( pgEditor.PageCount );

  UpdateCaption;
  tabs.FocusActiveEditor;
  UpdateGui;
  UpdateStatusBar;

  NMRU.SaveToFile( NProfile.Paths.MRUFile );
end;


procedure TfrmMain.sbStatusBarClick(Sender: TObject);
var
  P: TPoint;
begin
  if ActiveEditorTab = nil then Exit;
  getCursorPos(P);

  if P.X < 60 then Exit;

  if P.X < 143 then
    actSearchGotoline.Execute
  else if P.X < 213 then
    actSearchGotocol.Execute
  else if P.X < 293 then
    ActiveEditorTab.Editor.ExecuteCommand(223, #0, nil)
  else if P.X < 333 then
  begin
    if ActiveEditorTab.Editor.ReadOnly then Exit;
    case ActiveEditorTab.BreakType of
      btWin: ActiveEditorTab.BreakType:= btMac;
      btMac: ActiveEditorTab.BreakType:= btLin;
    else
      ActiveEditorTab.BreakType:= btWin;
    end;
    ActiveEditorTab.Editor.Modified:= true;
  end;

  UpdateStatusBar;
end;


procedure TfrmMain.popMenuPopup(Sender: TObject);
begin
  UpdateGui;
end;


procedure TfrmMain.UpdateStatusBar;
var
  iLinha:integer;
  iColuna:integer;
  I: integer;
begin

  if not tabs.IsEditorActivated then
  begin
    for I:= 1 to  sbStatusBar.Panels.Count - 2 do
      sbStatusBar.Panels[I].Text:='...';
    Exit;
  end;

  if sbStatusBar.Visible then
  begin
    iLinha := ActiveEditorTab.Editor.CaretXY.Line;
    iColuna := ActiveEditorTab.Editor.CaretXY.Char;
    sbStatusBar.Panels[SB_INS].Text := IIF( ActiveEditorTab.Editor.InsertMode, l_Insert, l_Overwrite);
    sbStatusBar.Panels[SB_LIN].Text:= Format( 'Ln: %d', [IIF( NProfile.Config.LineAndColCountStartAt0, iLinha-1, iLinha )] );
    sbStatusBar.Panels[SB_COL].Text:= Format( 'Col: %d', [IIF( NProfile.Config.LineAndColCountStartAt0, iColuna-1, iColuna )] );

    Case ActiveEditorTab.BreakType of
      btWin: sbStatusBar.Panels[SB_EOL].Text:= 'WIN';
      btMac: sbStatusBar.Panels[SB_EOL].Text:= 'MAC';
      btLin: sbStatusBar.Panels[SB_EOL].Text:= 'LIN';
      btBin: sbStatusBar.Panels[SB_EOL].Text:= 'BIN';
    end;

    if not NShowinProgress then
      sbStatusBar.Panels[SB_HINT].Text:='';
  end;
end;

procedure TfrmMain.UpdateCaption;
var
  S: string;
begin
  if ActiveEditorTab = nil then
  begin
    Caption := NOTES_NICKNAME;
    Application.Title := NOTES_NICKNAME;
    Exit;
  end;

  if ActiveEditorTab.Editor.ReadOnly then
    S := ' [' + fTranslation.getMsgTranslation('ReadOnly', 'Read Only') + ']';

  if ( ActiveEditorTab.Tab.Caption <> '' ) and ActiveEditorTab.Editor.Modified and ( ActiveEditorTab.Tab.Caption[Length(ActiveEditorTab.Tab.Caption)] <> '*' ) then
    ActiveEditorTab.Tab.Caption:= ActiveEditorTab.Tab.Caption + '*';

  if Length( ActiveEditorTab.FullPath ) > 0 Then
    S := NOTES_NICKNAME + ' ('+ActiveEditorTab.Tab.Caption + S +') - '+ActiveEditorTab.FullPath
  Else
    S := NOTES_NICKNAME + ' ('+ActiveEditorTab.Tab.Caption + ')';

  Caption := S;
  Application.Title := S;

end;

procedure TfrmMain.mnOptionsClick(Sender: TObject);
Var
  mi: TMenuItem;
begin
  if ActiveEditorTab = nil then
    Exit;

  if ActiveEditorTab.BreakType= btWin then
    mnOptionsEOLWin.checked:= true
  else if ActiveEditorTab.BreakType= btLin then
    mnOptionsEOLLinux.checked:= true
  else
    mnOptionsEOLMac.checked:= true;

  mi:= mnOptionsFileType.Find(ActiveEditorTab.FileType);
  if mi <> nil then
    mi.Checked:= true;
end;

procedure TfrmMain.mnuChangeBreakType(Sender: TObject);
begin
  if ActiveEditorTab = nil then
    Exit;

  if Sender = mnOptionsEOLWin then
    ActiveEditorTab.BreakType:= btWin
  else if Sender = mnOptionsEOLLinux then
    ActiveEditorTab.BreakType:= btLin
  else if Sender = mnOptionsEOLMac then
    ActiveEditorTab.BreakType:= btMac;

  ActiveEditorTab.Editor.Modified:= true;
  UpdateGui;
  UpdateCaption;
end;

procedure TfrmMain.doChangeFiletype(Sender: TObject);
begin
  if (ActiveEditorTab <> nil) and (Sender Is TMenuItem) and ((Sender As TMenuItem).Hint <> '') then
    ActiveEditorTab.FileType:= (Sender As TMenuItem).Hint;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if fStopResize then Exit;
  if Assigned(PanelsManager) then
    PanelsManager.ResizePanels;
end;

procedure TfrmMain.MruOrFavClick(Sender: TObject; const FileName: String);
begin
  if FileExists( FileName ) then
  begin
    if tabs.LoadFile( FileName ) then
    begin
      pgEditor.ActivePageIndex:= pred( pgEditor.PageCount );
      NMRU.Add(FileName);
      NMRU.SaveToFile( NProfile.Paths.MRUFile );
    end;
  end
  else
  begin
    msgExclama( Format( fTranslation.getMsgTranslation('MruItemNotFound', 'The file "%s" could not be found.') , [FileName] ), Handle );
    NMRU.Remove(FileName);
    NMRU.SaveToFile( NProfile.Paths.MRUFile );
    NFav.Remove(FileName);
    NFav.SaveToFile( NProfile.Paths.FavoritesFile );
  end;
  pgEditor.ActivePageIndex:= Pred( pgEditor.PageCount );
  UpdateCaption;
  tabs.FocusActiveEditor;
  UpdateGui;
  UpdateStatusBar;
end;

procedure TfrmMain.mnFileClick(Sender: TObject);
begin
  UpdateGui;
  nmru.LoadFromFile(nProfile.Paths.MRUFile);
  nmru.ReloadMenus;
  nfav.LoadFromFile(nProfile.Paths.FavoritesFile);
  nfav.ReloadMenus;
end;

/////////// MACROS ////////////////


procedure TfrmMain.NotesRunMacro(sender: TObject);
Var
  S: string;
begin
  if not (Sender is TMenuItem) then Exit;

  S:= (Sender As TMenuItem).Hint;
  if not FileExists(S) then
    Exit;

  fMacros.LoadFromFile(S);
  fMacros.PlayMacro;
  fMacros.ClearCode;
end;


procedure TfrmMain.BuildFolderBasedMenus;
begin
  Screen.Cursor := crHourglass;
  try
    BuildMenuFromFolder(NProfile.Paths.RunDir, mnRun, mnRunOWNER, NotesRunFile, false, NOTES_RUN_EXTENSION, true);
    BuildMenuFromFolder(NProfile.Paths.MacrosDir, mnMacros, mnMacrosOWNER, NotesRunMacro, false, NOTES_MACRO_EXT, True);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.actMacrosRunExecute(Sender: TObject);
begin
  fMacros.PlayMacro;
end;

procedure TfrmMain.actMacrosRecordExecute(Sender: TObject);
begin
  fMacros.RecordMacro;
  if fMacros.Recording then
    sbStatusBar.Panels.Items[SB_MACRO].Text:= 'Gravando'
  else
    sbStatusBar.Panels.Items[SB_MACRO].Text:= '';
end;

procedure TfrmMain.actMacrosSaveExecute(Sender: TObject);
begin
  dlgSave.Filter:= 'Notes Script (*.js)|*.js|(*.*)|*.*';
  if dlgSave.Execute then
    fMacros.SaveToFile(dlgSave.FileName);
end;

procedure TfrmMain.actMacrosConfigureExecute(Sender: TObject);
begin
  with TfrmFolderMenuEditor.Create(nil) do
  begin
    try
      RootNodeName:= 'Macros';
      KeymapActionList:= ActionList;
      Caption:= 'Configurar Macros...';
      ItemsFolder:= nProfile.Paths.MacrosDir;
      ItemsExtension:= NotesGlobals.NOTES_MACRO_EXT;
      OnEditItem:= DoEditMacro;
      ShowModal;
    finally
      Free;
    end;
  end;
  BuildFolderBasedMenus;
end;


procedure TfrmMain.DoEditMacro(const FileName: string;
  var ReloadItems: boolean);
Var
  et: TNotesEditorTabPos;
begin
  ReloadItems:= true;
  et.Line:=1;
  et.Col:= 1;
  et.FileName:= FileName;
  GoToEditorTabPos(et, false);
end;


////////////// MENU EXECUTAR /////////////


{ Roda o app especificado pelo item clicado }
procedure TfrmMain.NotesRunFile(sender: TObject);
Var
  S: string;
begin
  if OutType <> -1 then Exit;
  if not (Sender is TMenuItem) then Exit;

  S:= (Sender As TMenuItem).Hint;
  if S = '' then
    Exit;
  if not FileExists(S) then
    Exit;

  if ActiveEditorTab <> nil then
    fLastRunActiveFile:= ActiveEditorTab.FullPath
  else
    fLastRunActiveFile:= '';

  NProfile.RunMgr.LoadItem(S, fLastRunItem);

  if Assigned(actRunLast.OnExecute) then
    actRunLast.OnExecute(nil);
end;


/////////// AÇÕES  ///////////////// AÇÕES //////////////////

{ AÇÕES DO MENU ARQUIVO }

procedure TfrmMain.actFileRevertExecute(Sender: TObject);
begin
  if not tabs.IsEditorActivated or not FileExists(ActiveEditorTab.FullPath) then
    Exit;
  if msgYesNo( fTranslation.getMsgTranslation('ConfirmRevert', 'Você tem certeza que quer reverter para o arquivo salvo?! O Notes não poderá desfazer esta operação.'),
    handle) = IDYes then
    tabs.LoadFile(ActiveEditorTab.FullPath);
  tabs.FocusActiveEditor;
  UpdateStatusBar;
  UpdateCaption;
  UpdateGui;
end;

procedure TfrmMain.actFileBookmakersAddExecute(Sender: TObject);
begin
  if (tabs.IsEditorActivated = false)  or ( ActiveEditorTab.FullPath = '' ) then
    Exit;
  NFav.LoadFromFile( NProfile.Paths.FavoritesFile );
  NFav.Add( ActiveEditorTab.FullPath );
  NFav.SaveToFile( NProfile.Paths.FavoritesFile );
end;

procedure TfrmMain.actFileBookmakersEditExecute(Sender: TObject);
Var
  T: TStrings;
  I: integer;
begin
  T:= TStringList.Create;
  NFav.LoadFromFile(NProfile.Paths.FavoritesFile);
  T.AddStrings(NFav.List);
  if EditList(T, [leAddFile], fTransLation.getMsgTranslation('BookmakersDlgTitle', 'Arquivos favoritos'),
    #13 + fTranslation.getMsgTranslation('BookmakersDlgText', 'Below you can edit your file bookmakers.')) then
  begin
    Nfav.Clear;
    for I := 0 to T.Count -1  do
      Nfav.Add(T.Strings[I]);
    NFav.SaveToFile(NProfile.Paths.FavoritesFile);
  end;
  T.Free;
end;

procedure TfrmMain.actFileSaveSelectionExecute(Sender: TObject);
begin
  if not tabs.IsEditorActivated then
    Exit;

  dlgSave.Title := fTranslation.getMsgTranslation('SaveSelectionTitle', 'Save selection...');
  if dlgSave.Execute and ( Length( dlgSave.FileName ) > 0 ) then
    StrToFile( dlgSave.FileName, ActiveEditorTab.Editor.SelText );
end;

procedure TfrmMain.actFileCloseExecute(Sender: TObject);
Var
  I: integer;
  S: string;
begin

  if (ActiveEditorTab <> nil) and (ActiveEditorTab.Editor.Modified) then
  begin
    S:= Copy(ActiveEditorTab.Tab.Caption, 1, length(ActiveEditorTab.Tab.Caption)-1);
    I:= MsgQuest(Format(fTranslation.getMsgTranslation('ConfirmSaveBeforeClose', 'Do you want to save "%s" before close?'), [S]), Handle);
    if I = IDYes then
    begin
      actFileSaveAs.Execute;
      // Se continua modificado, é por que o usuário cancelou
      if ActiveEditorTab.Editor.Modified then
        Exit;
    end else
    if I = IDCancel then
     Exit;
  end;

  tabs.CloseActiveTab;
  UpdateStatusBar;
  UpdateCaption;
end;

procedure TfrmMain.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actFileCloseAllExecute(Sender: TObject);
Var
  I, R: integer;
  S: string;
begin

  for I:= 0 to tabs.EditorsTabCount - 1 do
  begin
    if tabs.EditorsTab[I].Editor.Modified then
    begin
      pgEditor.ActivePage:= tabs.EditorsTab[I].Tab;
      UpdateCaption;
      S:= Copy(ActiveEditorTab.Tab.Caption, 1, length(ActiveEditorTab.Tab.Caption)-1);
      R:= MsgQuest(Format(fTranslation.getMsgTranslation('ConfirmSaveBeforeClose', 'Do you want to save "%s" before close?'), [S]), Handle);
      if R = IDYes then
      begin
        actFileSaveAs.Execute;
        // Se continua modificado, é por que o usuário cancelou
        if (tabs.EditorsTab[I] <> nil) and (tabs.EditorsTab[I].Editor.Modified) then
          Exit;
      end else
      if R = IDCancel then
        Exit;
    end;
  end;

  tabs.CloseAllEditors;
  tabs.ShowStartPage;
  UpdateStatusBar;
  UpdateCaption;
  UpdateGui;
end;

procedure TfrmMain.actFileOpenAtCursorExecute(Sender: TObject);
begin
  ShowMessage('Hi! Implemente-me, por favor. É só procurar por actFileOpenAtCursorExecute e digitar um pouco de código! Vamo-lá!');
end;

procedure TfrmMain.actFilePrintExecute(Sender: TObject);
begin
  if ActiveEditorTab = nil then Exit;

  with TfrmConfigPrint.Create(Self) do
  begin
    try
      ShowDialog(ActiveEditorTab);
    finally
      Free;
    end;
  end;
end;


procedure TfrmMain.actFileSaveAllExecute(Sender: TObject);
var
  I: integer;
  SaveActive: TNotesEditorTab;
begin
  SaveActive:= ActiveEditorTab;
  try
    // arquivos que ainda não foram salvos no HD
    for I := Pred( tabs.EditorsTabCount ) downto 0 do
    begin
      if (tabs.EditorsTab[I].FullPath = '') then
      begin
        pgEditor.ActivePage:= tabs.EditorsTab[I].Tab;
        actFileSave.Execute;
        // Se ainda estiver modificado, o usuário cancelou
        if tabs.EditorsTab[I].Editor.Modified then Exit;
      end;
    end;

    for I := Pred( tabs.EditorsTabCount ) downto 0 do
    begin
      try
        tabs.EditorsTab[I].SaveToFile(tabs.EditorsTab[I].FullPath);
        tabs.EditorsTab[I].Editor.Modified := False;
      except
        msgExclama( fTranslation.getMsgTranslation('ErrorCannotSaveFile', 'Error! The file could not be saved!') , Handle);
      end;
    end;

  finally
    if SaveActive <> nil then
      pgEditor.ActivePage:= SaveActive.Tab;
    UpdateGUI;
    UpdateCaption;
    UpdateStatusBar;
  end;
end;

procedure TfrmMain.actFileNewExecute(Sender: TObject);
begin
  with TfrmNewFile.Create(nil) do
  begin
    try
      if (ActiveEditorTab <> nil) and (ActiveEditorTab.FileType <> '') then
        SelectedFileType:= ActiveEditorTab.FileType
      else
        SelectedFileType:= NProfile.Config.DefaultFileType;

      if (ShowModal = mrOk) and (FileExists(SelectedTemplate)) then
      begin
        tabs.CreateNewEditor;
        pgEditor.ActivePageIndex:= pred( pgEditor.PageCount );
        if ActiveEditorTab = nil then Exit;
        ActiveEditorTab.FileType:= SelectedFileType;
        ActiveEditorTab.Editor.Lines.LoadFromFile( SelectedTemplate );
        UpdateCaption;
        tabs.FocusActiveEditor;

        eventServer.NotifyEvent(self, 'filecreated', SelectedFileType);

        UpdateStatusBar;
        UpdateGUI;
        if ActiveEditorTab.Editor.CanFocus then
         ActiveEditorTab.Editor.SetFocus;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actFileOpenExecute(Sender: TObject);
begin
  dlgOpen.Filter:= nProfile.getDlgFilters;

  if (ActiveEditorTab <> nil) and (ActiveEditorTab.FullPath <> '') then
  begin
    dlgOpen.InitialDir:= ExtractFilePath(ActiveEditorTab.FullPath);
  end else
  begin
    NMRU.LoadFromFile(NProfile.Paths.MRUFile);
    if NMRU.ItemsCount > 0 then
      dlgOpen.InitialDir:= NMRU.List.Strings[0];
  end;

  if dlgOpen.Execute then
    tabs.LoadMultipleFiles( dlgOpen.Files );

  UpdateCaption;
  tabs.FocusActiveEditor;
  UpdateStatusBar;
  UpdateGui;
end;

procedure TfrmMain.actFileSaveExecute(Sender: TObject);
begin
  if not tabs.IsEditorActivated then
    Exit;

  if FileExists(ActiveEditorTab.FullPath) then
  begin
    try
      ActiveEditorTab.SaveToFile( ActiveEditorTab.FullPath );
      ActiveEditorTab.Editor.Modified := False;
    except
      msgExclama( fTranslation.getMsgTranslation('ErrorCannotSaveFile', 'Error! The file could not be saved!') , Handle);
    end;
    UpdateGUI;
    UpdateCaption;
    UpdateStatusBar;
  end
  else
    actFileSaveAs.Execute;
end;

procedure TfrmMain.actFileSaveAsExecute(Sender: TObject);
var
  FileNameRecovery: string;
  S: string;
begin
  if not tabs.IsEditorActivated then Exit;

  dlgSave.Filter:= nProfile.getDlgFilters;

  if ActiveEditorTab.FullPath <> '' then
  begin
    dlgSave.InitialDir:= ExtractFilePath(ActiveEditorTab.FullPath);
  end else
  begin
    NMRU.LoadFromFile(NProfile.Paths.MRUFile);
    if NMRU.ItemsCount > 0 then
      dlgSave.InitialDir:= NMRU.List.Strings[0];

    S:= addSlash(dlgSave.InitialDir) + ActiveEditorTab.Tab.Caption + nProfile.FileTypesMgr.getFirstFileTypeExtension(ActiveEditorTab.FileType);
    S:= StringReplace(S, '*', '', [rfReplaceAll]);
    dlgSave.FileName:= S;
  end;

  FileNameRecovery := ActiveEditorTab.FullPath;
  if dlgSave.Execute and ( length(dlgSave.FileName) > 0 ) then
  begin
    ActiveEditorTab.FullPath := dlgSave.FileName;

    NMRU.Add( ActiveEditorTab.FullPath );
    NMRU.SaveToFile( NProfile.Paths.MRUFile );

    try
      FileSetReadOnly(ActiveEditorTab.FullPath, false);
      SetFileAttributes( PChar( ActiveEditorTab.FullPath ), FILE_ATTRIBUTE_ARCHIVE );
      ActiveEditorTab.SaveToFile( ActiveEditorTab.FullPath );
      ActiveEditorTab.Editor.ReadOnly := False;
      ActiveEditorTab.Editor.Modified := False;
    except
      ActiveEditorTab.Editor.ReadOnly := True;
      msgExclama( fTranslation.getMsgTranslation('ErrorCannotSaveFile', 'Error! The file could not be saved!') , Handle);
    end;
  end;
  tabs.FocusActiveEditor;
  UpdateStatusBar;
  UpdateCaption;
  UpdateGUI;
end;

procedure TfrmMain.actFileMruClearExecute(Sender: TObject);
begin
  nmru.Clear;
  nmru.SaveToFile(nprofile.Paths.MRUFile);
  tabs.ReloadStartPage;
end;

procedure TfrmMain.actFileStatisticsExecute(Sender: TObject);
begin
  with TfrmStatistics.Create(nil) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

{
 AÇÕES DO MENU EDITAR
 *******************************************************************
   As ações do menu editar ficam na classe TNotesTabsManager
 *******************************************************************
}

{ AÇÕES DO MENU LOCALIZAR }

procedure TfrmMain.actSearchFindExecute(Sender: TObject);
var
  I: integer;
begin
  if ActiveEditorTab = nil then Exit;

  for I:= 0 to self.ComponentCount - 1 do
    if self.Components[I] is TfrmSearch then Exit;

  with TfrmSearch.Create(self) do
  begin
    SearchDlgType:= sdSearch;
    Show;
  end;
end;



procedure TfrmMain.actSearchFindPreviusExecute(Sender: TObject);
begin
  if SearchReplace.SearchStr <> '' then
    SearchReplace.Previus;
end;



procedure TfrmMain.actSearchFindNextExecute(Sender: TObject);
begin
  if SearchReplace.SearchStr <> '' then
    SearchReplace.Next;
end;

procedure TfrmMain.actSearchReplaceExecute(Sender: TObject);
var
  I: integer;
begin
  if ActiveEditorTab = nil then Exit;

  for I:= 0 to self.ComponentCount - 1 do
    if self.Components[I] is TfrmSearch then Exit;

  with TfrmSearch.Create(self) do
  begin
    SearchDlgType:= sdReplace;
    Show;
  end;
end;

procedure TfrmMain.actSearchGotolineExecute(Sender: TObject);
Var
  L, C: integer;
begin
  if ActiveEditorTab = nil then Exit;
  L:= ActiveEditorTab.Editor.CaretY;
  C:= ActiveEditorTab.Editor.Lines.Count;
  if NProfile.Config.LineAndColCountStartAt0 then
  begin
    Dec(L);
    Dec(C);
  end;
  if C = 0 then C:= 1;

  L := StrToIntDef(

  InputBox(fTranslation.getMsgTranslation('GotoLineTitle', 'Go to line...'),
    Format(fTranslation.getMsgTranslation('GotoLineText', 'What line (between %d and %d) do you want to go?'),
    [integer(not NProfile.Config.LineAndColCountStartAt0), C]),
      IntToStr(L)), L);

  if L < IIF(NProfile.Config.LineAndColCountStartAt0, 0 ,1) then
    L:= IIF(NProfile.Config.LineAndColCountStartAt0, 0 ,1);

  if L > C then
    L:= C;

  if (L < 1) or (L > ActiveEditorTab.Editor.Lines.Count) then
    ActiveEditorTab.Editor.CaretY:= 1
  else
    ActiveEditorTab.Editor.CaretY := L;

  if ActiveEditorTab.Editor.CanFocus then
    ActiveEditorTab.Editor.SetFocus;
end;


procedure TfrmMain.actSearchGotocolExecute(Sender: TObject);
var
  CurCol: integer;
  MaxCol: integer;
begin

  if tabs.IsEditorActivated then
  begin

    MaxCol:= length(ActiveEditorTab.Editor.LineText) + 1;
    if NProfile.Config.LineAndColCountStartAt0 then Dec(MaxCol);

    CurCol := StrToIntDef( InputBox( fTranslation.getMsgTranslation('GotoColTitle', 'Go to column'),
      Format(fTranslation.getMsgTranslation('GotoColText', 'What column (betwen %d and %d) do you want to go?'),
      [integer(not NProfile.Config.LineAndColCountStartAt0), MaxCol]),
      IntToStr( ActiveEditorTab.Editor.CaretX)),
      ActiveEditorTab.Editor.CaretX );

    if CurCol < IIF(NProfile.Config.LineAndColCountStartAt0, 0 ,1) then
      CurCol:= IIF(NProfile.Config.LineAndColCountStartAt0, 0 ,1);

    if CurCol > MaxCol then
      CurCol:= MaxCol;

    if (CurCol < 1) then
      ActiveEditorTab.Editor.CaretX:= 1
    else
      ActiveEditorTab.Editor.CaretX := CurCol;

    if ActiveEditorTab.Editor.CanFocus then
      ActiveEditorTab.Editor.SetFocus;
  end;
end;


procedure TfrmMain.actSearchNextMarkExecute(Sender: TObject);
begin
  tabs.EditorNextMark(sender);
end;

procedure TfrmMain.actSearchPreviusMarkExecute(Sender: TObject);
begin
  tabs.EditorPreviusMark(sender);
end;

procedure TfrmMain.actSearchToggleMarkExecute(Sender: TObject);
begin
  tabs.EditorTogleMark(sender);
end;

procedure TfrmMain.actSearchGotoMarkExecute(Sender: TObject);
Var
  I: integer;
begin
  if ActiveEditorTab = nil then Exit;
  if ActiveEditorTab.Marks.Count < 1 then Exit;

  with TfrmList.Create(nil) do
  begin
    try
      Values.BeginUpdate;
      ActiveEditorTab.Marks.Sort;
      try
        for I:= 0 to ActiveEditorTab.Marks.Count -1 do
        begin
          if ActiveEditorTab.Marks.Items[I] <= ActiveEditorTab.Editor.Lines.Count then
            Values.Add(IntToStr(ActiveEditorTab.Marks.Items[I]) + ': ' + ActiveEditorTab.Editor.Lines[ActiveEditorTab.Marks.Items[I]-1]);
        end;
      finally
        Values.EndUpdate;
      end;

      caption:= 'Ir para marcador...';
      if ShowModal = mrOk then
        ActiveEditorTab.GoToLine(ActiveEditorTab.Marks.Items[getSelectedIndex]);
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actSearchClearMarksExecute(Sender: TObject);
begin
  tabs.EditorClearMarks(self);
end;

procedure TfrmMain.actSearchNextProblemExecute(Sender: TObject);
begin
  if ProblemsList.Count > 0 then
    ProblemsList.gotoNext;
end;

procedure TfrmMain.actSearchPreviusProblemExecute(Sender: TObject);
begin
  if ProblemsList.Count > 0 then
    ProblemsList.gotoPrevius;
end;


procedure TfrmMain.actSearchClearProblemsExecute(Sender: TObject);
begin
  if ProblemsList.Count > 0 then
    ProblemsList.Clear;
  if ActiveEditorTab <> nil then
    ActiveEditorTab.Editor.InvalidateGutter;
end;

procedure TfrmMain.actSearchForwardExecute(Sender: TObject);
begin
  EditLocationsList.gotoNext;
end;

procedure TfrmMain.actSearchBackExecute(Sender: TObject);
begin
  EditLocationsList.gotoPrevius;
end;

procedure TfrmMain.actEditTogleColumSelExecute(Sender: TObject);
begin
  if ActiveEditorTab <> nil then
  begin
    if ActiveEditorTab.Editor.SelectionMode <> smNormal then
      ActiveEditorTab.Editor.SelectionMode:= smNormal
    else
      ActiveEditorTab.Editor.SelectionMode:= smColumn;

     actEditTogleColumSel.Checked:= ActiveEditorTab.Editor.SelectionMode <> smNormal;
  end;
end;


{ AÇÕES DO MENU VER }

procedure TfrmMain.actViewNexttabExecute(Sender: TObject);
begin
  if pgEditor.PageCount < 1 then Exit;
  if pgEditor.ActivePageIndex < pgEditor.PageCount -1 then
    pgEditor.ActivePageIndex:= pgEditor.ActivePageIndex + 1
  else
    pgEditor.ActivePageIndex:= 0;
  UpdateCaption;
  UpdateStatusBar;
end;

procedure TfrmMain.actViewPreviustabExecute(Sender: TObject);
begin
  if pgEditor.PageCount < 1 then Exit;
  if pgEditor.ActivePageIndex > 0 then
    pgEditor.ActivePageIndex:= pgEditor.ActivePageIndex - 1
  else
    pgEditor.ActivePageIndex:= pgEditor.PageCount -1;
  UpdateCaption;
  UpdateStatusbar;
end;

procedure TfrmMain.actViewStatusBarExecute(Sender: TObject);
begin
  sbStatusbar.Visible:= not sbStatusbar.Visible;
  actViewStatusBar.Checked:= sbStatusbar.Visible;
  UpdateStatusBar;
end;

procedure TfrmMain.actViewToolbarExecute(Sender: TObject);
begin
  ToolBar.Visible := not ToolBar.Visible;
  actViewToolbar.Checked:= ToolBar.Visible;
  UpdateGui;
end;

procedure TfrmMain.actViewHidePanelsExecute(Sender: TObject);
begin
  PanelsManager.HideAllPanels;
end;

{ AÇÕES DO MENU PROJETO }

procedure TfrmMain.actProjectNewExecute(Sender: TObject);
begin
//
end;

procedure TfrmMain.actProjectOpenExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actProjectOpenAllExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actProjectAddExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actProjectRemoveExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actProjectOptionsExecute(Sender: TObject);
begin
  //
end;

{ AÇÕES DO MENU EXECUTAR }

//////////////////
//TODO Os procedimentos abaixo precisam ir para alguma outra unit
//////////////

function parseRunFileVars(const S: string; edTab: TNotesEditorTab ): string;
begin
  Result:= S;
  if edTab = nil then Exit;
  Result:= StringReplace(S, '%file%', edTab.FullPath, [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%filename%', ExtractFileName(edTab.FullPath), [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%filetitle%', ChangeFileExt(ExtractFileName(edTab.FullPath), ''), [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%ext%', ExtractFileExt(edTab.FullPath), [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%path%', ExtractFilePath(edTab.FullPath), [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%WordAtCursor%', edTab.Editor.WordAtCursor , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%selLength%', intToStr(length(edTab.Editor.SelText)) , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%selStart%', intToStr(edTab.Editor.SelStart) , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%line%', intToStr(edTab.Editor.CaretY) , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%col%', intToStr(edTab.Editor.CaretX) , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '%linesCount%', intToStr(edTab.Editor.Lines.Count) , [rfReplaceAll, rfIgnoreCase]);

  //************* IMPLEMENTAR *******************
  // %projName% %projDir% %projFile% %buildFile%
  //*********************************************
end;

// resulta FALSE se o usuário cancelar uma ação
function ParseRunFileFuncs(var S: string ): boolean;
Var
  sa: TStringArray;
  I, saLen, Len: integer;
  tk: string;
  name, param: string;
  delimiterpos: integer;
  Value: string;
begin
  Result:= false;
  sa:= ExplodeStr(S, '%');
  saLen:= length(sa);
  // se a string não tiver sido quebrada em ao menos 3 partes,
  // ela não tem nehuma função
  if saLen < 2 then
  begin
    Result:= true;
    setLength(sa, 0);
    Exit;
  end;
  // a string tem o seguinte formato:
  // texto qualquer %nomedafunção(parâmetro)% texto qualquer

  for I:= 1 to saLen - 1 do
  begin
    tk:= sa[I];
    Len:= length(tk);

    if (Len > 3) and (tk[Len] = ')') then
    begin
      // retiramos o último parêntese
      Dec(Len);
      setLength(tk, Len);

      delimiterpos:= fastPos(tk, '(', Len, 1, 1);

      if delimiterpos > 0 then
      begin
        name:= copy(tk, 1, delimiterpos-1);
        param:= copy(tk, delimiterpos+1, Len);

        if name <> '' then
        begin
          if SameText(name, 'askfile') then
          begin
            // %askfile()%
            With TOpenDialog.Create(nil) do
            begin
              try
                Filter:= '(*.*)|*.*';
                Title:= param;
                if Execute then
                  Value:= FileName
                else
                  Exit;
              finally
                free;
              end;
            end;
          end else
          if SameText(name, 'askstr') then
          begin
            // %askstr()%
            if not inputQuery('Notes', param, Value) then Exit;
          end else
          if SameText(name, 'env') then
          begin
            // %env()%
            if param <> '' then
              Value:= GetEnvironmentVariable(param)
            else
              raise Exception.Create('You have not declared what enviroment variable do you want.');
          end;
          // substituí a função pelo valor que ela retornou
          S:= StringReplace( S , '%' + tk + ')%' , Value, [rfReplaceAll]);
        end;
      end;
    end;
  end;
  SetLength(sa, 0);
  Result:= true;
end;

procedure TfrmMain.actRunLastExecute(Sender: TObject);
Var
  ri: TNotesRunItem;
  sl: TStringList;
  I: integer;
begin
  if Trim(fLastRunItem.Cmd) = '' then Exit;

  if tabs.IsEditorActivated then
  begin
    if fLastRunItem.SaveBeforeRun then
    begin
      actFileSave.Execute;
      //se o usuário tiver cancelado ao salvar, então saímos.
      if (ActiveEditorTab.Editor.Modified) or (ActiveEditorTab.FullPath = '') then Exit;
    end;
  end;

  ProblemsList.Clear;
  if tabs.IsEditorActivated then
    ActiveEditorTab.Editor.InvalidateGutter;

  if fLastRunItem.Cmd <> EmptyStr then
  begin
    ri.Cmd:= parseRunFileVars(fLastRunItem.Cmd, ActiveEditorTab);
    ri.Args:= parseRunFileVars(fLastRunItem.Args, ActiveEditorTab);
    ri.Dir:= parseRunFileVars(fLastRunItem.Dir, ActiveEditorTab);
    // O diretório não pode estar entre aspas!!!
    ri.Dir:= StringReplace(ri.Dir, '"', '', [rfReplaceAll]);

    if (parseRunFileFuncs(ri.Cmd) = false) or (parseRunFileFuncs(ri.Args) = false) or
         (parseRunFileFuncs(ri.Dir) = false) then
      Exit;

  ri.Output:= fLastRunItem.Output;

  if ri.Output = otNone then
  begin
    if ri.Dir <> '' then
      SetCurrentDir(ri.Dir);
    ShellExecute(self.Handle, 'open', PChar(ri.Cmd), PChar(ri.Args), PChar(ri.Dir), SW_SHOWNORMAL);
    Exit;
  end;

  // Se o painel já estiver rodando algo, saímos
  if StrToBoolDef(PanelsManager.getPanelProperty('Output', 'Running'), false) then Exit;

  // Colocamos o cursor no final do texto
  if ri.Output = otActiveEditor then
    tabs.EditorEditorBottom(nil);

  if ri.Output = otStandardOutput then
  begin
    PanelsManager.setPanelProperty('Output', 'CaptureTo', 'outputwindow');
  end else
  begin
    if tabs.IsEditorActivated then
      PanelsManager.setPanelProperty('Output', 'CaptureTo', 'editor')
    else
      raise Exception.Create('There is no editor active, can not capture the output to the editor.');
  end;


  PanelsManager.setPanelProperty('Output', 'FileToRun', ri.Cmd);
  PanelsManager.setPanelProperty('Output', 'Arguments', ri.Args);
  PanelsManager.setPanelProperty('Output', 'WorkDir', ri.Dir);
  PanelsManager.setPanelProperty('Output', 'AutoScanForProblems', BoolToStr(true, true));
  PanelsManager.setPanelProperty('Output', 'ErrorsAutoParse', BoolToStr(fLastRunItem.AutoUnderstandOutput, true));
  PanelsManager.setPanelProperty('Output', 'ErrorsRegexParser', fLastRunItem.OutputRegex);
  PanelsManager.setPanelProperty('Output', 'ErrorsReFilePos', fLastRunItem.REFilePos);
  PanelsManager.setPanelProperty('Output', 'ErrorsReLinePos', fLastRunItem.RELinePos);

  sl:= TStringList.Create;
  try
    for I := 0 to Pred(tabs.EditorsTabCount) do
      if tabs.EditorsTab[I].FullPath <> EmptyStr then
        sl.Add(tabs.EditorsTab[I].FullPath);

    PanelsManager.setPanelProperty('Output', 'ErrorsValidFiles', sl.Text);
  finally
    sl.Free;
  end;

  // limpamos o conteúdo atual do painel
  PanelsManager.PanelExecute('Output', 'clear', '');
  // Roda o programa!
  PanelsManager.PanelExecute('Output', 'Run', '');

  end;
end;

procedure TfrmMain.actRunFileExecute(Sender: TObject);
begin
  if not tabs.IsEditorActivated then Exit;
  if OutType <> -1 then Exit;
  if ActiveEditorTab.FullPath = '' then Exit;

  fLastRunActiveFile:= ActiveEditorTab.FullPath;

  fLastRunItem.Cmd:= ActiveEditorTab.FullPath;
  fLastRunItem.Args:='';
  fLastRunItem.Dir:= ExtractFilePath(ActiveEditortab.FullPath);
  fLastRunItem.Output:= otNone;
  fLastRunItem.SaveBeforeRun:= false;

  actRunLast.Execute;
end;

////////////////////////////
procedure TfrmMain.actRunConfigureExecute(Sender: TObject);
begin
  with TfrmFolderMenuEditor.Create(nil) do
  begin
    try
      RootNodeName:= 'Items';
      KeymapActionList:= ActionList;
      Caption:= 'Configurar menu executar...';
      ItemsFolder:= nProfile.Paths.RunDir;
      ItemsExtension:= NotesGlobals.NOTES_RUN_EXTENSION;
      OnEditItem:= DoEditRunItem;
      OnCreateItem:= DoCreateRunItem;
      ShowModal;
    finally
      Free;
    end;
  end;
  BuildFolderBasedMenus;
end;

procedure TfrmMain.DoCreateRunItem(const FileName: string;
  var ReloadItems: boolean);
begin
  nProfile.RunMgr.NewItem(FileName);

  if fileExists(FileName) then
    with TfrmEditmnuRunItem.Create(nil) do
      try
        EditFile(FileName);
        ShowModal;
      finally
        Free;
      end;

  ReloadItems:= true;
end;

procedure TfrmMain.DoEditRunItem(const FileName: string;
  var ReloadItems: boolean);
begin
  if fileExists(FileName) then
    with TfrmEditmnuRunItem.Create(nil) do
      try
        EditFile(FileName);
        ShowModal;
      finally
        Free;
      end;

  ReloadItems:= false;
end;


{ AÇÕES DO MENU COMANDOS }

procedure TfrmMain.actCommandsUndoExecute(Sender: TObject);
var
  S: string;
begin
  if not tabs.IsEditorActivated then
    Exit;
  S := ActiveEditorTab.Editor.Text;
  ActiveEditorTab.Editor.Text := NUndoTool;
  NUndoTool := S;
end;


procedure TfrmMain.actCommandsFixlinebreaksExecute(Sender: TObject);
var
  S: string;
begin
  if not tabs.IsEditorActivated then
    Exit;
  NUndoTool := ActiveEditorTab.Editor.Text;
  actCommandsUndo.Enabled := True;
  S := ActiveEditorTab.Editor.Text;
  S := FastReplace( S, ASCII_CRLF, ASCII_BEEP, False );
  S := FastReplace( S, ASCII_LF, ASCII_BEEP, False );
  S := FastReplace( S, ASCII_CR, ASCII_BEEP, False );
  S := FastReplace( S, ASCII_LF+ASCII_CR, ASCII_BEEP, False );
  ActiveEditorTab.Editor.Text := FastReplace( S, ASCII_BEEP, ASCII_CRLF, False );
end;

procedure TfrmMain.actCommandsWrapExecute(Sender: TObject);
var
  strTemp: string;
begin
  if not tabs.IsEditorActivated then
    Exit;
  if (ActiveEditorTab.Editor.SelEnd - ActiveEditorTab.Editor.SelStart) > 0 then
  begin
    if not InputQuery(fTranslation.getMsgTranslation('WrapLinesTitle', 'Wrap lines...'),
    fTranslation.getMsgTranslation('WrapLinesText', 'What the number of columns the selection must have?'),
    strTemp) then Exit;
    if IsNumber( strTemp ) then
    begin
      NUndoTool:=ActiveEditorTab.Editor.Text;
      actCommandsUndo.Enabled:=true;
      ActiveEditorTab.Editor.SelText:= WordWrap(ActiveEditorTab.Editor.SelText,strToInt(strTemp));
      //wrapText
    end;
  end;
end;


procedure TfrmMain.actCommandsInsertFileExecute(Sender: TObject);
var
  intPosRestore:integer;
  strTitleRestore:String;
begin
  if not tabs.IsEditorActivated then
    Exit;
  // guardamos as configurações
  strTitleRestore := dlgOpen.Title;
  intPosRestore := ActiveEditorTab.Editor.selstart;
  //mudamos as configurações
  dlgOpen.Title := fTranslation.getMsgTranslation('InsertFileTitle', 'Choose a file to insert...');
  dlgOpen.FileName := '';
  try
    if dlgOpen.Execute and FileExists( dlgOpen.FileName ) then
    begin
      NUndoTool := ActiveEditorTab.Editor.Text;
      actCommandsUndo.Enabled := true;
      SendMessage( ActiveEditorTab.Editor.Handle, WM_SETREDRAW, 0, 0 );
      ActiveEditorTab.Editor.Lines.BeginUpdate;
      ActiveEditorTab.Editor.SelText := FiletoStr( dlgOpen.FileName );
      ActiveEditorTab.Editor.Lines.EndUpdate;
      SendMessage(ActiveEditorTab.Editor.Handle, WM_SETREDRAW, -1, 0 );
      InvalidateRect(ActiveEditorTab.Editor.Handle, nil,true);
    end;
  finally
    //restauramos as configurações
    dlgOpen.Title := strTitleRestore;
    ActiveEditorTab.Editor.SelStart := intPosRestore;
  end;
end;

procedure TfrmMain.actCommandsInsertDateTimeExecute(Sender: TObject);
begin
  if tabs.IsEditorActivated then
  begin
    NUndoTool := ActiveEditorTab.Editor.Text;
    actCommandsUndo.Enabled := True;
    ActiveEditorTab.Editor.SelText := FormatDateTime( 'dd/mm/yy hh:nn', Now );
  end;
end;

procedure TfrmMain.actCommandsInsertImageExecute(Sender: TObject);
var
  TheHeight, TheWidth: word;
  Ih, Iw: integer;
  strNameRestore, strFilterRestore,
  strALT, strTemp, strRelativePath: string;
begin
  if not tabs.IsEditorActivated then
    Exit;
  if ActiveEditorTab.FullPath <> '' then
  begin
    NUndoTool := ActiveEditorTab.Editor.Text;
    actCommandsUndo.Enabled:= true;
    strNameRestore:= dlgOpen.FileName;
    strFilterRestore:= dlgOpen.Filter;
    dlgOpen.Filter:= fTranslation.getMsgTranslation('ImagesFilter', 'Image Files') + ' (*.gif,*.jpg,*.jpeg,*.png)|*.gif;*.jpg;*.jpeg;*.png';
    if dlgOpen.Execute then
    begin
      strTemp:= dlgOpen.FileName;
      strTemp:=  AnsiLowerCase( ExtractFileExt(StrTemp) );

      try
        if strTemp = '.gif' then
          GetGIFSize( dlgOpen.FileName, TheWidth, TheHeight )
        else if (strTemp = '.jpg') or (strTemp = '.jpeg') then
          GetJPGSize(dlgOpen.FileName,TheWidth,TheHeight)
        else if strTemp = '.png' then
          GetPNGSize(dlgOpen.FileName,TheWidth,TheHeight)
        else
          ShowMessage(fTranslation.getMsgTranslation('ErrorUnknownImageFormat', 'Unknown image format.'));
      except
        raise Exception.Create('Notes image library error.');
      end;

      Iw := integer(TheWidth);
      Ih := integer(TheHeight);
      strRelativePath:= ExtractFilePath( ActiveEditorTab.FullPath );
        // criamos um caminho relativo ao arquivo
      strRelativePath:= stringReplace( dlgOpen.FileName,strRelativePath,'',[] );
        // trocamos todas as '\' pelo formato do html, ou seja, '/'
      strRelativePath:= FastReplace(strRelativePath,'\','/',True);
      strALT:= ExtractFileName(dlgOpen.FileName);
      strALT:= ChangeFileExt(strALT,'');
      ActiveEditorTab.Editor.SelText:= '<img src="'+ strRelativePath +'" alt="'+ strALT +'" width="'+  intToStr(Iw) + '" height="'+ intToStr(Ih)+ '" align="left" border="0">';
    end;
  end;
  dlgOpen.Filter:= strFilterRestore;
  dlgOpen.FileName:= strNameRestore;
end;

procedure TfrmMain.actCommandsConvertSpecialHtmlCharsExecute(
  Sender: TObject);
begin
  if not tabs.IsEditorActivated then
    Exit;
  NUndoTool := ActiveEditorTab.Editor.Text;
  actCommandsUndo.Enabled := True;
  ActiveEditorTab.Editor.SelText := TXT2HTML( ActiveEditorTab.Editor.SelText );
end;

procedure TfrmMain.actCommandsCompressHtmlExecute(Sender: TObject);
var
  strTemp: string;
begin
  if not tabs.IsEditorActivated then
    Exit;

  strTemp:= ActiveEditorTab.Editor.Text;
  NUndoTool:= strTemp;
  actCommandsUndo.Enabled:= true;
  // PROCEDIMENTO PARA COMPRIMIR HTML
  // fins de linha
  strReplaceAll(strTemp, #13#10#13#10,'');
  strReplaceAll(strTemp, #13#13,'');
  strReplaceAll(strTemp, #10#10,'');
  // tabs duplos
  strReplaceAll(strTemp, #9#9, #9);
  // espaços duplos
  strReplaceAll(strTemp, #32#32, #32);
  // espaços e tabs ao início da linha
  strReplaceAll(strTemp, #10#32, #10);
  strReplaceAll(strTemp, #13#32, #13);
  strReplaceAll(strTemp, #10#9, #10);
  strReplaceAll(strTemp, #13#9, #13);

  // repte-se tudo, pois ao fazer a primeira compressão
  // é possível que apareçam novos caracteres a serem comprimidos

  // fins de linha
  strReplaceAll(strTemp, #13#10#13#10,'');
  strReplaceAll(strTemp, #13#13,'');
  strReplaceAll(strTemp, #10#10,'');
  // tabs duplos
  strReplaceAll(strTemp, #9#9, #9);
  // espaços duplos
  strReplaceAll(strTemp, #32#32, #32);
  // espaços e tabs ao início da linha
  strReplaceAll(strTemp, #10#32, #10);
  strReplaceAll(strTemp, #13#32, #13);
  strReplaceAll(strTemp, #10#9, #10);
  strReplaceAll(strTemp, #13#9, #13);

  ActiveEditorTab.Editor.BeginUpdate;
  ActiveEditorTab.Editor.Text:=strTemp;
  ActiveEditorTab.Editor.EndUpdate;
  msgInfo(Format(fTranslation.getMsgTranslation('InfoHTMLCompressed','HTML compressed. %d bytes removed :)') , [length(NUndoTool) - length(strTemp)]) , Handle);
end;

procedure TfrmMain.actCommandsVerifyHTMLExecute(Sender: TObject);
begin
  PanelsManager.ShowPanel('Verification');
  Application.ProcessMessages;
  PanelsManager.PanelExecute('Verification', 'FixHTML', '');
end;

procedure TfrmMain.actCommandsFixHTMLExecute(Sender: TObject);
Var
  S: string;
begin
  if ActiveEditortab = nil then exit;
  if ActiveEditorTab.Editor.ReadOnly then Exit;
  NUndoTool := ActiveEditorTab.Editor.Text;
  actCommandsUndo.Enabled := True;
  PanelsManager.ShowPanel('Verification');
  Application.ProcessMessages;
  S:= PanelsManager.PanelExecute('Verification', 'FixHTML', '');
  if Trim(S) <> '' then
  begin
    ActiveEditorTab.Editor.Text:= S;
    ActiveEditorTab.Editor.Modified:= true;
  end else
  begin
    MsgExclama(fTranslation.getMsgTranslation('ErrorHTMLTidy','HTML Tidy: the HTML have too much errors for me :('), handle);
  end;
end;

procedure TfrmMain.actCommandsHTMLtoXHTMLExecute(Sender: TObject);
Var
  S: string;
begin
  if ActiveEditortab = nil then exit;
  if ActiveEditorTab.Editor.ReadOnly then Exit;
  NUndoTool := ActiveEditorTab.Editor.Text;
  actCommandsUndo.Enabled := True;
  PanelsManager.ShowPanel('Verification');
  Application.ProcessMessages;
  S:= PanelsManager.PanelExecute('Verification', 'FixXHTML', '');
  if Trim(S) <> '' then
  begin
    ActiveEditorTab.Editor.Text:= S;
    ActiveEditorTab.Editor.Modified:= true;
  end else
  begin
    MsgExclama(fTranslation.getMsgTranslation('ErrorHTMLTidy','HTML Tidy: the HTML have too much errors for me :('), handle);
  end;
end;

procedure TfrmMain.actCommandsVerifyXMLExecute(Sender: TObject);
begin
  PanelsManager.ShowPanel('Verification');
  Application.ProcessMessages;
  PanelsManager.PanelExecute('Verification', 'FixXML', '');
end;

{ AÇÕES DO MENU OPÇÕES }


procedure TfrmMain.actOptionsTopmostExecute(Sender: TObject);
const
  TopMost: array[Boolean] of Cardinal = ( HWND_NOTOPMOST, HWND_TOPMOST );
begin
  actOptionsTopmost.Checked := not actOptionsTopmost.Checked;
  SetWindowPos( Handle, TopMost[actOptionsTopmost.Checked], 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE );
end;

procedure TfrmMain.actOptionsReadonlyExecute(Sender: TObject);
begin
  if not tabs.IsEditorActivated then
    Exit;

  if ActiveEditorTab.Editor.ReadOnly then
  begin
    actOptionsReadonly.Checked := False;
    FileSetReadOnly( ActiveEditorTab.FullPath , False);
    SetFileAttributes( PChar( ActiveEditorTab.FullPath ), FILE_ATTRIBUTE_ARCHIVE );
  end else
  begin
    actOptionsReadonly.Checked:= true;
    FileSetReadOnly( ActiveEditorTab.FullPath , true);
  end;

  ActiveEditorTab.Editor.ReadOnly := not ActiveEditorTab.Editor.ReadOnly;

  UpdateGUI;
  UpdateCaption;
end;


procedure TfrmMain.actOptionsProfilesExecute(Sender: TObject);
begin
  with TfrmProfileManager.Create(nil) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;


procedure TfrmMain.actOptionsTemplatesExecute(Sender: TObject);
begin
  With TfrmConfigTemplates.Create(nil) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actOptionsKeymapExecute(Sender: TObject);
begin
  With TFrmShortcuts.Create(nil) do
  begin
    try
      setActionList(ActionList);
      ShowModal;
      fKeymap.LoadKeymap(nProfile.Paths.KeymapFile);
    finally
      free;
    end;
  end;
end;

procedure TfrmMain.actOptionsPreferencesExecute(Sender: TObject);
begin
  with TfrmConfig.Create(nil) do
  begin
    try
      ShowModal;
      if ModalResult = mrOK then
        ApplyConfig;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actOptionsWordWrapExecute(Sender: TObject);
begin
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.WordWrap:= not ActiveEditorTab.Editor.WordWrap;
  UpdateGui;
end;


{ AÇÕES DO MENU AJUDA }


procedure TfrmMain.actHelpIndexExecute(Sender: TObject);
begin
  ShellExecute( Handle, 'open', PChar(AddSlash(NExePath) + 'help\index.htm'),
    nil, nil, SW_SHOWNORMAL );
end;

procedure TfrmMain.actHelpReadmeExecute(Sender: TObject);
begin
  if FileExists( NExePath + 'readme.txt' ) then
  begin
    if tabs.LoadFile( NExePath + 'readme.txt') then
    pgEditor.ActivePageIndex:= pred( pgEditor.PageCount );
  end;
  UpdateStatusBar;
end;

procedure TfrmMain.actHelpCopyExecute(Sender: TObject);
begin
  if FileExists( NExePath + 'LICENSE' ) then
  begin
    if tabs.LoadFile( NExePath + 'LICENSE') then
    pgEditor.ActivePageIndex:= Pred( pgEditor.PageCount );
  end;
  UpdateStatusBar;
end;

procedure TfrmMain.actHelpWebsiteExecute(Sender: TObject);
begin
  ShellExecute( Handle, 'open', 'https://github.com/jonasraoni/notes', nil, nil, SW_SHOWNORMAL );
end;

procedure TfrmMain.actHelpBugreportExecute(Sender: TObject);
var
  S: string;
begin
  S:= 'https://github.com/jonasraoni/notes';
  ShellExecute( Handle, 'open', PChar( S ), nil, nil, SW_SHOWNORMAL );
end;

procedure TfrmMain.actHelpAboutExecute(Sender: TObject);
begin
  with TfrmAbout.Create( nil ) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.actHelpTellAFriendExecute(Sender: TObject);
Var
  S: string;
begin
  S:= 'mailto:?subject=Notes&body=https://github.com/jonasraoni/notes';
  ShellExecute( Handle, 'open', PChar( S ), nil, nil, SW_SHOWNORMAL );
end;

procedure TfrmMain.ActionListExecute(Action: TBasicAction;
  var Handled: Boolean);
  begin
  if fMacros.Recording then
    fMacros.RecordAction( (Action As TAction).Name );
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (ActiveEditorTab <> nil) and (ActiveEditorTab.Editor.Handle = getFocus) then
  begin
    eventServer.NotifyEvent(self, 'charpressed', String(key));
    if fMacros.Recording then
      fMacros.RecordKeystroke(key);
  end;
end;

procedure TfrmMain.SetStatusBarMessage(const msg: string);
begin
  sbStatusBar.Panels[SB_HINT].Text:= msg;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  eventServer.NotifyEvent(self, 'keyspressed', ShortcutToText(Shortcut(Key, Shift)));
end;

procedure TfrmMain.DoRestoreNotesShortcuts(Sender: TObject);
begin
  fKeymap.AssignToActionList(ActionList);
end;

procedure TfrmMain.DoStopNotesShortcuts(Sender: TObject);
begin
  fKeymap.ClearActionListShortcuts(ActionList);
end;


end.
