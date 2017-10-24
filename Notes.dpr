//
//    Notes - editor para programadores
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
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//


(*
  @abstract(NotestabsManager - classe para gerenciamento das tabs/documentos.)
  @author(Equipe do Notes, visite-nos em https://github.com/jonasraoni/notes)
*)
program Notes;

{$R *.res}

uses
  Controls,
  InstanceManager in '3rdparty\InstanceManager.pas',
  SysUtils,
  Forms,
  frm_princ in 'forms\frm_princ.pas' {frmMain},
  frm_addItem in 'forms\frm_addItem.pas' {frmAddLibItem},
  frm_editRunItem in 'forms\frm_editRunItem.pas' {frmEditmnuRunItem},
  frm_About in 'forms\frm_About.pas' {frmAbout},
  NotesHTML in 'core\NotesHTML.pas',
  NotesUtils in 'core\NotesUtils.pas',
  NotesXML in 'core\NotesXML.pas',
  NotesMRU in 'core\NotesMRU.pas',
  NotesImgSize in 'core\NotesImgSize.pas',
  NotesConfig in 'core\NotesConfig.pas',
  NotesGlobals in 'core\NotesGlobals.pas',
  NotesProfile in 'core\NotesProfile.pas',
  NotesOptions in 'core\NotesOptions.pas',
  NotesEditorTab in 'editor\NotesEditorTab.pas',
  SynEdit in 'editor\SynEdit.pas',
  SynEditHighlighter in 'editor\SynEditHighlighter.pas',
  SynEditKbdHandler in 'editor\SynEditKbdHandler.pas',
  SynEditKeyCmds in 'editor\SynEditKeyCmds.pas',
  SynEditKeyConst in 'editor\SynEditKeyConst.pas',
  SynEditMiscClasses in 'editor\SynEditMiscClasses.pas',
  SynEditMiscProcs in 'editor\SynEditMiscProcs.pas',
  SynEditPrint in 'editor\SynEditPrint.pas',
  SynEditPrinterInfo in 'editor\SynEditPrinterInfo.pas',
  SynEditPrintHeaderFooter in 'editor\SynEditPrintHeaderFooter.pas',
  SynEditPrintMargins in 'editor\SynEditPrintMargins.pas',
  SynEditPrintMarginsDialog in 'editor\SynEditPrintMarginsDialog.pas',
  SynEditPrintPreview in 'editor\SynEditPrintPreview.pas',
  SynEditPrintTypes in 'editor\SynEditPrintTypes.pas',
  SynEditSearch in 'editor\SynEditSearch.pas',
  SynEditTextBuffer in 'editor\SynEditTextBuffer.pas',
  SynEditTypes in 'editor\SynEditTypes.pas',
  SynTextDrawer in 'editor\SynTextDrawer.pas',
  SynEditStrConst in 'editor\SynEditStrConst.pas',
  DosCommand in '3rdparty\DosCommand.pas',
  FastStrings in '3rdparty\FastStrings.pas',
  NotesCmdParser in 'core\NotesCmdParser.pas',
  LibXmlParser in '3rdparty\LibXmlParser.pas',
  frm_ConfigPrint in 'forms\frm_ConfigPrint.pas' {frmConfigPrint},
  NotesOutputParser in 'core\NotesOutputParser.pas',
  NotesTimers in 'core\NotesTimers.pas',
  NotesToolTip in 'core\NotesToolTip.pas',
  NotesPanels in 'panels\NotesPanels.pas',
  NotesLibraryPanel in 'panels\NotesLibraryPanel.pas',
  NotesCopyDeldialog in 'forms\NotesCopyDeldialog.pas' {frmCopyDel},
  NotesListEditor in 'forms\NotesListEditor.pas' {frmListEditor},
  NotesOutputPanel in 'panels\NotesOutputPanel.pas',
  NotesEditorTabPosList in 'core\NotesEditorTabPosList.pas',
  frm_NewFile in 'forms\frm_NewFile.pas' {frmNewFile},
  NotesTemplates in 'core\NotesTemplates.pas',
  frm_ConfigTemplates in 'forms\frm_ConfigTemplates.pas' {frmConfigTemplates},
  frm_ProfileMan in 'forms\frm_ProfileMan.pas' {frmProfileManager},
  frm_NewProfile in 'forms\frm_NewProfile.pas' {frmNewProfile},
  frm_Installer in 'forms\frm_Installer.pas' {frmInstaller},
  NotesSeeker in 'core\NotesSeeker.pas',
  NotesSearchReplace in 'core\NotesSearchReplace.pas',
  frm_Search in 'forms\frm_Search.pas' {frmSearch},
  NotesStartPage in 'forms\NotesStartPage.pas',
  XG in 'XG\XG.pas',
  XGComps in 'XG\XGComps.pas',
  XGContainers in 'XG\XGContainers.pas',
  XgCSSParser in 'XG\XgCSSParser.pas',
  XGServices in 'XG\XGServices.pas',
  XGTypeHandlers in 'XG\XGTypeHandlers.pas',
  XGUtils in 'XG\XGUtils.pas',
  NotesProject in 'core\NotesProject.pas',
  SmallTidyWraper in '3rdparty\SmallTidyWraper.pas',
  NotesVerificationPanel in 'panels\NotesVerificationPanel.pas',
  NotesTabsManager in 'core\NotesTabsManager.pas',
  frm_Shortcuts in 'forms\frm_Shortcuts.pas' {frmShortcuts},
  NotesKeymaps in 'core\NotesKeymaps.pas',
  frm_EditShortcut in 'forms\frm_EditShortcut.pas' {frmEditShotcut},
  NotesActions in 'core\NotesActions.pas',
  NotesTranslation in 'core\NotesTranslation.pas',
  SynEditWordWrap in 'editor\SynEditWordWrap.pas',
  NotesHighlighterStyles in 'editor\NotesHighlighterStyles.pas',
  NotesHighlighter in 'editor\NotesHighlighter.pas',
  frm_config in 'forms\frm_config.pas' {frmConfig},
  NotesFileTypeOptions in 'core\NotesFileTypeOptions.pas',
  NotesPackage in 'core\NotesPackage.pas',
  ZLibEx in '3rdparty\zlibex.pas',
  NotesRememberEditorState in 'editor\NotesRememberEditorState.pas',
  RegExpr in '3rdparty\RegExpr.pas',
  jsintf in '3rdparty\jsbridge\jsintf.pas',
  js15decl in '3rdparty\jsbridge\js15decl.pas',
  jsbridge in '3rdparty\jsbridge\jsbridge.pas',
  NotesJavaScriptApi in 'core\NotesJavaScriptApi.pas',
  ptrarray in '3rdparty\jsbridge\ptrarray.pas',
  namedarray in '3rdparty\jsbridge\namedarray.pas',
  jsintf_bridge in '3rdparty\jsbridge\jsintf_bridge.pas',
  jsbridge_pvt in '3rdparty\jsbridge\jsbridge_pvt.pas',
  NotesMacros in 'core\NotesMacros.pas',
  frm_FolderMenuEditor in 'forms\frm_FolderMenuEditor.pas' {frmFolderMenuEditor},
  NotesFolderMenus in 'core\NotesFolderMenus.pas',
  NotesSmartIndent in 'editor\NotesSmartIndent.pas',
  frm_ListDlg in 'forms\frm_ListDlg.pas' {frmList},
  NotesEventServer in 'core\NotesEventServer.pas',
  NotesCodeCompletitionDlg in 'editor\NotesCodeCompletitionDlg.pas',
  VirtualTrees in '3rdparty\VirtualTrees.pas',
  NotesSearchPanel in 'panels\NotesSearchPanel.pas',
  frm_Statistics in 'forms\frm_Statistics.pas' {frmStatistics};

var
  I: integer;

begin
  Application.Initialize;
  Application.Title := 'Notes 2004';

  NExeFile := ParamStr(0);
  NExePath := ExtractFilePath( NExeFile );
  NDataPath := AddSlash( GetSpecialFolder( sfAppData ) + NOTES_DATA_DIRNAME );
  NProfilesPath := AddSlash( NDataPath + NOTES_PROFILES_DIRNAME );

  // LINHA DE COMANDO
  NotesCmdLine.Cmd:= string(GetCommandLine);
  NotesCmdLine.Parse;

  //###############################
  // mudança de profile pela linha de comando
  if (NotesCmdLine.SwitchToProfile <> '') and (NProfile.ProfileExists(NotesCmdLine.SwitchToProfile)) then
  begin
    Nprofile.SetProfile(NotesCmdLine.SwitchToProfile);
  end
  else
  // profile ativo
  if NProfile.getActiveProfile <> '' then
  begin
    NProfile.SetProfile( NProfile.getActiveProfile );
  end else
  // nenhum profile ainda, o Notes não deve estar instalado. Instalemos :)
  begin
    with TfrmInstaller.Create(nil) do
    begin
      try
        I:= ShowModal;
      finally
        Free;
      end;
    end;

    if I <> mrOk then
      Application.Terminate;

    NProfile.SetProfile( NProfile.getActiveProfile );
  end;
  //###############################

  initInstanceManager;

  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

