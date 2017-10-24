(*
@abstract(uNotesShellExt - DLL para Shell Extension do NotesSE2004)
@author(Daniel Roma)
@author(Anderson Barbieri)
Código de implementação da Shell Extension do NotesSE2004, inserindo
um menu de contexto no Windows Explorer.
Inclui códigos para manipulação da Área de Transferência e um
pequeno algorítmo de compactação de HTML
*)

unit uNotesShellExt;

interface

uses
  Windows, ActiveX, ComObj, ShlObj, Graphics, Classes;

type
  TContextMenu = class(TComObject, IShellExtInit, IContextMenu)
  private
    //Arquivo clicado no Explorer
    fFileName: array[0..MAX_PATH] of Char;
    //Path para a DLL (teoricamente, a do Notes tbem... :D)
    fNotesPath: string;
    //Buffer para o Path da DLL
    fNotesPathBuf: array[0..MAX_PATH] of Char;
    //Ícone do Notes
    NotesIcon: TBitMap;
    //Ícone 'Abrir'
    OpenIcon: TBitMap;
    //Ícone da HomePage do Notes
    WWWIcon: TBitMap;
    //Ícone 'Copiar'
    CopyIcon: TBitMap;
    //Ícone 'Favoritos'
    FavIcon: TBitMap;
    //Bitmap para ser inserido no menu de contexto: 'Sobre o NotesSE2004'
    AboutBitMap: TBitMap;
  protected
    //Inicialização do Shell Extension
    function IShellExtInit.Initialize = SEIInitialize;
    function SEIInitialize(pidlFolder: PItemIDList; lpdobj: IDataObject;hKeyProgID: HKEY): HResult; stdcall;
    //Menu de Contexto do Explorer
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast,uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;pszName: LPSTR; cchMax: UINT): HResult; stdcall;
  end;

const
  Class_ContextMenu: TGUID = '{EB48850B-D521-4B24-8E57-8D3B654E8FA4}';

implementation

{$R NotesShell.res}


uses ComServ, SysUtils, ShellApi, Registry, StrUtils;


(*
@code(aBitMap) - TBitMap que receberá a imagem
@code(aResourceName) - nome do recurso que contém a imagem desejada
função que carrega um BitMap a partir do Resource, redimensionando pro sistema do usuário
*)
procedure fLoadImage(out aBitMap: TBitMap;aResourceName:String);
var tmpBitMap: TBitMap;
    userBitMapSize: integer;
begin
  if aBitMap = nil then
  begin
    tmpBitMap := TBitmap.Create;
    userBitMapSize := GetSystemMetrics(SM_CXMENUCHECK);

    try
      tmpBitMap.LoadFromResourceName(hInstance,aResourceName);
      aBitMap := TBitmap.Create;
      aBitMap.Width := userBitMapSize;
      aBitMap.Height := userBitMapSize;
      aBitMap.Canvas.StretchDraw(rect(0,0,userBitMapSize,userBitMapSize),tmpBitMap);
    finally
      tmpBitMap.Free;
    end;
  end;
end;


(*
@code(aText) - String a ser colocada na área de transferência
*)
procedure SetClipboardText(aText:PChar);
var Data: THandle;
    DataPtr: Pointer;
begin
  OpenClipboard(GetDesktopWindow);
  try
    Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, length(aText) + 1);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(aText^, DataPtr^, length(aText));
        EmptyClipboard;
        SetClipboardData(CF_TEXT, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    CloseClipboard;
  end;
end;



(*
@code(S) - String onde serão procurados os padrões
@code(aSearch) - padrão a ser procurado
@code(aReplace) - padrão a ser inserido nas ocorrências
Substitui TODAS as ocorrências de uma string, mesmo que depois da primeira
substituição o mesmo padrão ressurja
*)
procedure StrReplaceAll(var S: string; const aSearch, aReplace: string);
begin
  While Pos(aSearch, S) > 0 do
    S:= StringReplace(S, aSearch, aReplace,  [rfReplaceAll]);
end;


(*
@code(S) - String com código HTML
Comprime um código HTML
*)
function compHTML(const S:String): string;
begin
  Result:= S;
  // fins de linha
  strReplaceAll(Result, #13#10#13#10,'');
  strReplaceAll(Result, #13#13,'');
  strReplaceAll(Result, #10#10,'');
  // tabs duplos
  strReplaceAll(Result, #9#9, #9);
  // espaços duplos
  strReplaceAll(Result, #32#32, #32);
  // espaços e tabs ao início da linha
  strReplaceAll(Result, #10#32, #10);
  strReplaceAll(Result, #13#32, #13);
  strReplaceAll(Result, #10#9, #10);
  strReplaceAll(Result, #13#9, #13);
  // repte-se tudo, pois ao fazer a primeira compressão
  // é possível que apareçam novos caracteres a serem comprimidos
  // fins de linha
  strReplaceAll(Result, #13#10#13#10,'');
  strReplaceAll(Result, #13#13,'');
  strReplaceAll(Result, #10#10,'');
  // tabs duplos
  strReplaceAll(Result, #9#9, #9);
  // espaços duplos
  strReplaceAll(Result, #32#32, #32);
  // espaços e tabs ao início da linha
  strReplaceAll(Result, #10#32, #10);
  strReplaceAll(Result, #13#32, #13);
  strReplaceAll(Result, #10#9, #10);
  strReplaceAll(Result, #13#9, #13);
end;


// Função de inicialização do Shell Extension...
// Não... não faço a mínima ideia do que representam os parâmetros e o resultado...
// :)
function TContextMenu.SEIInitialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
  hKeyProgID: HKEY): HResult;
var
  StgMedium: TStgMedium;
  FormatEtc: TFormatEtc;
begin
  if (lpdobj = nil) then begin
    Result := E_INVALIDARG;
    Exit;
  end;

  with FormatEtc do begin
    cfFormat := CF_HDROP;
    ptd      := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex   := -1;
    tymed    := TYMED_HGLOBAL;
  end;

  //Carrega os ícones a partir do Resource File
  fLoadImage(NotesIcon,'NotesIcon');
  fLoadImage(OpenIcon,'OpenIcon');
  fLoadImage(WWWIcon,'WWWIcon');
  fLoadImage(CopyIcon,'CopyIcon');
  fLoadImage(FavIcon,'FavIcon');

  // Rotina de criação do BitMap para ser colocado no meio do menu...
  if AboutBitMap = nil then
  begin
    AboutBitMap := TBitmap.Create;
    AboutBitMap.Width := 165;
    AboutBitMap.Height := 15;
    AboutBitMap.Canvas.Brush.Color := clCream;
    AboutBitMap.Canvas.Pen.Color := clBlue;
    AboutBitMap.Canvas.Rectangle(0,0,165,15);
    AboutBitMap.Canvas.Font.Color := clNavy;
    AboutBitMap.Canvas.TextOut(7,1,'HomePage do Notes SE 2004');
  end;

  Result := lpdobj.GetData(FormatEtc, StgMedium);
  if Failed(Result) then
    Exit;
  // Os códigos que eu estudei permitem o tratamento de apenas um arquivo por vez.
  // Assim, caso só um arquivo esteja selecionado, ele passa o nome do mesmo para fFileName
  // Caso Contrário, ele nem chega a chamar o pop-up do Notes.
  if (DragQueryFile(StgMedium.hGlobal, $FFFFFFFF, nil, 0) = 1) then begin
    DragQueryFile(StgMedium.hGlobal, 0, fFileName, SizeOf(fFileName));
    Result := NOERROR;
  end
  else begin
    FFileName[0] := #0;
    Result := E_FAIL;
  end;
  ReleaseStgMedium(StgMedium);

end;


(*
Como essa função é chamada pelo explorer, não temos que nos preocupar em saber
qual a posição inicial, ou identificadores de comando. Apenas dar condições
dos valores passados serem trabalhados
@code(MENU) - Handle para o menu de contexto
@code(indexMenu) - posição do primeiro item a ser incluido no menu (zero based)
@code(idCmdFirst) - primeiro identificador de comando a ser utilizado
@code(idCmdLast) - identificador máximo a ser utilizado
@code(uFlags) - Flags opcionais
Retorna o número de menus criados
*)
function TContextMenu.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
          idCmdLast, uFlags: UINT): HResult;
var mySub: HMENU;
    idCmd: Cardinal;
begin
  Result := 0;

  if ((uFlags and $0000000F) = CMF_NORMAL) or ((uFlags and CMF_EXPLORE) <> 0) then
  begin
    idCmd := idCmdFirst; //inicializa o identificador de comando interno

    //Editar com o Notes - Posição inicial (IndexMenu)
    InsertMenu(Menu, indexMenu, MF_STRING or MF_BYPOSITION,idCmd, 'Editar com o Notes');
    if OpenIcon <> nil then
      SetMenuItemBitmaps(Menu,indexMenu,MF_BYPOSITION,OpenIcon.Handle,OpenIcon.Handle);


    mySub := CreatePopupMenu; //Cria o menu principal, que abriga os outros.
    // Posição: IndexMenu + 1
    InsertMenu(Menu, indexMenu+1, MF_STRING or MF_BYPOSITION or MF_POPUP, mySub, 'Notes SE 2004');
    if NotesIcon <> nil then
      SetMenuItemBitmaps(Menu,indexMenu+1,MF_BYPOSITION,NotesIcon.Handle,NotesIcon.Handle); //Coloca o ícone do notes ao lado do item

    //O que está dentro do mySub
    begin

      //Usar como Template do Notes
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_STRING or MF_BYPOSITION,idCmd, 'Usar como Template do Notes');
      if OpenIcon <> nil then
        SetMenuItemBitmaps(mySub,idCmd - idCmdFirst - 1,MF_BYPOSITION,OpenIcon.Handle,OpenIcon.Handle);


      //-----------------Separador----------------------------
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_SEPARATOR,0,nil);


      //Copiar conteúdo
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_STRING or MF_BYPOSITION,idCmd, 'Copiar conteúdo');
      if CopyIcon <> nil then
        SetMenuItemBitmaps(mySub,idCmd - idCmdFirst - 1,MF_BYPOSITION,CopyIcon.Handle,CopyIcon.Handle);

      //Copiar nome do arquivo
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_STRING or MF_BYPOSITION,idCmd, 'Copiar nome do arquivo');
      if CopyIcon <> nil then
        SetMenuItemBitmaps(mySub,idCmd - idCmdFirst - 1,MF_BYPOSITION,CopyIcon.Handle,CopyIcon.Handle);


      //Informações do arquivo
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_STRING or MF_BYPOSITION,idCmd, 'Informações do arquivo');


      //-----------------Separador----------------------------
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_SEPARATOR,0,nil);

      //Comprimir HTML
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_STRING or MF_BYPOSITION,idCmd, 'Comprimir HTML');

      //-----------------Separador----------------------------
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_SEPARATOR,0,nil);

      //Adicionar aos favoritos do Notes
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_STRING or MF_BYPOSITION,idCmd, 'Adicionar aos favoritos do Notes');
      if FavIcon <> nil then
        SetMenuItemBitmaps(mySub,idCmd - idCmdFirst - 1,MF_BYPOSITION,FavIcon.Handle,FavIcon.Handle);

      //-----------------Separador----------------------------
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_SEPARATOR,0,nil);

      //HomePage do Notes
      inc(idCmd);
      InsertMenu(mySub, idCmd - idCmdFirst - 1, MF_BITMAP or MF_BYPOSITION,idCmd, PChar(AboutBitMap.Handle));
      if WWWIcon <> nil then
        SetMenuItemBitmaps(mySub, idCmd - idCmdFirst - 1,MF_BYPOSITION,WWWIcon.Handle,WWWIcon.Handle);
    end;

    // o valor de retorno deve ser o número de menus que foram criados
    Result := idCmd - idCmdFirst + 1;
  end;
end;


(*
@code(lpici) Ponteiro para a estrutura de comandos do Shell, onde estará contido o ítem clicado
retorna EFAIL caso tenha havido algum problema, ou NOERROR caso esteja tudo certo
*)
function TContextMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult;
resourcestring
  sPathError = 'Erro ao definir diretório ativo!';

var
  PrevDir: string;
  TargetFile: string;
  cmdLine: String;
  aFileContent: TStringList;
begin
  Result := E_FAIL;
  // Certifica que é o Explorer que está acessando a DLL, e não um aplicativo qquer
  if (HiWord(Integer(lpici.lpVerb)) <> 0) then
  begin
    Exit;
  end;

  //Pega a localização da DLL e extrai o Path pro Notes
  GetModuleFileName(hInstance,fNotesPathBuf,Sizeof(fNotesPathBuf));
  fNotesPath := ExtractFilePath(fNotesPathBuf);

  TargetFile := fNotesPath + 'Notes.exe';
  if not FileExists(TargetFile) then
  begin
    //Chamada abaixo em API, pois tirar o Dialogs.dcu economizou quase 300kb!
    MessageBox(GetDesktopWindow,'Notes não instalado corretamente!' + #13 + 'Tente instalar novamente por favor.','Erro de Instalação',MB_OK or MB_ICONERROR);
    exit;
  end;

  cmdLine := '';

  case LoWord(Integer(lpici.lpVerb)) of
    0: // "Editar com o Notes"
      cmdLine := TargetFile + ' "' + fFileName + '"';



    1: // "Usar como Template do Notes"
      cmdLine := TargetFile + ' -t "' + fFileName + '"';
  //2: -----------------Separador----------------------------
    3: //Copiar conteúdo
    begin
      aFileContent := TStringList.Create;
      aFileContent.LoadFromFile(fFileName);
      SetClipboardText(PChar(aFileContent.Text));
      aFileContent.Free;
    end;

    4: //Copiar nome do arquivo
      SetClipboardText(fFileName);
    5: //Informações do arquivo
      cmdLine := TargetFile + ' -i "' + fFileName + '"';

  //6: -----------------Separador----------------------------
    7: //Comprimir HTML
    begin
      aFileContent := TStringList.Create;
      aFileContent.LoadFromFile(fFileName);
      aFileContent.Text := compHTML(aFileContent.Text);
      aFileContent.SaveToFile(fFileName);
      aFileContent.Free;
    end;

  //8:-----------------Separador----------------------------
    9://Adicionar aos favoritos do Notes
    begin
      //aproveita a variavel NotesExe, já existente, para guardar o nome do arquivo XML
      TargetFile := fNotesPath + 'fav.xml';
      aFileContent := TStringList.Create;
      if FileExists(TargetFile) then
      begin
        aFileContent.LoadFromFile(TargetFile);
        while trim(aFileContent[aFileContent.Count-1]) = '' do
          aFileContent.Delete(aFileContent.Count-1);
      end;
      aFileContent.Add(fFileName);
      aFileContent.SaveToFile(TargetFile);
    end;
  //10:-----------------Separador----------------------------
    11: // "HomePage do Notes"
      ShellExecute(GetDesktopWindow,nil,'https://github.com/jonasraoni/notes',nil,nil,SW_MAXIMIZE);
  end;

  if cmdLine <> '' then
  begin
    PrevDir := GetCurrentDir;
    try
      WinExec(PChar(cmdLine), lpici.nShow);
      Result := NOERROR;
    finally
      SetCurrentDir(PrevDir);
    end;
  end;
end;


(*
@code(idCmd) - deslocamento do Índice de comando: 0 primeiro ítem inserido, 1 para o seguinte...
@code(uType) - flags opcionais
@code(pwReserved) - parâmtro reservado ao sistema
@code(pszName) - Endereço do buffer onde deve ser colocado a string
@code(cchMax) - Tamanho do Buffer
retorna E_INVALIDARG caso não seja uma chamada para o texto da barra de status
Essa função descreve o texto que será exibido na barra de status do explorer
*)
function TContextMenu.GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
  pszName: LPSTR; cchMax: UINT): HRESULT;
begin
  Result := E_INVALIDARG;
  if (uType = GCS_HELPTEXT) then
  begin
    case idCmd of
      0: // "Editar com o Notes"
      begin
        StrCopy(pszName, PChar('Edita o arquivo "' + ExtractFileName(fFileName) + '" com o Notes SE 2004'));
        Result := NOERROR;
      end;

      1: // "Usar como Template do Notes"
      begin
        StrCopy(pszName, PChar('Usa o arquivo "' + ExtractFileName(fFileName) + '" como template com o Notes SE 2004'));
        Result := NOERROR;
      end;

    //2:-----------------Separador----------------------------

      3: // "Copiar Conteudo"
      begin
        StrCopy(pszName, PChar('Copia o conteúdo do arquivo "' + ExtractFileName(fFileName) + '" para a Área de Transferência'));
        Result := NOERROR;
      end;

      4: // "Copiar nome do arquivo"
      begin
        StrCopy(pszName, PChar('Copia o nome do arquivo (' + fFileName + ') para a Área de Transferência'));
        Result := NOERROR;
      end;

      5: // "Informações do Arquivo"
      begin
        StrCopy(pszName, PChar('Exibe informações do arquivo "' + ExtractFileName(fFileName) + '"'));
        Result := NOERROR;
      end;

    //6:-----------------Separador----------------------------

      7: // "Comprimir HTML"
      begin
        StrCopy(pszName, PChar('Comprime o arquivo "' + ExtractFileName(fFileName) + '" caso este contenha apenas código HTML'));
        Result := NOERROR;
      end;

    //8:-----------------Separador----------------------------

      9: // "Adicionar ao favoritos"
      begin
        StrCopy(pszName, PChar('Adiciona o arquivo "' + ExtractFileName(fFileName) + '" ao favoritos do Notes'));
        Result := NOERROR;
      end;

    //10:-----------------Separador----------------------------

      11: // "HomePage do Notes SE 2004"
      begin
        StrCopy(pszName, PChar('HomePage do Notes SE 2004'));
        Result := NOERROR;
      end;
    end;
  end;
end;


//------------------------------------------------------------------------------
//As funções e classes abaixo são responsáveis pelo registro da DLL no Windows
//------------------------------------------------------------------------------

type
  TContextMenuFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

procedure TContextMenuFactory.UpdateRegistry(Register: Boolean);
var
  ClassID: string;
begin
  //Registra o Menu de contexto
  if Register then begin
    inherited UpdateRegistry(Register);

    ClassID := GUIDToString(Class_ContextMenu);
    CreateRegKey('*\shellex', '', '');
    CreateRegKey('*\shellex\ContextMenuHandlers', '', '');
    CreateRegKey('*\shellex\ContextMenuHandlers\Notes', '', ClassID);

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions', True);
          OpenKey('Approved', True);
          WriteString(ClassID, 'Notes SE 2004 Shell Extension');
        finally
          Free;
        end;
  end
  else begin
    DeleteRegKey('*\shellex\ContextMenuHandlers\Notes');
    DeleteRegKey('*\shellex\ContextMenuHandlers');
    DeleteRegKey('*\shellex');

    inherited UpdateRegistry(Register);
  end;
end;

initialization
  TContextMenuFactory.Create(ComServer, TContextMenu, Class_ContextMenu,'', 'Notes SE 2004 Shell Extension', ciMultiInstance,tmApartment);
end.
