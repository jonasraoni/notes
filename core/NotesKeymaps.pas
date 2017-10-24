//
//    NotesKeymaps - classes para lidar com os Keymaps do Notes
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
@abstract(NotesKeymaps - classes para lidar com os Keymaps do Notes.)
@author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesKeymaps;

interface

uses Windows, SysUtils, NotesGlobals, Classes,
  ActnList, Contnrs;

type
  // Assinatura de uma ação. É composta
  // pela tag da ação mais um hash do nome
  // da ação.
  TNotesKeymapActionSignature = record
    tag: integer;
    hash: integer;
  end;

type
  { Classe que contém um item de um Keymap. Um Keymap
    é uma lista de ações com shortcuts associdas a ela.
    Cada ação tem um TNotesKeyMapItem. }
  TNotesKeyMapItem = class(TObject)
  private
    fShortcut: TShortCut;
    fExtraShortCuts: TStrings;
    fHasPrimaryShortcut: boolean;
    fSignature: TNotesKeymapActionSignature;
    function getCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Seta as propriedades de uma ação de acordo com as proriedades da classe
    procedure AssignToAction(Action: TAction);
    // Carrega as propriedades de uma ação
    procedure AssignFromAction(Action: TAction);
    // Carrega as prorpeidades de um outro item
    procedure AssignFromItem(Item: TNotesKeyMapItem);
    // Preenche os items de um TStrings com as shortcuts
    procedure AssignToTStrings(obj: TStrings);
    // Carrega as shortcuts de uma instância de TStrings
    procedure AssignFromTStrings(obj: TStrings);
    { Adiciona uma shortcut. A primeira shortcut adicionada é
      a primária. As outras vão para a lista de extras. }
    procedure addShortcut(Shortcut: TShortcut);
    { Pega a shortcut por index. Passando 0 no index, retorna
      a Shortcut primária. Depois de 0, retorna as shortcuts
      da lista extra. }
    function getShortcut(index: integer): TShortcut;
    { Associa o item a ação passada. Através da ação passada,
      uma TNotesKeymapActionSignature é calcluda e armazenada. Para
      saber se uma ação está associada a este item, use a função
      IsAssocToAction. }
    procedure setActionAssoc(Action: TAction);
    { Retorna true se o item estiver associado a TAction passada. }
    function isAssocToAction(Action: TAction): boolean;
    { Deleta todas as shortcuts }
    procedure Clear;
    { Permite ler/setar a assinatura. }
    property ActionSignature: TNotesKeymapActionSignature read fSignature write fSignature;
    { Retorna se a shortcut primária está setada. }
    property HasPrimaryShortcut: boolean read fHasPrimaryShortcut;
    { Número total de shortcuts }
    property Count: integer read getCount;
    { Shortcut primária - é que aparecerá nos menus }
    property PrimaryShortCut: TShortcut read fShortcut write fShortcut;
    { Shortcuts extras }
    property ExtraShortCuts: TStrings read fExtraShortCuts;
  end;

{    ----------------------------
     "NOTESKEYMAP" (ASSINATURA)
    ----------------------------
     Contagem de KeymapItems
    ----------------------------

    n X ---------------------------
          Tag + Hash
        ---------------------------
          Count
        ---------------------------
          n X --------------------
               TShortcut
              --------------------
        ---------------------------
    ----------------------------

--> n X quer dizer que a sessão repete
    inúmeras vezes
 }

type
  { Classe que permite manipular Keymaps do Notes. Um arquivo
    de Keymap tem a extenção NKM (Notes KeyMap) e pode ser
    carregado usando LoadKeymap. O NKM é um arquivo binário,
    para editá-lo use os diálogos do Notes. Para usar o
    Keymap carregado em uma lista de ações, chame AssignToActionList. }
  TNotesKeyMap= class(TObjectList)
  private
    function getKmi(Index: integer): TNotesKeymapItem;
    procedure setKmi(Index: integer; const Value: TNotesKeymapItem);
  public
    constructor Create;
    // Adiciona um TNotesKeymapItem a lista
    function AddKeymapItem(Item: TNotesKeymapItem): integer;
    // Carrega um keymap de um arquivo NKM
    procedure LoadKeymap(FileName: string);
    // Salva o keymap
    procedure SaveKeymap(FileName: string);
    // Seta as shotcuts da TActionList passada de acordo com o Keymap atual
    procedure AssignToActionList(ActionList: TActionList);
    // Deleta todas as shortcuts de uma ActionList
    procedure ClearActionListShortcuts(ActionList: TActionList);
    // Deleta as shortcuts relacionadas a edição
    procedure ClearActionListEditingShortcuts(ActionList: TActionList);
    // Retorna o Item que contém a tag passada
    function getItemByAction(Action: TAction): TNotesKeymapItem;
    // Permite manipular os items da lista
    property KeymapItems[Index: integer]: TNotesKeymapItem read getKmi write setKmi;
  end;


implementation

uses Menus, NotesUtils;

const
  NOTESKEYMAP_SIGNLEN= 12;
  NOTESKEYMAP_SIGNATURE: string[NOTESKEYMAP_SIGNLEN]= 'NOTES KEYMAP';

{ TNotesKeyMapItem }

procedure TNotesKeyMapItem.addShortcut(Shortcut: TShortcut);
begin
  if fHasPrimaryShortcut then
    fExtraShortCuts.Add(ShortcutToText(Shortcut))
  else
    fShortCut:= ShortCut;

  fHasPrimaryShortcut:= true;
end;

procedure TNotesKeyMapItem.AssignFromAction(Action: TAction);
begin
  fShortCut:= Action.ShortCut;
  fExtraShortCuts.Clear;
  fExtraShortCuts.Assign(Action.SecondaryShortCuts);
  fHasPrimaryShortcut:= (ShortcutToText(fShortcut) <> '');
end;

procedure TNotesKeyMapItem.AssignFromItem(Item: TNotesKeyMapItem);
begin
  fShortcut:= item.PrimaryShortCut;
  fHasPrimaryShortcut:= item.HasPrimaryShortcut;
  fExtraShortCuts.Clear;
  fExtraShortCuts.Assign(Item.ExtraShortCuts);
end;

procedure TNotesKeyMapItem.AssignFromTStrings(obj: TStrings);
Var
  I: integer;
begin
  Clear;
  if obj.Count = 0 then Exit;

  if obj.Count = 1 then
  begin
    fShortcut:= TextToShortcut(obj.Strings[0]);
    fHasPrimaryShortcut:= true;
  end else
  begin
    fShortcut:= TextToShortcut(obj.Strings[0]);
    fHasPrimaryShortcut:= true;
    fExtraShortCuts.BeginUpdate;
    for I:= 1 to obj.Count -1 do
      fExtraShortCuts.Add(obj.Strings[I]);
    fExtraShortCuts.EndUpdate;
  end;
end;

procedure TNotesKeyMapItem.AssignToAction(Action: TAction);
Var
  I: integer;
begin
  Action.ShortCut:= fShortCut;
  Action.SecondaryShortCuts.Clear;
  Action.SecondaryShortCuts.BeginUpdate;
  for I:= 0 to fExtraShortCuts.Count - 1 do
    Action.SecondaryShortCuts.Add(fExtraShortCuts.Strings[I]);
  Action.SecondaryShortCuts.EndUpdate;
end;

procedure TNotesKeyMapItem.AssignToTStrings(obj: TStrings);
Var
  I: integer;
begin
  obj.Clear;
  if not fHasPrimaryShortcut then Exit;
  obj.BeginUpdate;
  obj.Add(ShortcutToText(fShortCut));
  for I:= 0 to fExtraShortCuts.Count - 1 do
    obj.Add(fExtraShortCuts.Strings[I]);
  obj.EndUpdate;
end;

procedure TNotesKeyMapItem.Clear;
begin
  fHasPrimaryShortcut:= false;
  fShortCut:= 0;
  fExtraShortCuts.Clear;
end;

constructor TNotesKeyMapItem.Create;
begin
  fHasPrimaryShortcut:= false;
  fShortCut:= 0;
  fExtraShortCuts:= TStringList.Create;
end;

destructor TNotesKeyMapItem.Destroy;
begin
  Clear;
  fExtraShortCuts.Free;
  inherited;
end;

function TNotesKeyMapItem.getCount: integer;
begin
  Result:= 0;
  if not fHasPrimaryShortcut then Exit;

  Result:= 1 + fExtraShortCuts.Count;

end;

function TNotesKeyMapItem.getShortcut(index: integer): TShortcut;
begin
  Result:= 0;
  if not fHasPrimaryShortcut then Exit;
  if index = 0 then
    Result:= fShortcut
  else
    Result:= TextToShortcut(fExtraShortcuts.Strings[index-1]);
end;

function TNotesKeyMapItem.isAssocToAction(Action: TAction): boolean;
begin
  Result:= false;
  if (Action.Tag = fSignature.tag) and (strHash(Action.Name) = fSignature.hash) then
    Result:= true;
end;

procedure TNotesKeyMapItem.setActionAssoc(Action: TAction);
begin
  fSignature.tag:= Action.Tag;
  fSignature.hash:= strHash(Action.Name);
end;

{ TNotesKeyMap }

function TNotesKeyMap.AddKeymapItem(Item: TNotesKeymapItem): integer;
begin
  Result:= self.Add(Item);
end;

procedure TNotesKeyMap.AssignToActionList(ActionList: TActionList);
Var
  I: integer;
  K: TNotesKeymapItem;
begin
  for I:= 0 to ActionList.ActionCount -1 do
  begin
    if ActionList.Actions[I] is TAction then
    begin
      K:= getItemByAction(ActionList.Actions[I] As TAction);
      if K <> nil then
        K.AssignToAction(ActionList.Actions[I] As TAction);
    end;
  end;
end;

procedure TNotesKeyMap.ClearActionListEditingShortcuts(
  ActionList: TActionList);
Var
  I: integer;
begin
  for I:= 0 to ActionList.ActionCount -1 do
  begin
    if ActionList.Actions[I] is TAction then
    begin
      if ((ActionList.Actions[I] As TAction).Category = 'Edit')
        or ((ActionList.Actions[I] As TAction).Category = 'Editor') then
      begin
        (ActionList.Actions[I] As TAction).ShortCut:= 0;
        (ActionList.Actions[I] As TAction).SecondaryShortCuts.Clear;
      end;
    end;
  end;
end;

procedure TNotesKeyMap.ClearActionListShortcuts(ActionList: TActionList);
Var
  I: integer;
begin
  for I:= 0 to ActionList.ActionCount -1 do
  begin
    if ActionList.Actions[I] is TAction then
    begin
      (ActionList.Actions[I] As TAction).ShortCut:= 0;
      (ActionList.Actions[I] As TAction).SecondaryShortCuts.Clear;
    end;
  end;
end;

constructor TNotesKeyMap.Create;
begin
  inherited;
  self.OwnsObjects:= true;
end;

function TNotesKeyMap.getItemByAction(Action: TAction): TNotesKeymapItem;
Var
  I: integer;
begin
  Result:= nil;
  for I:= 0 to self.Count -1 do
    if TNotesKeymapItem(self.Items[I]).isAssocToAction(Action) then
    begin
      Result:= TNotesKeymapItem(self.Items[I]);
      Break;
    end;
end;

function TNotesKeyMap.getKmi(Index: integer): TNotesKeymapItem;
begin
  Result:= TNotesKeymapItem(self.Items[Index]);
end;

procedure TNotesKeyMap.LoadKeymap(FileName: string);
var
  fs: TFileStream;
  S: string[NOTESKEYMAP_SIGNLEN];
  I, J, N: integer;
  K: TNotesKeymapItem;
  sc: TShortCut;
  IntBuf: integer;
  sig: TNotesKeymapActionSignature;
begin
  if not FileExists(FileName) then
    raise Exception.Create(ClassName+'.LoadKeymap: the file "'+FileName+'" could not be found.');
  self.Clear;
  fs:= TFileStream.Create( FileName, fmOpenRead );
  try
    // Assinatura
    fs.Read(S, sizeOf(S));
    if S <> NOTESKEYMAP_SIGNATURE then
      raise Exception.Create(ClassName+'.LoadKeymap: the file "'+FileName+'" is not a valid Notes Keymap.');
    // Número de keymaps
    fs.Read(N, sizeOf(integer));

    for I:= 0 to N-1 do
    begin
      K:= TNotesKeymapItem.Create;
      try
        fs.Read(IntBuf, sizeOf(integer));
        sig.tag:= IntBuf;
        fs.Read(IntBuf, sizeOf(integer));
        sig.hash:= IntBuf;
        K.ActionSignature:= sig;
        // Lemos o número de shortcuts em C
        fs.Read(IntBuf, sizeOf(integer));
        for J:= 0 to IntBuf-1 do
        begin
          // Lemos C shortcuts do arquivo
          fs.Read(sc, sizeOf(TShortcut));
          K.addShortcut(sc);
        end;
      finally
        // Adicionamos o keymap a lista
        // É importante que o item seja mesmo adicionado a lista
        // para que a lista destrua ele depois
        self.AddKeymapItem(K);
      end;
    end;

  finally
    fs.Free;
  end;
end;

procedure TNotesKeyMap.SaveKeymap(FileName: string);
Var
  S: string[NOTESKEYMAP_SIGNLEN];
  fs: TFileStream;
  I, J: integer;
  sc: TShortCut;
  N: integer;
  CountPos: int64;
  RealCount: integer;
begin
  // fmShareExclusive para evitar que alguém bagunce o formato do arquivo
  fs:= TFileStream.Create( FileName, fmCreate, fmShareExclusive	);
  try
    S:= NOTESKEYMAP_SIGNATURE;
    fs.Write(S, sizeOf(S));
    CountPos:= fs.Position;
    fs.Write(self.Count, sizeOf(integer));
    RealCount:= 0;
    for I:= 0 to self.Count - 1 do
    begin
      { Só adicionamos um item do keymap se ele
       tiver shortcuts. Assim diminuímos o tamanho do
       arquivo do keymap e fazemos o keymap ser carregado
       mais rápido depois. }
      if self.KeymapItems[I].Count > 0 then
      begin
        N:= self.KeymapItems[I].ActionSignature.tag;
        fs.Write( N, sizeOf(integer));
        N:= self.KeymapItems[I].ActionSignature.hash;
        fs.Write( N, sizeOf(integer));
        N:= self.KeymapItems[I].Count;
        fs.Write( N, sizeOf(integer));
        for J:= 0 to self.KeymapItems[I].Count - 1 do
        begin
          sc:= self.KeymapItems[I].getShortcut(J);
          fs.Write( sc, sizeOf(TShortcut) );
        end;
        inc(RealCount);
      end;
    end;
    // gravamos o número de items que foram realmente salvos
    fs.Seek(CountPos, soBeginning);
    fs.Write(RealCount, sizeOf(integer));
  finally
    fs.Free;
  end;
end;

procedure TNotesKeyMap.setKmi(Index: integer;
  const Value: TNotesKeymapItem);
begin
  self.Items[Index]:= addr(Value);
end;

end.
