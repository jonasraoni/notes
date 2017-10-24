object frmFolderMenuEditor: TfrmFolderMenuEditor
  Left = 227
  Top = 118
  BorderStyle = bsDialog
  Caption = 'frmFolderMenuEditor'
  ClientHeight = 277
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 269
    Top = 83
    Width = 129
    Height = 6
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 269
    Top = 189
    Width = 129
    Height = 6
    Shape = bsTopLine
  end
  object Bevel3: TBevel
    Left = 269
    Top = 231
    Width = 129
    Height = 6
    Shape = bsTopLine
  end
  object tvItems: TTreeView
    Left = 8
    Top = 9
    Width = 250
    Height = 258
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
  end
  object btAdd: TButton
    Left = 270
    Top = 17
    Width = 128
    Height = 25
    Caption = '&Adicionar...'
    TabOrder = 1
    OnClick = btAddClick
  end
  object btAddFolder: TButton
    Left = 270
    Top = 49
    Width = 128
    Height = 25
    Caption = 'Adicionar &categoria...'
    TabOrder = 2
    OnClick = btAddFolderClick
  end
  object btEdit: TButton
    Left = 270
    Top = 93
    Width = 128
    Height = 25
    Caption = '&Editar...'
    TabOrder = 3
    OnClick = btEditClick
  end
  object btShortcut: TButton
    Left = 270
    Top = 125
    Width = 128
    Height = 25
    Caption = '&Tecla de atalho...'
    TabOrder = 4
    OnClick = btShortcutClick
  end
  object btDel: TButton
    Left = 270
    Top = 198
    Width = 128
    Height = 25
    Caption = '&Deletar...'
    TabOrder = 6
    OnClick = btDelClick
  end
  object btClose: TButton
    Left = 270
    Top = 241
    Width = 128
    Height = 25
    Cancel = True
    Caption = '&Fechar'
    TabOrder = 7
    OnClick = btCloseClick
  end
  object btRename: TButton
    Left = 270
    Top = 157
    Width = 128
    Height = 25
    Caption = '&Renomear...'
    TabOrder = 5
    OnClick = btRenameClick
  end
end
