object frmConfigTemplates: TfrmConfigTemplates
  Left = 162
  Top = 66
  BorderStyle = bsDialog
  Caption = 'Configura'#231#227'o dos templates para novos arquivos...'
  ClientHeight = 431
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbCategorys: TGroupBox
    Left = 8
    Top = 8
    Width = 464
    Height = 169
    Caption = '&Linguagens e categorias'
    TabOrder = 0
    object Bevel4: TBevel
      Left = 324
      Top = 83
      Width = 121
      Height = 9
      Shape = bsBottomLine
    end
    object Bevel5: TBevel
      Left = 325
      Top = 42
      Width = 121
      Height = 9
      Shape = bsBottomLine
    end
    object tvCat: TTreeView
      Left = 8
      Top = 16
      Width = 309
      Height = 143
      HideSelection = False
      Indent = 19
      TabOrder = 0
      OnChange = tvCatChange
    end
    object btNewCat: TButton
      Left = 326
      Top = 18
      Width = 121
      Height = 25
      Caption = 'Nova &Categoria'
      TabOrder = 1
      OnClick = btNewCatClick
    end
    object btDelCat: TButton
      Left = 326
      Top = 99
      Width = 121
      Height = 25
      Caption = 'Deletar Cate&goria'
      TabOrder = 3
      OnClick = btDelCatClick
    end
    object btRenameCat: TButton
      Left = 326
      Top = 58
      Width = 121
      Height = 25
      Caption = 'Renome&ar categoria'
      TabOrder = 2
      OnClick = btRenameCatClick
    end
  end
  object gbTemplates: TGroupBox
    Left = 8
    Top = 183
    Width = 464
    Height = 208
    Caption = '&Templates'
    TabOrder = 1
    object Bevel1: TBevel
      Left = 329
      Top = 43
      Width = 121
      Height = 9
      Shape = bsBottomLine
    end
    object Bevel2: TBevel
      Left = 329
      Top = 155
      Width = 121
      Height = 9
      Shape = bsBottomLine
    end
    object Bevel3: TBevel
      Left = 331
      Top = 97
      Width = 120
      Height = 9
      Shape = bsBottomLine
    end
    object lbTemplates: TListBox
      Left = 8
      Top = 16
      Width = 312
      Height = 181
      ItemHeight = 13
      TabOrder = 0
    end
    object btNew: TButton
      Left = 329
      Top = 19
      Width = 121
      Height = 25
      Caption = '&Novo Template'
      TabOrder = 1
      OnClick = btNewClick
    end
    object btEdit: TButton
      Left = 329
      Top = 61
      Width = 121
      Height = 25
      Caption = '&Editar'
      ModalResult = 1
      TabOrder = 2
      OnClick = btEditClick
    end
    object btEditDescr: TButton
      Left = 330
      Top = 95
      Width = 121
      Height = 25
      Caption = 'De&scri'#231#227'o'
      TabOrder = 3
      OnClick = btEditDescrClick
    end
    object btDelTemplate: TButton
      Left = 330
      Top = 173
      Width = 121
      Height = 25
      Caption = '&Deletar'
      TabOrder = 5
      OnClick = btDelTemplateClick
    end
    object btRename: TButton
      Left = 330
      Top = 129
      Width = 121
      Height = 25
      Caption = '&Renomear'
      TabOrder = 4
      OnClick = btRenameClick
    end
  end
  object btClose: TButton
    Left = 339
    Top = 400
    Width = 121
    Height = 25
    Cancel = True
    Caption = '&Fechar'
    ModalResult = 1
    TabOrder = 2
  end
end
