object frmShortcuts: TfrmShortcuts
  Left = 182
  Top = 77
  BorderStyle = bsDialog
  Caption = 'Configura'#231#227'o das teclas de atalho'
  ClientHeight = 431
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object gbKeymaps: TGroupBox
    Left = 8
    Top = 342
    Width = 449
    Height = 52
    Caption = 'Esquemas Pr'#233'-definidos '
    TabOrder = 1
    object lbKeymaps: TLabel
      Left = 8
      Top = 24
      Width = 52
      Height = 13
      Caption = 'Es&quemas:'
    end
    object cbKeymaps: TComboBox
      Left = 72
      Top = 20
      Width = 209
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '(Escolha um esquema)'
      OnChange = cbKeymapsChange
      Items.Strings = (
        '(Escolha um esquema)')
    end
    object btSaveKeymap: TButton
      Left = 294
      Top = 18
      Width = 143
      Height = 25
      Caption = 'Salvar &esquema atual'
      TabOrder = 1
      OnClick = btSaveKeymapClick
    end
  end
  object gbCurrentKeymap: TGroupBox
    Left = 8
    Top = 8
    Width = 450
    Height = 329
    Caption = 'Esquema de teclas de atalho atual '
    TabOrder = 0
    object lbCategory: TLabel
      Left = 8
      Top = 16
      Width = 48
      Height = 13
      Caption = 'Cate&goria:'
      FocusControl = lsCategorys
    end
    object lbActions: TLabel
      Left = 120
      Top = 16
      Width = 33
      Height = 13
      Caption = 'A'#231#245'e&s:'
      FocusControl = lvActions
    end
    object lbActionShortcuts: TLabel
      Left = 120
      Top = 230
      Width = 82
      Height = 13
      Caption = '&Teclas de atalho:'
    end
    object lsCategorys: TListBox
      Left = 8
      Top = 32
      Width = 105
      Height = 288
      ItemHeight = 13
      Items.Strings = (
        'Arquivo'
        'Editar'
        'Localizar'
        'Ver'
        'Projeto'
        'Executar'
        'Macros'
        'Commandos'
        'Op'#231#245'es'
        'Ajuda'
        'Comandos do Editor')
      TabOrder = 0
      OnClick = lsCategorysClick
    end
    object lvActions: TListView
      Left = 120
      Top = 32
      Width = 319
      Height = 193
      Columns = <
        item
          Caption = 'A'#231#227'o'
          MinWidth = 50
          Width = 150
        end
        item
          Caption = 'Descri'#231#227'o'
          MinWidth = 60
          Width = 250
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnSelectItem = lvActionsSelectItem
    end
    object lsShortcuts: TListBox
      Left = 120
      Top = 248
      Width = 184
      Height = 70
      ItemHeight = 13
      TabOrder = 2
    end
    object btAddShortcut: TButton
      Left = 313
      Top = 233
      Width = 120
      Height = 25
      Caption = '&Adicionar'
      TabOrder = 3
      OnClick = btAddShortcutClick
    end
    object btEditShortCut: TButton
      Left = 313
      Top = 264
      Width = 120
      Height = 25
      Caption = '&Editar'
      TabOrder = 4
      OnClick = btEditShortCutClick
    end
    object btRemoveShortcut: TButton
      Left = 313
      Top = 295
      Width = 121
      Height = 25
      Caption = '&Remover'
      TabOrder = 5
      OnClick = btRemoveShortcutClick
    end
  end
  object btOk: TButton
    Left = 250
    Top = 402
    Width = 97
    Height = 25
    Caption = '&OK'
    TabOrder = 2
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 360
    Top = 402
    Width = 89
    Height = 25
    Cancel = True
    Caption = '&Cancelar'
    ModalResult = 2
    TabOrder = 3
  end
end
