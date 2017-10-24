object frmListEditor: TfrmListEditor
  Left = 209
  Top = 100
  BorderStyle = bsDialog
  Caption = 'frmListEditor'
  ClientHeight = 325
  ClientWidth = 367
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
  object lDescr: TLabel
    Left = 8
    Top = 8
    Width = 30
    Height = 13
    Caption = 'lDescr'
    WordWrap = True
  end
  object bv1: TBevel
    Left = 256
    Top = 163
    Width = 105
    Height = 9
    Shape = bsTopLine
  end
  object bv2: TBevel
    Left = 256
    Top = 198
    Width = 105
    Height = 9
    Shape = bsBottomLine
  end
  object bv3: TBevel
    Left = 8
    Top = 286
    Width = 353
    Height = 4
    Shape = bsBottomLine
  end
  object lbStrs: TListBox
    Left = 8
    Top = 58
    Width = 241
    Height = 185
    ItemHeight = 13
    TabOrder = 0
  end
  object btOK: TButton
    Left = 194
    Top = 296
    Width = 81
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 7
  end
  object btCancel: TButton
    Left = 284
    Top = 296
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancelar'
    ModalResult = 2
    TabOrder = 8
  end
  object btAddFile: TButton
    Left = 256
    Top = 64
    Width = 105
    Height = 25
    Caption = 'Ad&icionar Arquivo'
    TabOrder = 1
    OnClick = btAddFileClick
  end
  object btAddFolder: TButton
    Left = 256
    Top = 96
    Width = 105
    Height = 25
    Caption = 'Adicionar &Pasta'
    TabOrder = 2
    OnClick = btAddFolderClick
  end
  object btAdd: TButton
    Left = 256
    Top = 128
    Width = 105
    Height = 25
    Caption = '&Adicionar'
    TabOrder = 3
    OnClick = btAddClick
  end
  object btEdit: TButton
    Left = 256
    Top = 172
    Width = 105
    Height = 25
    Caption = '&Editar'
    TabOrder = 4
    OnClick = btEditClick
  end
  object btDel: TButton
    Left = 256
    Top = 214
    Width = 105
    Height = 25
    Caption = '&Deletar'
    TabOrder = 5
    OnClick = btDelClick
  end
  object gbOpts: TPanel
    Left = 4
    Top = 256
    Width = 353
    Height = 25
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 6
    object Label1: TLabel
      Left = 5
      Top = 4
      Width = 196
      Height = 13
      Caption = 'Delimitador usado para separar as strings:'
      Transparent = False
    end
    object edDelimiter: TEdit
      Left = 210
      Top = 0
      Width = 17
      Height = 21
      MaxLength = 1
      TabOrder = 0
      Text = ';'
      OnChange = edDelimiterChange
    end
  end
end
