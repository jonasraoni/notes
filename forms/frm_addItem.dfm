object frmAddLibItem: TfrmAddLibItem
  Left = 143
  Top = 121
  BorderStyle = bsDialog
  Caption = 'Adicionar Item'
  ClientHeight = 308
  ClientWidth = 483
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
  object lCode: TLabel
    Left = 8
    Top = 52
    Width = 36
    Height = 13
    Caption = 'C'#243'digo:'
    FocusControl = meCode
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lHelp: TLabel
    Left = 8
    Top = 220
    Width = 30
    Height = 13
    Caption = 'Ajuda:'
    FocusControl = meHelp
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lName: TLabel
    Left = 8
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Nome:'
    FocusControl = edName
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object meCode: TMemo
    Left = 56
    Top = 48
    Width = 417
    Height = 153
    ScrollBars = ssBoth
    TabOrder = 1
    WantTabs = True
    WordWrap = False
  end
  object meHelp: TMemo
    Left = 56
    Top = 216
    Width = 417
    Height = 49
    ScrollBars = ssVertical
    TabOrder = 2
    WantTabs = True
  end
  object edName: TEdit
    Left = 56
    Top = 12
    Width = 417
    Height = 21
    TabOrder = 0
  end
  object btOK: TButton
    Left = 256
    Top = 274
    Width = 97
    Height = 25
    Caption = '&OK'
    TabOrder = 3
    OnClick = btOKClick
  end
  object btCancel: TButton
    Left = 368
    Top = 274
    Width = 105
    Height = 25
    Cancel = True
    Caption = '&Cancelar'
    Default = True
    ModalResult = 2
    TabOrder = 4
  end
end
