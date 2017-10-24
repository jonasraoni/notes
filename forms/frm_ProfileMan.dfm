object frmProfileManager: TfrmProfileManager
  Left = 265
  Top = 171
  BorderStyle = bsDialog
  Caption = 'Configura'#231#227'o dos Profiles...'
  ClientHeight = 219
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lActiveProfile: TLabel
    Left = 8
    Top = 8
    Width = 62
    Height = 13
    Caption = 'Profile Ativo:'
  end
  object Bevel1: TBevel
    Left = 221
    Top = 123
    Width = 89
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel2: TBevel
    Left = 219
    Top = 168
    Width = 89
    Height = 9
    Shape = bsBottomLine
  end
  object lActiveProfileName: TLabel
    Left = 78
    Top = 8
    Width = 107
    Height = 13
    Caption = 'lActiveProfileName'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lProfileDir: TLabel
    Left = 8
    Top = 31
    Width = 45
    Height = 13
    Caption = 'Diret'#243'rio:'
  end
  object lbProfiles: TListBox
    Left = 8
    Top = 62
    Width = 202
    Height = 148
    ItemHeight = 13
    TabOrder = 0
  end
  object btNew: TButton
    Left = 221
    Top = 64
    Width = 89
    Height = 25
    Caption = '&Novo Profile'
    TabOrder = 1
    OnClick = btNewClick
  end
  object btSetActive: TButton
    Left = 221
    Top = 96
    Width = 89
    Height = 25
    Caption = '&Ativar'
    TabOrder = 2
    OnClick = btSetActiveClick
  end
  object btClose: TButton
    Left = 221
    Top = 184
    Width = 89
    Height = 25
    Cancel = True
    Caption = '&Fechar'
    TabOrder = 4
    OnClick = btCloseClick
  end
  object btDel: TButton
    Left = 221
    Top = 140
    Width = 89
    Height = 25
    Caption = '&Deletar'
    TabOrder = 3
    OnClick = btDelClick
  end
  object edProfileDir: TEdit
    Left = 78
    Top = 28
    Width = 226
    Height = 17
    AutoSize = False
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
    Text = 'edProfileDir'
  end
end
