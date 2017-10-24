object frmNewFile: TfrmNewFile
  Left = 136
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Novo Arquivo...'
  ClientHeight = 353
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 6
    Top = 314
    Width = 513
    Height = 9
    Shape = bsTopLine
  end
  object lCategory: TLabel
    Left = 10
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Cat&egoria:'
    FocusControl = tvCat
  end
  object lTemplates: TLabel
    Left = 218
    Top = 8
    Width = 53
    Height = 13
    Caption = '&Templates:'
    FocusControl = lbTemplates
  end
  object lbTemplates: TListBox
    Left = 216
    Top = 25
    Width = 300
    Height = 237
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 1
    OnClick = lbTemplatesClick
    OnDblClick = lbTemplatesDblClick
  end
  object meHelp: TMemo
    Left = 216
    Top = 271
    Width = 300
    Height = 34
    Cursor = crArrow
    Color = clBtnFace
    ParentShowHint = False
    ReadOnly = True
    ShowHint = False
    TabOrder = 2
  end
  object btOK: TButton
    Left = 305
    Top = 323
    Width = 97
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 3
    OnClick = btOKClick
  end
  object btCancel: TButton
    Left = 417
    Top = 323
    Width = 97
    Height = 25
    Cancel = True
    Caption = '&Cancelar'
    ModalResult = 2
    TabOrder = 4
  end
  object tvCat: TTreeView
    Left = 8
    Top = 25
    Width = 200
    Height = 280
    HideSelection = False
    Indent = 19
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    SortType = stText
    TabOrder = 0
    OnChange = tvCatChange
  end
end
