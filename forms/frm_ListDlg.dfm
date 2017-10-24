object frmList: TfrmList
  Left = 177
  Top = 140
  BorderStyle = bsDialog
  Caption = 'frmList'
  ClientHeight = 330
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object paBottom: TPanel
    Left = 0
    Top = 289
    Width = 474
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Bevel1: TBevel
      Left = 6
      Top = 4
      Width = 465
      Height = 2
      Shape = bsBottomLine
    end
    object btOk: TButton
      Left = 289
      Top = 12
      Width = 83
      Height = 25
      Caption = '&OK'
      Default = True
      TabOrder = 0
      OnClick = btOkClick
    end
    object btCancel: TButton
      Left = 381
      Top = 12
      Width = 84
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object paCenter: TPanel
    Left = 0
    Top = 29
    Width = 474
    Height = 260
    Align = alClient
    BevelOuter = bvNone
    Caption = 'paCenter'
    TabOrder = 1
    DesignSize = (
      474
      260)
    object lbItems: TListBox
      Left = 8
      Top = 8
      Width = 459
      Height = 250
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 474
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object edSel: TEdit
      Left = 8
      Top = 8
      Width = 459
      Height = 21
      AutoSelect = False
      AutoSize = False
      HideSelection = False
      TabOrder = 0
      OnChange = edSelChange
    end
  end
end
