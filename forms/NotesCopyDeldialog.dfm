object frmCopyDel: TfrmCopyDel
  Left = 227
  Top = 235
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'CopyDelDialog'
  ClientHeight = 114
  ClientWidth = 410
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
  object lDescr: TLabel
    Left = 8
    Top = 8
    Width = 385
    Height = 33
    AutoSize = False
    Caption = 'lDescr'
  end
  object lFile: TLabel
    Left = 8
    Top = 56
    Width = 385
    Height = 13
    AutoSize = False
    Caption = 'Preparando...'
  end
  object Gauge: TProgressBar
    Left = 8
    Top = 78
    Width = 305
    Height = 20
    Smooth = True
    Step = 2
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 322
    Top = 75
    Width = 78
    Height = 25
    Caption = '&Cancelar'
    TabOrder = 1
    OnClick = btCancelClick
  end
end
