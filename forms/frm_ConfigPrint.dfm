object frmConfigPrint: TfrmConfigPrint
  Left = 232
  Top = 170
  BorderStyle = bsDialog
  ClientHeight = 245
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 208
    Top = 213
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancelar: TButton
    Left = 288
    Top = 213
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    TabOrder = 1
    OnClick = btnCancelarClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 168
    Height = 97
    Caption = 'Incluir:'
    TabOrder = 2
    object chkCabecalho: TCheckBox
      Left = 8
      Top = 24
      Width = 140
      Height = 17
      Caption = 'Nome do arquivo'
      TabOrder = 0
    end
    object chkNumeroPagina: TCheckBox
      Left = 8
      Top = 48
      Width = 140
      Height = 17
      Caption = 'N'#250'mero da p'#225'gina'
      TabOrder = 1
    end
    object chkNumeroLinha: TCheckBox
      Left = 8
      Top = 72
      Width = 140
      Height = 17
      Caption = 'N'#250'mero de cada linha'
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 184
    Top = 8
    Width = 177
    Height = 97
    Caption = 'Op'#231#245'es:'
    TabOrder = 3
    object chkQuebrarLinha: TCheckBox
      Left = 8
      Top = 24
      Width = 150
      Height = 17
      Caption = 'Quebrar Linhas'
      TabOrder = 0
    end
    object chkCores: TCheckBox
      Left = 8
      Top = 72
      Width = 150
      Height = 17
      Caption = 'Impress'#227'o colorida'
      TabOrder = 1
    end
    object chkSyntax: TCheckBox
      Left = 8
      Top = 48
      Width = 150
      Height = 17
      Caption = 'Usar colora'#231#227'o de syntaxe'
      TabOrder = 2
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 120
    Width = 353
    Height = 81
    Caption = 'Configura'#231#227'o da impress'#227'o:'
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 273
      Height = 13
      Caption = 'Configura'#231#245'es relativas a papel, orienta'#231#227'o e impressora'
    end
    object btnConfigurar: TButton
      Left = 192
      Top = 48
      Width = 145
      Height = 25
      Caption = '&Configurar Impress'#227'o...'
      TabOrder = 0
      OnClick = btnConfigurarClick
    end
  end
end
