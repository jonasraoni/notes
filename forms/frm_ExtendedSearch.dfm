object frmExtSearch: TfrmExtSearch
  Left = 130
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Localizar/Substituir Extendido...'
  ClientHeight = 409
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 45
    Height = 13
    Caption = 'Localizar:'
  end
  object lbReplace: TLabel
    Left = 12
    Top = 100
    Width = 46
    Height = 13
    Caption = 'Substituir:'
  end
  object btSearch: TButton
    Left = 462
    Top = 10
    Width = 102
    Height = 25
    Caption = '&Localizar Pr'#243'xima'
    TabOrder = 0
  end
  object btSearchAll: TButton
    Left = 462
    Top = 40
    Width = 102
    Height = 25
    Caption = 'Localizar &todas'
    TabOrder = 1
  end
  object btReplace: TButton
    Left = 462
    Top = 88
    Width = 102
    Height = 25
    Caption = '&Substituir'
    TabOrder = 2
  end
  object btReplaceALL: TButton
    Left = 462
    Top = 118
    Width = 102
    Height = 25
    Caption = 'Substituir t&udo'
    TabOrder = 3
  end
  object btClose: TButton
    Left = 462
    Top = 166
    Width = 102
    Height = 25
    Caption = '&Fechar'
    TabOrder = 4
    OnClick = btCloseClick
  end
  object paSearchOptions: TPanel
    Left = 0
    Top = 184
    Width = 449
    Height = 100
    BevelOuter = bvNone
    TabOrder = 5
    object gbSearchWhere: TGroupBox
      Left = 8
      Top = 12
      Width = 233
      Height = 82
      Caption = 'Onde:'
      TabOrder = 0
      object rbText: TRadioButton
        Left = 8
        Top = 16
        Width = 121
        Height = 17
        Caption = 'Todo texto'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbClicks
      end
      object rbSelText: TRadioButton
        Left = 8
        Top = 36
        Width = 121
        Height = 17
        Caption = 'Na sele'#231#227'o'
        TabOrder = 1
        OnClick = rbClicks
      end
      object rbAllOpenedFiles: TRadioButton
        Left = 104
        Top = 16
        Width = 121
        Height = 17
        Caption = 'Arquivos abertos'
        TabOrder = 2
        OnClick = rbClicks
      end
      object rbProjectFiles: TRadioButton
        Left = 104
        Top = 36
        Width = 121
        Height = 17
        Caption = 'Arquivos do projeto'
        TabOrder = 3
        OnClick = rbClicks
      end
      object rbFolder: TRadioButton
        Left = 104
        Top = 56
        Width = 121
        Height = 17
        Caption = 'Diret'#243'rio'
        TabOrder = 4
        OnClick = rbClicks
      end
    end
    object chBack: TCheckBox
      Left = 272
      Top = 80
      Width = 169
      Height = 17
      Caption = 'Procurar de tr'#225's para frente'
      TabOrder = 1
    end
    object chCaseSensitive: TCheckBox
      Left = 272
      Top = 57
      Width = 169
      Height = 17
      Caption = 'Diferenciar mai'#250'sculas/min'#250'sc.'
      TabOrder = 2
    end
    object chPattern: TCheckBox
      Left = 272
      Top = 34
      Width = 169
      Height = 17
      Caption = 'Usar coringas (* e ?)'
      TabOrder = 3
    end
    object chWholeWords: TCheckBox
      Left = 272
      Top = 12
      Width = 169
      Height = 17
      Caption = 'Somente palavras inteiras'
      TabOrder = 4
    end
  end
  object gbFolderSearch: TGroupBox
    Left = 8
    Top = 293
    Width = 449
    Height = 108
    Caption = 'Localizar no diret'#243'rio:'
    TabOrder = 6
    object Label2: TLabel
      Left = 12
      Top = 28
      Width = 42
      Height = 13
      Caption = 'Diret'#243'rio:'
    end
    object Label3: TLabel
      Left = 12
      Top = 58
      Width = 44
      Height = 13
      Caption = 'M'#225'scara:'
    end
    object cbFolder: TComboBox
      Left = 72
      Top = 24
      Width = 337
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'cbFolder'
    end
    object cbMask: TComboBox
      Left = 72
      Top = 54
      Width = 337
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      Text = 'cbMask'
    end
    object chScanSubFolders: TCheckBox
      Left = 12
      Top = 84
      Width = 109
      Height = 17
      Caption = 'Incluir subpastas'
      TabOrder = 2
    end
    object chIgnoreBinary: TCheckBox
      Left = 156
      Top = 84
      Width = 141
      Height = 17
      Caption = 'Ignorar arquivos bin'#225'rios'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object btGetFolder: TButton
      Left = 418
      Top = 24
      Width = 18
      Height = 20
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
  object meSearchStr: TMemo
    Left = 12
    Top = 24
    Width = 437
    Height = 65
    ScrollBars = ssBoth
    TabOrder = 7
    WantTabs = True
    WordWrap = False
  end
  object meReplaceStr: TMemo
    Left = 12
    Top = 118
    Width = 437
    Height = 65
    ScrollBars = ssBoth
    TabOrder = 8
  end
end
