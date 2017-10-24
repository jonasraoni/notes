object frmSearch: TfrmSearch
  Left = 157
  Top = 137
  BorderStyle = bsDialog
  Caption = 'frmSearch'
  ClientHeight = 312
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object cbSearchStr: TComboBox
    Left = 68
    Top = 12
    Width = 308
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object btSearch: TButton
    Left = 386
    Top = 10
    Width = 102
    Height = 25
    Caption = '&Localizar Pr'#243'xima'
    Default = True
    TabOrder = 2
    OnClick = btSearchClick
  end
  object btSearchAll: TButton
    Left = 386
    Top = 40
    Width = 102
    Height = 25
    Caption = 'Localizar &todas'
    TabOrder = 3
    OnClick = btSearchAllClick
  end
  object btReplace: TButton
    Left = 386
    Top = 84
    Width = 102
    Height = 25
    Caption = '&Substituir'
    TabOrder = 4
    OnClick = btReplaceClick
  end
  object btReplaceALL: TButton
    Left = 386
    Top = 114
    Width = 102
    Height = 25
    Caption = 'Substituir t&udo'
    TabOrder = 5
    OnClick = btReplaceALLClick
  end
  object btClose: TButton
    Left = 386
    Top = 158
    Width = 102
    Height = 25
    Cancel = True
    Caption = '&Fechar'
    TabOrder = 6
    OnClick = btCloseClick
  end
  object paSearchOptions: TPanel
    Left = 0
    Top = 68
    Width = 381
    Height = 129
    BevelOuter = bvNone
    TabOrder = 7
    object gbSearchWhere: TGroupBox
      Left = 8
      Top = 0
      Width = 138
      Height = 121
      Caption = 'Onde'
      TabOrder = 0
      object rbText: TRadioButton
        Left = 8
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Todo te&xto'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbClicks
      end
      object rbSelText: TRadioButton
        Left = 8
        Top = 36
        Width = 113
        Height = 17
        Caption = 'Na sel&e'#231#227'o'
        TabOrder = 1
        OnClick = rbClicks
      end
      object rbAllOpenedFiles: TRadioButton
        Left = 8
        Top = 56
        Width = 113
        Height = 17
        Caption = 'Arquivos &abertos'
        TabOrder = 2
        OnClick = rbClicks
      end
      object rbProjectFiles: TRadioButton
        Left = 8
        Top = 76
        Width = 113
        Height = 17
        Caption = 'Arqui&vos do projeto'
        Enabled = False
        TabOrder = 3
        OnClick = rbClicks
      end
      object rbFolder: TRadioButton
        Left = 8
        Top = 96
        Width = 113
        Height = 17
        Caption = '&Diret'#243'rio'
        TabOrder = 4
        OnClick = rbClicks
      end
    end
    object gbOptions: TGroupBox
      Left = 155
      Top = 0
      Width = 222
      Height = 121
      Caption = 'Op'#231#245'es'
      TabOrder = 1
      object chWholeWords: TCheckBox
        Left = 8
        Top = 36
        Width = 169
        Height = 17
        Caption = 'Somente palavras i&nteiras'
        TabOrder = 0
        OnClick = chClicks
      end
      object chWildCards: TCheckBox
        Left = 8
        Top = 76
        Width = 169
        Height = 17
        Caption = 'Usar corin&gas (* e ?)'
        TabOrder = 1
        OnClick = chClicks
      end
      object chCaseSensitive: TCheckBox
        Left = 8
        Top = 56
        Width = 183
        Height = 17
        Caption = 'Diferenciar &mai'#250'sculas/min'#250'sculas'
        TabOrder = 2
        OnClick = chClicks
      end
      object chBack: TCheckBox
        Left = 8
        Top = 16
        Width = 169
        Height = 17
        Caption = 'Procurar de tr'#225's para &frente'
        TabOrder = 3
        OnClick = chClicks
      end
      object chRegExp: TCheckBox
        Left = 8
        Top = 96
        Width = 169
        Height = 17
        Caption = 'Usar express'#245'es &regulares'
        TabOrder = 4
        OnClick = chClicks
      end
      object btGrepHelp: TButton
        Left = 166
        Top = 94
        Width = 18
        Height = 20
        Caption = ' ? '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
        OnClick = btGrepHelpClick
      end
    end
  end
  object cbReplaceStr: TComboBox
    Left = 68
    Top = 40
    Width = 308
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object gbFolderSearch: TGroupBox
    Left = 8
    Top = 197
    Width = 481
    Height = 108
    Caption = 'Localizar no diret'#243'rio'
    TabOrder = 8
    object cbFolder: TComboBox
      Left = 72
      Top = 24
      Width = 315
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
    object cbMask: TComboBox
      Left = 72
      Top = 54
      Width = 315
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      Text = '*.*'
    end
    object chScanSubFolders: TCheckBox
      Left = 8
      Top = 84
      Width = 109
      Height = 17
      Caption = '&Incluir subpastas'
      TabOrder = 2
    end
    object chIgnoreBinary: TCheckBox
      Left = 144
      Top = 84
      Width = 141
      Height = 17
      Caption = 'Ignorar arquivos &bin'#225'rios'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object btSearchFolder: TButton
      Left = 394
      Top = 21
      Width = 75
      Height = 25
      Caption = '&Procurar...'
      TabOrder = 4
      OnClick = btGetFolderClick
    end
    object lDir: TStaticText
      Left = 12
      Top = 28
      Width = 56
      Height = 17
      Caption = 'Diret'#243'ri&o:'
      TabOrder = 5
      Transparent = False
    end
    object lMask: TStaticText
      Left = 12
      Top = 58
      Width = 55
      Height = 17
      Caption = 'M'#225'sc&ara:'
      TabOrder = 6
      Transparent = False
    end
  end
  object lSearch: TStaticText
    Left = 12
    Top = 16
    Width = 56
    Height = 17
    Caption = 'Locali&zar:'
    TabOrder = 9
    Transparent = False
  end
  object lbReplace: TStaticText
    Left = 12
    Top = 43
    Width = 51
    Height = 17
    Caption = 'Su&bstituir:'
    TabOrder = 10
    Transparent = False
  end
end
