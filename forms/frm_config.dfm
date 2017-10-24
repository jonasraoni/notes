object frmConfig: TfrmConfig
  Left = 193
  Top = 26
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'Configura'#231#245'es'
  ClientHeight = 485
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgPages: TPageControl
    Left = 4
    Top = 4
    Width = 405
    Height = 446
    ActivePage = tsGeneral
    MultiLine = True
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'Geral'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      object gbEditorLook: TGroupBox
        Left = 8
        Top = 8
        Width = 382
        Height = 124
        Caption = 'Apar'#234'ncia'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object lFont: TLabel
          Left = 12
          Top = 24
          Width = 32
          Height = 13
          Caption = '&Fonte:'
          FocusControl = cbFont
        end
        object lSize: TLabel
          Left = 200
          Top = 24
          Width = 48
          Height = 13
          Caption = '&Tamanho:'
        end
        object lRightMargin: TLabel
          Left = 201
          Top = 53
          Width = 129
          Height = 13
          Caption = 'Posi'#231#227'o da &margem direita:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lScope: TLabel
          Left = 202
          Top = 87
          Width = 38
          Height = 13
          Caption = '&Escopo:'
          Enabled = False
          FocusControl = cbScope
        end
        object cbFont: TComboBox
          Left = 50
          Top = 22
          Width = 133
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object cbSize: TComboBox
          Left = 259
          Top = 22
          Width = 57
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = '8'
          OnChange = cbSizeChange
          Items.Strings = (
            '8'
            '10'
            '12'
            '14'
            '20'
            '24'
            '36'
            '48')
        end
        object chShowGutter: TCheckBox
          Left = 12
          Top = 50
          Width = 97
          Height = 19
          Caption = 'Mostrar &Gutter'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object chLineNum: TCheckBox
          Left = 12
          Top = 72
          Width = 144
          Height = 19
          Caption = 'Mostrar &linhas na Gutter'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
        end
        object edRightMargin: TEdit
          Left = 340
          Top = 50
          Width = 27
          Height = 21
          TabOrder = 3
          Text = '72'
          OnChange = intEdChange
          OnKeyPress = IntEditsKeyPress
        end
        object cbScope: TComboBox
          Left = 248
          Top = 83
          Width = 123
          Height = 21
          Style = csDropDownList
          Enabled = False
          ItemHeight = 13
          TabOrder = 5
        end
        object chHighlightMatchingBrackets: TCheckBox
          Left = 12
          Top = 96
          Width = 145
          Height = 17
          Caption = 'Mostrar &bra'#231'os pares'
          TabOrder = 6
        end
      end
      object gbEditorSaveOptions: TGroupBox
        Left = 8
        Top = 140
        Width = 383
        Height = 78
        Caption = 'Backup de arquivos'
        TabOrder = 1
        object lAutoSave: TLabel
          Left = 12
          Top = 48
          Width = 151
          Height = 13
          Caption = 'Salvar automaticamente &a cada'
          Enabled = False
          FocusControl = edAutoSaveInterval
        end
        object lMin: TLabel
          Left = 210
          Top = 48
          Width = 37
          Height = 13
          Caption = 'minutos'
          Enabled = False
        end
        object chBackupOnSave: TCheckBox
          Left = 12
          Top = 24
          Width = 188
          Height = 17
          Caption = 'Criar backup do arquivo ao &salvar'
          Enabled = False
          TabOrder = 0
        end
        object edAutoSaveInterval: TEdit
          Left = 172
          Top = 44
          Width = 32
          Height = 21
          Enabled = False
          TabOrder = 1
          Text = '15'
          OnChange = intEdChange
          OnKeyPress = IntEditsKeyPress
        end
      end
      object gbOtherOptions: TGroupBox
        Left = 8
        Top = 225
        Width = 384
        Height = 180
        Caption = 'Outras op'#231#245'es'
        TabOrder = 2
        object lDefaultFileType: TLabel
          Left = 12
          Top = 90
          Width = 115
          Height = 13
          Caption = '&Tipo de arquivo padr'#227'o:'
          FocusControl = cbDefaultFileType
        end
        object lAtInit: TLabel
          Left = 12
          Top = 118
          Width = 47
          Height = 13
          Caption = 'Ao &iniciar:'
          FocusControl = cbStartup
        end
        object lLanguage: TLabel
          Left = 12
          Top = 149
          Width = 55
          Height = 13
          Caption = 'Li&nguagem:'
          Enabled = False
          FocusControl = cbLanguage
        end
        object chAllowMultipleInstances: TCheckBox
          Left = 12
          Top = 19
          Width = 203
          Height = 19
          Caption = 'Permitir &m'#250'ltiplas inst'#226'ncias do Notes'
          TabOrder = 0
        end
        object cbDefaultFileType: TComboBox
          Left = 136
          Top = 87
          Width = 201
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
        end
        object cbStartup: TComboBox
          Left = 80
          Top = 115
          Width = 257
          Height = 21
          Style = csDropDownList
          Enabled = False
          ItemHeight = 13
          TabOrder = 4
        end
        object chRemeberFileInfo: TCheckBox
          Left = 12
          Top = 42
          Width = 306
          Height = 19
          Caption = 'Lembrar da '#250'ltima &posi'#231#227'o de edi'#231#227'o e dos marcadores '
          TabOrder = 1
        end
        object chOfficeXP: TCheckBox
          Left = 12
          Top = 64
          Width = 257
          Height = 19
          Caption = 'Desenhar controles com o estilo do Office &XP'
          Enabled = False
          TabOrder = 2
        end
        object cbLanguage: TComboBox
          Left = 80
          Top = 145
          Width = 257
          Height = 21
          Style = csDropDownList
          Enabled = False
          ItemHeight = 13
          TabOrder = 5
        end
      end
    end
    object tsColors: TTabSheet
      Caption = 'Cores'
      ImageIndex = 3
      object lFileType: TLabel
        Left = 8
        Top = 16
        Width = 57
        Height = 13
        Caption = 'Cores &para:'
        FocusControl = cbColorFileType
      end
      object gbSyntax: TGroupBox
        Left = 8
        Top = 43
        Width = 382
        Height = 212
        Caption = 'Colora'#231#227'o da sintaxe'
        TabOrder = 1
        object shForeground: TShape
          Left = 200
          Top = 140
          Width = 25
          Height = 22
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object shBackground: TShape
          Left = 200
          Top = 171
          Width = 25
          Height = 22
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object lSelToken: TLabel
          Left = 14
          Top = 21
          Width = 155
          Height = 13
          Caption = 'Selecione uma categoria abai&xo:'
          FocusControl = lsTokens
        end
        object lsTokens: TListBox
          Left = 12
          Top = 40
          Width = 176
          Height = 157
          ItemHeight = 13
          TabOrder = 0
          OnClick = DoAttrChange
          OnKeyUp = lsTokensKeyUp
        end
        object chBold: TCheckBox
          Left = 200
          Top = 37
          Width = 73
          Height = 25
          Caption = '&Negrito'
          TabOrder = 1
        end
        object chItalic: TCheckBox
          Left = 200
          Top = 60
          Width = 59
          Height = 25
          Caption = '&It'#225'lico'
          TabOrder = 2
        end
        object chUnderline: TCheckBox
          Left = 200
          Top = 83
          Width = 85
          Height = 25
          Caption = '&Sublinhado'
          TabOrder = 3
        end
        object chStrikeOut: TCheckBox
          Left = 200
          Top = 106
          Width = 77
          Height = 25
          Caption = '&Riscado'
          TabOrder = 4
        end
        object btForeground: TButton
          Left = 232
          Top = 138
          Width = 142
          Height = 25
          Caption = 'Cor do &Texto...'
          TabOrder = 5
          OnClick = ColorButtonClick
        end
        object btBackground: TButton
          Left = 232
          Top = 170
          Width = 142
          Height = 25
          Caption = 'Cor do &Fundo...'
          TabOrder = 6
          OnClick = ColorButtonClick
        end
      end
      object cbColorFileType: TComboBox
        Left = 78
        Top = 12
        Width = 295
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = DoFileTypeChange
      end
      object gbOtherColors: TGroupBox
        Left = 8
        Top = 262
        Width = 382
        Height = 143
        Caption = 'Outras cores'
        TabOrder = 2
        object shEditorColor: TShape
          Left = 12
          Top = 19
          Width = 25
          Height = 21
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object shGutterTextColor: TShape
          Left = 200
          Top = 17
          Width = 25
          Height = 21
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object shGutterColor: TShape
          Left = 200
          Top = 48
          Width = 25
          Height = 21
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object shMarginColor: TShape
          Left = 200
          Top = 78
          Width = 25
          Height = 21
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object shLineColor: TShape
          Left = 12
          Top = 48
          Width = 25
          Height = 21
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object shScopeColor: TShape
          Left = 12
          Top = 78
          Width = 25
          Height = 21
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object shMatchingBraketsColor: TShape
          Left = 12
          Top = 109
          Width = 25
          Height = 21
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object shErrorColor: TShape
          Left = 200
          Top = 109
          Width = 25
          Height = 21
          Pen.Color = clBtnShadow
          Pen.Width = 2
          OnMouseDown = DoChangeShapeColor
        end
        object btEditorColor: TButton
          Left = 44
          Top = 16
          Width = 142
          Height = 24
          Caption = 'Cor do &Editor...'
          TabOrder = 0
          OnClick = ColorButtonClick
        end
        object btGutterTextColor: TButton
          Left = 232
          Top = 14
          Width = 142
          Height = 24
          Caption = 'Cor do Te&xto da gutter...'
          TabOrder = 4
          OnClick = ColorButtonClick
        end
        object btGutterColor: TButton
          Left = 232
          Top = 45
          Width = 142
          Height = 24
          Caption = 'Cor da &Gutter...'
          TabOrder = 5
          OnClick = ColorButtonClick
        end
        object btMarginColor: TButton
          Left = 232
          Top = 75
          Width = 142
          Height = 24
          Caption = 'Cor da &Margem...'
          TabOrder = 6
          OnClick = ColorButtonClick
        end
        object btLineColor: TButton
          Left = 44
          Top = 45
          Width = 142
          Height = 24
          Caption = 'Cor da &linha atual...'
          TabOrder = 1
          OnClick = ColorButtonClick
        end
        object btScopeColor: TButton
          Left = 44
          Top = 75
          Width = 142
          Height = 24
          Caption = 'Co&r do Escopo atual...'
          TabOrder = 2
          OnClick = ColorButtonClick
        end
        object btMatchingBraketsColor: TButton
          Left = 44
          Top = 106
          Width = 142
          Height = 24
          Caption = 'Cor dos &bra'#231'os pares...'
          TabOrder = 3
          OnClick = ColorButtonClick
        end
        object btErrorColor: TButton
          Left = 232
          Top = 106
          Width = 142
          Height = 24
          Caption = 'Cor p&ara Erros...'
          TabOrder = 7
          OnClick = ColorButtonClick
        end
      end
    end
    object tsEditor: TTabSheet
      Caption = 'Editor'
      ImageIndex = 4
      object gbGeneralEditorOpt: TGroupBox
        Left = 8
        Top = 217
        Width = 382
        Height = 188
        Caption = 'Op'#231#245'es gerais:'
        TabOrder = 1
        object chSmartHome: TCheckBox
          Left = 12
          Top = 20
          Width = 351
          Height = 17
          Caption = 
            'Smart&Home (a tecla home posiciona o cursor no primeiro caracter' +
            ')'
          TabOrder = 0
        end
        object chSmartTab: TCheckBox
          Left = 12
          Top = 42
          Width = 351
          Height = 17
          Caption = '&SmartTab (tab, delete e backspace ajudam a identar o c'#243'digo)'
          TabOrder = 1
        end
        object chDelTrailingSpaces: TCheckBox
          Left = 12
          Top = 64
          Width = 351
          Height = 17
          Caption = 'Limpar espa'#231'os &in'#250'teis ao fim da linha'
          TabOrder = 2
        end
        object chLineColStartAt0: TCheckBox
          Left = 12
          Top = 87
          Width = 351
          Height = 17
          Caption = 'Contagem de linha e coluna iniciam em 0 (&zero)'
          TabOrder = 3
        end
        object chAllowCaretAfterEOL: TCheckBox
          Left = 12
          Top = 110
          Width = 351
          Height = 17
          Caption = 'Permitir cursor ap'#243's o &fim da linha/fim do arquivo'
          TabOrder = 4
        end
        object chShowSpecialChars: TCheckBox
          Left = 12
          Top = 133
          Width = 351
          Height = 17
          Caption = '&Mostrar caracteres especiais'
          TabOrder = 5
        end
        object chAutoCloseSymbols: TCheckBox
          Left = 12
          Top = 157
          Width = 326
          Height = 17
          Caption = 'Fechar &aspas, chaves, par'#234'nteses, etc. automaticamente'
          TabOrder = 6
        end
      end
      object gbEspecificEditorOpt: TGroupBox
        Left = 8
        Top = 9
        Width = 382
        Height = 200
        Caption = 'Op'#231#245'es por tipo de arquivo'
        TabOrder = 0
        object lIndentSize: TLabel
          Left = 12
          Top = 58
          Width = 113
          Height = 13
          Caption = 'Tamanho da ide&nta'#231#227'o:'
          FocusControl = edIdentSize
        end
        object lTabSize: TLabel
          Left = 12
          Top = 87
          Width = 125
          Height = 13
          Caption = 'Tamanho do caracter &tab:'
          FocusControl = edTabSize
        end
        object lOnEnter: TLabel
          Left = 225
          Top = 58
          Width = 92
          Height = 13
          Caption = 'Ao quebrar a linha:'
        end
        object lOptionsFileType: TLabel
          Left = 8
          Top = 23
          Width = 65
          Height = 13
          Caption = 'O&p'#231#245'es para:'
          FocusControl = cbOptionsFileType
        end
        object lFilter: TLabel
          Left = 12
          Top = 168
          Width = 96
          Height = 13
          Caption = 'Filtro de arqui&vos:   '
          FocusControl = edFilters
        end
        object edIdentSize: TEdit
          Left = 149
          Top = 54
          Width = 33
          Height = 21
          TabOrder = 0
          Text = '2'
          OnChange = intEdChange
          OnKeyPress = IntEditsKeyPress
        end
        object edTabSize: TEdit
          Left = 149
          Top = 84
          Width = 33
          Height = 21
          TabOrder = 1
          Text = '2'
          OnChange = intEdChange
          OnKeyPress = IntEditsKeyPress
        end
        object chTabToSpaces: TCheckBox
          Left = 12
          Top = 113
          Width = 173
          Height = 13
          Caption = 'Usar &espa'#231'os no lugar de Tab'
          TabOrder = 2
        end
        object rbAutoIndent: TRadioButton
          Left = 234
          Top = 74
          Width = 119
          Height = 17
          Caption = '&Usar AutoIndent'
          TabOrder = 3
        end
        object rbSmartIndent: TRadioButton
          Left = 234
          Top = 93
          Width = 120
          Height = 17
          Caption = 'Usar &Sma&rtIndent'
          TabOrder = 4
        end
        object rbNoAutoIndent: TRadioButton
          Left = 234
          Top = 112
          Width = 108
          Height = 17
          Caption = 'N'#227'o usar na&da'
          TabOrder = 5
        end
        object cbOptionsFileType: TComboBox
          Left = 86
          Top = 20
          Width = 283
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 6
          OnChange = DoFileTypeChange
        end
        object chAutoCloseTags: TCheckBox
          Left = 12
          Top = 137
          Width = 246
          Height = 17
          Caption = 'Fechar tags HTML/&XML automaticamente'
          TabOrder = 7
        end
        object edFilters: TEdit
          Left = 111
          Top = 163
          Width = 254
          Height = 21
          TabOrder = 8
          OnChange = edFiltersChanged
        end
      end
    end
    object tsSystem: TTabSheet
      Caption = 'Sistema'
      ImageIndex = 6
      object gbAssoc: TGroupBox
        Left = 8
        Top = 11
        Width = 382
        Height = 243
        Caption = 'Associa'#231#245'es de arquivo'
        TabOrder = 0
        object lChooseAssoc: TLabel
          Left = 12
          Top = 24
          Width = 294
          Height = 13
          Caption = 'Escolha os tipos de arquivo que voc'#234' quer &associar ao Notes:'
          Enabled = False
          FocusControl = clAssoc
        end
        object clAssoc: TCheckListBox
          Left = 12
          Top = 46
          Width = 355
          Height = 181
          Enabled = False
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object gbShellExt: TGroupBox
        Left = 8
        Top = 263
        Width = 382
        Height = 53
        Caption = 'Shell Extension'
        TabOrder = 1
        object chShellExt: TCheckBox
          Left = 12
          Top = 24
          Width = 301
          Height = 17
          Caption = 'Colocar itens do Notes no &menu de contexto do Windows'
          TabOrder = 0
        end
      end
    end
  end
  object btCancel: TButton
    Left = 321
    Top = 457
    Width = 86
    Height = 25
    Cancel = True
    Caption = '&Cancelar'
    ModalResult = 2
    TabOrder = 2
  end
  object btOK: TButton
    Left = 231
    Top = 457
    Width = 79
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btOKClick
  end
  object colorDlg: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 312
    Top = 65534
  end
end
