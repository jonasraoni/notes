object frmPrefs: TfrmPrefs
  Left = 181
  Top = 47
  BorderStyle = bsDialog
  Caption = 'Configura'#231#245'es Gerais'
  ClientHeight = 488
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 382
    Height = 452
    ActivePage = tsLook
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object tsLook: TTabSheet
      Caption = 'Apar'#234'ncia'
      ImageIndex = 3
      object Bevel1: TBevel
        Left = 12
        Top = 16
        Width = 349
        Height = 81
        Shape = bsFrame
      end
      object Bevel7: TBevel
        Left = 12
        Top = 352
        Width = 349
        Height = 49
        Shape = bsFrame
      end
      object spBgColor: TShape
        Left = 212
        Top = 26
        Width = 33
        Height = 17
        Cursor = crHandPoint
        Brush.Color = clWindow
        Pen.Color = clBtnShadow
      end
      object Label4: TLabel
        Left = 250
        Top = 26
        Width = 61
        Height = 13
        Caption = 'Cor do fundo'
      end
      object spSelColor: TShape
        Left = 212
        Top = 49
        Width = 33
        Height = 17
        Cursor = crHandPoint
        Brush.Color = clHighlight
        Pen.Color = clBtnShadow
      end
      object Label3: TLabel
        Left = 250
        Top = 49
        Width = 71
        Height = 13
        Caption = 'Cor da sele'#231#227'o'
      end
      object spSelTextColor: TShape
        Left = 212
        Top = 72
        Width = 33
        Height = 17
        Cursor = crHandPoint
        Brush.Color = clHighlightText
        Pen.Color = clBtnShadow
      end
      object Label5: TLabel
        Left = 250
        Top = 72
        Width = 87
        Height = 13
        Caption = 'Texto selecionado'
      end
      object Bevel2: TBevel
        Left = 12
        Top = 112
        Width = 349
        Height = 65
        Shape = bsFrame
      end
      object spGutterColor: TShape
        Left = 212
        Top = 120
        Width = 33
        Height = 17
        Cursor = crHandPoint
        Brush.Color = clBtnShadow
        Pen.Color = clBtnShadow
      end
      object Label6: TLabel
        Left = 250
        Top = 121
        Width = 61
        Height = 13
        Caption = 'Cor da gutter'
      end
      object Label9: TLabel
        Left = 212
        Top = 148
        Width = 72
        Height = 13
        Caption = 'Margem direita:'
      end
      object Label1: TLabel
        Left = 24
        Top = 32
        Width = 30
        Height = 13
        Caption = 'Fonte:'
      end
      object Label2: TLabel
        Left = 24
        Top = 65
        Width = 48
        Height = 13
        Caption = 'Tamanho:'
      end
      object Bevel5: TBevel
        Left = 12
        Top = 192
        Width = 349
        Height = 145
        Shape = bsFrame
      end
      object Label10: TLabel
        Left = 24
        Top = 370
        Width = 94
        Height = 13
        Caption = 'Estilo dos controles:'
      end
      object cbFont: TComboBox
        Left = 80
        Top = 28
        Width = 105
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
      object cbSize: TComboBox
        Left = 80
        Top = 60
        Width = 105
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        Text = '10'
        Items.Strings = (
          '9'
          '10'
          '12'
          '14')
      end
      object chShowgutter: TCheckBox
        Left = 24
        Top = 120
        Width = 153
        Height = 25
        Caption = 'Mostrar Gutter (borda)'
        TabOrder = 2
      end
      object chLineNum: TCheckBox
        Left = 24
        Top = 144
        Width = 177
        Height = 25
        Caption = 'Mostrar n'#250'mero das linhas'
        TabOrder = 3
      end
      object edRightMargin: TEdit
        Left = 296
        Top = 142
        Width = 33
        Height = 21
        TabOrder = 4
        Text = '72'
      end
      object chToolbarToolTips: TCheckBox
        Left = 24
        Top = 200
        Width = 225
        Height = 17
        Caption = 'Mostrar dicas na barra de ferramentas'
        TabOrder = 5
      end
      object chShowPath: TCheckBox
        Left = 24
        Top = 222
        Width = 329
        Height = 17
        Caption = 'Mostrar caminho em Favoritos e Arquivos recentes'
        TabOrder = 6
      end
      object cbControlsStyle: TComboBox
        Left = 136
        Top = 366
        Width = 185
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 10
        Text = 'MS Office XP'
        Items.Strings = (
          'MS Office XP'
          'GnomeDark'
          'Bluish'
          'Normal')
      end
      object chAlwaysHidePainels: TCheckBox
        Left = 24
        Top = 244
        Width = 329
        Height = 17
        Caption = 'Esconder os pain'#233'is ao iniciar'
        TabOrder = 7
      end
      object chLibSync: TCheckBox
        Left = 24
        Top = 266
        Width = 329
        Height = 17
        Caption = 'Sincronizar a biblioteca com o tipo de arquivo atual'
        TabOrder = 8
      end
      object chAutoHideStartPage: TCheckBox
        Left = 24
        Top = 308
        Width = 305
        Height = 17
        Caption = 'Esconder StartPage automaticamente'
        TabOrder = 9
      end
      object chAutoExpandLibrary: TCheckBox
        Left = 24
        Top = 288
        Width = 321
        Height = 17
        Caption = 'Expandir categorias da biblioteca automaticamente'
        TabOrder = 11
      end
    end
    object tsLang: TTabSheet
      Caption = 'Tipos de Arquivo'
      ImageIndex = 3
      object Bevel10: TBevel
        Left = 12
        Top = 16
        Width = 349
        Height = 289
        Shape = bsFrame
      end
      object Label15: TLabel
        Left = 28
        Top = 32
        Width = 144
        Height = 13
        Caption = 'Tipos de arquivos dispon'#237'veis:'
      end
      object lbFileTypes: TListBox
        Left = 24
        Top = 52
        Width = 321
        Height = 205
        ItemHeight = 13
        TabOrder = 0
      end
      object btEditFileType: TButton
        Left = 170
        Top = 268
        Width = 169
        Height = 25
        Caption = '&Editar tipo de arquivo'
        TabOrder = 1
        OnClick = btEditFileTypeClick
      end
    end
    object tsGeral: TTabSheet
      Caption = 'Edi'#231#227'o'
      ImageIndex = 2
      object Bevel6: TBevel
        Left = 12
        Top = 16
        Width = 349
        Height = 105
        Shape = bsFrame
      end
      object Label8: TLabel
        Left = 24
        Top = 32
        Width = 113
        Height = 13
        Caption = 'Tipo padr'#227'o de arquivo:'
      end
      object Bevel3: TBevel
        Left = 12
        Top = 136
        Width = 349
        Height = 145
        Shape = bsFrame
      end
      object chBackup: TCheckBox
        Left = 24
        Top = 64
        Width = 193
        Height = 17
        Caption = 'Fazer backup do arquivo ao salvar'
        TabOrder = 1
      end
      object cbDefaultFileType: TComboBox
        Left = 152
        Top = 28
        Width = 169
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Texto'
        Items.Strings = (
          'Texto'
          'HTML'
          'JavaScript'
          'CSS'
          'XML'
          'PHP'
          'ASP (VBScript)'
          'Perl'
          'Java'
          'Object Pascal'
          'C-C++'
          'C#'
          'Visual Basic'
          'Assembler'
          'Fortran'
          'Python'
          'Tcl-Tk'
          'SQL'
          'LaTeX'
          'Ruby')
      end
      object chBackupDir: TCheckBox
        Left = 48
        Top = 88
        Width = 257
        Height = 17
        Caption = 'Fazer o bakup no diret'#243'rio de bakup do Notes'
        TabOrder = 2
      end
      object chCopyAlwaysOn: TCheckBox
        Left = 24
        Top = 198
        Width = 257
        Height = 25
        Caption = 'Habilitar "copiar" quando n'#227'o houver sele'#231#227'o'
        TabOrder = 5
      end
      object chLineColStartAt0: TCheckBox
        Left = 24
        Top = 222
        Width = 257
        Height = 25
        Caption = 'Contagem de Linha e Coluna come'#231'a em Zero'
        TabOrder = 6
      end
      object chTrailingSpaces: TCheckBox
        Left = 24
        Top = 246
        Width = 257
        Height = 25
        Caption = 'Limpar espa'#231'os in'#250'teis ao fim da linha'
        TabOrder = 7
      end
      object chSmartHome: TCheckBox
        Left = 24
        Top = 150
        Width = 145
        Height = 25
        Caption = 'Usar SmartHome'
        TabOrder = 3
      end
      object chSmartTabs: TCheckBox
        Left = 24
        Top = 174
        Width = 145
        Height = 25
        Caption = 'Usar SmartTabs'
        TabOrder = 4
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Associa'#231#245'es'
      ImageIndex = 1
      object Bevel8: TBevel
        Left = 12
        Top = 16
        Width = 349
        Height = 233
        Shape = bsFrame
      end
      object Label7: TLabel
        Left = 32
        Top = 32
        Width = 296
        Height = 13
        Caption = 'Marque os tipos de arquivos que voc'#234' quer associar ao Notes:'
      end
      object Bevel9: TBevel
        Left = 12
        Top = 264
        Width = 349
        Height = 73
        Shape = bsFrame
      end
      object Label12: TLabel
        Left = 24
        Top = 280
        Width = 89
        Height = 13
        Caption = 'Menu de contexto:'
      end
      object chShellExt: TCheckBox
        Left = 32
        Top = 304
        Width = 321
        Height = 17
        Caption = 'Ativar extens'#227'o do Notes no menu de contexto do Windows'
        TabOrder = 1
      end
      object chlbAssoc: TCheckListBox
        Left = 32
        Top = 56
        Width = 313
        Height = 177
        ItemHeight = 13
        Items.Strings = (
          'ASP (*.asp;*.asa)'
          'C# (*.cs)'
          'HTML (*.htm,*.html)'
          'XML (*.xml,*.dtd,*.xsd,*.xsl,*.xslt)'
          'PHP (*.php,*.php3,*.php4,*.phtml)'
          'ASP .Net (*.aspx,*.ascx,*.asmx,*.mspx,*.asax)'
          'Perl (*.pl,*.pm,*.cgi)'
          'C/C++ (*.c,*.cpp,*.h,*.hpp,*.cxx,*.tli)'
          'Java (*.java,*.jav)'
          'JavaServer Pages (*.jsp,*.jst)')
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 452
    Width = 382
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btOK: TButton
      Left = 176
      Top = 6
      Width = 89
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      OnClick = btOKClick
    end
    object btCancel: TButton
      Left = 280
      Top = 6
      Width = 89
      Height = 25
      Cancel = True
      Caption = '&Cancelar'
      TabOrder = 1
      OnClick = btCancelClick
    end
  end
end
