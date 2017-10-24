object frmEditmnuRunItem: TfrmEditmnuRunItem
  Left = 176
  Top = 75
  BorderStyle = bsDialog
  Caption = 'Editar item'
  ClientHeight = 386
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object paBottom: TPanel
    Left = 0
    Top = 354
    Width = 452
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btOK: TButton
      Left = 240
      Top = 1
      Width = 89
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      OnClick = btOKClick
    end
    object btCancel: TButton
      Left = 338
      Top = 1
      Width = 103
      Height = 25
      Cancel = True
      Caption = '&Cancelar'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object gbPaths: TGroupBox
    Left = 8
    Top = 8
    Width = 440
    Height = 121
    Caption = 'Caminhos'
    TabOrder = 0
    object lFileToRun: TLabel
      Left = 8
      Top = 22
      Width = 47
      Height = 13
      Caption = '&Executar:'
      FocusControl = edCmd
    end
    object lArgs: TLabel
      Left = 8
      Top = 52
      Width = 62
      Height = 13
      Caption = 'Ar&gumentos:'
      FocusControl = edArgs
    end
    object lDir: TLabel
      Left = 8
      Top = 87
      Width = 49
      Height = 13
      Caption = '&Dir. inicial:'
      FocusControl = edDir
    end
    object edCmd: TEdit
      Left = 74
      Top = 18
      Width = 320
      Height = 21
      Hint = 
        'Programa (compilador, navegador, interpretador, etc.) a ser exec' +
        'utado'
      TabOrder = 0
    end
    object btProcurar: TButton
      Left = 404
      Top = 17
      Width = 22
      Height = 22
      Caption = '+'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      PopupMenu = poVars
      TabOrder = 1
      OnClick = btExecClick
    end
    object edArgs: TEdit
      Left = 74
      Top = 50
      Width = 320
      Height = 21
      Hint = 'Argumentos que voc'#234' quer passar ao programa executado'
      TabOrder = 2
    end
    object btArgs: TButton
      Left = 404
      Top = 49
      Width = 22
      Height = 22
      Caption = '+'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btArgsClick
    end
    object edDir: TEdit
      Left = 74
      Top = 83
      Width = 320
      Height = 21
      Hint = 'Diret'#243'rio inicial'
      TabOrder = 4
    end
    object btDir: TButton
      Left = 404
      Top = 83
      Width = 22
      Height = 22
      Caption = '+'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = btDirClick
    end
  end
  object gbOptions: TGroupBox
    Left = 8
    Top = 136
    Width = 440
    Height = 207
    Caption = 'Op'#231#245'es'
    TabOrder = 1
    object lOutput: TLabel
      Left = 8
      Top = 54
      Width = 30
      Height = 13
      Caption = '&Sa'#237'da:'
      FocusControl = cbOut
    end
    object cbOut: TComboBox
      Left = 74
      Top = 50
      Width = 235
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbOutChange
    end
    object paGbOptions: TPanel
      Left = 2
      Top = 80
      Width = 432
      Height = 117
      BevelOuter = bvNone
      TabOrder = 2
      object lRegEx: TLabel
        Left = 88
        Top = 43
        Width = 208
        Height = 13
        Caption = 'Express'#227'o &regular para interpretar a sa'#237'da:'
        FocusControl = edRegEx
      end
      object lFilePos: TLabel
        Left = 88
        Top = 96
        Width = 130
        Height = 13
        Caption = 'Sub-express'#227'o do ar&quivo:'
        FocusControl = edFilePos
      end
      object lLinePos: TLabel
        Left = 265
        Top = 97
        Width = 116
        Height = 13
        Caption = 'Sub-express'#227'o da &linha:'
        FocusControl = edLinePos
      end
      object chAutoUnderstandOutput: TCheckBox
        Left = 72
        Top = 7
        Width = 280
        Height = 25
        Caption = 'Dei&xar o Notes entender a sa'#237'da automaticamente'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chAutoUnderstandOutputClick
      end
      object edRegEx: TEdit
        Left = 88
        Top = 62
        Width = 337
        Height = 21
        TabOrder = 1
      end
      object edFilePos: TEdit
        Left = 222
        Top = 93
        Width = 24
        Height = 21
        TabOrder = 2
        Text = '1'
      end
      object edLinePos: TEdit
        Left = 386
        Top = 92
        Width = 24
        Height = 21
        TabOrder = 3
        Text = '2'
      end
    end
    object chSaveBeforeRun: TCheckBox
      Left = 74
      Top = 22
      Width = 297
      Height = 17
      Hint = 
        'Especifica se o Notes deve salvar o arquivo antes de executar o ' +
        'programa'
      Caption = 'Sal&var arquivo antes de executar'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object btOptions: TButton
      Left = 319
      Top = 47
      Width = 107
      Height = 25
      Hint = 'Mais op'#231#245'es para a captura da sa'#237'da do aplicativo'
      Caption = 'Ver O&p'#231#245'es'
      TabOrder = 3
      OnClick = TogleOptions
    end
  end
  object poVars: TPopupMenu
    Left = 24
    Top = 352
    object mnuInsertFileName: TMenuItem
      Caption = 'Procurar Arquivo...'
      OnClick = mnuInsertFileNameClick
    end
    object mnuArgsFolder: TMenuItem
      Caption = 'Procurar Pasta...'
      OnClick = mnuArgsFolderClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuVarsProj: TMenuItem
      Caption = 'Vari'#225'veis do projeto'
      Enabled = False
      object pvProjName: TMenuItem
        Caption = 'Nome do projeto                 %ProjName%'
        Hint = 'Macro que insere o nome do projeto'
        OnClick = pvClick
      end
      object pvProjDir: TMenuItem
        Caption = 'Diret'#243'rio do projeto            %ProjDir%'
        Hint = 'Macro que insere o diret'#243'rio do projeto'
        OnClick = pvClick
      end
      object pvProjFile: TMenuItem
        Caption = 'Arquivo do Projeto             %ProjFile%'
        Hint = 'Nome do arquivo do projeto'
        OnClick = pvClick
      end
    end
    object mnuFileVars: TMenuItem
      Caption = 'Vari'#225'veis do arquivo'
      object pvFile: TMenuItem
        Caption = 'Arquivo                           %File%'
        Hint = 'Macro que insere o caminho completo para o arquivo'
        OnClick = pvClick
      end
      object pvFileTitle: TMenuItem
        Caption = 'Nome sem extens'#227'o       %FileTitle%'
        Hint = 
          'Macro para inserir o nome do arquivo sem extens'#227'o e sem o caminh' +
          'o'
        OnClick = pvClick
      end
      object pvFileName: TMenuItem
        Caption = 'Nome do arquivo            %FileName%'
        Hint = 'Macro para inserir o nome do arquivo sem o caminho'
        OnClick = pvClick
      end
      object pvPath: TMenuItem
        Caption = 'Diret'#243'rio do arquivo        %Path%'
        Hint = 'Macro que insere o caminho completo para o arquivo'
        OnClick = pvClick
      end
      object pvExt: TMenuItem
        Caption = 'Extens'#227'o do arquivo       %Ext%'
        Hint = 'Macro que insere a extens'#227'o do arquivo (sem o ponto)'
        OnClick = pvClick
      end
    end
    object mnuVarsText: TMenuItem
      Caption = 'Vari'#225'veis do texto'
      object pvSelLength: TMenuItem
        Caption = 'Tamanho da Sele'#231#227'o       %SelLength%'
        Hint = 'Macro que insere o tamanho do texto selecionado'
        OnClick = pvClick
      end
      object pvSelStart: TMenuItem
        Caption = 'In'#237'cio da sele'#231#227'o             %SelStart%'
        Hint = 'Macro que insere a posi'#231#227'o do cursor/in'#237'cio da sele'#231#227'o'
        OnClick = pvClick
      end
      object pvWordAtCursor: TMenuItem
        Caption = 'Palavra no cursor           %WordAtCursor%'
        Hint = 'Macro que insere a palavra que estiver sob o cursor'
        OnClick = pvClick
      end
      object pvLine: TMenuItem
        Caption = 'Linha atual                      %Line%'
        Hint = 'Macro que insere a linha atual'
        OnClick = pvClick
      end
      object pvCol: TMenuItem
        Caption = 'Coluna atual                   %Col%'
        Hint = 'Macro que insere a coluna atual'
        OnClick = pvClick
      end
      object pvLinesCount: TMenuItem
        Caption = 'N'#250'mero de linhas           %LinesCount%'
        Hint = 'Macro que insere o n'#250'mero de linhas'
        OnClick = pvClick
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mnuFuncs: TMenuItem
      Caption = 'Fun'#231#245'es'
      object pfAskFile: TMenuItem
        Caption = 'Perguntar por arquivo             %AskFile()%'
        Hint = 'Macro que pede para o usu'#225'rio escolher um arquivo'
        OnClick = pvClick
      end
      object pfAskStr: TMenuItem
        Caption = 'Perguntar por string                 %AskStr()%'
        Hint = 'Macro que pede ao usu'#225'rio uma string'
        OnClick = pvClick
      end
      object pfEnv: TMenuItem
        Caption = 'Inserir Enviroment Variable      %Env()%'
        Hint = 'Macro que insere uma vari'#225'vel de ambiente'
        OnClick = pvClick
      end
    end
  end
end
