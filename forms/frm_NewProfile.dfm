object frmNewProfile: TfrmNewProfile
  Left = 168
  Top = 125
  BorderStyle = bsDialog
  Caption = 'Novo Profile...'
  ClientHeight = 318
  ClientWidth = 492
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
  object pgWiz: TPageControl
    Left = -4
    Top = -8
    Width = 544
    Height = 346
    ActivePage = tsStart
    TabHeight = 1
    TabOrder = 0
    object tsStart: TTabSheet
      Caption = 'tsStart'
      ImageIndex = 2
      object Shape1: TShape
        Left = -8
        Top = -8
        Width = 513
        Height = 84
        Pen.Color = clBtnShadow
      end
      object Label5: TLabel
        Left = 24
        Top = 16
        Width = 80
        Height = 13
        Caption = 'Novo profile'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label6: TLabel
        Left = 48
        Top = 40
        Width = 325
        Height = 13
        Caption = 
          'Este wizard vai gui'#225'-lo passo a passo na cria'#231#227'o do seu novo pro' +
          'file'
        Transparent = True
        WordWrap = True
      end
      object Bevel3: TBevel
        Left = 4
        Top = 276
        Width = 484
        Height = 9
        Shape = bsBottomLine
      end
      object Label11: TLabel
        Left = 48
        Top = 112
        Width = 93
        Height = 13
        Caption = 'O que '#233' um profile?'
      end
      object Label12: TLabel
        Left = 48
        Top = 128
        Width = 353
        Height = 39
        Caption = 
          'O Notes armazena todas as suas configura'#231#245'es dentro de um profil' +
          'e. Um profile ent'#227'o '#233' como um pacote de configura'#231#245'es. O Notes p' +
          'recisa que um profile exista para que ele possa funcionar corret' +
          'amente.'
        WordWrap = True
      end
      object Label13: TLabel
        Left = 48
        Top = 224
        Width = 310
        Height = 13
        Caption = 'Clique em "Avan'#231'ar" para continuar com a cria'#231#227'o do seu profile.'
      end
    end
    object tsFileTypesConfig: TTabSheet
      Caption = 'tsFileTypesConfig'
      object Shape2: TShape
        Left = -8
        Top = -8
        Width = 513
        Height = 84
        Pen.Color = clBtnShadow
      end
      object Bevel1: TBevel
        Left = 4
        Top = 276
        Width = 484
        Height = 9
        Shape = bsBottomLine
      end
      object Label1: TLabel
        Left = 24
        Top = 16
        Width = 109
        Height = 13
        Caption = 'Tipos de arquivo'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label2: TLabel
        Left = 48
        Top = 40
        Width = 400
        Height = 26
        Caption = 
          'Selecione abaixo os tipos de arquivo (linguagens de programa'#231#227'o ' +
          'e outros tipos de arquivo texto) que voc'#234' quer que o Notes supor' +
          'te neste profile'
        Transparent = True
        WordWrap = True
      end
      object chlbFiletypes: TCheckListBox
        Left = 48
        Top = 96
        Width = 281
        Height = 169
        ItemHeight = 13
        TabOrder = 0
      end
      object btSelAll: TButton
        Left = 344
        Top = 104
        Width = 121
        Height = 25
        Caption = '&Selecionar Todos'
        TabOrder = 1
        OnClick = btSelAllClick
      end
      object btSelNone: TButton
        Left = 344
        Top = 144
        Width = 121
        Height = 25
        Caption = 'Selecionar &Nenhum'
        TabOrder = 2
        OnClick = btSelNoneClick
      end
    end
    object tsFiletypesInstall: TTabSheet
      Caption = 'tsFiletypesInstall'
      ImageIndex = 1
      object Shape3: TShape
        Left = -8
        Top = -8
        Width = 513
        Height = 84
        Pen.Color = clBtnShadow
      end
      object Bevel2: TBevel
        Left = 4
        Top = 276
        Width = 484
        Height = 9
        Shape = bsBottomLine
      end
      object Label3: TLabel
        Left = 48
        Top = 40
        Width = 309
        Height = 13
        Caption = 'O Notes est'#225' instalando os tipos de arquivo que voc'#234' selecionou'
        Transparent = True
        WordWrap = True
      end
      object Label4: TLabel
        Left = 24
        Top = 16
        Width = 183
        Height = 13
        Caption = 'Instalando Tipos de arquivo'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object laFiletypes: TLabel
        Left = 48
        Top = 136
        Width = 51
        Height = 13
        Caption = 'laFiletypes'
      end
      object pbFiletypes: TProgressBar
        Left = 48
        Top = 160
        Width = 380
        Height = 22
        TabOrder = 0
      end
    end
    object tsEmulationConfig: TTabSheet
      Caption = 'tsEmulationConfig'
      ImageIndex = 3
      object Shape4: TShape
        Left = -8
        Top = -8
        Width = 513
        Height = 84
        Pen.Color = clBtnShadow
      end
      object Label7: TLabel
        Left = 24
        Top = 16
        Width = 70
        Height = 13
        Caption = 'Emula'#231#245'es'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label8: TLabel
        Left = 48
        Top = 40
        Width = 341
        Height = 26
        Caption = 
          'O Notes pode configurar o seu profile para emular caracter'#237'stica' +
          's do editor/IDE que voc'#234' est'#225' acostumado a usar. Selecione uma e' +
          'mula'#231#227'o:'
        Transparent = True
        WordWrap = True
      end
      object Bevel4: TBevel
        Left = 4
        Top = 276
        Width = 484
        Height = 9
        Shape = bsBottomLine
      end
      object lbEmulations: TListBox
        Left = 48
        Top = 96
        Width = 380
        Height = 161
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object tsEmulationInstall: TTabSheet
      Caption = 'tsEmulationInstall'
      ImageIndex = 4
      object Shape5: TShape
        Left = -8
        Top = -8
        Width = 513
        Height = 84
        Pen.Color = clBtnShadow
      end
      object Label9: TLabel
        Left = 24
        Top = 16
        Width = 137
        Height = 13
        Caption = 'Instalando Emula'#231#227'o'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object Label10: TLabel
        Left = 48
        Top = 40
        Width = 272
        Height = 13
        Caption = 'O Notes est'#225' instalando a emula'#231#227'o que voc'#234' selecionou'
        Transparent = True
        WordWrap = True
      end
      object laEmulations: TLabel
        Left = 48
        Top = 136
        Width = 51
        Height = 13
        Caption = 'laFiletypes'
      end
      object Bevel5: TBevel
        Left = 4
        Top = 276
        Width = 484
        Height = 9
        Shape = bsBottomLine
      end
      object ProgressBar1: TProgressBar
        Left = 48
        Top = 160
        Width = 380
        Height = 22
        TabOrder = 0
      end
    end
  end
  object btCancel: TButton
    Left = 272
    Top = 288
    Width = 100
    Height = 25
    Caption = '&Cancelar'
    TabOrder = 1
    OnClick = btCancelClick
  end
  object btForward: TButton
    Left = 384
    Top = 288
    Width = 100
    Height = 25
    Caption = '&Avan'#231'ar >'
    TabOrder = 2
    OnClick = btForwardClick
  end
end
