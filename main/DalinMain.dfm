object FrmDalinMain: TFrmDalinMain
  Left = 0
  Top = 0
  HelpType = htKeyword
  Caption = 'Dalin'
  ClientHeight = 68
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = DataModule1.MainMenu1
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object stsBar: TStatusBar
    Left = 0
    Top = 49
    Width = 276
    Height = 19
    Panels = <
      item
        Width = 60
      end
      item
        Width = 80
      end
      item
        Style = psOwnerDraw
        Width = 50
      end>
    OnDrawPanel = stsBarDrawPanel
    ExplicitTop = 16
  end
  object Panel1: TPanel
    Left = 8
    Top = 200
    Width = 177
    Height = 65
    TabOrder = 1
    Visible = False
    object edtExInterval: TLabeledEdit
      Left = 56
      Top = 8
      Width = 41
      Height = 21
      EditLabel.Width = 44
      EditLabel.Height = 13
      EditLabel.Caption = #54872#50984#51452#44592
      LabelPosition = lpLeft
      NumbersOnly = True
      TabOrder = 0
      Text = '10'
    end
    object Button1: TButton
      Left = 111
      Top = 8
      Width = 50
      Height = 22
      Caption = #51201#50857
      TabOrder = 1
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 87
      Top = 36
      Width = 74
      Height = 21
      TabOrder = 2
    end
    object cbManual: TCheckBox
      Left = 16
      Top = 40
      Width = 65
      Height = 17
      Caption = #49688#46041#54872#50984
      TabOrder = 3
    end
  end
  object QryTimer: TTimer
    Enabled = False
    OnTimer = QryTimerTimer
    Left = 304
    Top = 96
  end
end
