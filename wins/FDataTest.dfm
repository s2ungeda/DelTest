object FrmTest: TFrmTest
  Left = 0
  Top = 0
  Caption = 'TEST'
  ClientHeight = 665
  ClientWidth = 593
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 15
    Width = 69
    Height = 13
    Caption = #51077#52636#44552#53580#49828#53944' '
  end
  object edtCode: TEdit
    Left = 160
    Top = 10
    Width = 49
    Height = 21
    TabOrder = 0
    Text = 'BTC'
  end
  object cbDpst: TCheckBox
    Left = 239
    Top = 10
    Width = 65
    Height = 17
    Caption = #51077#44552#51221#51648
    TabOrder = 1
  end
  object cbWd: TCheckBox
    Left = 310
    Top = 10
    Width = 65
    Height = 17
    Caption = #52636#44552#51221#51648
    TabOrder = 2
  end
  object Button1: TButton
    Left = 381
    Top = 8
    Width = 75
    Height = 25
    Caption = #53580#49828#53944
    TabOrder = 3
    OnClick = Button1Click
  end
  object ComboBox1: TComboBox
    Left = 91
    Top = 10
    Width = 63
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 4
    Text = #50629#48708#53944
    Items.Strings = (
      #48148#51060#45240#49828
      #50629#48708#53944
      #48727#50040)
  end
  object Panel1: TPanel
    Left = 8
    Top = 206
    Width = 577
    Height = 194
    Caption = 'Panel1'
    TabOrder = 5
    object Label3: TLabel
      Left = 28
      Top = 12
      Width = 41
      Height = 13
      Caption = 'KIP '#47196#44536
    end
    object ComboBox3: TComboBox
      Left = 82
      Top = 7
      Width = 63
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = #50629#48708#53944
      Items.Strings = (
        #50629#48708#53944
        #48727#50040)
    end
    object Edit2: TEdit
      Left = 151
      Top = 7
      Width = 49
      Height = 21
      TabOrder = 1
      Text = 'BTC'
    end
    object CheckBox4: TCheckBox
      Left = 230
      Top = 9
      Width = 50
      Height = 17
      Caption = #48372#44592
      TabOrder = 2
      OnClick = CheckBox4Click
    end
    object mKip: TMemo
      Left = 8
      Top = 32
      Width = 553
      Height = 153
      Lines.Strings = (
        '')
      ScrollBars = ssBoth
      TabOrder = 3
    end
    object 지우기: TButton
      Left = 286
      Top = 5
      Width = 51
      Height = 21
      Caption = #51648#50864#44592
      TabOrder = 4
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 406
    Width = 577
    Height = 194
    Caption = 'Panel1'
    TabOrder = 6
    object Label2: TLabel
      Left = 28
      Top = 12
      Width = 49
      Height = 13
      Caption = 'WCD '#47196#44536
    end
    object ComboBox2: TComboBox
      Left = 82
      Top = 7
      Width = 63
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = #50629#48708#53944
      Items.Strings = (
        #50629#48708#53944
        #48727#50040)
    end
    object Edit1: TEdit
      Left = 151
      Top = 7
      Width = 49
      Height = 21
      TabOrder = 1
      Text = 'BTC'
    end
    object CheckBox3: TCheckBox
      Tag = 1
      Left = 230
      Top = 9
      Width = 50
      Height = 17
      Caption = #48372#44592
      TabOrder = 2
      OnClick = CheckBox4Click
    end
    object mWcd: TMemo
      Left = 8
      Top = 32
      Width = 553
      Height = 153
      Lines.Strings = (
        '')
      ScrollBars = ssBoth
      TabOrder = 3
    end
    object Button2: TButton
      Tag = 1
      Left = 286
      Top = 5
      Width = 51
      Height = 21
      Caption = #51648#50864#44592
      TabOrder = 4
      OnClick = Button2Click
    end
  end
  object cbBT: TCheckBox
    Tag = 2
    Left = 24
    Top = 56
    Width = 49
    Height = 17
    Caption = #48727#50040
    TabOrder = 7
    OnClick = cbBTClick
  end
  object plBT: TPanel
    Left = 84
    Top = 49
    Width = 493
    Height = 32
    TabOrder = 8
    Visible = False
    object lbBT: TLabel
      Left = 8
      Top = 9
      Width = 3
      Height = 13
    end
    object btnBTDiscon: TButton
      Tag = 2
      Left = 225
      Top = 6
      Width = 49
      Height = 22
      Caption = #51333#47308
      TabOrder = 0
      OnClick = btnBTDisconClick
    end
    object btnBTCon: TButton
      Tag = 2
      Left = 289
      Top = 6
      Width = 49
      Height = 22
      Caption = #51217#49549
      TabOrder = 1
      OnClick = btnBTConClick
    end
    object btnBTSub: TButton
      Tag = 2
      Left = 358
      Top = 6
      Width = 49
      Height = 22
      Caption = 'Sub'
      TabOrder = 2
      OnClick = btnBTSubClick
    end
    object btnBTunsub: TButton
      Tag = 2
      Left = 430
      Top = 6
      Width = 49
      Height = 22
      Caption = 'UnSub'
      TabOrder = 3
      OnClick = btnBTunsubClick
    end
  end
  object cbUP: TCheckBox
    Tag = 1
    Left = 24
    Top = 94
    Width = 49
    Height = 17
    Caption = #50629#48708#53944
    TabOrder = 9
    OnClick = cbBTClick
  end
  object plUP: TPanel
    Left = 84
    Top = 87
    Width = 493
    Height = 32
    TabOrder = 10
    Visible = False
    object lbUp: TLabel
      Left = 8
      Top = 9
      Width = 3
      Height = 13
    end
    object btnUpDiscon: TButton
      Tag = 1
      Left = 225
      Top = 6
      Width = 49
      Height = 22
      Caption = #51333#47308
      TabOrder = 0
      OnClick = btnBTDisconClick
    end
    object btnUpcon: TButton
      Tag = 1
      Left = 289
      Top = 6
      Width = 49
      Height = 22
      Caption = #51217#49549
      TabOrder = 1
      OnClick = btnBTConClick
    end
    object btnUpsub: TButton
      Tag = 1
      Left = 358
      Top = 6
      Width = 49
      Height = 22
      Caption = 'Sub'
      TabOrder = 2
      OnClick = btnBTSubClick
    end
    object btnUpunsub: TButton
      Tag = 1
      Left = 430
      Top = 6
      Width = 49
      Height = 22
      Caption = 'UnSub'
      TabOrder = 3
    end
  end
  object cbBN: TCheckBox
    Left = 24
    Top = 132
    Width = 49
    Height = 17
    Caption = #48148#51060#45240
    TabOrder = 11
    OnClick = cbBTClick
  end
  object Panel3: TPanel
    Left = 84
    Top = 125
    Width = 493
    Height = 32
    TabOrder = 12
    Visible = False
    object lbBN: TLabel
      Left = 8
      Top = 9
      Width = 3
      Height = 13
    end
    object Button3: TButton
      Left = 225
      Top = 6
      Width = 49
      Height = 22
      Caption = #51333#47308
      TabOrder = 0
      OnClick = btnBTDisconClick
    end
    object Button4: TButton
      Left = 289
      Top = 6
      Width = 49
      Height = 22
      Caption = #51217#49549
      TabOrder = 1
      OnClick = btnBTConClick
    end
    object Button5: TButton
      Left = 358
      Top = 6
      Width = 49
      Height = 22
      Caption = 'Sub'
      TabOrder = 2
      OnClick = btnBTSubClick
    end
    object Button6: TButton
      Left = 430
      Top = 6
      Width = 49
      Height = 22
      Caption = 'UnSub'
      TabOrder = 3
      OnClick = Button6Click
    end
  end
  object Panel4: TPanel
    Left = 84
    Top = 163
    Width = 493
    Height = 32
    TabOrder = 13
    Visible = False
    object Label4: TLabel
      Left = 8
      Top = 9
      Width = 3
      Height = 13
    end
    object Button7: TButton
      Tag = 3
      Left = 225
      Top = 6
      Width = 49
      Height = 22
      Caption = #51333#47308
      TabOrder = 0
      OnClick = Button7Click
    end
    object Button8: TButton
      Tag = 3
      Left = 289
      Top = 6
      Width = 49
      Height = 22
      Caption = #51217#49549
      TabOrder = 1
      OnClick = Button8Click
    end
    object Button9: TButton
      Tag = 3
      Left = 358
      Top = 6
      Width = 49
      Height = 22
      Caption = 'Sub'
      TabOrder = 2
      Visible = False
      OnClick = btnBTSubClick
    end
    object Button10: TButton
      Tag = 3
      Left = 430
      Top = 6
      Width = 49
      Height = 22
      Caption = 'UnSub'
      TabOrder = 3
      Visible = False
      OnClick = Button6Click
    end
  end
  object CheckBox1: TCheckBox
    Tag = 3
    Left = 25
    Top = 171
    Width = 49
    Height = 17
    Caption = #48148'Fut'
    TabOrder = 14
    OnClick = cbBTClick
  end
  object kipTimer: TTimer
    Enabled = False
    OnTimer = kipTimerTimer
    Left = 216
    Top = 287
  end
  object wcdTimer: TTimer
    Enabled = False
    OnTimer = wcdTimerTimer
    Left = 328
    Top = 311
  end
  object wsTimer: TTimer
    OnTimer = wsTimerTimer
    Left = 528
    Top = 455
  end
end
