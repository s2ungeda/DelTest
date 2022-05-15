object FrmExRate: TFrmExRate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ExRate'
  ClientHeight = 116
  ClientWidth = 334
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
  object sbTimer: TSpeedButton
    Left = 89
    Top = 39
    Width = 48
    Height = 22
    AllowAllUp = True
    GroupIndex = 3
    Down = True
    Caption = 'Start'
    Flat = True
    OnClick = sbTimerClick
  end
  object lbPrice: TLabel
    Left = 8
    Top = 72
    Width = 3
    Height = 13
  end
  object lbLog: TLabel
    Left = 8
    Top = 92
    Width = 3
    Height = 13
  end
  object lbTimer: TLabel
    Left = 152
    Top = 42
    Width = 3
    Height = 13
  end
  object xas: TXASession
    Left = 416
    Top = 168
    Width = 0
    Height = 0
    OnLogin = xasLogin
    OnLogout = xasLogout
    OnDisconnect = xasDisconnect
    ControlData = {000A0000D8130000D8130000}
  end
  object Button1: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = #51217#49549
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 251
    Top = 8
    Width = 75
    Height = 25
    Caption = #51217#49549#51333#47308
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = #47196#44536#51064
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 170
    Top = 8
    Width = 75
    Height = 25
    Caption = #51312#54924
    TabOrder = 4
    OnClick = Button4Click
  end
  object xaq: TXAQuery
    Left = 504
    Top = 232
    Width = 0
    Height = 0
    OnReceiveData = xaqReceiveData
    OnReceiveMessage = xaqReceiveMessage
    OnReceiveSearchRealData = xaqReceiveSearchRealData
    ControlData = {
      000A0000D8130000D813000008001C0000005200650073005C00740033003500
      310038002E007200650073000000}
  end
  object xar: TXAReal
    Left = 512
    Top = 248
    Width = 0
    Height = 0
    ControlData = {
      000A0000D8130000D813000008001C0000005200650073005C00740033003500
      310038002E007200650073000000}
  end
  object Edit1: TEdit
    Left = 8
    Top = 39
    Width = 41
    Height = 21
    NumbersOnly = True
    TabOrder = 7
    Text = '10'
  end
  object Button5: TButton
    Left = 251
    Top = 39
    Width = 75
    Height = 25
    Caption = #49704#44592#44592
    TabOrder = 8
    OnClick = Button5Click
  end
  object cbWeek: TCheckBox
    Left = 251
    Top = 70
    Width = 65
    Height = 17
    Caption = #51452#47568#51228#50808
    Checked = True
    State = cbChecked
    TabOrder = 9
  end
  object TrayIcon1: TTrayIcon
    PopupMenu = PopupMenu1
    Visible = True
    Left = 256
    Top = 88
  end
  object PopupMenu1: TPopupMenu
    Left = 288
    Top = 88
    object N2: TMenuItem
      Caption = 'Show'
      OnClick = N2Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object N4: TMenuItem
      Caption = #51333#47308
      OnClick = N4Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = Timer1Timer
    Left = 200
    Top = 80
  end
  object Timer2: TTimer
    Enabled = False
    Left = 224
    Top = 80
  end
end
