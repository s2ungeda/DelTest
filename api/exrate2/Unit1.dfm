object FrmExRate: TFrmExRate
  Left = 0
  Top = 0
  Caption = 'ExRate'
  ClientHeight = 86
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 105
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 8
    Top = 34
    Width = 273
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 8
    Top = 60
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label4: TLabel
    Left = 136
    Top = 8
    Width = 34
    Height = 12
    Caption = 'Label1'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clPurple
    Font.Height = 12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Button1: TButton
    Left = 216
    Top = 3
    Width = 65
    Height = 25
    Caption = #49704#44592#44592
    TabOrder = 0
    OnClick = Button1Click
  end
  object pthnEngine: TPythonEngine
    Left = 24
    Top = 240
  end
  object RtnValue: TPythonDelphiVar
    Engine = pthnEngine
    Module = '__main__'
    VarName = 'result'
    OnSetData = RtnValueSetData
    Left = 80
    Top = 240
  end
  object EndValue: TPythonDelphiVar
    Engine = pthnEngine
    Module = '__main__'
    VarName = 'EndWin'
    OnSetData = EndValueSetData
    Left = 144
    Top = 240
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 208
    Top = 240
  end
  object PopupMenu1: TPopupMenu
    Left = 288
    Top = 241
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
  object TrayIcon1: TTrayIcon
    PopupMenu = PopupMenu1
    Visible = True
    Left = 256
    Top = 241
  end
end
