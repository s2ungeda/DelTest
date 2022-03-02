object FrmDalinMain: TFrmDalinMain
  Left = 0
  Top = 0
  HelpType = htKeyword
  Caption = 'Dalin'
  ClientHeight = 80
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = DataModule1.MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object stsBar: TStatusBar
    Left = 0
    Top = 61
    Width = 342
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
    ExplicitTop = 81
  end
  object QryTimer: TTimer
    Enabled = False
    OnTimer = QryTimerTimer
    Left = 160
    Top = 8
  end
end
