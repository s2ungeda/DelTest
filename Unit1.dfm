object FrmPriceTable: TFrmPriceTable
  Left = 0
  Top = 0
  Caption = 'Price Table'
  ClientHeight = 604
  ClientWidth = 845
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 845
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    ExplicitLeft = 288
    ExplicitTop = 120
    ExplicitWidth = 185
    object plTopLeft: TPanel
      Left = 0
      Top = 0
      Width = 417
      Height = 41
      Align = alLeft
      Caption = 'plTopLeft'
      TabOrder = 0
    end
    object plTopClient: TPanel
      Left = 417
      Top = 0
      Width = 428
      Height = 41
      Align = alClient
      Caption = 'plTopClient'
      TabOrder = 1
      ExplicitLeft = 552
      ExplicitTop = 16
      ExplicitWidth = 185
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 417
    Height = 544
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    ExplicitLeft = -6
    ExplicitTop = 47
    object plLeftTop: TPanel
      Left = 0
      Top = 0
      Width = 417
      Height = 257
      Align = alTop
      Caption = 'plLeftTop'
      TabOrder = 0
    end
    object plLeftClient: TPanel
      Left = 0
      Top = 257
      Width = 417
      Height = 287
      Align = alClient
      TabOrder = 1
      ExplicitLeft = 184
      ExplicitTop = 352
      ExplicitWidth = 185
      ExplicitHeight = 41
    end
  end
  object plClient: TPanel
    Left = 417
    Top = 41
    Width = 428
    Height = 544
    Align = alClient
    Caption = 'plClient'
    TabOrder = 2
    ExplicitLeft = 624
    ExplicitTop = 152
    ExplicitWidth = 185
    ExplicitHeight = 41
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 585
    Width = 845
    Height = 19
    Panels = <>
    ExplicitLeft = 256
    ExplicitTop = 568
    ExplicitWidth = 0
  end
end
