object FrmPriceTable: TFrmPriceTable
  Left = 549
  Top = 75
  Caption = #44608#54532#54788#54889#54364
  ClientHeight = 577
  ClientWidth = 987
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 558
    Width = 987
    Height = 19
    Panels = <>
  end
  object plLeft: TPanel
    Left = 0
    Top = 0
    Width = 987
    Height = 558
    Align = alClient
    BevelOuter = bvNone
    Caption = 'plLeft'
    TabOrder = 1
    object plLeftTop: TPanel
      Left = 0
      Top = 0
      Width = 987
      Height = 60
      Align = alTop
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 0
    end
    object plLeftClient: TPanel
      Left = 0
      Top = 60
      Width = 987
      Height = 498
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object sgKimp: TStringGrid
        Left = 0
        Top = 0
        Width = 987
        Height = 498
        Align = alClient
        ColCount = 13
        DefaultRowHeight = 19
        DefaultDrawing = False
        FixedCols = 0
        RowCount = 25
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
        TabOrder = 0
        OnDrawCell = sgKimpDrawCell
        OnKeyDown = sgKimpKeyDown
        OnMouseDown = sgKimpMouseDown
      end
    end
  end
end
