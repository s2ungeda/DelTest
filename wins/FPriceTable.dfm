object FrmPriceTable: TFrmPriceTable
  Left = 549
  Top = 75
  Caption = #44608#54532#54788#54889#54364
  ClientHeight = 525
  ClientWidth = 1009
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
    Top = 506
    Width = 1009
    Height = 19
    Panels = <>
  end
  object plLeft: TPanel
    Left = 0
    Top = 0
    Width = 1009
    Height = 506
    Align = alClient
    BevelOuter = bvNone
    Caption = 'plLeft'
    TabOrder = 1
    object plLeftClient: TPanel
      Left = 0
      Top = 0
      Width = 1009
      Height = 506
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object sgKimp: TStringGrid
        Left = 0
        Top = 0
        Width = 1009
        Height = 506
        Align = alClient
        ColCount = 13
        DefaultRowHeight = 19
        DefaultDrawing = False
        FixedCols = 0
        RowCount = 25
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
        ParentFont = False
        PopupMenu = PopupMenu1
        TabOrder = 0
        OnDrawCell = sgKimpDrawCell
        OnKeyDown = sgKimpKeyDown
        OnMouseDown = sgKimpMouseDown
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 368
    Top = 216
    object N1: TMenuItem
      Caption = #49444#51221
      OnClick = N1Click
    end
  end
end
