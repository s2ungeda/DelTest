object FrmDnwStates: TFrmDnwStates
  Left = 0
  Top = 0
  Caption = #51077#52636#44552#54788#54889
  ClientHeight = 411
  ClientWidth = 1030
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 392
    Width = 1030
    Height = 19
    Panels = <>
  end
  object plLeft: TPanel
    Left = 0
    Top = 0
    Width = 1030
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    Caption = 'plLeft'
    TabOrder = 1
    object plLeftTop: TPanel
      Left = 0
      Top = 0
      Width = 1030
      Height = 32
      Align = alTop
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 0
      DesignSize = (
        1030
        32)
      object Refresh: TButton
        Left = 941
        Top = 4
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = RefreshClick
      end
      object cbAuto: TCheckBox
        Left = 791
        Top = 7
        Width = 41
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Auto'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbAutoClick
      end
      object edtSec: TLabeledEdit
        Left = 838
        Top = 5
        Width = 39
        Height = 21
        Alignment = taRightJustify
        Anchors = [akRight, akBottom]
        EditLabel.Width = 54
        EditLabel.Height = 13
        EditLabel.Caption = '('#45800#50948' : '#52488' )'
        LabelPosition = lpRight
        NumbersOnly = True
        TabOrder = 2
        Text = '10'
      end
      object btnSort: TButton
        Left = 7
        Top = 4
        Width = 42
        Height = 25
        Caption = 'Sort'
        TabOrder = 3
        OnClick = btnSortClick
      end
    end
    object plLeftClient: TPanel
      Left = 0
      Top = 32
      Width = 1030
      Height = 360
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object sgDnw: TStringGrid
        Left = 0
        Top = 0
        Width = 1030
        Height = 360
        Align = alClient
        ColCount = 13
        DefaultRowHeight = 19
        DefaultDrawing = False
        FixedCols = 0
        RowCount = 25
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
        TabOrder = 0
        OnDrawCell = sgDnwDrawCell
      end
    end
  end
  object refreshTimer: TTimer
    OnTimer = refreshTimerTimer
    Left = 784
    Top = 320
  end
end
