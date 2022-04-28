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
  OnActivate = FormActivate
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
        Top = 32
        Width = 1009
        Height = 474
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
      object plLeftTop: TPanel
        Left = 0
        Top = 0
        Width = 1009
        Height = 32
        Align = alTop
        BevelOuter = bvLowered
        ParentBackground = False
        TabOrder = 1
        DesignSize = (
          1009
          32)
        object Refresh: TButton
          Left = 941
          Top = 4
          Width = 47
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = #51201#50857
          TabOrder = 0
          OnClick = RefreshClick
        end
        object cbAuto: TCheckBox
          Left = 783
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
          Left = 830
          Top = 5
          Width = 39
          Height = 21
          Alignment = taRightJustify
          Anchors = [akRight, akBottom]
          EditLabel.Width = 65
          EditLabel.Height = 13
          EditLabel.Caption = '('#45800#50948' : '#48128#47532' )'
          LabelPosition = lpRight
          NumbersOnly = True
          TabOrder = 2
          Text = '200'
        end
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
  object refreshTimer: TTimer
    Enabled = False
    OnTimer = refreshTimerTimer
    Left = 208
    Top = 144
  end
end
