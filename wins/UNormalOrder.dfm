object FrmNormalOrder: TFrmNormalOrder
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #51068#48152#51452#47928
  ClientHeight = 208
  ClientWidth = 532
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
  object Panel1: TPanel
    Left = 330
    Top = 0
    Width = 202
    Height = 208
    Align = alRight
    Caption = 'Panel1'
    TabOrder = 0
    object sgHoga: TStringGrid
      Left = 1
      Top = 1
      Width = 200
      Height = 206
      Align = alClient
      ColCount = 3
      DefaultRowHeight = 19
      DefaultDrawing = False
      DoubleBuffered = True
      FixedCols = 0
      RowCount = 10
      FixedRows = 0
      ParentDoubleBuffered = False
      ParentShowHint = False
      ScrollBars = ssNone
      ShowHint = False
      TabOrder = 0
      OnDrawCell = sgHogaDrawCell
      OnMouseDown = sgHogaMouseDown
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 330
    Height = 208
    Align = alLeft
    TabOrder = 1
    object edtQty: TLabeledEdit
      Left = 46
      Top = 31
      Width = 65
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = #49688#47049
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object edtPrice: TLabeledEdit
      Left = 46
      Top = 58
      Width = 94
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = #45800#44032
      LabelPosition = lpLeft
      TabOrder = 1
    end
    object cbExKind: TComboBox
      Left = 4
      Top = 4
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 2
      Text = #48727#50040
      OnChange = cbExKindChange
      Items.Strings = (
        #48148#51060#45240#49828
        #50629#48708#53944
        #48727#50040)
    end
    object edtCode: TEdit
      Left = 75
      Top = 6
      Width = 76
      Height = 21
      TabOrder = 3
      OnKeyDown = edtCodeKeyDown
    end
    object rbBuy: TRadioButton
      Left = 157
      Top = 10
      Width = 46
      Height = 17
      Caption = #47588#49688
      Checked = True
      TabOrder = 4
      TabStop = True
      OnClick = rbBuyClick
    end
    object rbSell: TRadioButton
      Left = 157
      Top = 33
      Width = 46
      Height = 17
      Caption = #47588#46020
      TabOrder = 5
      OnClick = rbSellClick
    end
    object cbReduce: TCheckBox
      Left = 157
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Reduct-Only'
      TabOrder = 6
    end
    object btnOrder: TButton
      Left = 216
      Top = 11
      Width = 109
      Height = 39
      Caption = #47588#49688' '#51452#47928
      TabOrder = 7
      OnClick = btnOrderClick
    end
    object sgBal: TStringGrid
      Left = 4
      Top = 135
      Width = 320
      Height = 63
      ColCount = 4
      DefaultRowHeight = 19
      FixedCols = 0
      RowCount = 3
      FixedRows = 0
      TabOrder = 8
      ColWidths = (
        59
        96
        57
        98)
    end
    object Button1: TButton
      Left = 4
      Top = 106
      Width = 75
      Height = 25
      Caption = #51092#44256#51312#54924
      TabOrder = 9
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 85
      Top = 106
      Width = 75
      Height = 25
      Caption = #51452#47928#51312#54924
      TabOrder = 10
      OnClick = Button2Click
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 280
    Top = 96
  end
end
