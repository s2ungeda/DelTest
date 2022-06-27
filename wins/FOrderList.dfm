object FrmOrderList: TFrmOrderList
  Left = 0
  Top = 0
  Caption = #51452#47928#47532#49828#53944
  ClientHeight = 293
  ClientWidth = 643
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
    Left = 0
    Top = 0
    Width = 643
    Height = 25
    Align = alTop
    TabOrder = 0
    object ComboBox1: TComboBox
      Left = 5
      Top = 2
      Width = 68
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = #51204#52404
      OnChange = ComboBox1Change
      Items.Strings = (
        #51204#52404
        #48148#51060#45240#49828
        #50629#48708#53944
        #48727#50040)
    end
    object CheckBox1: TCheckBox
      Left = 88
      Top = 5
      Width = 43
      Height = 17
      Caption = #51217#49688
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Tag = 1
      Left = 145
      Top = 5
      Width = 40
      Height = 17
      Caption = #52404#44208
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object CheckBox3: TCheckBox
      Tag = 2
      Left = 200
      Top = 5
      Width = 41
      Height = 17
      Caption = #52712#49548
      TabOrder = 3
      OnClick = CheckBox1Click
    end
    object CheckBox4: TCheckBox
      Tag = 3
      Left = 250
      Top = 5
      Width = 41
      Height = 17
      Caption = #44144#48512
      TabOrder = 4
      OnClick = CheckBox1Click
    end
  end
  object sgOrder: TStringGrid
    Left = 0
    Top = 25
    Width = 643
    Height = 268
    Align = alClient
    DefaultColAlignment = taCenter
    DefaultRowHeight = 19
    DefaultDrawing = False
    FixedCols = 0
    ScrollBars = ssVertical
    TabOrder = 1
    OnDrawCell = sgOrderDrawCell
    OnMouseDown = sgOrderMouseDown
  end
  object PopupMenu1: TPopupMenu
    Left = 512
    Top = 208
    object N1: TMenuItem
      Caption = #52712#49548#51452#47928
    end
  end
end
