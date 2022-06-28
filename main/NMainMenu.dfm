object DataModule1: TDataModule1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 172
  Width = 432
  object MainMenu1: TMainMenu
    Left = 112
    Top = 88
    object nFile: TMenuItem
      Caption = #54028#51068
      object N1: TMenuItem
        Caption = #51333#47308
      end
    end
    object nAccount: TMenuItem
      Caption = #44144#47000#49548
      object nExchange: TMenuItem
        Caption = #51077#52636#44552#54788#54889#54364
        OnClick = nExchangeClick
      end
    end
    object nOrder: TMenuItem
      Caption = #51452#47928
      object N5: TMenuItem
        Caption = #51452#47928#52285
        OnClick = N5Click
      end
      object N3: TMenuItem
        Tag = 1
        Caption = #51452#47928#47532#49828#53944
        OnClick = N5Click
      end
      object N4: TMenuItem
        Tag = 2
        Caption = #44228#51340#51221#48372
        OnClick = N5Click
      end
    end
    object nQuote: TMenuItem
      Caption = #49884#49464
      object Kimp1: TMenuItem
        Caption = 'Kimp '#54788#54889#54364
        OnClick = Kimp1Click
      end
      object N6: TMenuItem
        Tag = 1
        Caption = #49884#49464#47784#45768#53552#47553
        OnClick = Kimp1Click
      end
      object N2: TMenuItem
        Tag = 2
        Caption = #45824#54364' KIP'
        OnClick = Kimp1Click
      end
      object MainWDC1: TMenuItem
        Tag = 3
        Caption = #45824#54364' WCD'
        OnClick = Kimp1Click
      end
      object test2: TMenuItem
        Tag = 4
        Caption = 'test'
        OnClick = Kimp1Click
      end
    end
  end
end
