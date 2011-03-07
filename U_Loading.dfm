object F_Loading: TF_Loading
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Hitsugaya '
  ClientHeight = 66
  ClientWidth = 434
  Color = clBlue
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object L_Loading: TLabel
    Left = 8
    Top = 8
    Width = 418
    Height = 27
    Alignment = taCenter
    AutoSize = False
    BiDiMode = bdLeftToRight
    Caption = 'Checking path and software availability...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -21
    Font.Name = 'Tempus Sans ITC'
    Font.Style = [fsBold]
    ParentBiDiMode = False
    ParentFont = False
    Layout = tlCenter
  end
  object PB_Loading: TProgressBar
    Left = 8
    Top = 41
    Width = 418
    Height = 17
    ParentShowHint = False
    Step = 1
    ShowHint = False
    TabOrder = 0
  end
end
