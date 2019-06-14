//
//
//  UNIT      : Contains TAssuranceSelector Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 25/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAssuranceSelector;

interface
uses
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  CheckLst,
  UAbstractObject,
  UAbstractYRCData,
  UAbstractComponent,
  UDataEditComponent;

type
  TAssuranceSelector = class(TAbstractForm)
  protected
    FAssuranceSelector:TCheckListBox;
    FBtnPanel,
    FEdtPanel : TPanel;
    FBtnAdd,
    FBtnDelete,
    FBtnOk,
    FBtnCancel: TButton;
    FEdtAssuranceInterval : TEdit;
    FLblAssuranceInterval : TLabel;
    FAssuranceInterval : integer;
    function GetAssuranceInterval: integer;
    procedure SetAssuranceInterval(const AValue: integer);
    procedure CreateMemberObjects;override;
    procedure CenterControls;
  public
    function LanguageHasChanged: boolean; override;
    function GetCurrentAssuranceInterval : integer;
    procedure Resize; override;
    procedure PopulateAssurance(AYearValues, ASavedValues:TAssuranceIntervalArray);
    procedure ReadAssuranceSaved(var ASavedValues: TAssuranceIntervalArray; AYearValues : TAssuranceIntervalArray);
    property AssuranceSelector : TCheckListBox read FAssuranceSelector;
    property BtnAdd    : TButton read FBtnAdd;
    property BtnDelete : TButton read FBtnDelete;
    property EdtAssuranceInterval : TEdit read FEdtAssuranceInterval;
    property AssuranceInterval : integer read GetAssuranceInterval write SetAssuranceInterval;
  end;

implementation

uses
  Math,
  SysUtils,
  UErrorHandlingOperations;

{ TAssuranceSelector }

procedure TAssuranceSelector.CreateMemberObjects;
const OPNAME = 'TAssuranceSelector.CreateMemberObjects';
begin
  inherited;
  try
    FAssuranceSelector           := TCheckListBox.Create(Self);
    FBtnPanel                    := TPanel.Create(Self);
    FEdtPanel                    := TPanel.Create(Self);

    FLblAssuranceInterval        := TLabel.Create(Self);
    FEdtAssuranceInterval        := TEdit.Create(Self);

    FBtnAdd                      := TButton.Create(Self);
    FBtnDelete                   := TButton.Create(Self);

    FBtnOk                       := TButton.Create(Self);
    FBtnCancel                   := TButton.Create(Self);

    FAssuranceSelector.Parent    := Self;
    FBtnPanel.Parent             := Self;
    FEdtPanel.Parent             := Self;
    FLblAssuranceInterval.Parent := FEdtPanel;
    FEdtAssuranceInterval.Parent := FEdtPanel;
    FBtnAdd.Parent               := FBtnPanel;
    FBtnDelete.Parent            := FBtnPanel;
    FBtnOk.Parent                := FBtnPanel;
    FBtnCancel.Parent            := FBtnPanel;

    FBtnAdd.Enabled              := True;
    FBtnDelete.Enabled           := False;

    FBtnAdd.ModalResult          := mrNone;
    FBtnDelete.ModalResult       := mrNone;
    FBtnOk.ModalResult           := mrOk;
    FBtnCancel.ModalResult       := mrCancel;

    FBtnAdd.OnClick              := nil;
    FBtnDelete.OnClick           := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAssuranceSelector.Resize;
const OPNAME = 'TAssuranceSelector.Resize';
begin
  inherited;
  try
    FBtnPanel.Align            := alNone;
    FEdtPanel.Align            := alNone;
    FAssuranceSelector.Align   := alNone;

    FBtnPanel.Align            := alBottom;
    FEdtPanel.Align            := alBottom;
    FAssuranceSelector.Align   := alClient;

    CenterControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAssuranceSelector.CenterControls;
const OPNAME = 'TAssuranceSelector.CenterControls';
var
  LWidth: integer;
begin
  try
    LWidth                     := FBtnPanel.ClientWidth div 2;
    LWidth                     := LWidth - FBtnOk.Width - FBtnCancel.Width;
    LWidth                     := Max(1,LWidth);
    LWidth                     := LWidth div 3;

    FLblAssuranceInterval.Left := 5;
    FEdtAssuranceInterval.Left := FLblAssuranceInterval.Left + FLblAssuranceInterval.Width + 5;

    FBtnAdd.Left               := 5;
    FBtnDelete.Left            := FBtnAdd.Left + FBtnAdd.Width + 5;

    FBtnOk.Left                := (FBtnPanel.ClientWidth div 2) + LWidth;
    FBtnCancel.Left            :=  FBtnPanel.ClientWidth - LWidth - FBtnCancel.Width;

    FLblAssuranceInterval.Top  := (FEdtPanel.Height - FLblAssuranceInterval.Height) div 2;
    FEdtAssuranceInterval.Top  := (FEdtPanel.Height - FEdtAssuranceInterval.Height) div 2;

    FBtnAdd.Top                := (FBtnPanel.Height - FBtnAdd.Height) div 2;
    FBtnDelete.Top             := (FBtnPanel.Height - FBtnDelete.Height) div 2;

    FBtnOk.Top                 := (FBtnPanel.Height - FBtnOk.Height) div 2;
    FBtnCancel.Top             := (FBtnPanel.Height - FBtnCancel.Height) div 2;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAssuranceSelector.LanguageHasChanged: boolean;
const OPNAME = 'TAssuranceSelector.LanguageHasChanged';
var
  LAssuranceInterval : integer;
begin
  Result := Inherited LanguageHasChanged;
  try
    Self.Caption := FAppModules.Language.GetString('YieldReliability.FormCaption');
    FEdtPanel.Caption  := '';
    FLblAssuranceInterval.Caption := FAppModules.Language.GetString('LabelText.AssuranceInterval');
    LAssuranceInterval := GetCurrentAssuranceInterval;
    if (LAssuranceInterval > 0) then
      FEdtAssuranceInterval.Text := IntToStr(LAssuranceInterval)
    else
      FEdtAssuranceInterval.Text := '';
    FBtnPanel.Caption  := '';
    FBtnAdd.Caption    := FAppModules.Language.GetString('ButtonCaption.Add');
    FBtnDelete.Caption := FAppModules.Language.GetString('ButtonCaption.Delete');
    FBtnOk.Caption     := FAppModules.Language.GetString('ButtonCaption.OK');
    FBtnCancel.Caption := FAppModules.Language.GetString('ButtonCaption.Cancel');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAssuranceSelector.PopulateAssurance(AYearValues, ASavedValues: TAssuranceIntervalArray);
const OPNAME = 'TAssuranceSelector.PopulateAssurance';
var
  LIndex: integer;
begin
  try
    FAssuranceSelector.Items.Clear;
    for LIndex := 0 to Length(AYearValues) - 1 do
      FAssuranceSelector.Items.AddObject(Format('1/%d years', [AYearValues[LIndex]]), TObject(AYearValues[LIndex]));

    for LIndex := Low(ASavedValues) to High(ASavedValues) do
       FAssuranceSelector.Checked[LIndex] := (ASavedValues[LIndex] > 0);
    FAssuranceSelector.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAssuranceSelector.ReadAssuranceSaved(var ASavedValues: TAssuranceIntervalArray; AYearValues : TAssuranceIntervalArray);
const OPNAME = 'TAssuranceSelector.ReadAssuranceSaved';
var
  LIndex: integer;
begin
  try
    SetLength(ASavedValues, Length(AYearValues));
    for LIndex := Low(AYearValues) to High(AYearValues) do
      ASavedValues[LIndex] := AYearValues[LIndex];
    {ASavedValues[0] := 5;
    ASavedValues[1] := 10;
    ASavedValues[2] := 20;
    ASavedValues[3] := 50;
    ASavedValues[4] := 100;
    ASavedValues[5] := 200;
    ASavedValues[6] := 500;}
    for LIndex := Low(ASavedValues) to High(ASavedValues) do
       if not FAssuranceSelector.Checked[LIndex] then
         ASavedValues[LIndex] := 0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAssuranceSelector.GetAssuranceInterval: integer;
const OPNAME = 'TAssuranceSelector.GetAssuranceInterval';
begin
  Result := 0;
  try
    FAssuranceInterval := StrToInt(FEdtAssuranceInterval.Text);
    Result             := FAssuranceInterval;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAssuranceSelector.SetAssuranceInterval(const AValue: integer);
const OPNAME = 'TAssuranceSelector.SetAssuranceInterval';
begin
  try
    FAssuranceInterval         := AValue;
    FEdtAssuranceInterval.Text := IntToStr(FAssuranceInterval);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAssuranceSelector.GetCurrentAssuranceInterval: integer;
const OPNAME = 'TAssuranceSelector.GetCurrentAssuranceInterval';
begin
  Result := 0;
  try
    if (FAssuranceSelector.ItemIndex >= 0) then
      Result := Integer(FAssuranceSelector.Items.Objects[FAssuranceSelector.ItemIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
