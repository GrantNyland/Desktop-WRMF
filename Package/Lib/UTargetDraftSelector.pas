//
//
//  UNIT      : Contains TAbstractComponent Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 18/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTargetDraftSelector;

interface
uses
  Classes,
  VCL.Controls,
  VCL.Forms,
  Vcl.Samples.Spin,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.CheckLst,
  UAbstractObject,
  UYRCGraphDataObject,
  UAbstractComponent,
  UDataEditComponent,
  UAbstractModelData;

type
  TSelectorMode = (smSingleSelect,smMultipleSelect);
  TTargetDraftSelectionChange = procedure(ASelectionIndeces: TIntegerArray) of object;
  TTargetDraftSelector = class(TAbstractPanel)
  protected
    FName: TLabel;
    FSingleSelectorIndex : integer;
    FSelectorMode: TSelectorMode;
    FSingleSelectorLabel: TLabel;
    FSingleSelector: TComboBox;
    FMultipleSelect: TCheckListBox;
    FTargetDraftSelectionChange: TTargetDraftSelectionChange;
    procedure CreateMemberObjects; override;
    procedure SetSelectorMode(ASelectorMode: TSelectorMode);
    procedure SelectorClick(Sender: TObject);
  public
    // Overriden from Delphi.
    procedure Resize; override;
    procedure SelectAllTargetDrafts;
    function LanguageHasChanged: boolean; override;
    function PopulateTargetDraft(ATargetDrafts: array of Double): boolean;
    function SelectByIndex(AItemIndex : integer): boolean;
    property SingleSelector: TComboBox read FSingleSelector;
    property MultipleSelect: TCheckListBox read FMultipleSelect;
    property SelectorMode: TSelectorMode read FSelectorMode write SetSelectorMode;
    property OnTargetDraftSelectionChange: TTargetDraftSelectionChange read FTargetDraftSelectionChange write FTargetDraftSelectionChange;
  end;

implementation

uses
  VCL.Graphics,
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TTargetDraftSelector }

procedure TTargetDraftSelector.CreateMemberObjects;
const OPNAME = 'TTargetDraftSelector.CreateMemberObjects';
begin
  inherited;
  try
    Self.Height      := 28;
    Self.BevelOuter  := bvNone;
    OnTargetDraftSelectionChange := nil;

    FName           := TLabel.Create(Self);
    FName.Parent    := Self;
    FName.Layout    := tlCenter;
    FName.Alignment := taRightJustify;
    FName.Visible   := True;

    FSingleSelectorLabel         := TLabel.Create(Self);
    FSingleSelectorLabel.Parent  := Self;
    FSingleSelectorLabel.Visible := True;

    FSingleSelector             := TComboBox.Create(Self);
    FSingleSelector.Parent      := Self;
    FSingleSelector.Color       := clWhite;
    FSingleSelector.Visible     := False;
    FSingleSelector.Style       := csDropDownList;


    FMultipleSelect             := TCheckListBox.Create(Self);
    FMultipleSelect.Parent      := Self;
    FMultipleSelect.MultiSelect := True;
    FMultipleSelect.Color       := clBtnFace;
    FMultipleSelect.BevelKind   := bkTile;
    FMultipleSelect.BorderStyle := bsNone;
    FMultipleSelect.Color       := clBtnFace;
    FMultipleSelect.Flat        := False;
    FMultipleSelect.Visible := False;

    //FSingleSelector.Align := alClient;
    //FMultipleSelect.Align := alClient;

    FSingleSelector.OnChange      := SelectorClick;
    FMultipleSelect.OnClickCheck  := SelectorClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTargetDraftSelector.Resize;
const OPNAME = 'TTargetDraftSelector.Resize';
begin
  inherited;
  try
    FMultipleSelect.Align            := alNone;
    FMultipleSelect.Width            := 0;

    if (FSingleSelector.Visible) then
    begin
      FName.Visible            := False;
      FMultipleSelect.Visible  := False;

      FSingleSelectorLabel.Top  := 8;
      FSingleSelectorLabel.Left := 3;

      FSingleSelector.Top       := 5;
      FSingleSelector.Left      := FSingleSelectorLabel.Left + FSingleSelectorLabel.Width + 5;
      FSingleSelector.Width     := Self.ClientWidth - FSingleSelector.Left - 5;
    end;

    if(FMultipleSelect.Visible) then
    begin
      FName.Visible  := True;
      if FName.Visible then
      begin
        FName.Align  := alLeft;
        FName.Width  := (Self.ClientWidth div 9) - 5;
      end;
      FMultipleSelect.Align              := alClient;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTargetDraftSelector.PopulateTargetDraft(ATargetDrafts: array of Double): boolean;
const OPNAME = 'TTargetDraftSelector.PopulateTargetDraft';
var
  LCount: integer;
  LValue: string;
begin
  Result := False;
  try
    FSingleSelectorIndex := -1;

    FSingleSelector.Items.Clear;
    FMultipleSelect.Items.Clear;

//    FSingleSelector.Columns := Length(ATargetDrafts) + 1;
    FMultipleSelect.Columns := Length(ATargetDrafts) + 1;

    FSingleSelector.Items.Add(FAppModules.Language.GetString('TTargetDraftSelector.None'));
    FMultipleSelect.Items.Add(FAppModules.Language.GetString('TTargetDraftSelector.None'));
    for LCount := 0 to High(ATargetDrafts) do
    begin
      LValue := Trim(SmartFloatFormatForFiles(ATargetDrafts[LCount],10,6));
      if (Length(LValue) > 2) then
        if(pos('.0',LValue) = (Length(LValue) - 1)) then
          LValue := Copy(LValue,1,Length(LValue)-2);
      if(pos('.',LValue) > 0) then
        LValue := RightTrimChars(LValue,'0');

      FSingleSelector.Items.Add(LValue);
      FMultipleSelect.Items.Add(LValue);
      //FSingleSelector.Items.Add(Format('%10.3f',[ATargetDrafts[LCount]]));
      //FMultipleSelect.Items.Add(Format('%10.3f',[ATargetDrafts[LCount]]));
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTargetDraftSelector.SelectorClick(Sender: TObject);
const OPNAME = 'TTargetDraftSelector.SelectorClick';
var
  LCount,
  LIndex: integer;
  LSelectedIndeces: TIntegerArray;
begin
  try
    if Assigned(OnTargetDraftSelectionChange) then
    begin
      if (Sender is TComboBox) then
      begin
        if(FSingleSelector.ItemIndex <> FSingleSelectorIndex) then
        begin
          FSingleSelectorIndex := FSingleSelector.ItemIndex;
          SetLength(LSelectedIndeces,1);
          LSelectedIndeces[0] := FSingleSelector.ItemIndex - 1;
          OnTargetDraftSelectionChange(LSelectedIndeces);
        end;
      end
      else
      if (Sender is TCheckListBox) then
      begin
        SetLength(LSelectedIndeces,FMultipleSelect.SelCount);
        LIndex := 0;
        for LCount := 0 to FMultipleSelect.Items.Count -1 do
        begin
          if FMultipleSelect.Selected[LCount] then
          begin
            LSelectedIndeces[LIndex] := LCount - 1;
            LIndex := LIndex + 1;
          end;
        end;
        OnTargetDraftSelectionChange(LSelectedIndeces);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTargetDraftSelector.SetSelectorMode(ASelectorMode: TSelectorMode);
const OPNAME = 'TTargetDraftSelector.SetSelectorMode';
begin
  try
    case ASelectorMode of
      smSingleSelect:
        begin
          FSingleSelector.Visible := True;
          FMultipleSelect.Visible := False;
        end;
      smMultipleSelect:
        begin
          FSingleSelector.Visible := False;
          FMultipleSelect.Visible := True;
        end;
    end;//Case
//    FSingleSelectorLabel.Visible := FSingleSelector.Visible;
    Self.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTargetDraftSelector.SelectAllTargetDrafts;
const OPNAME = 'TTargetDraftSelector.SelectAllTargetDrafts';
begin
  try
    if (FSingleSelector.Items.Count > 0) then
    begin
      FSingleSelector.ItemIndex := 0;
      FSingleSelector.OnChange(FSingleSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTargetDraftSelector.LanguageHasChanged: boolean;
const OPNAME = 'TTargetDraftSelector.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FSingleSelectorLabel.Caption       := FAppModules.Language.GetString('TTargetDraftSelector.TargetDrafts');
    FName.Caption                      := FAppModules.Language.GetString('TTargetDraftSelector.TargetDrafts');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTargetDraftSelector.SelectByIndex(AItemIndex : integer): boolean;
const OPNAME = 'TTargetDraftSelector.SelectByIndex';
begin
  Result := False;
  try
    if(FSelectorMode = smSingleSelect) then
    begin
      if(FSingleSelector.Items.Count > 0) then
      begin
        if (AItemIndex < 0) then
         AItemIndex := 0;
        if (AItemIndex > (FSingleSelector.Items.Count-1)) then
          AItemIndex := FSingleSelector.Items.Count-1;
        FSingleSelector.ItemIndex := AItemIndex;
        FSingleSelector.OnChange(FSingleSelector);
        Result := True;
        {if(FSingleSelector.ItemIndex <> AItemIndex) then
        begin
          FSingleSelector.ItemIndex := AItemIndex;
          FSingleSelector.OnChange(FSingleSelector);
          Result := True;
        end;}
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
