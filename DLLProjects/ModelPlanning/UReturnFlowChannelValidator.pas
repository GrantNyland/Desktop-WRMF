//
//
//  UNIT      : Contains TReturnFlowChannelValidator Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 14/06/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UReturnFlowChannelValidator;

interface
uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UReturnFlowChannelDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TReturnFlowChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    //FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure DoAddCorrespondingChannelClick(Sender: TObject);
    procedure DoDeleteCorrespondingChannelClick(Sender: TObject);
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;

    procedure OnGrdCorrespondingChannelSelectCell (Sender        : TObject;
                                                ACol, ARow    : Integer;
                                                var CanSelect : Boolean);
    procedure OnGrdCorrespondingChannelTopLeftChanged (Sender : TObject);
    procedure UpdateMonthlyPotentialEvap(AIndex : integer; AValue : string);
    procedure UpdateAssumedFactor(AIndex : integer;AValue : string);
    procedure UpdateCorrespondingChannel;
    procedure UpdateGaugeNumber;
    procedure UpdateMonthlyAvrgFactor;
    procedure UpdateCalibrationFactor;
    procedure UpdateMonthlyAvrgNetEvap;
    procedure UpdateRoutingConstant;
    procedure UpdateCurtailmentFactor;
    procedure UpdateMultiplicationFactor;

    procedure ValidateGaugeNumber(AReturnFlowChannel : IReturnFlowChannel);
    procedure ValidateMonthlyAvrgFactor(AReturnFlowChannel :IReturnFlowChannel);
    procedure ValidateCalibrationFactor(AReturnFlowChannel : IReturnFlowChannel);
    procedure ValidateMonthlyAvrgNetEvap(AReturnFlowChannel : IReturnFlowChannel);
    procedure ValidateRoutingConstant(AReturnFlowChannel : IReturnFlowChannel);
    procedure ValidateCurtailmentFactor(AReturnFlowChannel : IReturnFlowChannel);
    procedure ValidateMultiplicationFactor(AReturnFlowChannel : IReturnFlowChannel);
    procedure ValidatePotentialMonthlyEvap(AReturnFlowChannel : IReturnFlowChannel);

    procedure ValidateCorrespondingChannel(AReturnFlowChannel : IReturnFlowChannel);
    procedure ValidateAbstractionChannel(AReturnFlowChannel : IReturnFlowChannel);
    procedure ValidateAssumedFactor(AReturnFlowChannel : IReturnFlowChannel);

    procedure PopulateCorrespondingChannel(AReturnFlowChannel : IReturnFlowChannel);
    procedure RePopulateDataViewer;
    function GetReturnFlowChannelDialog : TReturnFlowChannelDialog;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    property ReturnFlowChannelDialog : TReturnFlowChannelDialog read GetReturnFlowChannelDialog;
    //roperty FeatureID : integer read FFeatureID write FFeatureID;
end;

implementation
uses
  SysUtils,
  VCL.Forms,
  VCL.Grids,
  UUtilities,
  UHelpContexts,
  UAbstractFileNamesObject,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations,
  UYieldModelDataObject,
  UControlCreationUtilities;
{ TReturnFlowChannelValidator }

procedure TReturnFlowChannelValidator.ClearDataViewer;
const OPNAME = 'TReturnFlowChannelValidator.ClearDataViewer';
begin
  inherited;

end;

procedure TReturnFlowChannelValidator.CreateDialog;
const OPNAME = 'TReturnFlowChannelValidator.CreateDialog';
begin
  try
    FPanel := TReturnFlowChannelDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.CreateMemberObjects;
const OPNAME = 'TReturnFlowChannelValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    CreateDialog;
    with ReturnFlowChannelDialog do
    begin
      EdtDemandCentreID.FieldProperty := FAppModules.FieldProperties.FieldProperty('DemandChannel');
      EdtDemandCentreID.OnEnter        := OnEditControlEnter;
      EdtDemandCentreID.OnExit         := OnEditControltExit;

      EdtNumOfCorrespondingChannels.FieldProperty := FAppModules.FieldProperties.FieldProperty('NumOfCorrespondingChannels');//
      EdtNumOfCorrespondingChannels.OnEnter        := OnEditControlEnter;
      EdtNumOfCorrespondingChannels.OnExit         := OnEditControltExit;

      EdtGaugeNumber.FieldProperty := FAppModules.FieldProperties.FieldProperty('ReturnFlowGaugeNumber');
      EdtGaugeNumber.OnEnter        := OnEditControlEnter;
      EdtGaugeNumber.OnExit         := OnEditControltExit;

      EdtMonthlyAvrgFactor.FieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyAvrgFactor');
      EdtMonthlyAvrgFactor.OnEnter        := OnEditControlEnter;
      EdtMonthlyAvrgFactor.OnExit         := OnEditControltExit;

      EdtCalibrationFactor.FieldProperty := FAppModules.FieldProperties.FieldProperty('CalibrationFactor');
      EdtCalibrationFactor.OnEnter        := OnEditControlEnter;
      EdtCalibrationFactor.OnExit         := OnEditControltExit;

      EdtMonthlyAvrgNetEvap.FieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyAvrgNetEvap');
      EdtMonthlyAvrgNetEvap.OnEnter        := OnEditControlEnter;
      EdtMonthlyAvrgNetEvap.OnExit         := OnEditControltExit;

      EdtRoutingConstant.FieldProperty := FAppModules.FieldProperties.FieldProperty('RoutingConstant');
      EdtRoutingConstant.OnEnter        := OnEditControlEnter;
      EdtRoutingConstant.OnExit         := OnEditControltExit;

      EdtCurtailmentFactor.FieldProperty := FAppModules.FieldProperties.FieldProperty('CurtailmentFactor');
      EdtCurtailmentFactor.OnEnter        := OnEditControlEnter;
      EdtCurtailmentFactor.OnExit         := OnEditControltExit;

      EdtMultiplicationFactor.FieldProperty := FAppModules.FieldProperties.FieldProperty('MultiplicationFactor');
      EdtMultiplicationFactor.OnEnter        := OnEditControlEnter;
      EdtMultiplicationFactor.OnExit         := OnEditControltExit;

      CbxChannel.FieldProperty := FAppModules.FieldProperties.FieldProperty('CorrespondingChannel');
      CbxChannel.OnEnter       := OnEditControlEnter;
      CbxChannel.OnChange      := OnEditControltExit;

      GrdCorrespondingChannels.OnSelectCell     := OnGrdCorrespondingChannelSelectCell;
      GrdCorrespondingChannels.OnTopLeftChanged := OnGrdCorrespondingChannelTopLeftChanged;

      BtnAddCorrespondingChannel.OnClick        := DoAddCorrespondingChannelClick;
      BtnDeleteCorrespondingChannel.OnClick     := DoDeleteCorrespondingChannelClick;
      
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.DestroyMemberObjects;
const OPNAME = 'TReturnFlowChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TReturnFlowChannelValidator.DoContextValidation';
var
  LReturnFlowChannel : IReturnFlowChannel;
  LChannel : IGeneralFlowChannel;
begin
  inherited;
  try
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByChannelNumber[Identifier];
    LReturnFlowChannel := LChannel.ReturnFlowChannel;
    if LReturnFlowChannel <> nil then
    begin
      case AValidationType of
        dvtReturnFlowChannelData :
        begin
          ValidateGaugeNumber(LReturnFlowChannel);
          ValidateMonthlyAvrgFactor(LReturnFlowChannel);
          ValidateCalibrationFactor(LReturnFlowChannel);
          ValidateMonthlyAvrgNetEvap(LReturnFlowChannel);
          ValidateRoutingConstant(LReturnFlowChannel);
          ValidateCurtailmentFactor(LReturnFlowChannel);
          ValidateMultiplicationFactor(LReturnFlowChannel);
          ValidatePotentialMonthlyEvap(LReturnFlowChannel);
          ValidateCorrespondingChannel(LReturnFlowChannel);
          ValidateAbstractionChannel(LReturnFlowChannel);
          ValidateAssumedFactor(LReturnFlowChannel);
        end;
        dvtGaugeNumber : ValidateGaugeNumber(LReturnFlowChannel);
        dvtMonthlyAvrgFactor : ValidateMonthlyAvrgFactor(LReturnFlowChannel);
        dvtCalibrationFactor : ValidateCalibrationFactor(LReturnFlowChannel);
        dvtMonthlyAvrgNetEvap : ValidateMonthlyAvrgNetEvap(LReturnFlowChannel);
        dvtRoutingConstant : ValidateRoutingConstant(LReturnFlowChannel);
        dvtCurtailmentFactor : ValidateCurtailmentFactor(LReturnFlowChannel);
        dvtMultiplicationFactor : ValidateMultiplicationFactor(LReturnFlowChannel);
        dvtPotentialMonthlyEvap : ValidatePotentialMonthlyEvap(LReturnFlowChannel);
        dvtCorrespondingChannel: ValidateCorrespondingChannel(LReturnFlowChannel);
        dvtAbstractionChannel : ValidateAbstractionChannel(LReturnFlowChannel);
        dvtAssumedFactor : ValidateAssumedFactor(LReturnFlowChannel);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelValidator.GetReturnFlowChannelDialog: TReturnFlowChannelDialog;
const OPNAME = 'TReturnFlowChannelValidator.GetReturnFlowChannelDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TReturnFlowChannelDialog(FPanel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelValidator.Initialise: boolean;
const OPNAME = 'TReturnFlowChannelValidator.Initialise';
var
  LIndex : integer;
  LChannel : IGeneralFlowChannel;
  LYieldModelData : IYieldModelData;
  LChannelList : IChannelList;
begin
  Result := False;
  try
    LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
    LChannelList    := LYieldModelData.NetworkElementData.ChannelList;

    ReturnFlowChannelDialog.CbxChannel.Clear;
    ReturnFlowChannelDialog.CbxChannel.Items.AddObject('0 - None', TObject(0));
    for LIndex := 0 to LChannelList.ChannelCount - 1 do
    begin
      LChannel := LChannelList.ChannelByIndex[lIndex];
      if (LChannel.ChannelType in [2]) then
      begin
        ReturnFlowChannelDialog.CbxChannel.Items.AddObject(LChannel.ChannelName, TObject(LChannel.ChannelNumber));
      end;
    end;
    ReturnFlowChannelDialog.CbxChannel.Sorted := True;
    ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.OnColEnter         := OnStringGridColEnter;
    ReturnFlowChannelDialog.GrdCorrespondingChannels.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    ReturnFlowChannelDialog.GrdCorrespondingChannels.OnColEnter         := OnStringGridColEnter;
    Result := FPanel.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReturnFlowChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TReturnFlowChanneldialog.ReturnFlowChannelDataDialog');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TReturnFlowChannelValidator.OnEditControlEnter';
begin
  inherited;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TReturnFlowChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if (Sender = ReturnFlowChannelDialog.CbxChannel) then
      UpdateCorrespondingChannel;
//    if (Sender = ReturnFlowChannelDialog.EdtNumOfCorrespondingChannels) then
//      UpdateNumOfCorrespondingChannel;
    if (Sender = ReturnFlowChannelDialog.EdtGaugeNumber) then
      UpdateGaugeNumber;
    if (Sender = ReturnFlowChannelDialog.EdtMonthlyAvrgFactor) then
      UpdateMonthlyAvrgFactor;
    if (Sender = ReturnFlowChannelDialog.EdtCalibrationFactor) then
      UpdateCalibrationFactor;
    if (Sender = ReturnFlowChannelDialog.EdtMonthlyAvrgNetEvap) then
      UpdateMonthlyAvrgNetEvap;
    if (Sender = ReturnFlowChannelDialog.EdtRoutingConstant) then
      UpdateRoutingConstant;
    if (Sender = ReturnFlowChannelDialog.EdtCurtailmentFactor) then
      UpdateCurtailmentFactor;
    if (Sender = ReturnFlowChannelDialog.EdtMultiplicationFactor) then
      UpdateMultiplicationFactor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.OnGrdCorrespondingChannelSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TReturnFlowChannelValidator.OnGrdCorrespondingChannelSelectCell';
var
  LChannel : string;
begin
  try
    with ReturnFlowChannelDialog do
    begin
      if ACol = 0 then
      begin
        CbxChannel.Top  := 2 + GrdCorrespondingChannels.Top + ((1 + GrdCorrespondingChannels.DefaultRowHeight) *
                                    (ARow - GrdCorrespondingChannels.TopRow + 1));
        LChannel := Trim(GrdCorrespondingChannels.Cells[0, ARow]);
        CbxChannel.ItemIndex := CbxChannel.Items.IndexOf(LChannel);
        CbxChannel.Visible := True;
        if (GrdCorrespondingChannels.ValidationError[ACol, ARow, gveCellContext] <> '') then
        begin
          CbxChannel.ValidationError   := GrdCorrespondingChannels.ValidationError[ACol, ARow, gveCellContext];
          CbxChannel.InValidationError := True;
          CbxChannel.ShowErrorState(True);
        end
        else
        begin
          CbxChannel.ValidationError   := '';
          CbxChannel.InValidationError := False;
          CbxChannel.ShowErrorState(False);
        end;
      end
      else
        CbxChannel.Visible := False;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.OnGrdCorrespondingChannelTopLeftChanged(Sender: TObject);
const OPNAME = 'TReturnFlowChannelValidator.OnGrdCorrespondingChannelTopLeftChanged';
begin
  try
    ReturnFlowChannelDialog.CbxChannel.Visible := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReturnFlowChannelValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with ReturnFlowChannelDialog do
    begin
      if (ReturnFlowChannelDialog.GrdMonthlyPotentialEvap = ASender) then
       UpdateMonthlyPotentialEvap(ARow, Trim(ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.Cells[1,ARow]));
      if (ReturnFlowChannelDialog.GrdCorrespondingChannels = ASender) then
        UpdateAssumedFactor(ARow,Trim(ReturnFlowChannelDialog.GrdCorrespondingChannels.Cells[ACol,ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReturnFlowChannelValidator.PopulateDataViewer;
const OPNAME = 'TReturnFlowChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtReturnFlowChannelData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.RePopulateDataViewer;
const OPNAME = 'TReturnFlowChannelValidator.RePopulateDataViewer';
var
  LTotal : double;
  LIndex              : integer;
  LFieldProperty1      : TAbstractFieldProperty;
  //LFieldProperty2      : TAbstractFieldProperty;
//  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
  LMonths             : TMonthNamesArray;
  LYieldModelData : IYieldModelData;
begin
  try
    if (Identifier >= 0) then
    begin
      LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
      LChannel := LYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[Identifier];
      SetLength(LMonths,12);
      LMonths := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNamesArray;
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (LChannel <> nil) and ( LReturnFlowChannel <> nil) then
      begin
        ReturnFlowChannelDialog.CbxChannel.Visible := False;
        ReturnFlowChannelDialog.EdtDemandCentreID.SetFieldValue(LChannel.ChannelName);
        ReturnFlowChannelDialog.EdtNumOfCorrespondingChannels.SetFieldValue(LReturnFlowChannel.NumOfCorrespondingChannels);
        ReturnFlowChannelDialog.EdtGaugeNumber.SetFieldValue(LReturnFlowChannel.GaugeNumber);
        ReturnFlowChannelDialog.EdtMonthlyAvrgFactor.SetFieldValue(LReturnFlowChannel.MonthlyAvrgFactor);
        ReturnFlowChannelDialog.EdtCalibrationFactor.SetFieldValue(LReturnFlowChannel.CalibrationFactor);
        ReturnFlowChannelDialog.EdtMonthlyAvrgNetEvap.SetFieldValue(LReturnFlowChannel.MonthlyAvrgNetEvap);
        ReturnFlowChannelDialog.EdtRoutingConstant.SetFieldValue(LReturnFlowChannel.RoutingConstant);
        ReturnFlowChannelDialog.EdtCurtailmentFactor.SetFieldValue(LReturnFlowChannel.CurtailmentFactor);
        ReturnFlowChannelDialog.EdtMultiplicationFactor.SetFieldValue(LReturnFlowChannel.MultiplicationFactor);
        LTotal := 0.0;
        LFieldProperty1 := FAppModules.FieldProperties.FieldProperty('PotentialMonthlyEvap');
        //LFieldProperty2 := FAppModules.FieldProperties.FieldProperty('PotentialMonthlyEvapValue');
        ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.RowCount := 13;
        for LIndex := 1 to 12  do
        begin
          ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.Cells[0, LIndex-1] := LMonths[LIndex];
          ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.AddFieldProperty(LFieldProperty1);
          ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.Cells[1,LIndex -1] := Format('%4.f',
          [LReturnFlowChannel.MonthlyPotentialEvapByIndex[LIndex]]);
          LTotal := LTotal + LReturnFlowChannel.MonthlyPotentialEvapByIndex[LIndex];
        end;
        ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.Cells[0,12] := 'Total';
        ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.Cells[1,12] := FloatToStr(LTotal);
        ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.IsRowEnabled[12] := False;
        ReturnFlowChannelDialog.GrdCorrespondingChannels.RowCount :=
        LReturnFlowChannel.NumOfCorrespondingChannels + 1;
        ReturnFlowChannelDialog.CbxChannel.Visible := False;
        PopulateCorrespondingChannel(LReturnFlowChannel);
      end;
   end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.PopulateCorrespondingChannel(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.PopulateCorrespondingChannel';
var
  LIndex : integer;
  LFieldProperty      : TAbstractFieldProperty;
begin
  try
    ReturnFlowChannelDialog.GrdCorrespondingChannels.RowCount :=
    AReturnFlowChannel.NumOfCorrespondingChannels + 1;
    ReturnFlowChannelDialog.GrdCorrespondingChannels.Height :=
    3 + (1 +  ReturnFlowChannelDialog.GrdCorrespondingChannels.DefaultRowHeight) *
    ReturnFlowChannelDialog.GrdCorrespondingChannels.RowCount;
    ReturnFlowChannelDialog.CbxChannel.Visible := False;
    ReturnFlowChannelDialog.EdtNumOfCorrespondingChannels.SetFieldValue(AReturnFlowChannel.NumOfCorrespondingChannels);
    for LIndex := 0 to AReturnFlowChannel.NumOfCorrespondingChannels-1 do
    begin
      LFieldProperty := FAppModules.FieldProperties.FieldProperty('CorrespondingChannel');
      ReturnFlowChannelDialog.GrdCorrespondingChannels.AddFieldProperty(LFieldProperty);

      ReturnFlowChannelDialog.GrdCorrespondingChannels.Cells[0,LIndex+1] :=
      Format(LFieldProperty.FormatStringGrid {'%d'},[AReturnFlowChannel.CorrespondingChannelByIndex[LIndex].ChannelNumber]);

      LFieldProperty := FAppModules.FieldProperties.FieldProperty('AbstractionChannel');
      ReturnFlowChannelDialog.GrdCorrespondingChannels.AddFieldProperty(LFieldProperty);

      ReturnFlowChannelDialog.GrdCorrespondingChannels.Cells[1,LIndex+1] :=
      Format(LFieldProperty.FormatStringGrid {'%d'},[AReturnFlowChannel.CorrespondingChannelByIndex[LIndex].AbstractionChannel]);

      LFieldProperty := FAppModules.FieldProperties.FieldProperty('AssumedFactor');
      ReturnFlowChannelDialog.GrdCorrespondingChannels.AddFieldProperty(LFieldProperty);

      ReturnFlowChannelDialog.GrdCorrespondingChannels.Cells[2,LIndex+1] :=
      Format(LFieldProperty.FormatStringGrid {'%3.4f'},[AReturnFlowChannel.CorrespondingChannelByIndex[LIndex].AssumedFactor]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TReturnFlowChannelValidator.SaveState: boolean;
const OPNAME = 'TReturnFlowChannelValidator.SaveState';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                         ANewValue: string): boolean;
const OPNAME = 'TReturnFlowChannelValidator.StudyDataHasChanged';
begin
  Result := False; 
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TReturnFlowChannelValidator.StudyHasChanged';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
{
procedure TReturnFlowChannelValidator.UpdateNumOfCorrespondingChannel;
const OPNAME = 'TReturnFlowChannelValidator.UpdateNumOfCorrespondingChannel';
var
  LMessage : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByIdentifier[FFeatureID];
    if (LChannel <> nil) AND (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (FAppModules.FieldProperties.ValidateFieldProperty(
          ReturnFlowChannelDialog.EdtNumOfCorrespondingChannels.FieldProperty.FieldName,
          ReturnFlowChannelDialog.EdtNumOfCorrespondingChannels.Text,LMessage)) then
      begin
        LReturnFlowChannel.NumOfCorrespondingChannels :=
        StrToInt(ReturnFlowChannelDialog.EdtNumOfCorrespondingChannels.Text);
        RePopulateDataViewer;
        DoContextValidation(dvtNumOfCorrespondingChannels);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
 }
procedure TReturnFlowChannelValidator.UpdateGaugeNumber;
const OPNAME = 'TReturnFlowChannelValidator.UpdateGaugeNumber';
var
  LMessage : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (FAppModules.FieldProperties.ValidateFieldProperty( ReturnFlowChannelDialog.EdtGaugeNumber.FieldProperty.FieldName,
        ReturnFlowChannelDialog.EdtGaugeNumber.Text,LMessage)) then
      begin
        LReturnFlowChannel.GaugeNumber := StrToInt(Trim(ReturnFlowChannelDialog.EdtGaugeNumber.Text));
        RePopulateDataViewer;
        DoContextValidation(dvtGaugeNumber);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateMonthlyAvrgFactor;
const OPNAME = 'TReturnFlowChannelValidator.UpdateMonthlyAvrgFactor';
var
  LMessage : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (FAppModules.FieldProperties.ValidateFieldProperty( ReturnFlowChannelDialog.EdtMonthlyAvrgFactor.FieldProperty.FieldName,
        Trim(ReturnFlowChannelDialog.EdtMonthlyAvrgFactor.Text),LMessage)) then
      begin
        LReturnFlowChannel.MonthlyAvrgFactor := StrToFloat(Trim(ReturnFlowChannelDialog.EdtMonthlyAvrgFactor.Text));
        RePopulateDataViewer;
        DoContextValidation(dvtMonthlyAvrgFactor);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateCalibrationFactor;
const OPNAME = 'TReturnFlowChannelValidator.UpdateCalibrationFactor';
var
  LMessage : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (FAppModules.FieldProperties.ValidateFieldProperty( ReturnFlowChannelDialog.EdtCalibrationFactor.FieldProperty.FieldName,
        Trim(ReturnFlowChannelDialog.EdtCalibrationFactor.Text),LMessage)) then
      begin
        LReturnFlowChannel.CalibrationFactor := StrToFloat(Trim(ReturnFlowChannelDialog.EdtCalibrationFactor.Text));
        RePopulateDataViewer;
        DoContextValidation(dvtCalibrationFactor);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateMonthlyAvrgNetEvap;
const OPNAME = 'TReturnFlowChannelValidator.UpdateMonthlyAvrgNetEvap';
var
  LMessage : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (FAppModules.FieldProperties.ValidateFieldProperty(ReturnFlowChannelDialog.EdtMonthlyAvrgNetEvap.FieldProperty.FieldName,
        Trim(ReturnFlowChannelDialog.EdtMonthlyAvrgNetEvap.Text),LMessage)) then
      begin
        LReturnFlowChannel.MonthlyAvrgNetEvap := StrToFloat(Trim(ReturnFlowChannelDialog.EdtMonthlyAvrgNetEvap.Text));
        RePopulateDataViewer;
        DoContextValidation(dvtMonthlyAvrgNetEvap);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateRoutingConstant;
const OPNAME = 'TReturnFlowChannelValidator.UpdateRoutingConstant';
var
  LMessage : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (FAppModules.FieldProperties.ValidateFieldProperty(ReturnFlowChannelDialog.EdtRoutingConstant.FieldProperty.FieldName,
        Trim(ReturnFlowChannelDialog.EdtRoutingConstant.Text),LMessage)) then
      begin
        LReturnFlowChannel.RoutingConstant := StrToFloat(Trim(ReturnFlowChannelDialog.EdtRoutingConstant.Text));
        RePopulateDataViewer;
        DoContextValidation(dvtRoutingConstant);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateCurtailmentFactor;
const OPNAME = 'TReturnFlowChannelValidator.UpdateCurtailmentFactor';
var
  LMessage : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (FAppModules.FieldProperties.ValidateFieldProperty(ReturnFlowChannelDialog.EdtCurtailmentFactor.FieldProperty.FieldName,
        Trim(ReturnFlowChannelDialog.EdtCurtailmentFactor.Text),LMessage)) then
      begin
        LReturnFlowChannel.CurtailmentFactor := StrToFloat(Trim(ReturnFlowChannelDialog.EdtCurtailmentFactor.Text));
        RePopulateDataViewer;
        DoContextValidation(dvtCurtailmentFactor);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateMultiplicationFactor;
const OPNAME = 'TReturnFlowChannelValidator.UpdateMultiplicationFactor';
var
  LMessage : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (FAppModules.FieldProperties.ValidateFieldProperty(ReturnFlowChannelDialog.EdtMultiplicationFactor.FieldProperty.FieldName,
        Trim(ReturnFlowChannelDialog.EdtMultiplicationFactor.Text),LMessage)) then
      begin
        LReturnFlowChannel.MultiplicationFactor := StrToFloat(Trim(ReturnFlowChannelDialog.EdtMultiplicationFactor.Text));
        RePopulateDataViewer;
        DoContextValidation(dvtMultiplicationFactor);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateCorrespondingChannel;
const OPNAME = 'TReturnFlowChannelValidator.UpdateCorrespondingChannel';
var
  LMessage : string;
  LIndex,
  LChannelNum : integer;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;

      if (ReturnFlowChannelDialog.GrdCorrespondingChannels.Col = 0 ) and
       (ReturnFlowChannelDialog.GrdCorrespondingChannels.Row > 0) then
      begin
        LChannelNum := 0;
        if (ReturnFlowChannelDialog.CbxChannel.ItemIndex >= 0) then
          LChannelNum := Integer(ReturnFlowChannelDialog.CbxChannel.Items.Objects[ReturnFlowChannelDialog.CbxChannel.ItemIndex]);
        if LChannelNum = 0 then
          Exit;
        LIndex := ReturnFlowChannelDialog.GrdCorrespondingChannels.Row;
        ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[0, LIndex, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'CorrespondingChannel', IntToStr(LChannelNum),LMessage)) then
        begin
          LReturnFlowChannel.CorrespondingChannelByIndex[LIndex-1].ChannelNumber := LChannelNum;
          RePopulateDataViewer;
          DoContextValidation(dvtCorrespondingChannel);
        end
        else
        begin
          ReturnFlowChannelDialog.CbxChannel.ValidationError := LMessage;
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[1, LIndex, gveCellContext] := LMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateAssumedFactor(AIndex : integer;AValue: string);
const OPNAME = 'TReturnFlowChannelValidator.UpdateAssumedFactor';
var
  LMessage            : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      if (ReturnFlowChannelDialog.GrdCorrespondingChannels.Col = 1 ) and
       (ReturnFlowChannelDialog.GrdCorrespondingChannels.Row > 0) then
      begin
        ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[1, AIndex, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'AbstractionChannel', AValue,LMessage)) then
        begin
          LReturnFlowChannel.CorrespondingChannelByIndex[AIndex-1].AbstractionChannel := StrToInt(AValue);
          RePopulateDataViewer;
          DoContextValidation(dvtAbstractionChannel);
        end
        else
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[1, AIndex, gveCellContext] := lMessage;
      end;
      if (ReturnFlowChannelDialog.GrdCorrespondingChannels.Col = 2 ) and
       (ReturnFlowChannelDialog.GrdCorrespondingChannels.Row > 0) then
      begin
        ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[2, AIndex, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'AssumedFactor', AValue,LMessage)) then
        begin
          LReturnFlowChannel.CorrespondingChannelByIndex[AIndex-1].AssumedFactor := StrToFloat(AValue);
          RePopulateDataViewer;
          DoContextValidation(dvtAssumedFactor);
        end
        else
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[1, AIndex, gveCellContext] := lMessage;
      end;
      
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.UpdateMonthlyPotentialEvap(AIndex : integer; AValue: string);
const OPNAME = 'TReturnFlowChannelValidator.UpdateMonthlyPotentialEvap';
var
  LValue              : double;
  LStrValue           : string;
  LMessage            : string;
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    if (ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.Col = 1) then
    begin
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
      LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
      if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
      begin
        LReturnFlowChannel := LChannel.ReturnFlowChannel;
        with ReturnFlowChannelDialog do
        begin
          GrdMonthlyPotentialEvap.ValidationError[1, AIndex, gveCellContext] := '';
          LStrValue := Trim(GrdMonthlyPotentialEvap.Cells[1,AIndex]);
          if (LStrValue = '') then
            LStrValue := '0.0';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'PotentialMonthlyEvap', LStrValue,LMessage, AIndex+1)) then
          begin
            LValue := StrToFloat(LStrValue);
            LReturnFlowChannel.MonthlyPotentialEvapByIndex[AIndex+1] := LValue;
            RePopulateDataViewer;
            DoContextValidation(dvtPotentialMonthlyEvap);
          end
          else
          begin
            GrdMonthlyPotentialEvap.ValidationError[1, AIndex, gveCellContext] := LMessage;
            DoContextValidation(dvtPotentialMonthlyEvap);
          end;
        end;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReturnFlowChannelValidator.DoAddCorrespondingChannelClick(Sender: TObject);
const OPNAME = 'TReturnFlowChannelValidator.DoAddCorrespondingChannelClick';
var
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      LReturnFlowChannel.NewCorrespondingChannel(0);
      LReturnFlowChannel.NumOfCorrespondingChannels :=
      (LReturnFlowChannel.NumOfCorrespondingChannels + 1);
      PopulateCorrespondingChannel(LReturnFlowChannel);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.DoDeleteCorrespondingChannelClick(Sender: TObject);
const OPNAME = 'TReturnFlowChannelValidator.DoDeleteCorrespondingChannelClick';
var
  LChannelList        : IChannelList;
  LChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
  LCorrespondingChannel : ICorrespondingChannel;
  LIndex : integer;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList;
    LChannel     := LChannelList.ChannelByChannelNumber[Identifier];
    if (LChannel <> nil) and (LChannel.ReturnFlowChannel <> nil) then
    begin
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      LIndex := ReturnFlowChannelDialog.GrdCorrespondingChannels.Row;
      LCorrespondingChannel := LReturnFlowChannel.CorrespondingChannelByIndex[LIndex-1];
      if LCorrespondingChannel <> nil then
      begin
        if LReturnFlowChannel.RemoveCorrespondingChannelByChannel(LCorrespondingChannel.ChannelNumber) then
          PopulateCorrespondingChannel(LReturnFlowChannel);
      end;  
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateGaugeNumber(AReturnFlowChannel: IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateGaugeNumber';
begin
  try
    if (not AReturnFlowChannel.Validate(FErrorMessage, 'ReturnFlowGaugeNumber')) then
      ReturnFlowChannelDialog.EdtGaugeNumber.ContextValidationError := FErrorMessage
    else
      ReturnFlowChannelDialog.EdtGaugeNumber.ContextValidationError := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateMonthlyAvrgFactor(AReturnFlowChannel :IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateMonthlyAvrgFactor';
begin
  try
    if (not AReturnFlowChannel.Validate(FErrorMessage, 'MonthlyAvrgFactor')) then
      ReturnFlowChannelDialog.EdtMonthlyAvrgFactor.ContextValidationError := FErrorMessage
    else
      ReturnFlowChannelDialog.EdtMonthlyAvrgFactor.ContextValidationError := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateCalibrationFactor(AReturnFlowChannel:IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateCalibrationFactor';
begin
  try
    if (not AReturnFlowChannel.Validate(FErrorMessage, 'CalibrationFactor')) then
      ReturnFlowChannelDialog.EdtCalibrationFactor.ContextValidationError := FErrorMessage
    else
      ReturnFlowChannelDialog.EdtCalibrationFactor.ContextValidationError := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateMonthlyAvrgNetEvap(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateMonthlyAvrgNetEvap';
begin
  try
    if (not AReturnFlowChannel.Validate(FErrorMessage, 'MonthlyAvrgNetEvap')) then
      ReturnFlowChannelDialog.EdtMonthlyAvrgNetEvap.ContextValidationError := FErrorMessage
    else
      ReturnFlowChannelDialog.EdtMonthlyAvrgNetEvap.ContextValidationError := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateRoutingConstant(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateRoutingConstant';
begin
  try
    if (not AReturnFlowChannel.Validate(FErrorMessage, 'RoutingConstant')) then
      ReturnFlowChannelDialog.EdtRoutingConstant.ContextValidationError := FErrorMessage
    else
      ReturnFlowChannelDialog.EdtRoutingConstant.ContextValidationError := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateCurtailmentFactor(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateCurtailmentFactor';
begin
  try
    if (not AReturnFlowChannel.Validate(FErrorMessage, 'CurtailmentFactor')) then
      ReturnFlowChannelDialog.EdtCurtailmentFactor.ContextValidationError := FErrorMessage
    else
      ReturnFlowChannelDialog.EdtCurtailmentFactor.ContextValidationError := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateMultiplicationFactor(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateMultiplicationFactor';
begin
  try
    if (not AReturnFlowChannel.Validate(FErrorMessage, 'MultiplicationFactor')) then
      ReturnFlowChannelDialog.EdtMultiplicationFactor.ContextValidationError := FErrorMessage
    else
      ReturnFlowChannelDialog.EdtMultiplicationFactor.ContextValidationError := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidatePotentialMonthlyEvap(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidatePotentialMonthlyEvap';
var
  LIndex,
  LRow : integer;
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  LFieldProperties: TAbstractFieldProperty;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('PotentialMonthlyEvap');
    try
      if (AReturnFlowChannel.Validate(FErrorMessage, 'PotentialMonthlyEvap')) then
      begin
        for LRow := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
          ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.ValidationError[1, LRow, gveCellContext] := ''
      end
      else
      begin
        ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
        for LRow := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
        begin
          LIndex := LErrorCols.IndexOf(IntToStr(LRow));
          if (LIndex >= 0) then
            ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.ValidationError[1, LRow, gveCellContext] := LErrorMsgs.Strings[lIndex]
          else
            ReturnFlowChannelDialog.GrdMonthlyPotentialEvap.ValidationError[1, LRow, gveCellContext] := ''
        end;
      end;
    finally
      FreeAndNil(LErrorCols);
      FreeAndNil(LErrorMsgs);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateCorrespondingChannel(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateCorrespondingChannel';
var
  LCorrespondingChannel : ICorrespondingChannel;
  LIndex : integer;
begin
  try
    if AReturnFlowChannel <> nil then
    begin
      LIndex := ReturnFlowChannelDialog.GrdCorrespondingChannels.Row;
      if LIndex > 0 then
        LCorrespondingChannel := AReturnFlowChannel.CorrespondingChannelByIndex[LIndex-1];
      if LCorrespondingChannel <> nil  then
      begin
        if not(LCorrespondingChannel.Validate(FErrorMessage, 'CorrespondingChannel')) then
        begin
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[0,LIndex,gveCellContext] := FErrorMessage;
        end
        else
        begin
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[0,LIndex,gveCellContext] := '';
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateAbstractionChannel(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateAbstractionChannel';
var
  LCorrespondingChannel : ICorrespondingChannel;
  LIndex : integer;
begin
  try
    if AReturnFlowChannel <> nil then
    begin
      LIndex := ReturnFlowChannelDialog.GrdCorrespondingChannels.Row;
      if LIndex > 0 then
        LCorrespondingChannel := AReturnFlowChannel.CorrespondingChannelByIndex[LIndex-1];
      if LCorrespondingChannel <> nil  then
      begin
        if not(LCorrespondingChannel.Validate(FErrorMessage, 'AbstractionChannel')) then
        begin
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[1,LIndex,gveCellContext] := FErrorMessage;
        end
        else
        begin
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[1,LIndex,gveCellContext] := '';
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelValidator.ValidateAssumedFactor(AReturnFlowChannel : IReturnFlowChannel);
const OPNAME = 'TReturnFlowChannelValidator.ValidateAssumedFactor';
var
  LCorrespondingChannel : ICorrespondingChannel;
  LIndex : integer;
begin
  try
    if AReturnFlowChannel <> nil then
    begin
      LIndex := ReturnFlowChannelDialog.GrdCorrespondingChannels.Row;
      if LIndex > 0 then
        LCorrespondingChannel := AReturnFlowChannel.CorrespondingChannelByIndex[LIndex-1];
      if LCorrespondingChannel <> nil  then
      begin
        if not(LCorrespondingChannel.Validate(FErrorMessage, 'AssumedFactor')) then
        begin
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[2,LIndex,gveCellContext] := FErrorMessage;
        end
        else
        begin
          ReturnFlowChannelDialog.GrdCorrespondingChannels.ValidationError[2,LIndex,gveCellContext] := '';
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
