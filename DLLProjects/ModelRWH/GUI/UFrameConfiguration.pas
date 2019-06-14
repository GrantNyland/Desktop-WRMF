unit UFrameConfiguration;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  UColourButtons,
  UAbstractObject,
  URWHDataObject;

type

  TfrmConfiguration = class(TFrame)
    gbRoof: TGroupBox;
    lblRoofArea: TLabel;
    lblRunoffCoefficient: TLabel;
    lbPercent: TLabel;
    edtRoofArea: TEdit;
    edtRunoffCoefficient: TEdit;
    gbRunType: TGroupBox;
    lblStartingVolume: TLabel;
    lblUpperLevel: TLabel;
    lblLowerLevel: TLabel;
    edtStartingVolume: TEdit;
    edtUpperLevel: TEdit;
    edtLowerLevel: TEdit;
    gbStorage: TGroupBox;
    lblTankCount: TLabel;
    lblTankVolume: TLabel;
    lblTankCountMax: TLabel;
    edtTankCount: TEdit;
    strgrdTankVolume: TStringGrid;
    gbHousehold: TGroupBox;
    lblHouseholdCount: TLabel;
    lblMembersPerHousehold: TLabel;
    edtHouseholdCount: TEdit;
    edtMembersPerHousehold: TEdit;
    edtPerCapitaDemand: TEdit;
    pnlButtons: TPanel;
    btnSave: TColourBitBtn;
    btnReset: TColourBitBtn;
    lblPerCapitaDemand: TLabel;
    btnNew: TColourBitBtn;
    btnDelete: TColourBitBtn;
    btnCopy: TColourBitBtn;
    lblPerCapitaDemandUnits: TLabel;
    lblVolumeUnits: TLabel;
    lblStartingVolumeUnits: TLabel;
    lblRoofAreaUnits: TLabel;
    cmboxTankSizes: TComboBox;
    lblParentName: TLabel;
    edtName: TEdit;
    procedure cmboxRunTypeChange(Sender: TObject);
    procedure strgrdTankVolumeSetEditText(Sender: TObject; ACol,ARow: Integer; const Value: String);
    procedure btnSaveClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure cmboxTankSizesExit(Sender: TObject);
    procedure strgrdTankVolumeSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure edtStartingVolumeExit(Sender: TObject);
    procedure edtTankCountExit(Sender: TObject);
  protected
    { Private declarations }
    FAppModules          : TAppModules;
    FEditRunConfig       : TRWHRunConfig;
    FOnRunConfigAdded    : TNotifyEvent;
    FOnRunConfigDeleted  : TNotifyEvent;
    FEditContext         : TChangeContext;
    FPopulating          : boolean;
    procedure ClearDialog;
    procedure PopulateObject;
    procedure PopulateDialog;
    procedure SetButtonState;
    procedure SetComponentsEnabled(AEnabled:boolean);
    procedure SetAppModules(AAppModules : TAppModules);
  public
    { Public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean;
    function Finalise   : boolean;
    procedure SelectRunConfig(ARunConfig : TRWHRunConfig);
    function LanguageHasChanged : boolean;
    property AppModules : TAppModules read FAppModules write SetAppModules;
    property OnRunConfigAdded    : TNotifyEvent read FOnRunConfigAdded write FOnRunConfigAdded;
    property OnRunConfigDeleted  : TNotifyEvent read FOnRunConfigDeleted write FOnRunConfigDeleted;
  end;

var
  frmConfiguration : TfrmConfiguration;

implementation

{$R *.dfm}

uses
  //UFrameWork,
  //UsysLevel,
  //UsysRunType,
  //UsysStandardTank,
  UConstants,
  UUtilities,
  //UAbstractClass,
  //UFormCopyConfiguration,
  UErrorHandlingOperations;

{ TfrmConfiguration }

procedure TfrmConfiguration.AfterConstruction;
const OPNAME = 'TfrmConfiguration.AfterConstruction';
begin
  inherited;
  try
    FEditRunConfig    := nil;
    FPopulating       := False;
    FEditContext      := sdccSelect;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.BeforeDestruction;
const OPNAME = 'TfrmConfiguration.AfterConstruction';
begin
  inherited;
  try
    if(FEditRunConfig <> nil) then
      FreeAndNil(FEditRunConfig);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.SetAppModules(AAppModules: TAppModules);
const OPNAME = 'TfrmConfiguration.SetAppModules';
begin
  try
    FAppModules := AAppModules;
    if(AAppModules <> nil) then
      FEditRunConfig  := TRWHRunConfig.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmConfiguration.Initialise: boolean;
const OPNAME = 'TfrmConfiguration.Initialise';
begin
  Result := False;
  try
    FEditRunConfig.Initialise;
    strgrdTankVolume.DefaultRowHeight := cmboxTankSizes.Height;
    {if gbRunType.Visible then
    begin
      lblHouseholdCount.Visible := True;
      edtHouseholdCount.Visible := True;
      gbHousehold.Height        := 110;
      lblRunoffCoefficient.Visible := True;
      lbPercent.Visible            := True;
      edtRunoffCoefficient.Visible := True;
      gbRoof.Height                := 77;
    end
    else
    begin
      lblHouseholdCount.Visible := False;
      edtHouseholdCount.Visible := False;
      gbHousehold.Height        := 85;
      lblRunoffCoefficient.Visible := False;
      lbPercent.Visible            := False;
      edtRunoffCoefficient.Visible := False;
      gbRoof.Height                := 46;
    end;}
    btnResetClick(nil);
    PopulateDialog;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmConfiguration.Finalise: boolean;
const OPNAME = 'TfrmConfiguration.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmConfiguration.LanguageHasChanged: boolean;
const OPNAME = 'TfrmConfiguration.LanguageHasChanged';
begin
  Result := False;
  try
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.ClearDialog;
const OPNAME = 'TfrmConfiguration.ClearDialog';
{var
  LIndex : integer;}
begin
  try
    edtName.Text                := '';
    edtStartingVolume.Text      := '';
    edtUpperLevel.Text          := '';
    edtLowerLevel.Text          := '';
    edtRoofArea.Text            := '';
    edtRunoffCoefficient.Text   := '';
    edtHouseholdCount.Text      := '';
    edtMembersPerHousehold.Text := '';
    edtPerCapitaDemand.Text     := '';
    edtTankCount.Text           := '';
    strgrdTankVolume.ColCount   := 1;
    strgrdTankVolume.Cells[0,0] := '';
    cmboxTankSizes.Visible      := False;


    //pnlButtons.Enabled          := True;//FEditRunConfig.Populated;
    //gbRunType.Enabled           := FEditRunConfig.Populated;
    //gbRoof.Enabled              := FEditRunConfig.Populated;
    //gbStorage.Enabled           := FEditRunConfig.Populated;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.PopulateDialog;
const OPNAME = 'TfrmConfiguration.PopulateDialog';
var
  LTankCount : integer;
  LMultiplier : double;
begin
  try
    FPopulating := True;
    try
      ClearDialog;
      if FEditRunConfig.Populated then
      begin
        LMultiplier   := 1000.0;
        edtName.Text := FEditRunConfig.RunName;
        if(FEditRunConfig.RunStartVolume <> NullFloat) then
          edtStartingVolume.Text      := FormatFloat('##0.00',FEditRunConfig.RunStartVolume*LMultiplier);
        if(FEditRunConfig.RunStartLevel <> NullFloat) then
          edtUpperLevel.Text          := FormatFloat('##0.00',FEditRunConfig.RunStartLevel);
        if(FEditRunConfig.RunStopLevel <> NullFloat) then
          edtLowerLevel.Text          := FormatFloat('##0.00',FEditRunConfig.RunStopLevel);

        if(FEditRunConfig.RoofArea <> NullFloat) then
          edtRoofArea.Text            := FormatFloat('##0.00',FEditRunConfig.RoofArea);
        if(FEditRunConfig.RoofRunoffCoef <> NullFloat) then
          edtRunoffCoefficient.Text   := FormatFloat('##0.00',FEditRunConfig.RoofRunoffCoef*100.0);

        if(FEditRunConfig.HouseHoldNumber <> NullInteger) then
          edtHouseholdCount.Text      := IntToStr(FEditRunConfig.HouseHoldNumber);
        if(FEditRunConfig.HouseHoldMembers <> NullInteger) then
          edtMembersPerHousehold.Text := IntToStr(FEditRunConfig.HouseHoldMembers);
        if(FEditRunConfig.HouseHoldDemandPP <> NullFloat) then
          edtPerCapitaDemand.Text     := FormatFloat('##0.00',FEditRunConfig.HouseHoldDemandPP*LMultiplier);

        LTankCount := FEditRunConfig.TankCount;
        if(LTankCount > 0) then
        begin
          edtTankCount.Text := IntToStr(LTankCount);
          strgrdTankVolume.ColCount := LTankCount;
          strgrdTankVolume.Enabled := True;
          if(LTankCount >= 1) then
            strgrdTankVolume.Cells[0,0] := FormatFloat('##0.00',FEditRunConfig.TankSize01*LMultiplier);
          if(LTankCount >= 2) then
            strgrdTankVolume.Cells[1,0] := FormatFloat('##0.00',FEditRunConfig.TankSize02*LMultiplier);
          if(LTankCount >= 3) then
            strgrdTankVolume.Cells[2,0] := FormatFloat('##0.00',FEditRunConfig.TankSize03*LMultiplier);
          if(LTankCount >= 4) then
            strgrdTankVolume.Cells[3,0] := FormatFloat('##0.00',FEditRunConfig.TankSize04*LMultiplier);
          if(LTankCount >= 5) then
            strgrdTankVolume.Cells[4,0] := FormatFloat('##0.00',FEditRunConfig.TankSize05*LMultiplier);
          if(LTankCount >= 6) then
            strgrdTankVolume.Cells[5,0] := FormatFloat('##0.00',FEditRunConfig.TankSize06*LMultiplier);
          if(LTankCount >= 7) then
            strgrdTankVolume.Cells[6,0] := FormatFloat('##0.00',FEditRunConfig.TankSize07*LMultiplier);
          if(LTankCount >= 8) then
            strgrdTankVolume.Cells[7,0] := FormatFloat('##0.00',FEditRunConfig.TankSize08*LMultiplier);
          if(LTankCount >= 9) then
            strgrdTankVolume.Cells[8,0] := FormatFloat('##0.00',FEditRunConfig.TankSize09*LMultiplier);
          if(LTankCount >= 10) then
            strgrdTankVolume.Cells[9,0] := FormatFloat('##0.00',FEditRunConfig.TankSize10*LMultiplier);
        end;
      end;
      SetComponentsEnabled(FEditRunConfig.Populated);
      SetButtonState;
    finally
      FPopulating := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.PopulateObject;
const OPNAME = 'TfrmConfiguration.PopulateObject';
var
  LMultiplier : double;
  LTankCount  : integer;
begin
  try
      LMultiplier   := 1000.0;
      FEditRunConfig.RunName :=  Trim(edtName.Text);

      if(edtStartingVolume.Text = '') then
        FEditRunConfig.RunStartVolume    := NullFloat
      else
        FEditRunConfig.RunStartVolume    := StrToFloat(Trim(edtStartingVolume.Text))/LMultiplier;

      if(edtUpperLevel.Text = '') then
        FEditRunConfig.RunStartLevel    := NullFloat
      else
        FEditRunConfig.RunStartLevel    := StrToFloat(Trim(edtUpperLevel.Text));

      if(edtLowerLevel.Text = '') then
        FEditRunConfig.RunStopLevel    := NullFloat
      else
        FEditRunConfig.RunStopLevel    := StrToFloat(Trim(edtLowerLevel.Text));

      if(edtRoofArea.Text = '') then
        FEditRunConfig.RoofArea    := NullFloat
      else
        FEditRunConfig.RoofArea    := StrToFloat(Trim(edtRoofArea.Text));

      if(edtRunoffCoefficient.Text = '') then
        FEditRunConfig.RoofRunoffCoef    := NullFloat
      else
        FEditRunConfig.RoofRunoffCoef    := StrToFloat(Trim(edtRunoffCoefficient.Text))/100.0;

      if(edtHouseholdCount.Text = '') then
        FEditRunConfig.HouseHoldNumber    := NullInteger
      else
        FEditRunConfig.HouseHoldNumber    := StrToInt(Trim(edtHouseholdCount.Text));

      if(edtMembersPerHousehold.Text = '') then
        FEditRunConfig.HouseHoldMembers    := NullInteger
      else
        FEditRunConfig.HouseHoldMembers    := StrToInt(Trim(edtMembersPerHousehold.Text));

      if(edtPerCapitaDemand.Text = '') then
        FEditRunConfig.HouseHoldDemandPP    := NullFloat
      else
        FEditRunConfig.HouseHoldDemandPP    := StrToFloat(Trim(edtPerCapitaDemand.Text))/LMultiplier;

      LTankCount           := StrToInt(Trim(edtTankCount.Text));
      if(LTankCount >= 1) then
      begin
        if(strgrdTankVolume.Cells[0,0] = '') then
          FEditRunConfig.TankSize01 := NullFloat
        else
          FEditRunConfig.TankSize01 := StrToFloat(Trim(strgrdTankVolume.Cells[0,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize01 := NullFloat;

      if(LTankCount >= 2) then
      begin
        if(strgrdTankVolume.Cells[1,0] = '') then
          FEditRunConfig.TankSize02 := NullFloat
        else
          FEditRunConfig.TankSize02 := StrToFloat(Trim(strgrdTankVolume.Cells[1,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize02 := NullFloat;

      if(LTankCount >= 3) then
      begin
        if(strgrdTankVolume.Cells[2,0] = '') then
          FEditRunConfig.TankSize03 := NullFloat
        else
          FEditRunConfig.TankSize03 := StrToFloat(Trim(strgrdTankVolume.Cells[2,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize03 := NullFloat;

      if(LTankCount >= 4) then
      begin
        if(strgrdTankVolume.Cells[3,0] = '') then
          FEditRunConfig.TankSize04 := NullFloat
        else
          FEditRunConfig.TankSize04 := StrToFloat(Trim(strgrdTankVolume.Cells[3,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize04 := NullFloat;

      if(LTankCount >= 5) then
      begin
        if(strgrdTankVolume.Cells[4,0] = '') then
          FEditRunConfig.TankSize05 := NullFloat
        else
          FEditRunConfig.TankSize05 := StrToFloat(Trim(strgrdTankVolume.Cells[4,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize05 := NullFloat;

      if(LTankCount >= 6) then
      begin
        if(strgrdTankVolume.Cells[5,0] = '') then
          FEditRunConfig.TankSize06 := NullFloat
        else
          FEditRunConfig.TankSize06 := StrToFloat(Trim(strgrdTankVolume.Cells[5,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize06 := NullFloat;

      if(LTankCount >= 7) then
      begin
        if(strgrdTankVolume.Cells[6,0] = '') then
          FEditRunConfig.TankSize07 := NullFloat
        else
          FEditRunConfig.TankSize07 := StrToFloat(Trim(strgrdTankVolume.Cells[6,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize07 := NullFloat;

      if(LTankCount >= 8) then
      begin
        if(strgrdTankVolume.Cells[7,0] = '') then
          FEditRunConfig.TankSize08 := NullFloat
        else
          FEditRunConfig.TankSize08 := StrToFloat(Trim(strgrdTankVolume.Cells[7,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize08 := NullFloat;

      if(LTankCount >= 9) then
      begin
        if(strgrdTankVolume.Cells[8,0] = '') then
          FEditRunConfig.TankSize09 := NullFloat
        else
          FEditRunConfig.TankSize09 := StrToFloat(Trim(strgrdTankVolume.Cells[8,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize09 := NullFloat;

      if(LTankCount >= 10) then
      begin
        if(strgrdTankVolume.Cells[9,0] = '') then
          FEditRunConfig.TankSize10 := NullFloat
        else
          FEditRunConfig.TankSize10 := StrToFloat(Trim(strgrdTankVolume.Cells[9,0]))/LMultiplier;
      end
      else
        FEditRunConfig.TankSize10 := NullFloat;

      FEditRunConfig.Changed := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.SetButtonState;
const OPNAME = 'TfrmConfiguration.SetButtonState';
begin
  try
    btnNew.Enabled    := not FEditRunConfig.Changed;
    btnCopy.Enabled   := FEditRunConfig.Populated and (not FEditRunConfig.Changed);
    btnSave.Enabled   := FEditRunConfig.Changed;
    btnReset.Enabled  := FEditRunConfig.Changed;
    btnDelete.Enabled := FEditRunConfig.Populated and FEditRunConfig.SavedInDB and (not FEditRunConfig.Changed) ;
    cmboxTankSizes.Enabled   := (StrToIntDef(edtTankCount.Text,0) > 0);
    strgrdTankVolume.Enabled := cmboxTankSizes.Enabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.SelectRunConfig(ARunConfig: TRWHRunConfig);
const OPNAME = 'TfrmConfiguration.ClearDialog';
begin
  try
    if(ARunConfig = nil) then
    begin
      FEditRunConfig.Initialise;
      ClearDialog;
      SetComponentsEnabled(FEditRunConfig.Populated);
      SetButtonState;
    end
    else
    begin
      FEditRunConfig.Assign(ARunConfig);
      PopulateDialog;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.edtTankCountExit(Sender: TObject);
const OPNAME = 'TfrmConfiguration.edtTankCountChange';
var
  LCount,
  LIndex      : integer;
begin
  try
    if FPopulating then Exit;
    if(Trim(edtTankCount.Text) = '') then Exit;
    LCount := StrToInt(edtTankCount.Text);
    if(LCount > 5) or (LCount < 1) then
    begin
      ShowMessage('Number of tanks to consider has a maximum of five(5). Please enter a value not greater than 5');
      Exit;
    end;
    strgrdTankVolume.ColCount := LCount;
    if(LCount > FEditRunConfig.TankCount) then
    begin
      for LIndex := FEditRunConfig.TankCount to strgrdTankVolume.ColCount-1 do
      begin
        strgrdTankVolume.Cells[LIndex,0] := '0.00';
      end;
    end;

    FEditRunConfig.Changed := True;
    strgrdTankVolume.Enabled  := True;
    SetComponentsEnabled(FEditRunConfig.Changed);
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.strgrdTankVolumeSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
const OPNAME = 'TfrmConfiguration.strgrdTankVolumeSetEditText';
begin
  try
    if FPopulating then Exit;
    edtStartingVolumeExit(Sender);
    //FEditRunConfig.Changed := True;
    //SetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.cmboxRunTypeChange(Sender: TObject);
const OPNAME = 'TfrmConfiguration.cmboxRunTypeChange';
begin
  try
    if FPopulating then Exit;
    {LRunType := TsysRunType(cmboxRunType.Items.Objects[cmboxRunType.ItemIndex]);
    if(LRunType <> nil) then
    begin
      FEditRunConfig.Run_TypeID      := LRunType.RunTypeID;
      FEditRunConfig.Run_StartVolume := LRunType.StartVolume;
      FEditRunConfig.Run_StartLevel  := LRunType.StartLevel;
      FEditRunConfig.Run_StopLevel   := LRunType.StopLevel;

      if(FEditRunConfig.Run_StartVolume <> NullFloat) then
        edtStartingVolume.Text      := FormatFloat('##0.00',FEditRunConfig.Run_StartVolume);
      if(FEditRunConfig.Run_StartLevel <> NullFloat) then
        edtUpperLevel.Text          := FormatFloat('##0.00',FEditRunConfig.Run_StartLevel);
      if(FEditRunConfig.Run_StopLevel <> NullFloat) then
        edtLowerLevel.Text          := FormatFloat('##0.00',FEditRunConfig.Run_StopLevel);

      FEditRunConfig.Changed := True;
      SetButtonState;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.btnNewClick(Sender: TObject);
const OPNAME = 'TfrmConfiguration.btnNewClick';
var
  LName : string;
begin
  try
    btnNew.Color := btnNew.OnMouseColor;
    LName := InputBox('Please enter a unique name for this run configuration settings.','Name:','');
    if(LName <> '') then
    begin
      if(RWHModelData.RunConfigList.RWHRunConfigByName[LName] <> nil) then
        ShowMessage('Run configuration setting named('+LName+') already exist. PLease enter a new unique name.')
      else
      begin
        FEditRunConfig.PopulateWithDefaults;
        FEditRunConfig.RunName   := LName;
        FEditContext             := sdccAdd;
        FEditRunConfig.Changed   := True;
        FEditRunConfig.Populated := True;
        btnSaveClick(Self);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.btnDeleteClick(Sender: TObject);
const OPNAME = 'TfrmConfiguration.btnDeleteClick';
      CMesg  = 'Are you sure you want to delete the configuration data?';
var
  LRunConfig       : TRWHRunConfig;
begin
  try
    if(mrYes <> MessageDlg(CMesg,mtConfirmation,[mbYes,mbCancel	],0)) then Exit;
    FEditContext  := sdccDelete;
    LRunConfig    := TRWHRunConfig.Create(FAppModules);
    try
      LRunConfig.Assign(FEditRunConfig);
      if RWHModelData.DeleteRunConfigurationData(FEditRunConfig) then
      begin
        FEditRunConfig.Initialise;
        if Assigned(FOnRunConfigDeleted) then
           FOnRunConfigDeleted(LRunConfig);
      end;
    finally
      LRunConfig.Free;
    end;
    FEditContext      := sdccSelect;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.btnCopyClick(Sender: TObject);
const OPNAME = 'TfrmConfiguration.btnCopyClick';
var
  LName : string;
  LRunConfig       : TRWHRunConfig;
begin
  try
    LName := InputBox('Please enter a unique name for this run configuration settings.','Name:','');
    if(LName <> '') then
    begin
      if(RWHModelData.RunConfigList.RWHRunConfigByName[LName] <> nil) then
        ShowMessage('Run configuration setting named('+LName+') already exist. PLease enter a new unique name.')
      else
      begin
        LRunConfig := TRWHRunConfig.Create(FAppModules);
        try
          LRunConfig.Assign(FEditRunConfig);
          LRunConfig.Identifier   := NullInteger;
          FEditRunConfig.RunName  := LName;
          FEditContext            := sdccAdd;
          FEditRunConfig.Changed  := True;
          if RWHModelData.AddRunConfigurationData(LRunConfig) then
          begin
            FEditRunConfig.Initialise;
            if Assigned(FOnRunConfigAdded) then
               FOnRunConfigAdded(LRunConfig);
          end;
        finally
          LRunConfig.Free;
        end;
      end;
    end;
    FEditContext      := sdccSelect;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.btnSaveClick(Sender: TObject);
const OPNAME = 'TfrmConfiguration.btnSaveClick';
begin
  try
    if(FEditContext = sdccAdd) then
    begin
      if RWHModelData.AddRunConfigurationData(FEditRunConfig) then
      begin
        if Assigned(FOnRunConfigAdded) then
           FOnRunConfigAdded(FEditRunConfig);
      end;
    end
    else
    begin
      if FEditRunConfig.Changed then
      begin
        PopulateObject;
        if RWHModelData.UpdateRunConfigurationData(FEditRunConfig) then
          PopulateDialog;
      end;
    end;
    FEditRunConfig.Changed := False;
    FEditContext := sdccSelect;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.btnResetClick(Sender: TObject);
const OPNAME = 'TfrmConfiguration.btnResetClick';
begin
  try
    FEditRunConfig.Changed := False;
    PopulateDialog;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.edtStartingVolumeExit(Sender: TObject);
const OPNAME = 'TfrmConfiguration.edtStartingVolumeChange';
var
  LTankCount : integer;
  LMultiplier : double;
begin
  try
    LMultiplier   := 100.0;
    if(edtStartingVolume.Text <> FormatFloat('##0.00',FEditRunConfig.RunStartVolume)) then
      FEditRunConfig.Changed := True;
    if(edtUpperLevel.Text     <> FormatFloat('##0.00',FEditRunConfig.RunStartLevel)) then
      FEditRunConfig.Changed := True;
    if(edtLowerLevel.Text     <> FormatFloat('##0.00',FEditRunConfig.RunStopLevel)) then
      FEditRunConfig.Changed := True;

    if(edtRoofArea.Text           <> FormatFloat('##0.00',FEditRunConfig.RoofArea)) then
      FEditRunConfig.Changed := True;
    if(edtRunoffCoefficient.Text  <> FormatFloat('##0.00',FEditRunConfig.RoofRunoffCoef*LMultiplier)) then
      FEditRunConfig.Changed := True;

    if(edtHouseholdCount.Text       <> IntToStr(FEditRunConfig.HouseHoldNumber)) then
      FEditRunConfig.Changed := True;
    if(edtMembersPerHousehold.Text  <> IntToStr(FEditRunConfig.HouseHoldMembers)) then
      FEditRunConfig.Changed := True;
    if(edtPerCapitaDemand.Text      <> FormatFloat('##0.00',FEditRunConfig.HouseHoldDemandPP*LMultiplier)) then
      FEditRunConfig.Changed := True;

    if(edtTankCount.Text            <> IntToStr(FEditRunConfig.TankCount)) then
      FEditRunConfig.Changed := True;

    LTankCount := StrToIntDef(edtTankCount.Text,0);
    if(LTankCount >= 1)  and (strgrdTankVolume.Cells[0,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize01)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 2)  and (strgrdTankVolume.Cells[1,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize02)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 3)  and (strgrdTankVolume.Cells[2,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize03)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 4)  and (strgrdTankVolume.Cells[3,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize04)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 5)  and (strgrdTankVolume.Cells[4,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize05)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 6)  and (strgrdTankVolume.Cells[5,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize06)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 7)  and (strgrdTankVolume.Cells[6,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize07)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 8)  and (strgrdTankVolume.Cells[7,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize08)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 9)  and (strgrdTankVolume.Cells[8,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize09)) then
      FEditRunConfig.Changed := True;
    if(LTankCount >= 10) and (strgrdTankVolume.Cells[9,0] <> FormatFloat('##0.00',FEditRunConfig.TankSize10)) then
      FEditRunConfig.Changed := True;

    SetButtonState;
    SetComponentsEnabled(FEditRunConfig.Changed);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.SetComponentsEnabled(AEnabled: boolean);
const OPNAME = 'TfrmConfiguration.SetComponentsEnabled';
begin
  try
    if AEnabled then
    begin
      edtStartingVolume.Enabled         := True;
      edtUpperLevel.Enabled             := True;
      edtLowerLevel.Enabled             := True;
      edtHouseholdCount.Enabled         := True;
      edtMembersPerHousehold.Enabled    := True;
      edtPerCapitaDemand.Enabled        := True;
      edtRoofArea.Enabled               := True;
      edtRunoffCoefficient.Enabled      := True;
      edtTankCount.Enabled              := True;
      //strgrdTankVolume.Enabled          := True;

      edtStartingVolume.Color         := clWindow;
      edtUpperLevel.Color             := clWindow;
      edtLowerLevel.Color             := clWindow;
      edtHouseholdCount.Color         := clWindow;
      edtMembersPerHousehold.Color    := clWindow;
      edtPerCapitaDemand.Color        := clWindow;
      edtRoofArea.Color               := clWindow;
      edtRunoffCoefficient.Color      := clWindow;
      edtTankCount.Color              := clWindow;
      strgrdTankVolume.Color          := clWindow;

      lblStartingVolume.Enabled       := True;
      lblUpperLevel.Enabled           := True;
      lblLowerLevel.Enabled           := True;
      lblMembersPerHousehold.Enabled  := True;
      lblPerCapitaDemand.Enabled      := True;
      lblHouseholdCount.Enabled       := True;
      lblRoofArea.Enabled             := True;
      lblRunoffCoefficient.Enabled    := True;
      lblTankCount.Enabled            := True;
      lblTankVolume.Enabled           := True;
    end
    else
    begin
      edtStartingVolume.Color         := clSilver;
      edtUpperLevel.Color             := clSilver;
      edtLowerLevel.Color             := clSilver;
      edtHouseholdCount.Color         := clSilver;
      edtMembersPerHousehold.Color    := clSilver;
      edtPerCapitaDemand.Color        := clSilver;
      edtRoofArea.Color               := clSilver;
      edtRunoffCoefficient.Color      := clSilver;
      edtTankCount.Color              := clSilver;
      strgrdTankVolume.Color          := clSilver;

      edtStartingVolume.Enabled         := False;
      edtUpperLevel.Enabled             := False;
      edtLowerLevel.Enabled             := False;
      edtHouseholdCount.Enabled         := False;
      edtMembersPerHousehold.Enabled    := False;
      edtPerCapitaDemand.Enabled        := False;
      edtRoofArea.Enabled               := False;
      edtRunoffCoefficient.Enabled      := False;
      edtTankCount.Enabled              := False;
      strgrdTankVolume.Enabled          := False;

      lblStartingVolume.Enabled       := False;
      lblUpperLevel.Enabled           := False;
      lblLowerLevel.Enabled           := False;
      lblMembersPerHousehold.Enabled  := False;
      lblPerCapitaDemand.Enabled      := False;
      lblHouseholdCount.Enabled       := False;
      lblRoofArea.Enabled             := False;
      lblRunoffCoefficient.Enabled    := False;
      lblTankCount.Enabled            := False;
      lblTankVolume.Enabled           := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.cmboxTankSizesExit(Sender: TObject);
const OPNAME = 'TfrmConfiguration.cmboxTankSizesExit';
begin
  try
    if FPopulating then Exit;
    if(strgrdTankVolume.Cells[strgrdTankVolume.Col, strgrdTankVolume.Row] <> cmboxTankSizes.Text) then
    begin
      strgrdTankVolume.Cells[strgrdTankVolume.Col, strgrdTankVolume.Row] := cmboxTankSizes.Text;
      edtStartingVolumeExit(Sender);
    end;
    cmboxTankSizes.Visible := False;
    strgrdTankVolume.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmConfiguration.strgrdTankVolumeSelectCell(Sender: TObject;  ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TfrmConfiguration.strgrdTankVolumeSelectCell';
var
  R: TRect;
begin
  try
    R := strgrdTankVolume.CellRect(ACol, ARow);
    R.Left := R.Left + strgrdTankVolume.Left;
    R.Right := R.Right + strgrdTankVolume.Left;
    R.Top := R.Top + strgrdTankVolume.Top;
    R.Bottom := R.Bottom + strgrdTankVolume.Top;
    with cmboxTankSizes do
    begin
      Left := R.Left + 1;
      Top := R.Top + 1;
      Width := (R.Right + 1) - R.Left;
      Height := (R.Bottom + 1) - R.Top;
      Text := strgrdTankVolume.Cells[ACol, ARow];
      Visible := True;
      SetFocus;
    end;
    CanSelect := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
