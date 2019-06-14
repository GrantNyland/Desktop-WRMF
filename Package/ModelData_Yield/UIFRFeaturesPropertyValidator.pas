//
//
//  UNIT      : Contains the class TIFRFeaturesPropertyValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2007/02/06
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UIFRFeaturesPropertyValidator;

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
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UIFRFeaturesPropertyDialog;

type
  TIFRFeaturesPropertyValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FPopulating : boolean;
    procedure CreateMemberObjects; override;
    procedure RePopulateDataViewer;
    procedure UpdateReferenceInflowType(Sender: TObject);
    function  IFRFeaturesPropertyDialog: TIFRFeaturesPropertyDialog;
  public
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
  end;

implementation

uses
  SysUtils,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TIFRFeaturesPropertyValidator }

procedure TIFRFeaturesPropertyValidator.CreateMemberObjects;
const OPNAME = 'TIFRFeaturesPropertyValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TIFRFeaturesPropertyDialog.Create(FPanelOwner,FAppModules);

    with IFRFeaturesPropertyDialog do
    begin
      ReferenceInflowTypeRadioGroup.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRInflowOption');
      ReferenceInflowTypeRadioGroup.OnEnter       := OnEditControlEnter;
      ReferenceInflowTypeRadioGroup.OnClick       := UpdateReferenceInflowType;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeaturesPropertyValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIFRFeaturesPropertyValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.Properties');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeaturesPropertyValidator.ClearDataViewer;
const OPNAME = 'TIFRFeaturesPropertyValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    IFRFeaturesPropertyDialog.ReferenceInflowTypeRadioGroup.ItemIndex := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeaturesPropertyValidator.PopulateDataViewer;
const OPNAME = 'TIFRFeaturesPropertyValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeaturesPropertyValidator.RePopulateDataViewer;
const OPNAME = 'TIFRFeaturesPropertyValidator.RePopulateDataViewer';
var
  LReferenceInflowType: integer;
begin
  try
    FPopulating := True;
    try
      LReferenceInflowType := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IFRFeatureList.InflowOption-1;
      case LReferenceInflowType of
        0:IFRFeaturesPropertyDialog.ReferenceInflowTypeRadioGroup.ItemIndex := LReferenceInflowType;
        1:IFRFeaturesPropertyDialog.ReferenceInflowTypeRadioGroup.ItemIndex := LReferenceInflowType;
      else
        IFRFeaturesPropertyDialog.ReferenceInflowTypeRadioGroup.ItemIndex := -1;
      end;
    finally
      FPopulating := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeaturesPropertyValidator.IFRFeaturesPropertyDialog : TIFRFeaturesPropertyDialog;
const OPNAME = 'TIFRFeaturesPropertyValidator.IFRFeaturesPropertyDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TIFRFeaturesPropertyDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeaturesPropertyValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIFRFeaturesPropertyValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
   if (AFieldName = 'IFRInflowOption') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeaturesPropertyValidator.UpdateReferenceInflowType(Sender: TObject);
const OPNAME = 'TIFRFeaturesPropertyValidator.UpdateReferenceInflowType';
var
  LInflowOption : integer;
begin
  try
    if FPopulating then Exit;
    LInflowOption := IFRFeaturesPropertyDialog.ReferenceInflowTypeRadioGroup.ItemIndex;
    if(LInflowOption >= 0) then
      TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IFRFeatureList.InflowOption := LInflowOption+1;
    with IFRFeaturesPropertyDialog do
    begin
      case LInflowOption of
        0 : OptionDescriptionsLabel.Caption := FAppModules.Language.GetString('LabelCaption.NaturalOptionDescr');
        1 : OptionDescriptionsLabel.Caption := FAppModules.Language.GetString('LabelCaption.DevelopedOptionDescr1')+
                                               FAppModules.Language.GetString('LabelCaption.DevelopedOptionDescr2');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

