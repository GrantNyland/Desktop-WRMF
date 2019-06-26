//
//
//  UNIT      : Contains TStomsaModelDataObject Class
//  AUTHOR    : Grant Nyland
//  DATE      : 2019-06-25
//  COPYRIGHT : Copyright © 2019 DWS
//
//
unit UStomsaModelDataObject;

interface

uses
  VoaimsCom_TLB,
  UParameterData,
  UViewModelDataObject,
  UAbstractModelData;

type
  TStomsaModelDataObject = class(TAbstractModelData, IStomsaModelData)
  protected
    FCatchmentData: TParamSetup;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_CatchmentList: IParamSetup; safecall;
  public
    function GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled:boolean): boolean; override;
  end;

implementation

uses
  SysUtils,
  UFileNames,
  UViewModelDataValidator,
  UErrorHandlingOperations;

procedure TStomsaModelDataObject.CreateMemberObjects;
const OPNAME = 'TStomsaModelDataObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FCatchmentData := TParamSetup.Create(FAppModules);
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaModelDataObject.DestroyMemberObjects;
const OPNAME = 'TStomsaModelDataObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FCatchmentData);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaModelDataObject.Get_CatchmentList: IParamSetup;
const OPNAME = 'TStomsaModelDataObject.Get_CatchmentList';
begin
  Result := nil;
  try
    Result := FCatchmentData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaModelDataObject.GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TStomsaModelDataObject.GetViewDataItems';
var
  LUpperViewId: string;
begin
  Result := False;
  try
    AHandled := False;
    if (Trim(AViewId) <> '') and Assigned(AItemsList) then
    begin
      LUpperViewId := UpperCase(Trim(AViewId));
      if(Pos('HYDROLOGYFILENAMES',LUpperViewId) = 1) then
        AHandled := GetHydrologyFilesViewDataItems(AViewId, AItemsList, TModelFileNames(FileNamesObject))
      else
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

