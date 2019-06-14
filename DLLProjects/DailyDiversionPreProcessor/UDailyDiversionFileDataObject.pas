unit UDailyDiversionFileDataObject;

interface
uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UDataFileObjects,
  UDailyFlowDataObject,
  UReleaseStructureObject,
  UPathsObject,
  UDailyInstreamFlowDataObject,
  UDailyIFRData,
  UChannelData,
  UAbstractObject;

type

  TDailyDiversionFileDataObject = class(TDataFileObjects)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    // Station Daily data *.csv
    FDailyFlowDataObject : TDailyFlowDataObject;
    // Station Daily Instream Flow data *.csv
    FDailyInstreamFlowDataObject : TDailyInstreamFlowDataObject;
    // F014 *.dat
    FFile14DataObject : TReleaseControlStructureObject;
    //File Wyrm.dat
    FPathsObject : TPathsObject;
    procedure Reset;override;
    function Initialise: boolean;override;




end;
implementation
uses
  UConstants,
  UErrorHandlingOperations;
{ TDailyDiversionFileDataObject }

procedure TDailyDiversionFileDataObject.CreateMemberObjects;
const OPNAME = 'TDailyDiversionFileDataObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDailyFlowDataObject := TDailyFlowDataObject.Create;
    FDailyInstreamFlowDataObject := TDailyInstreamFlowDataObject.Create;
    FFile14DataObject := TReleaseControlStructureObject.Create;
    FPathsObject := TPathsObject.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionFileDataObject.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionFileDataObject.DestroyMemberObjects';
begin
  try
    FDailyFlowDataObject.Free;
    FDailyInstreamFlowDataObject.Free;
    FFile14DataObject.Free;
    FPathsObject.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionFileDataObject.Initialise: boolean;
const OPNAME = 'TDailyDiversionFileDataObject.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := FDailyFlowDataObject.Initialise;
    Result := Result and FDailyInstreamFlowDataObject.Initialise;
    Result := Result and FFile14DataObject.Initialise;
    Result := Result and FPathsObject.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionFileDataObject.Reset;
const OPNAME = 'TDailyDiversionFileDataObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
 