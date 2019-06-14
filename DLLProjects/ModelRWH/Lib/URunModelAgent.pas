{*******************************************************}
{                                                       }
{  UNIT      : Contains TRunModelAgent                  }
{  AUTHOR    : Dziedzi Ramulondi                        }
{  DATE      : 27/07/2011                               }
{  COPYRIGHT : Copyright © 2011 T-Systems               }
{                                                       }
{*******************************************************}

unit URunModelAgent;

interface

uses Classes,Contnrs,Forms,
     UProvince,
     UMagDist,
     UStation,
     URunConfig,
     URRWHSolver,
     UAbstractClass;

type

  TRunModelAgent = class(TAbstractClass)
  private
  protected
    FSolver               : TSolver;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean; override;
    function RunModel(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UFrameWork,
  UUtilities,
  DateUtils,
  UStationPerMagDist,
  UStationPerProvince,
  UErrorHandlingUtilities;

  { TRunModelAgent }

procedure TRunModelAgent.AfterConstruction;
const OPNAME = 'TRunModelAgent.AfterConstruction';
begin
  inherited;
  try
    FSolver                := TSolver.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunModelAgent.BeforeDestruction;
const OPNAME = 'TRunModelAgent.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FSolver);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunModelAgent.Initialise: boolean;
const OPNAME = 'TRunModelAgent.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunModelAgent.RunModel(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TRunModelAgent.LoadSelectedStationRainfallStations';
{var
  LIndex   : integer;
  LStation: TStation;}
begin
  Result := False;
  try
    {LStation := TStation(FRainfallStationList[LIndex]);
    Result := True;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
