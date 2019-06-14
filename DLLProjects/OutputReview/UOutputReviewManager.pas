//
//
//  UNIT      : Contains TOutputReviewManager Class
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputReviewManager;

interface

uses
  VCL.ComCtrls,
  UDataViewerManager,
  UAbstractModelObjects;

type
  TOutputReviewManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
  end;

implementation

uses
  SysUtils,
  UOutputReviewSheet,
  UErrorHandlingOperations;

procedure TOutputReviewManager.CreateMemberObjects;
const OPNAME = 'TOutputReviewManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TOutputReviewSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
