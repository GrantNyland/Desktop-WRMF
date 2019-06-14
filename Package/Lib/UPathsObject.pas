//
//
//  UNIT      : Contains TPathsObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPathsObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type


  TPathsObject = class(TAbstractDataObject)
  protected
    FFileNamePrefix: TString;
    FFileNamePrefixComment: TString;
    FInputFilesPath: TString;
    FInputFilesPathComment: TString;
    FOutputFilesPath: TString;
    FOutputFilesPathComment: TString;
    FHydrologyPath: TString;
    FSpecifiedDemandPath: TString;
    FComment: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property FileNamePrefix: TString         read FFileNamePrefix         write FFileNamePrefix;
    property FileNamePrefixComment: TString  read FFileNamePrefixComment  write FFileNamePrefixComment;
    property InputFilesPath: TString         read FInputFilesPath         write FInputFilesPath;
    property InputFilesPathComment: TString  read FInputFilesPathComment  write FInputFilesPathComment;
    property OutputFilesPath: TString        read FOutputFilesPath        write FOutputFilesPath;
    property OutputFilesPathComment: TString read FOutputFilesPathComment write FOutputFilesPathComment;
    property HydrologyPath: TString          read FHydrologyPath          write FHydrologyPath;
    property SpecifiedDemandPath: TString    read FSpecifiedDemandPath    write FSpecifiedDemandPath;
    property Comment: TStringList            read FComment;
    procedure Reset;override;
    function Initialise: boolean;override;
    function Populated: boolean;
  end;

implementation


uses UErrorHandlingOperations;

{TPathsObject}

procedure TPathsObject.CreateMemberObjects;
const OPNAME = 'TPathsObject.CreateMemberObjects';
Begin
  try
    FFileNamePrefix         := TString.Create;
    FFileNamePrefixComment  := TString.Create;
    FInputFilesPath         := TString.Create;
    FInputFilesPathComment  := TString.Create;
    FOutputFilesPath        := TString.Create;
    FOutputFilesPathComment := TString.Create;
    FHydrologyPath          := TString.Create;
    FSpecifiedDemandPath    := TString.Create;
    FComment                := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPathsObject.DestroyMemberObjects;
const OPNAME = 'TPathsObject.DestroyMemberObjects';
Begin
  try
    FFileNamePrefix.Free;
    FFileNamePrefixComment.Free;
    FInputFilesPath.Free;
    FInputFilesPathComment.Free;
    FOutputFilesPath.Free;
    FOutputFilesPathComment.Free;
    FHydrologyPath.Free;
    FSpecifiedDemandPath.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPathsObject.Reset;
const OPNAME = 'TPathsObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPathsObject.Initialise: boolean;
const OPNAME = 'TPathsObject.Initialise';
Begin
  Result := False;
  try
    //Data and initialised.
    FComment.Clear;

    FFileNamePrefix.FData := '';
    FFileNamePrefix.FInitalised := False;
    FFileNamePrefix.FLength := 8;
    FFileNamePrefix.FDecimal := 0;
    FFileNamePrefix.FDefaultPadding := True;


    FFileNamePrefixComment.FData := '';
    FFileNamePrefixComment.FInitalised := False;
    FFileNamePrefixComment.FLength := 0;
    FFileNamePrefixComment.FDecimal := 0;
    FFileNamePrefixComment.FDefaultPadding := True;

    FInputFilesPath.FData := '';
    FInputFilesPath.FInitalised := False;
    FInputFilesPath.FLength := 40;
    FInputFilesPath.FDecimal := 0;
    FInputFilesPath.FDefaultPadding := True;

    FInputFilesPathComment.FData := '';
    FInputFilesPathComment.FInitalised := False;
    FInputFilesPathComment.FLength := 0;
    FInputFilesPathComment.FDecimal := 0;
    FInputFilesPathComment.FDefaultPadding := True;

    FOutputFilesPath.FData := '';
    FOutputFilesPath.FInitalised := False;
    FOutputFilesPath.FLength := 40;
    FOutputFilesPath.FDecimal := 0;
    FOutputFilesPath.FDefaultPadding := True;

    FOutputFilesPathComment.FData := '';
    FOutputFilesPathComment.FInitalised := False;
    FOutputFilesPathComment.FLength := 0;
    FOutputFilesPathComment.FDecimal := 0;
    FOutputFilesPathComment.FDefaultPadding := True;

    FHydrologyPath.FData := '';
    FHydrologyPath.FInitalised := False;
    FHydrologyPath.FLength := 0;
    FHydrologyPath.FDecimal := 0;
    FHydrologyPath.FDefaultPadding := True;

    FSpecifiedDemandPath.FData := '';
    FSpecifiedDemandPath.FInitalised := False;
    FSpecifiedDemandPath.FLength := 0;
    FSpecifiedDemandPath.FDecimal := 0;
    FSpecifiedDemandPath.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPathsObject.Populated: boolean;
const OPNAME = 'TPathsObject.Populated';
Begin
  Result := False;
  try
    Result := FFileNamePrefix.FInitalised and
              FOutputFilesPath.FInitalised;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
