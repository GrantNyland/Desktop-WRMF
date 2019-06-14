//
//
//  UNIT      : Contains TSystemModelDatasetConstructor Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USystemModelDatasetConstructor;

interface

uses
  USQLDatabaseLayer,
  UAbstractObject;

type
  TSystemModelDatasetConstructor = class(TAbstractDatasetConstructor)
  public
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; override;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UUserDataset,
  UUserAdministrationDataset,
  UEmptySQLDataset,
  ULanguageDataset,
  UStudyAreaDataset,
  UFieldPropertyDataset,
  UStudyDocumentsDataset,
  UErrorHandlingOperations;

function TSystemModelDatasetConstructor.CreateDataSet(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean;
const
  OPNAME = 'TSystemModelDatasetConstructor.CreateDataSet';
begin
  Result := True;
  ADataSet := nil;
  try

    // Create the data set.
    case TDataSetType(ADataSetType) of
      dtExecSQL             : ADataSet := TEmptySQLDataset.Create(FAppModules);
      dtUser                : ADataSet := TUserDataset.Create(FAppModules);
      dtUserAdministration  : ADataSet := TUserAdministrationDataset.Create(FAppModules);
      dtStudyDocumentsMenu  : ADataSet := TStudyDocumentsMenuDataset.Create(FAppModules);
      dtStudyDocumentsLinks : ADataSet := TStudyDocumentsLinksDataset.Create(FAppModules);
      dtSelectStudyArea     : ADataSet := TStudyAreaDataset.Create(FAppModules);
      dtFieldProperty       : ADataSet := TFieldPropertyDataset.Create(FAppModules);
      dtAvailableLanguage   : ADataSet := TAvailableLanguagesDataset.Create(FAppModules);
      dtLanguageText        : ADataSet := TLanguageTextDataset.Create(FAppModules);
    else
      Result := False;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
