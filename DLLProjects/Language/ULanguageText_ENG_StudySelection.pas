//
//  UNIT      : Contains language text.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit ULanguageText_ENG_StudySelection;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  AAdd('FormCaption','System','System');
  AAdd('FormCaption','Yield','Yield');
  AAdd('FormCaption','Planning','Planning');
  AAdd('FormCaption','Hydrology','Hydrology');
  AAdd('FormCaption','Rainfall','Rainfall');
  AAdd('FormCaption','BSPPS','Population Economic Socio Information Management System (PesIMS) - Ver 1.0.28');
  AAdd('FormCaption','DailyDiversion','DailyDiversion');
  AAdd('FormCaption','IFRPreProcessor','IFRPreProcessor');
  AAdd('FormCaption','YRC Model','YRC Model');
  AAdd('FormCaption','Project Gauges','Project Gauges');
  AAdd('FormCaption','Stomsa','Stomsa');
  AAdd('FormCaption','RainWaterHarvesting','Rain Water Harvesting');
  AAdd('FormCaption','DDTS','Dam Daily Time Step');
  AAdd('TDbTableDataManager','strExport','Export Data');
  AAdd('TDbTableDataManager','strBuildDataBase','Build DataBase');
  AAdd('TDbTableDataManager','strImport','Import Data');
  AAdd('TDbTableDataManager','strExportChangeLists','Export Change Lists Data');
  AAdd('TDbTableDataManager','strImportChangeLists','Import Change Lists Data');
  AAdd('TDbTableDataManager','strCopy','Copy Study Data');
  AAdd('TDbTableDataManager','strDelete','Delete Study Data');
  AAdd('TGISStudyAreaSelectorPanel','ModelLabel','Model');
  AAdd('TGISStudyAreaSelectorPanel','DateLabel','Date');
  AAdd('TfrmStudyCopyForm','StudyLabel','Study');
  AAdd('TfrmStudyEditForm','ModelLabel','Model');
  AAdd('TfrmStudyEditForm','StudyLabel','Study');
  AAdd('FilePath','PathNotDosCompatible','File path is incompatible with DOS applications. Make sure that any Directory/File name is not more than 8 characters and the total length is not more than 40 characters.');
end;

end.
