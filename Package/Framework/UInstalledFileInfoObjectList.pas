unit UInstalledFileInfoObjectList;

interface
uses
  Classes,
  Contnrs,
  SysUtils,
  UAbstractObject;
type

  TFileDataObject = class(TAbstractObject)
  protected
    FIsDirectory      : boolean;
    FFileName         : string;
    FDirectory        : string;
  public
    property IsDirectory : boolean read FIsDirectory write FIsDirectory;
    property FileName    : string  read FFileName    write FFileName;
    property Directory   : string read FDirectory    write FDirectory;
  end;

  TInstalledFiles = class(TAbstractObject)
  protected
    FFileName       : string;
  public
    function Initialise: Boolean; override;
    procedure AddInstalledFile ( AFileName : string  );
    property  FileName : string  read FFileName    write FFileName;

  end;

  TInstalledFileInfoObjectList = class(TAbstractObject)
  protected
    FCanBeUninstalled : boolean;
    FFileName : string;
    FFilesList : TStringList;
    FWritableFilesList : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetFileName   : string;
    function GetFileByIndex ( AIndex: integer ) : TInstalledFiles;
    function GetFileByName ( AName : string ) : TInstalledFiles;
    function GetWritableFileByName(AName : string) : TInstalledFiles;

    function GetFilesCount : integer;
  public
    function Initialise: Boolean; override;
    procedure AddFilesData(AFileName : string);
    procedure AddWritableFilesData(AFileName : string);
    property CanBeUninstalled : boolean read FCanBeUninstalled write FCanBeUninstalled;
    property InstalledFileByName [ AFileName : string ] : TInstalledFiles read GetFileByName;
    property WriterbleFileByName[AFileName : string] : TInstalledFiles read GetWritableFileByName;
  end;

  TInstalledFilesItemAddFunction = procedure ( AFileName : string ) of object;
  TWritableFilesAddFunction = procedure(AFileName : string) of object;

procedure LoadInstalledFiles ( AAdd : TInstalledFilesItemAddFunction );
procedure LoadWritableFiles(Adda : TWritableFilesAddFunction);



implementation
uses
  UErrorHandlingOperations;
{ TInstalledFiles }

procedure TInstalledFiles.AddInstalledFile ( AFileName : string );
const OPNAME = 'TInstalledFiles.AddInstalledFile';
begin
  try
    FFileName := AFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TInstalledFiles.Initialise : Boolean;
const OPNAME = 'TInstalledFiles.Initialise';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TInstalledFileInfoObjectList }

function TInstalledFileInfoObjectList.Initialise : Boolean;
const OPNAME = 'TInstalledFileInfoObjectList.Initialise';
begin
  Result := False;
  try
    LoadInstalledFiles(AddFilesData);
    LoadWritableFiles(AddWritableFilesData);
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TInstalledFileInfoObjectList.AddFilesData ( AFileName : string );
const OPNAME = 'TInstalledFileInfoObjectList.AddFilesData';
var
  LInstalledFiles : TInstalledFiles;
begin
  try
    LInstalledFiles := TInstalledFiles.Create;
    LInstalledFiles.AddInstalledFile ( AFileName );
    FFilesList.AddObject ( AFileName, LInstalledFiles );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TInstalledFileInfoObjectList.AddWritableFilesData(AFileName : string);
const OPNAME = 'TInstalledFileInfoObjectList.AddWritableFilesData';
var
  LInstalledFiles : TInstalledFiles;
begin
  try
    LInstalledFiles := TInstalledFiles.Create;
    LInstalledFiles.AddInstalledFile(AFileName );
    FWritableFilesList.AddObject(AFileName, LInstalledFiles);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TInstalledFileInfoObjectList.CreateMemberObjects;
const OPNAME = 'TInstalledFileInfoObjectList.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FFilesList := TStringList.Create;
    FWritableFilesList := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TInstalledFileInfoObjectList.DestroyMemberObjects;
const OPNAME = 'TInstalledFileInfoObjectList.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FFilesList);
    FreeAndNil(FWritableFilesList);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TInstalledFileInfoObjectList.GetFileByIndex ( AIndex : integer ) : TInstalledFiles;
const OPNAME = 'TInstalledFileInfoObjectList.GetFileByIndex';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TInstalledFileInfoObjectList.GetFileByName ( AName : string ) : TInstalledFiles;
const OPNAME = 'TInstalledFileInfoObjectList.GetFileByName';
var
  LIndex : integer;
begin
  Result := nil;
  try
    if ( Trim ( AName ) <> '' ) then
    begin
      LIndex := FFilesList.IndexOf ( AName );
      if ( LIndex >= 0 ) then
      begin
        Result := TInstalledFiles ( FFilesList.Objects [ LIndex ] );
      end
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TInstalledFileInfoObjectList.GetFileName : string;
const OPNAME = 'TInstalledFileInfoObjectList.GetFileName';
begin
  Result := '';
  try
    Result := FFileName
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TInstalledFileInfoObjectList.GetFilesCount: integer;
const OPNAME = 'TInstalledFileInfoObjectList.GetFilesCount';
begin
  Result := 0;
  try
    Result := FFilesList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TInstalledFileInfoObjectList.GetWritableFileByName(AName: string): TInstalledFiles;
const OPNAME = 'GetWritableFileByName.GetFileByName';
var
  LIndex : integer;
begin
  Result := nil;
  try
    if ( Trim(AName) <> '' ) then
    begin
      LIndex := FWritableFilesList.IndexOf(AName);
      if (LIndex >= 0) then
      begin
        Result := TInstalledFiles(FWritableFilesList.Objects[LIndex]);
      end
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure LoadWritableFiles(Adda : TWritableFilesAddFunction);
const OPNAME = 'LoadInstalledFiles';
begin
  try
    // database
    Adda('WRMFDatabase.mdb');
    Adda('WRSM2000.mdb');
    //wrmf
    Adda('WRMF.ini');
    Adda('VoaimsCom.ini');
    Adda('TestRainfallCom.xls');
    Adda('TestVoaimsCom.xls');
    Adda('Training - StorageVsYield 4.xls');
    Adda('Training - StorageVsYield 10.xls');
    Adda('StorageVsYield.xls');
    // Logs
    Adda('WRMF_ErrorLog.txt');
    Adda('VISIO_ErrorLog.txt');
    Adda('ViewSettings.ini');
    // network diagrams
    Adda('DefaultDrawing.VSD');
    Adda('DefaultDrawingGIS.VSD');
    Adda('DefaultDrawingHydro.VSD');
    Adda('DefaultDrawingHydroGIS.VSD');
    Adda('DefaultDrawingPlann.VSD');
    Adda('DefaultDrawingPlannGIS.VSD');
    Adda('DefaultDrawingYield.VSD');
    Adda('DefaultDrawingYieldGIS.VSD');
    Adda('HydroStencil.VSD');
    Adda('WRPMStencil.VSD');
    Adda('WRYMStencil.vss');
    Adda('B71 Fig 1.vsd');
    Adda('B71 Fig 2 GIS.vsd');
    Adda('B71.glf');
    Adda('Base Scenario.glf');
    Adda('Base Scenario-Fig 1.vsd');
    Adda('B72 Fig 1.vsd');
    Adda('Base Scenario-Fig 1.vss');
    Adda('Scenario 1-Fig 1.vsd');



    Adda('Daily Diversion Example.csv');
    Adda('Example_IFR.csv');
    Adda('BIGBEND Monthly Rainfall.csv');

    Adda('ComparatorSettings.ini');
    Adda('OutputComparator.exe');


    Adda('br_astats.exe');
    Adda('br_sawb.exe');
    Adda('Classr.exe');
    Adda('CLASSR_2013d.exe');
    Adda('ClassrDe.exe');
    Adda('crossref.all');
    Adda('FileConvert.exe');
    Adda('get_names.exe');
    Adda('HDYP08.exe');
    Adda('LF90.EER');
    Adda('Modified_Rainfall_IMS.csv');
    Adda('Patchr.exe');
    Adda('PATCHR_2013d.exe');
    Adda('PatchrDe.exe');
    Adda('tnt.exe');




  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure LoadInstalledFiles ( AAdd : TInstalledFilesItemAddFunction );
const OPNAME = 'LoadInstalledFiles';
begin
  try
    //Wrmf

    AAdd ( 'borlndmm.dll' );
    AAdd ( 'crossref.csv' );
    AAdd ( 'dclaxserver70.bpl' );
    AAdd ( 'qtintf70.dll' );
    AAdd ( 'TestRainfallCom.xls' );
    AAdd ( 'vclie70.bpl' );
    AAdd ( 'VoaimsCom.dll' );
    AAdd ( 'VoaimsCom.ini' );
    AAdd ( 'WRMF.exe' );
    AAdd ( 'WRMF.ini' );
    //bin
    AAdd ( 'bin' );
    AAdd ( '_movedlls.bat' );
    AAdd ( 'AccessControl.dll' );
    AAdd ( 'Changes.dll' );
    AAdd ( 'FieldProperties.dll' );
    AAdd ( 'FileEditor.dll' );
    AAdd ( 'GridEditor.dll' );
    AAdd ( 'GridOutputEditor.dll' );
    AAdd ( 'InformixDatabaseLayer.dll' );
    AAdd ( 'LanguageManager.dll' );
    AAdd ( 'MainForm.dll' );
    AAdd ( 'MetaData.dll' );
    AAdd ( 'ModelCapability.dll' );
    AAdd ( 'ModelHydrology.dll' );
    AAdd ( 'ModelPesIMS.dll' );
    AAdd ( 'ModelRainfallData.dll' );
    AAdd ( 'ModelYield.dll' );
    AAdd ( 'ModelYRC.dll' );
    AAdd ( 'MSAccessDatabaseLayer.dll' );
    AAdd ( 'OutputReview.dll' );
    AAdd ( 'ParadoxDatabaseLayer.dll' );
    AAdd ( 'PrintManager.dll' );
    AAdd ( 'StudyDocuments.dll' );
    AAdd ( 'StudySelection.dll' );
    AAdd ( 'TimeSeriesComparitor.dll' );
    AAdd ( 'UNZDLL.DLL' );
    AAdd ( 'ViewDataGraph.dll' );
    AAdd ( 'ViewOutputGraph.dll' );
    AAdd ( 'VisioNetworkVisualiser.dll' );
    AAdd ( 'WeatherEvents.dll' );
    AAdd ( 'WRMFGraphics.dll' );
    AAdd ( 'YieldReliabilityCurve.dll' );
    AAdd ( 'ZIPDLL.DLL' );
    AAdd ( 'ZipWrapper.dll' );
    AAdd ( 'WRMFSplashScreen.dll' );
    //Covers
    AAdd ( '30mingrid.dbf' );
    AAdd ( '30mingrid.sbn' );
    AAdd ( '30mingrid.sbx' );
    AAdd ( '30mingrid.shp' );
    AAdd ( '30mingrid.shx' );
    AAdd ( 'all towns and cities_points_east.dbf' );
    AAdd ( 'all towns and cities_points_east.ixs' );
    AAdd ( 'all towns and cities_points_east.mxs' );
    AAdd ( 'all towns and cities_points_east.sbn' );
    AAdd ( 'all towns and cities_points_east.sbx' );
    AAdd ( 'all towns and cities_points_east.shp' );
    AAdd ( 'all towns and cities_points_east.shx' );
    AAdd ( 'all towns and cities_poly.dbf' );
    AAdd ( 'all towns and cities_poly.sbn' );
    AAdd ( 'all towns and cities_poly.sbx' );
    AAdd ( 'all towns and cities_poly.shp' );
    AAdd ( 'all towns and cities_poly.shx' );

    AAdd ( 'all towns and cities_points_west.dbf' );
    AAdd ( 'all towns and cities_points_west.ixs' );
    AAdd ( 'all towns and cities_points_west.mxs' );
    AAdd ( 'all towns and cities_points_west.sbn' );
    AAdd ( 'all towns and cities_points_west.sbx' );
    AAdd ( 'all towns and cities_points_west.shp' );
    AAdd ( 'all towns and cities_points_west.shx' );

    AAdd ( 'avg_rainfall.dbf' );
    AAdd ( 'avg_rainfall.sbn' );
    AAdd ( 'avg_rainfall.sbx' );
    AAdd ( 'avg_rainfall.shp' );
    AAdd ( 'avg_rainfall.shx' );

    AAdd ( 'consgeo.dbf' );
    AAdd ( 'consgeo.meta' );
    AAdd ( 'consgeo.sbn' );
    AAdd ( 'consgeo.sbx' );
    AAdd ( 'consgeo.shp' );
    AAdd ( 'consgeo.shx' );

    AAdd ( 'dams and lakes.dbf' );
    AAdd ( 'dams and lakes.sbn' );
    AAdd ( 'dams and lakes.sbx' );
    AAdd ( 'dams and lakes.shp' );
    AAdd ( 'dams and lakes.shx' );
    AAdd ( 'dams and lakes_data.txt' );

    AAdd ( 'africa.dbf' );
    AAdd ( 'africa.shp' );
    AAdd ( 'africa.shx' );
    AAdd ( 'africa_data.txt' );

    AAdd ( 'MagDist.dbf' );
    AAdd ( 'MagDist.sbn' );
    AAdd ( 'MagDist.sbx' );
    AAdd ( 'MagDist.shp' );
    AAdd ( 'MagDist.shx' );
    AAdd ( 'MagDist_data.txt' );

    AAdd ( 'btn.dbf' );
    AAdd ( 'btn.sbn' );
    AAdd ( 'btn.sbx' );
    AAdd ( 'btn.shp' );
    AAdd ( 'btn.shx' );

    AAdd ( 'major towns and cities_point.dbf' );
    AAdd ( 'major towns and cities_point.sbn' );
    AAdd ( 'major towns and cities_point.sbx' );
    AAdd ( 'major towns and cities_point.shp' );
    AAdd ( 'major towns and cities_point.shx' );

    AAdd ( 'major towns and cities_poly.dbf' );
    AAdd ( 'major towns and cities_poly.sbn' );
    AAdd ( 'major towns and cities_poly.sbx' );
    AAdd ( 'major towns and cities_poly.shp' );
    AAdd ( 'major towns and cities_poly.shx' );

    AAdd ( 'national roads.dbf' );
    AAdd ( 'national roads.sbn' );
    AAdd ( 'national roads.sbx' );
    AAdd ( 'national roads.shp' );
    AAdd ( 'national roads.shx' );

    AAdd ( 'primary catchments.dbf' );
    AAdd ( 'primary catchments.shp' );
    AAdd ( 'primary catchments.shx' );

    AAdd ( 'prim_riv.dbf' );
    AAdd ( 'prim_riv.sbn' );
    AAdd ( 'prim_riv.sbx' );
    AAdd ( 'prim_riv.shp' );
    AAdd ( 'prim_riv.shx' );

    AAdd ( 'drgnq.dbf' );
    AAdd ( 'drgnq.sbn' );
    AAdd ( 'drgnq.sbx' );
    AAdd ( 'drgnq.shp' );
    AAdd ( 'drgnq.shx' );
    AAdd ( 'PROVINCE.CDX' );
    AAdd ( 'province.dbf' );
    AAdd ( 'PROVINCE.INF' );
    AAdd ( 'Province.sbn' );
    AAdd ( 'Province.sbx' );
    AAdd ( 'province.shp' );
    AAdd ( 'province.shx' );
    AAdd ( 'Province_data.txt' );
    AAdd ( 'Province_Extent.txt' );
    AAdd ( 'Province_PROVNAME.txt' );

    AAdd ( 'catchments.dbf' );
    AAdd ( 'catchments.sbn' );
    AAdd ( 'catchments.sbx' );
    AAdd ( 'catchments.shp' );
    AAdd ( 'catchments.shx' );
    AAdd ( 'WMAREAS.SHP' );
    AAdd ( 'WMAREAS.SHX' );

    AAdd ( 'spoor_rail.dbf' );
    AAdd ( 'spoor_rail.shp' );
    AAdd ( 'spoor_rail.shx' );

    AAdd ( 'rainfall_stations.dbf' );
    AAdd ( 'rainfall_stations.shp' );
    AAdd ( 'rainfall_stations.shx' );

    AAdd ( 'RSFlyOvr.dbf' );
    AAdd ( 'RSFlyOvr.shp' );
    AAdd ( 'RSFlyOvr.shx' );

    AAdd ( 'secondary catchments.dbf' );
    AAdd ( 'secondary catchments.shp' );
    AAdd ( 'secondary catchments.shx' );

    AAdd ( 'sec_riv.dbf' );
    AAdd ( 'sec_riv.sbn' );
    AAdd ( 'sec_riv.sbx' );
    AAdd ( 'sec_riv.shp' );
    AAdd ( 'sec_riv.shx' );

    AAdd ( 'study sub_areas.DBF' );
    AAdd ( 'study sub_areas.sbn' );
    AAdd ( 'study sub_areas.sbx' );
    AAdd ( 'study sub_areas.shp' );
    AAdd ( 'study sub_areas.shx' );

    AAdd ( 'topo.dbf' );
    AAdd ( 'topo.sbn' );
    AAdd ( 'topo.sbx' );
    AAdd ( 'topo.shp' );
    AAdd ( 'topo.shx' );
    AAdd ( 'topoleg.avl' );

    AAdd ( 'WMAREAS.CDX' );
    AAdd ( 'WMAREAS.dbf' );
    AAdd ( 'WMAREAS.SBN' );
    AAdd ( 'WMAREAS.SBX' );
    AAdd ( 'wmareas.shp' );
    AAdd ( 'wmareas.shx' );
    AAdd ( 'wmareas_data.txt' );
    AAdd ( 'wmareas_Extent.txt' );

    AAdd ( 'StudySelectionView.gvf' );
    AAdd ( 'NetworkVisualiserView.gvf' );
    AAdd ( 'RainGaugeView.gvf' );

//Data Files

    AAdd ( 'WRYM.zip' );
    AAdd ( 'WRSA.zip' );
    AAdd ( 'Wrsm.zip' );

//Database

    AAdd ( 'WRMFDatabase.mdb' );

//Dos

    AAdd ( 'LF90.EER' );
    AAdd ( 'WRYM-6.exe' );
    AAdd ( 'WRYM-61.exe' );
    AAdd ( 'WRYM-62.exe' );
    AAdd ( 'Wrym.exe' );

//Logs

    AAdd ( '00003.bmp' );
    AAdd ( '00004.bmp' );
    AAdd ( '00014.bmp' );
    AAdd ( '00018.bmp' );
    AAdd ( '00019.bmp' );
    AAdd ( '00022.bmp' );
    AAdd ( '00024.bmp' );
    AAdd ( '00025.bmp' );
    AAdd ( '00026.bmp' );
//    AAdd ( 'VaalDBMS_ErrorLog.txt' );
    AAdd ( 'ViewSettings.ini' );
//    AAdd ( 'WRMF_ErrorLog.txt' );

//Network Diagrams

    AAdd ( 'DefaultDrawing.VSD' );
    AAdd ( 'WRYMStencil.vss' );

//WRCData

    AAdd ( 'br_astats.exe' );
    AAdd ( 'br_sawb.exe' );
    AAdd ( 'Classr.exe' );
    AAdd ( 'get_names.exe' );
    AAdd ( 'HDYP08.EXE' );
    AAdd ( 'LF90.EER' );
    AAdd ( 'Patchr.exe' );
    AAdd ( 'tnt.exe' );

// Help

    AAdd ( 'Vaal database ODBC Setup Readme.txt' );
    AAdd ( 'VHIMS Training Manual.doc' );
    AAdd ( 'WRMF.cnt' );
    AAdd ( 'WRMF.HLP' );
    AAdd ( 'WRMFManual.RTF' );

//Reports
  (*
    AAdd ( '6755H.doc' );
    AAdd ( '6758H.doc' );
    AAdd ( '6762H.doc' );
    AAdd ( '6765H.doc' );
    AAdd ( '6765ha.doc' );
    AAdd ( '6765hc.doc' );
    AAdd ( '6765hd.doc' );
    AAdd ( '6765HE.DOC' );
    AAdd ( '6765hf.doc' );
    AAdd ( '6765HG.doc' );
    AAdd ( '6765hh.doc' );
    AAdd ( '6765HI.doc' );
    AAdd ( '6765HJ.doc' );
    AAdd ( 'ENVIRONMENTAL CONS EXECUTIVE SUMMARY.PDF' );

    AAdd ( '6785h.doc' );
    AAdd ( 'ASSESSMENT OF DATA REQUIREMENTS.PDF' );
    AAdd ( 'DATA INVENTORY VAAL WATER SUPPLY SYSTEM AREA.PDF' );
    AAdd ( 'EVALUATION OF WATER QUALITY MODEL. REQ. KOMATI & USUTU.PDF' );
    AAdd ( 'HIST. SPLIT IN DEMANDS & RETURN FLOW N & S PORTIONS OF RAND WS AREA.PDF' );
    AAdd ( 'INTEGRATED VAAL RIVER SYSTEMS ANALYSIS.PDF' );
    AAdd ( 'SHORT TERM STOCHASTIC YIELD ANALYSIS.pdf' );
    AAdd ( 'SUMMARY REPORT.PDF' );

    AAdd ( '6697H.doc' );
    AAdd ( '6722H.doc' );
    AAdd ( 'HYDROLOGY OF THE KOMATI RIVER CATCHMENT UPSTREAM OF SWAZILAND.PDF' );
    AAdd ( 'HYDROLOGY OF THE LOWER VAAL CATCHMENT.pdf' );
    AAdd ( 'HYDROLOGY OF THE MIDDLE VAAL CATCHMENT.PDF' );
    AAdd ( 'HYDROLOGY OF THE SENQU CATCHMENT.PDF' );
    AAdd ( 'HYDROLOGY OF THE TUGELA CATCHMENT.PDF' );
    AAdd ( 'HYDROLOGY OF THE UPPER VAAL CATCHMENT.pdf' );
    AAdd ( 'HYDROLOGY OF THE USUTU RIVER CATCHMENT UPSTREAM OF SWAZILAND.PDF' );
    AAdd ( 'HYDROLOGY OF THE VAAL BARRAGE CATCHMENT.PDF' );
    AAdd ( 'IRRIGATION AND FARM DAM INFORMATION FOR THE VAAL RIVER SYSTEM.PDF' );

    AAdd ( '7294hb.doc' );
    AAdd ( '7294hc.doc' );
    AAdd ( '7294hd.doc' );
    AAdd ( '7294he.doc' );
    AAdd ( '7294hf.doc' );
    AAdd ( '7294hg.doc' );
    AAdd ( '7296hc.doc' );
    AAdd ( '7296hd.doc' );
    AAdd ( '7296he.doc' );
    AAdd ( '7296hf.doc' );
    AAdd ( '7837hAppB.doc' );
    AAdd ( '7837hAppC.doc' );
    AAdd ( '7837hAppD.doc' );
    AAdd ( '7837hAppE.doc' );
    AAdd ( 'HYDRO SAL MODEL CALIBR-MIDDLE VAAL CATCHMENT.PDF' );
    AAdd ( 'HYDRO SAL MODEL CALIBR-UPPER VAAL CATCHMENT.PDF' );
    AAdd ( 'HYDRO SAL MODEL CALIBR-VAAL BARRAGE VOL A.PDF' );
    AAdd ( 'HYDRO SAL MODEL CALIBR-VAAL BARRAGE VOL B.PDF' );
    AAdd ( 'HYDRO-SAL MODEL CALIBR LOWER VAAL CATCHMENT.PDF' );

    AAdd ( 'aREADME.pdf' );
    AAdd ( 'readme.pdf' );

    AAdd ( 'HIST & L TERM STOCH YIELD ANALYSIS HEYSHOPE & ZAAIHOEK DAM.pdf' );
    AAdd ( 'HIST & L TERM STOCH YIELD ANALYSIS KOMATI & USUTU SUB SYSTEMS.pdf' );
    AAdd ( 'HIST & L TERM STOCH YIELD ANALYSIS OF SENQU SUB SYSTEM.pdf' );
    AAdd ( 'HIST & L TERM STOCH. YIELD ANALYSIS OF GROOTDRAAI & BLOEMHOF DAM.pdf' );
    AAdd ( 'Usutu Sc1.doc' );
    AAdd ( 'VERIFICATION AND VALIDATION OF STOCHASTIC FLOW SEQUENCES.PDF' );

     *)
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
