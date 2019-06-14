unit ULanguageText_ENG_BSP_Other;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin

  AAdd('BSPCaption','FBSPCloseButton','Close');
  AAdd('BSPCaption','FBSPAppendRecords','Append Record');
  AAdd('BSPCaption','FBSPSaveNewChangelist','Save Change List');
  AAdd('BSPCaption','FBSPRenameButton','Rename');
  AAdd('BSPCaption','FBSPChangeListChange','ChangeList');
  AAdd('BSPCaption','FBSPChangeListMetadata','Metadata');
  AAdd('BSPCaption','FClassificationNameLabel','Classification Name:');
  AAdd('BSPCaption','FExportGrid','Export to File');
  AAdd('BSPCaption','FSpecifyExistingDataUnitsToGrid','Specify Existing Data Units');
  AAdd('BSPCaption','FConvertToNewUnitsOnGrid','Convert To New Units');
  AAdd('BSPCaption','FClearUnitsOnGrid','Clear Units');
  AAdd('BSPCaption','FSaveNewDatasetFromGrid','Save view as new dataset');
  AAdd('BSPCaption','FConvertMultipleColumnOnGrid', 'Convert Multiple Column');
  AAdd('BSPCaption','FCalculateGrowthOnGrid', 'Calculate linear or exponential growth');
  AAdd('BSPCaption','FBSPExponential', 'Exponential growth');
  AAdd('BSPCaption','FBSPLinear', 'Linear growth');
  AAdd('BSPCaption','FBSPPercentageRate', 'Percentage rate');
  AAdd('BSPCaption','FBSPNumberOfPeriods', 'Number of periods');
  AAdd('BSPCaption','FBSPFilter', 'Filter');
  AAdd('BSPCaption','FDatasetMultimediaOnGrid','Multimedia File in Dataset');
  AAdd('BSPCaption','FBSPFilterRows', 'Filter Rows:');
  AAdd('BSPCaption','FBSPMenuItemRemoveDataset', 'Remove Dataset Permanently');
  AAdd('BSPCaption','FBSPMenuItemReinstateDataset', 'Reinstate Dataset');

  AAdd('BSPCaption','FClasisficationAbreviationLabel','Classification Abreviation:');
  AAdd('BSPCaption','FClassificationCodeLabel','Classification Code:') ;
  AAdd('BSPCaption','FClassificationDescriptionLabel','Classification Description:');
  AAdd('BSPCaption','FClassificationSequenceLabel','Classification Sequence:');
  AAdd('BSPCaption','FClassificationMoveCheckBox','Move Lock indicator');
  AAdd('BSPCaption','FClassificationSaveButton','Save');
  AAdd('BSPCaption','FCustomClassADDButton','Add');
  AAdd('BSPCaption','FCustomizedClassDescLabel','Class Description');
  AAdd('BSPCaption','FClassificationAddButton','Add');
  AAdd('BSPCaption','FClassificationCancelButton','Cancel');
  AAdd('BSPCaption','FClassificationDeleteButton','Delete');
  AAdd('BSPCaption','FClassificationCloseButton','Close');
  AAdd('BSPCaption','FCustomClassRemoveButton','Remove');
  AAdd('BSPCaption','FDetailsSheet','Details');
  AAdd('BSPCaption','FCustomizedTabSheet','Custom Classifications');
  AAdd('BSPCaption','FCustomizedClassLabel','Classification Name');
  AAdd('BSPHint','FClassificationPremoteButtonHint','Promote Classification');
  AAdd('BSPHint','FClassificationDemoteButtonHint','Demote Classification');
  AAdd('BSPHint','FClassificationSequenceUpButtonHint','Move Sequence up');
  AAdd('BSPHint','FClassificationSequenceDownButtonHint','Move Sequence Down');
  AAdd('BSPHint','FClassificationAddChildButtonHint','Add Child');
  AAdd('BSPHint','FCustomizationButtonHint','Add Custom Classification');
  AAdd('BSPHint','FClassificationRemoveFromGroupHint','Remove From Group');
  AAdd('BSPHint','FClassificationDeleteHint','Delete Classification');

  AAdd('ButtonHint','BSPVIEWDATA','View Data (Raw Format)');
  AAdd('ButtonHint','BSPMETADATA','Metadata');
  AAdd('ButtonHint','BSPSTAKEHOLDERS','Stakeholders');
  AAdd('ButtonHint','BSPTIMESERIESCOMPARATOR','Time Series Comparator');
  AAdd('ButtonHint','BSPSPATIALANALYSIS','Spatial Analysis');
  AAdd('ButtonHint','BSPCATEGORISATIONS','Categorisations');
  AAdd('ButtonHint','BSPDERIVEDATASETS','Derive Datasets');
  AAdd('ButtonHint','BSPRELATEDDATASETSTIMESERIESCOMPARATOR','Related Datasets - Time Series Comparator');
  AAdd('ButtonHint','BSPRELATEDDATASETSDERIVEDATASETS','Related Datasets - Derived Datasets');
  AAdd('ButtonHint','BSPRELATEDDATASETSDATASETS','Related Datasets - Datasets');
  AAdd('ButtonHint','BSPAPPORTIONMENTS','Apportionments');
  AAdd('ButtonHint','BSPFIELDLISTS','Field Lists');
  AAdd('ButtonHint','BSPAREAMAINTENANCE','Area Maintenance');
  AAdd('ButtonHint','BSPAREAMANAGERAPPORTIONMENTS','Area Manager - Apportionments');
  AAdd('ButtonHint','BSPCHANGELISTS','Change Lists');
  AAdd('ButtonHint','BSPDERIVEFIELDS','Derive Fields');
  AAdd('ButtonHint','BSPIMPORTDATASET','Import Dataset');
  AAdd('ButtonHint','BSPIMPORTAPPORTIONMENTS','Import Apportionments');
  AAdd('ButtonHint','BSPEXPORTDATASET','Export Dataset');
  AAdd('ButtonHint','BSPEXPORTSTAKEHOLDERS','Export Stakeholders');
  AAdd('ButtonHint','BSPEXPORTCATEGORISATIONS','Export Categorisations');
  AAdd('ButtonHint','BSPEXPORTBSPMETADATA','Export BSP Metadata');
  AAdd('ButtonHint','BSPECAPTUREDATASET','Capture dataset');

  AAdd('BSPCaption','FStakeholderInstitutionTabSheet','Institution');
  AAdd('BSPCaption','FStakeholderPersonTabSheet','Person');
  AAdd('BSPCaption','FStakeholderInstitutionGroupBox','Institution Information');
  AAdd('BSPCaption','FStakeholderPersonalGroupBox','Person Information');
  AAdd('BSPCaption','FStakeholderDeleteButton','Delete');
  AAdd('BSPCaption','FStakeholderAddButton','Add');
  AAdd('BSPCaption','FStakeholderCancelButton','Close');
  AAdd('BSPCaption','FStakeholderCellHeadingSurname','Surname');
  AAdd('BSPCaption','FStakeholderCellFirstName','First Name');
  AAdd('BSPCaption','FStakeholderCellInitials','Initials');
  AAdd('BSPCaption','FStakeholderCellTitle','Title');
  AAdd('BSPCaption','FStakeholderCellInstitutionName','Institution Name');
  AAdd('BSPCaption','FStakeholderCellInsitutionAbreviation','Abreviation');
  AAdd('BSPCaption','FStakeholderCellInstitutionType','Institution Type');
  AAdd('BSPCaption','FExportHeaderLabel','No dataset Selected');
  AAdd('BSPCaption','FExportHeaderDataCheck','Export Column Names');
  AAdd('BSPCaption','FExportChangeListCheck','Export Change Lists');
  AAdd('BSPCaption','FExportMetadataCheck','Export Metadata');
  AAdd('BSPCaption','FExportStakeholderCheck','Export Stakeholder Lists');
  AAdd('BSPCaption','FExportCategorizationCheck','Export Categorization Lists');
  AAdd('BSPCaption','FExportFileTypeLabel','File type to export');
  AAdd('BSPCaption','FExportFFileNameLabel','File name');
  AAdd('BSPCaption','FExportFHeaderLabel','No Dataset Selected');
  AAdd('BSPCaption','FExportButton','Export');
  AAdd('BSPCaption','FImportButton','Import');
  AAdd('BSPCaption','FFeaturePeriodGroupBox','Feature/Period Fields');
  AAdd('BSPCaption','FDataFieldGroupBox','Data Fields');
  AAdd('BSPCaption','FDatasetFormatGroup','Dataset');
  AAdd('BSPCaption','FAreaTypeLabel','Area type:');
  AAdd('BSPCaption','FDatasetNameLabel','Dataset Name:');
  AAdd('BSPCaption','FCapturePeriodLabel','Dataset Period');
  AAdd('BSPCaption','FCapturePeriodFromLabel','From');
  AAdd('BSPCaption','FCapturePeriodToLabel','To');
  AAdd('BSPCaption','FCaptureCalculationsGroup','Calculations');
  AAdd('BSPCaption','FCaptureInterpolate','Interpolate');
  AAdd('BSPCaption','FCaptureExtrapolate','Extrapolate');

  AAdd('BSPFormCaption','TExportStakeHoldersDataValidator','Export StakeHolders');
  AAdd('BSPFormCaption','TExportClassificationValidator ','Export Categorizations');
  AAdd('BSPFormCaption','TExportGridDataValidator ','Export Grid');
  AAdd('BSPFormCaption','TImportStakeholderValidator ','Import Stakeholders');
  AAdd('BSPFormCaption','TImportCategorizationValidator','Import Categorizations');
  AAdd('BSPCaption','FImportCategorizationRelationship','Import Categorization Relationships');


  AAdd('BSPCaption','FImportStakeHolderPerson','Import Stakeholder Person');
  AAdd('BSPCaption','FImportStakeHolderInsitution','Import Stakeholder Institution');
  AAdd('BSPCaption','FImportFiletype','File type to Import');
  AAdd('BSPCaption','FImportColumnNames','Import Column Names');


  AAdd('BSPCaption','FExportStakeHolderPerson','Export Stakeholder Person');
  AAdd('BSPCaption','FExportStakeHolderInsitution','Export Stakeholder Intitution');
  AAdd('BSPCaption','FExportFCancelButton','Cancel/Close');
  AAdd('BSPCaption','FExportFBrowseButton','Browse');
  AAdd('BSPCaption','SelectAll','Select All');
  AAdd('BSPCaption','ClearAll','Clear All');
  AAdd('BSPCaption','FTimeSeriesRelatedDatesets','Related Datasets');
  AAdd('BSPCaption','FTimeSeriesFields','Fields');
  AAdd('BSPCaption','FTimeseriesFromPeriod','From Period');
  AAdd('BSPCaption','FTimeseriestoPeriod','To Period');
  AAdd('BSPCaption','FTimeseriesSortingOption','Sorting Option');
  AAdd('BSPCaption','FTimeseriesSortingOptionArea','Area');
  AAdd('BSPCaption','FTimeseriesSortingOptionFields','Fields');
  AAdd('BSPCaption','FRelatedDatasetPanelDatasetTypes','Related Dataset Types');
  AAdd('BSPCaption','UpdateDatasetType','Update');
  AAdd('BSPCaption','ClearDatasetType','Clear');
  AAdd('BSPCaption','RelateddatasetAreatype','Dataset Area Type:');
  AAdd('BSPCaption','FRelateddatasetDescription','This funtionality is used to link relate datasets'+
                    ' for a specified reason to be used for the ');
  AAdd('BSPCaption','TSCHeader','TimeSeries Comparitor');
  AAdd('BSPCaption','TSCSetup','Setup');
  AAdd('BSPCaption','FRelatedDatasets','Datasets');
  AAdd('BSPCaption','FRelatedDatasetTypes','Related datasets within the relationship reason');
  AAdd('BSPCaption','FTimeseriesUsePeriod','Use Period Range');
  AAdd('BSPCaption','FTimeseriesUseZeroValues','Use Zero On Graph for no existing or non numerical values');
  AAdd('BSPCaption','TSCGrid','Timeseries Grid');
  AAdd('BSPCaption','TSCCombined','Combined Datasets');
  AAdd('BSPCaption','TSCCombiningDatasets','Combining Datasets');
  AAdd('BSPCaption','TSCBuildingTimeseries','Building Timeseries');
  AAdd('BSPCaption','BSPEXportClassificationRelationship','Export Classification Relationships');
  AAdd('BSPCaption','FTimeseriesGenerateTimeseriesBTN','Generate TimeSeries');
  AAdd('BSPCaption','FTimeseriesCaptionHeader','Time Series Comparitor: Comparing data for specified reasons for a period of time.');
  AAdd('MenuCaption','BSPRelated_Datasets', 'Datasets');
  AAdd('MenuCaption','ConfigureToolbar','Toolbar Configuration');
  AAdd('MenuCaption','ConfigureToolbarSep','-');

  AAdd('BSPMessage','FTimeseriesAddToSeries','Do You Want to Add Areas to series for comparision?');
  AAdd('BSPMessage','FTimeseriesIncorrectDates','The End Date Cannot be smaller than the Start Date.');

  AAdd('BSPMessage','FClassificationName','Please fill in a Classification Name');
  AAdd('BSPMessage','FClassificationAlreadyexist','Classification already exist.Please fill in a Different Classification Name');
  AAdd('BSPMessage','FClassificationSequence','Please fill in a Valid numeric Value for the Sequence');
  AAdd('BSPMessage','FStakeHolderNoTabSheet','No Active TabSheet Found');
  AAdd('BSPMessage','FStakeHolderDeleteRecord','Are you sure you want to delete the record?');
  AAdd('BSPMessage','FStakeHolderNotDeleteRecord','Not a Valid record for deletion ');
  AAdd('BSPMessage','FExportSelectaDataset','Please select a Dataset before exporting');
  AAdd('BSPMessage','FExportFilename','Please select a FileName before exporting');
  AAdd('BSPMessage','FExportCompleted','Export Completed');
  AAdd('BSPMessage','FNoExportFiletype','No Export Type Selected '+#13#10+
                                        'Would you like to export the files in a CSV Format?' );
  AAdd('BSPMessage','FExportOverwriteFile','The Current File Already Exists Do you want to overwrite it?');
end;

end.
