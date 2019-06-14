unit ULanguageText_ENG_BSP;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  (*
  AAdd('MenuCaption','LoadSource','Load Source');
  AAdd('MenuCaption','AreaManagement','Area Management');
  AAdd('MenuCaption','Changelist','Change List');
  AAdd('MenuCaption','FieldList','Field List');
  AAdd('MenuCaption','ReportTemplateDefinition','Report Template');
  AAdd('MenuCaption','Output','Output');
  AAdd('MenuCaption','Metadata','Metadata');
  AAdd('MenuCaption','CustomClassiifcation','Related Dataset Types');
  AAdd('MenuCaption','MetaDataStakeHolder','Stakeholder');
  AAdd('MenuCaption','DerivedDatasets','Derived Dataset');
  *)
  AAdd('MenuCaption','BSPRelatedDatasetsforTimeseriesSetup','Time Series Related Dataset Setup');
  AAdd('MenuCaption','BSPTools','Tools');
  AAdd('MenuCaption','BSPMetadata','BSPMetadata');
  AAdd('MenuCaption','MetadataBeginSep', '-');
  AAdd('MenuCaption','MetadataEndSep', '-');
  AAdd('MenuCaption','BSPAnalysis','Analysis Tools');
  AAdd('MenuCaption','BSPTimeSeriesComparator','Time Series Comparator');
  AAdd('MenuCaption','BSPSpatialAnalysis','Spatial Analysis');
  AAdd('MenuCaption','BSPGeneralUtilities','General Utilities');
  AAdd('MenuCaption','BSPDatasetUtilities', 'Dataset Utilities');
  AAdd('MenuCaption','BSPUtilities','Utilities');
  AAdd('MenuCaption','BSPApportionments','Apportionments');
  AAdd('MenuCaption','BSPStakeholders','Stakeholders');
  AAdd('MenuCaption','BSPCategorisations','Categorisations');
  AAdd('MenuCaption','BSPDeriveDatasets','Derive Datasets');
  AAdd('MenuCaption','BSPFieldLists','Field Lists');
  AAdd('MenuCaption','BSPAreaManager','Geographic Feature Manager');
  AAdd('MenuCaption','BSPArealist','Area Maintenance');
  AAdd('MenuCaption','BSPAreaApportionment','Apportionments');
  AAdd('MenuCaption','BSPLoadDataset','Load Dataset');
  AAdd('MenuCaption','BSPChangeLists','Change Lists');
  AAdd('MenuCaption','BSPDeriveFields','Derive Fields');
  AAdd('MenuCaption','BSPImport','Import');
  AAdd('MenuCaption','BSPDataset','Dataset');
  AAdd('MenuCaption','BSPApportionments','Apportionments');
  AAdd('MenuCaption','BSPExport','Export');
  AAdd('MenuCaption','BSPDataset','Dataset');
  AAdd('MenuCaption','BSPStakeholders','Stakeholders');
  AAdd('MenuCaption','BSPCategorisations','Categorisations');
  AAdd('MenuCaption','BSPMetadata','Metadata');
  AAdd('MenuCaption','BSPRelatedDatasets', 'Related Datasets');
  AAdd('MenuCaption','BSPViewData', 'View Data (Raw Format)');
  AAdd('MenuCaption','BSPAggregateData','Aggregate Data');
  AAdd('MenuCaption','BSPVerifyDataset','Verify Dataset');
  AAdd('MenuCaption','BSPVerifyStreamData','Verify Streamfile');
  AAdd('MenuCaption','BSPCaptureDataset','Capture Dataset');
  AAdd('MenuCaption','BSPDataDictionary','Data Dictionary');

  AAdd('MenuCaption','BSPTransferIn','Transfer In');
  AAdd('MenuCaption','BSPTransferOut','Transfer Out');
  AAdd('TabCaption','DerivedFields','Derived Fields');
  AAdd('BSPTabCaption','RelatedDataset','Related Dataset');
  AAdd('BSPCaption','FieldListAddButton', 'Add');
  AAdd('BSPCaption','FieldListDeleteButton', 'Delete');
  AAdd('BSPCaption','FFieldListLabel','Derived Field List');
  AAdd('BSPCaption','FFieldEditLabel','Derived Field');
  AAdd('BSPCaption','RelatedDatasetAddButton','Add');
  AAdd('BSPCaption','RelatedDatasetDeleteButton','Delete');
  AAdd('TabCaption', 'BSP.TBSPAreaNetworkTabsheet', 'Area Networks ');
  AAdd('BSPErrorMsg','DerivedFieldFieldCannotBeBlank','Field Name cannot be blank.');
  AAdd('MenuCaption','MetaDataClassification','Categorisations');
  AAdd('MenuCaption','ExportSource','Export Dataset');
  AAdd('BSPCaption','DerivedFieldEditPanelFieldName','FieldName');
  AAdd('BSPCaption','DerivedFieldEditPanelOk','Ok');
  AAdd('BSPCaption','DerivedFieldEditPanelCancel','Cancel');
  AAdd('BSPCaption','SelectFieldContainingFeature','Select the field Containing Feature');
  AAdd('BSPCaption','FieldListAddButton','Add');
  AAdd('BSPCaption','FieldListDeleteButton','Delete');
  AAdd('BSPCaption','FFieldListLabel','Field name');
  AAdd('BSPCaption','FFieldEditLabel','Field name');
  AAdd('BSPLoadValidatorErrorMessage','InvalidFileName','The filename is invalid.');
  AAdd('BSPLoadValidatorErrorMessage','InvalidAreaType','Please select a valid area type.');
  AAdd('BSPLoadValidatorErrorMessage','NoDatasetName','Please enter a valid dataset name.');
  AAdd('BSPLoadValidatorErrorMessage','DuplicateDatasetName','The dataset name that you entered already exists.');
  AAdd('BSPLoadValidatorErrorMessage','InvalidNoStakeholderSelected','Please select a stakeholder.');
  AAdd('BSPLoadValidatorErrorMessage','InvalidNoCustodianSelected','Plaese select a custodian.');
  AAdd('BSPLoadValidatorErrorMessage','InvalidNoSourcedFromSelected','Please select a sourced from.');
  AAdd('BSPLoadValidatorErrorMessage','InvalidNoAccuracyLevel','Plaese select an accuracy level.');
  AAdd('BSPLoadValidatorErrorMessage','InvalidNoScaleSelected','Please select a scale.');
  AAdd('BSPLoadValidatorErrorMessage','InvalidPeriodFromAfterPeriodTo','The from period is after the to period, plaese correct.');
  AAdd('BSPCaption','FieldListAddButton','Add');
  AAdd('BSPCaption','FieldListDeleteButton','Delete');
  AAdd('BSPCaption','FFieldListLabel','Field name');
  AAdd('BSPCaption','FFieldEditLabel','Field name');
  AAdd('BSPErrorMsg','DerivedFieldYouMustEnterAValidFieldName','You must enter a valid field name.');
  AAdd('BSPErrorMsg','DerivedFieldAlreadyExists','Field name already exists.');
  AAdd('TabCaption','BSP.TAreaTypeTabsheet', 'Area Type');
  AAdd('TabCaption','BSP.TAreaListTabsheet','Area List');
  AAdd('TabCaption','BSP.TListOfAreasTabsheet','List Of Areas');
  AAdd('TabCaption','BSP.TAreaHierarchyTabsheet','Area Hierarchy');
  AAdd('TabCaption','BSP.TAreaApportionmentTabsheet','Area Apportionment');
  AAdd('TabCaption','BSP.ExplorerByDatasetGroup','Groups');
  AAdd('TabCaption','BSP.ExplorerByAreaType','Area Type Groups');
  AAdd('TabCaption','BSP.ExplorerByDatasetName','Alphabetical');
  AAdd('TabCaption','BSP.ExplorerByStudy','By Study');
  // AAdd('TabCaption','BSP.TAreaApportionmentTabsheet','Area Apportionment');
  AAdd('TabCaption','BSP.TBSPAreaTypeHierarchyTabsheet','Area Type Hierarchy');
  AAdd('BSPCaption','FOperatorPanelLabel','Available Operators (Values are calculated strictly left to right i.e. no precedence).');
  AAdd('BSPCaption','FNumericalOperatorPanelLabel','Available Numerical Operators.');
  AAdd('BSPCaption','FParameterListPanelLabel','Available Fields');
  AAdd('BSPCaption','FEquationPanelLabel','User Defined Field Equation');
  AAdd('TabCaption','BSP.ExplorerReportSheet','Reports');
  AAdd('TabCaption','BSP.ExplorerViewSheet','Views');
  AAdd('TabCaption','FDataDictionaryDatasetPropertiesTabSheet','Dataset Properties');
  AAdd('TabCaption','FDataDictionaryFieldPropertiesTabSheet','Field Properties');
  AAdd('TabCaption','FDataDictionaryFieldPropertiesGridViewTabSheet','Grid View');
  AAdd('TabCaption','FDataDictionaryFieldPropertiesTreeViewTabSheet','Tree View');
  AAdd('TabCaption','Load','Load');
  AAdd('TabCaption','View','View');
  AAdd('TabCaption','Calculate','Calculate');
  AAdd('TabCaption','AreaConversion','Area Conversion');
  AAdd('TabCaption','AdditionalUtilities','Additional Utilities');
  AAdd('TabCaption','Help','Help');

  AAdd('ButtonReason','BSPLoadDatasetDisabled',   'This button is disabled because loading of datasets'#13#10 +
                                                  '  is currently not possible.');
  AAdd('ButtonReason','BSPAreaManagerDisabled',   'This button is disabled because area management '#13#10 +
                                                  '  is currently not possible.');
  AAdd('ButtonHint','BSPLOADDATASET','Load a Source File');
  AAdd('ButtonHint','BSPAREAMAINTENANCE','Geographical Feature Manager');
  AAdd('ButtonHint','BSPFULLSCREEN','Toggle Full Screen View');

  AAdd('ButtonHint','BSPDATADICTIONARY', 'Data Dictionary');
  AAdd('ButtonHint','BSPCOMPAREDATASETS','Compare Datasets');

  AAdd('BSPHint','FLoadDataPanelStep1AreaManagerButtonHint','Geographical Feature Manager');
  AAdd('ButtonHint','BSPCAPTUREDATASET','Capture a Dataset');
  AAdd('BSPHint','FLoadDataPanelStep1CategorizationsButtonHint','Categorisations');

  AAdd('ProgressMessage', 'LoadStep1' , 'Validating step 1.');
  AAdd('ProgressMessage', 'LoadStep2' , 'Validating step 2.');
  AAdd('ProgressMessage', 'LoadStep3' , 'Validating step 3.');
  AAdd('ProgressMessage', 'LoadStep4' , 'Validating step 4.');
  AAdd('ProgressMessage', 'LoadStep5' , 'Checking for duplicate areas/features');
  AAdd('ProgressMessage', 'LoadStep6' , 'Validating data types.');
  AAdd('ProgressMessage', 'LoadStep7' , 'Loading areas/features.');
  AAdd('ProgressMessage', 'LoadStep8' , 'Loading data.');
  AAdd('ProgressMessage', 'LoadStep9' , 'Creating the StreamFile');
  AAdd('ProgressMessage', 'LoadStep10' , 'Invoking metadata capture screen.');
  AAdd('BSPFormCaption', 'TAggregationValidator','Dataset Apportionments');
  AAdd('BSPFormCaption', 'TStakeHolderValidator','Stakeholder');
  AAdd('BSPFormCaption', 'TClassificationValidator','Categorisation');
  AAdd('BSPFormCaption', 'TDerivedDatasetValidator','Derived Dataset');
  AAdd('BSPFormCaption', 'TFieldListValidator','Field List');
  AAdd('BSPFormCaption', 'TDerivedFieldValidator','Derived Field');
  AAdd('BSPFormCaption', 'TRelatedDatasetValidator','Related Dataset');
  AAdd('BSPFormCaption', 'TBSPChangeListValidator','Changelist');
  AAdd('BSPFormCaption', 'TAreaManagerValidator','Geographic Feature Manager');
  AAdd('BSPFormCaption', 'TAreaManagerApportionmentValidator','Area Apportionments');
  AAdd('BSPFormCaption', 'TLoadDataValidator','Load Dataset');
  AAdd('BSPFormCaption', 'TExportFileValidator','Export Dataset');
  AAdd('BSPFormCaption', 'TViewdataTabSheetValidator','View Data');
  AAdd('BSPFormCaption', 'TCustomClassificationValidator','Custom Categorisation');
  AAdd('BSPFormCaption', 'TTimeSeriesTabsheetValidator','Temporal Comparitor');
  AAdd('BSPFormCaption', 'TBSPMetadataValidator','Metadata');
  AAdd('BSPFormCaption', 'TAggregateDataManagerValidator', 'Aggregate Data');
  AAdd('BSPFormCaption', 'TConfigureToolbarValidator','Configure Toolbar');
  AAdd('BSPFormCaption', 'TConfigureToolbarValidator','Configure Toolbar');
  AAdd('BSPFormCaption', 'TBSPDatasetCompareValidator','Compare Datasets');
  AAdd('BSPFormCaption', '  TBSPDatasetTransferValidatorIn','Transfer In');
  AAdd('BSPFormCaption', '  TBSPDatasetTransferValidatorOut','Transfer Out');
  AAdd('BSPFormCaption', 'TCaptureDatasetValidator','Capture Dataset');
  AAdd('BSPFormCaption', 'TDataDictionaryValidator','Data Dictionary');
  AAdd('BSPFormCaption', 'TDataValueUnitValidator','Data Value Unit');
  AAdd('BSPFormCaption', 'TDataUnitConversionValidator','Convert To New Units');
  AAdd('BSPFormCaption', 'TMultipleColumnUnitsConversionValidator','Convert Multiple Column');
  AAdd('BSPFormCaption', 'TMultimediaFileValidator','Select Multimedia Files');
  AAdd('BSPFormCaption', 'TApportionmentWizardValidator','Add New Apportionment');
  AAdd('BSPFormCaption', 'TAreaTypeWizardValidator','Add Area Wizard');
  AAdd('BSPFormCaption', 'TLoadApportionmentWizardValidator','Load Apportionment Profile');
  AAdd('BSPFormCaption', 'THierarchyWizardValidator','Create Hierarchy Wizard');
  AAdd('BSPFormCaption', 'TAreaListWizardValidator','Create Area List Wizard');

  AAdd('Caption', 'TAggregateDataManagerPanel.FSelectionGroup','Selection Action');
  AAdd('Caption', 'IncludePeriodCol','Include Period Column');
  AAdd('MenuCaption','BSPCompareDatasets','Compare Datasets');
  AAdd('MenuCaption','BSPConvertDataset','Convert Dataset');
  AAdd('MenuCaption','BSPImportFile','Import File');
  AAdd('ButtonHint','BSPConvertDataset','Convert Dataset');

end;

end.

