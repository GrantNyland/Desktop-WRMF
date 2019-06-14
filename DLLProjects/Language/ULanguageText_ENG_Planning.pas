{******************************************************************************}
{*  UNIT      : Contains language text for Rainfall model.                    *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/13                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit ULanguageText_ENG_Planning;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  AAdd('Model','Planning','Planning');

  AAdd('TabCaption','AllocationDefinition','Allocation Definition');
  AAdd('TabCaption','SwitchDefinition',    'Switch Definition');
  AAdd('TabCaption','UserPriorityClassification','User Priority Classification');
  AAdd('TabCaption','YieldCharacteristics','Short Term Yield Characteristics');
  AAdd('TabCaption','SubSystems','Sub-systems');
  AAdd('TabCaption','SupportStrategy','Support Strategy');
  AAdd('TabCaption','SupportChannels','Special Support Channels');
  AAdd('TabCaption','DemandSupportDefinition','Demand Definition');
  AAdd('TabCaption','Properties','Demand Centre Properties');
  AAdd('TabCaption','DemandSupport','Support Sub-Systems');
  AAdd('TabCaption','FCaptureDatasetDefineFieldsTabSheet','Define Fields');     
  AAdd('TabCaption','FCaptureDatasetEnterDataTabSheet','Enter Data');
  AAdd('TabCaption','FCaptureDatasetFillDataTabSheet','Fill Data');

  AAdd('PlanningGUI', 'SwitchDefFileName', 'Switch definition filename');
  AAdd('PlanningGUI', 'SwitchDefStartDate', 'Switch Definition start date');

  AAdd('PlanningGUI', 'AllocDefName', 'Allocation definition name');
  AAdd('PlanningGUI', 'AllocDefStartDate', 'Active period start date');
  AAdd('PlanningGUI', 'AllocDefEndDate','Active period end date :');
  AAdd('PlanningGUI', 'NrOfReliabilityClasses','Number of reliability classes :');
  AAdd('PlanningGUI', 'RILabel','Recurrence intervals for each reliability class :');
  AAdd('PlanningGUI', 'UserCategoryName','User categories :');
  AAdd('PlanningGUI', 'Distribution','Demand distribution in each reliability class');
  AAdd('PlanningGUI', 'AllocCurtailment','Allocation/curtailment levels :');
  AAdd('PlanningGUI', 'DemandCurtailment','Demand curtailments in each reliability class');
  AAdd('PlanningGUI', 'AllocLevelWarning ', 'Number of Allocation Levels are usually the same as the number of Reliability Intervals.');
  AAdd('PlanningGUI', 'AllocDefFileName', 'Filename :');

  AAdd('PlanningGUI', 'NrOfLoadCases','No of load cases/target drafts :');
  AAdd('PlanningGUI', 'PeriodLength','Period length (years) :');
  AAdd('PlanningGUI', 'PeriodLengthDescr','(upon which short term yield curves are based)');
  AAdd('PlanningGUI', 'NrOfStartStoragePercs','No of starting storage volume fractions :');
  AAdd('PlanningGUI', 'NrOfCurveSets','No. of decision months/curve sets :');
  AAdd('PlanningGUI', 'NrOfCurveSetsDescr', '(one set of curves for each decision month)');
  AAdd('PlanningGUI', 'StartVolumes', 'Start volumes (as fraction of live storage):');
  AAdd('PlanningGUI', 'MonthCurveSet', 'Curve set to be used in each month:');

  AAdd('PlanningGUI', 'GrowthType','Growth type :');
  AAdd('PlanningGUI', 'ResidentInSubSystem','(in which demand centre is resident)');
  AAdd('PlanningGUI', 'DemandDefName','Demand centre name :');
  AAdd('PlanningGUI', 'TargetDemand','Target / base year demand :');
  AAdd('PlanningGUI', 'UserCategory','User category :');
  AAdd('PlanningGUI', 'SupportArc1','Support channel 1 :');
  AAdd('PlanningGUI', 'SupportArc2','Support channel 2 :');

  AAdd('PlanningGUI', 'LoadCase','Load Case');
  AAdd('PlanningGUI', 'NONFIRM','NON-FIRM');
  AAdd('PlanningGUI', 'FIRM','FIRM');
  AAdd('PlanningGUI', 'SupportMaxCapacity','Support up to the maximum capacity');
  AAdd('PlanningGUI', 'SupportPriorityClass','Support is determined for priority class');
  AAdd('PlanningGUI', 'Month','Month' );
  AAdd('PlanningGUI', 'CurveSet','Curve Set');
  AAdd('PlanningGUI', 'Subsystem','Sub-system');
  AAdd('PlanningGUI', 'None','None');
  AAdd('PlanningGUI', 'Factor','Factor');
  AAdd('PlanningGUI', 'SupportChannel','Support Channel');
  AAdd('PlanningGUI', 'Undefined','Undefined');
  AAdd('PlanningGUI', 'Position','Position');
  AAdd('PlanningGUI', 'Before','Before');
  AAdd('PlanningGUI', 'After','After');
  AAdd('PlanningGUI', 'Value','Value');
  AAdd('PlanningGUI', 'Label','Label');
  AAdd('PlanningGUI', 'Description','Description');
  AAdd('PlanningGUI', 'Total','Total');

  AAdd('PlanningGUI', 'GrowthReturnFlow','(-1) Growth for return flow');
  AAdd('PlanningGUI', 'NoGrowth','(0) No growth');
  AAdd('PlanningGUI', 'DemandGrowth','(+1) Demand growth');
  AAdd('PlanningGUI', 'DemandDefs','Demand Definitions');

  AAdd('PlanningGUI', 'SubSystemName','Sub-system name :');
  AAdd('PlanningGUI', 'StartDate','Start date :');
  AAdd('PlanningGUI', 'EndDate','End date :');
  AAdd('PlanningGUI', 'Subtracted','Subtracted sub-system :');
  AAdd('PlanningGUI', 'Supporting','Supporting sub-system :');
  AAdd('PlanningGUI', 'YieldShort','1 : 200 year Yield @ Full (ST) :');
  AAdd('PlanningGUI', 'LowestStreamFlow','Lowest annual natural runoff :');
  AAdd('PlanningGUI', 'YieldLong','1 : 200 year Yield (LT) :');
  AAdd('PlanningGUI', 'FirmYield','Yield Indicator :');
  AAdd('PlanningGUI', 'Reservoirs','Reservoirs determining system storage :');
  AAdd('PlanningGUI', 'StartStoragePerc','Starting storage (fraction of live storage) :');
  AAdd('PlanningGUI', 'DecisionCurveSet','Decision months / curve set :');

  AAdd('PlanningGUI', 'SupportCalcType',' Type of support calculation ');
  AAdd('PlanningGUI', 'RoutingChannels','Non-firm yield support routing channels :');

  AAdd('PlanningGUI', 'SupportChannelName','Channel to be controlled :');
  AAdd('PlanningGUI', 'ControllingSubSystems','Controlling sub-systems :');
  AAdd('PlanningGUI', 'SupportChannelDescr','Support structure channels with contol based on excess or deficit in a particular sub-system');

  AAdd('PlanningGUI', 'SupportStrategy','Type of support strategy :');
  AAdd('PlanningGUI', 'BalancingOption','Balancing option among sub-systems with deficits :');
  AAdd('PlanningGUI', 'FixedPosition','Sub-systems to be solved in a fixed position :');
  AAdd('PlanningGUI', 'SpecificOrder','Sub-systems to be solved in a specific sequence :');
  AAdd('PlanningGUI', 'NewSubSystem','New sub-system');
  AAdd('PlanningGUI', 'GeneralSubSystemSupport',  'General sub-system support');
  AAdd('PlanningGUI', 'SpecificSubSystemSupport', 'Specific sub-system support');
  AAdd('PlanningGUI', 'MayNotDeleteResidentSupSubSys', 'Support sub-system in which the demand centre is resident may not be deleted.');
  AAdd('PlanningGUI', 'OrderInWhichSubSystemsAreSolved', 'Order in which sub-systems are solved :');
  AAdd('PlanningGUI', 'DefaultOrderA',     '(A) Default order');
  AAdd('PlanningGUI', 'OrderBOverridesA',  '(B) Overrides order specified in A');
  AAdd('PlanningGUI', 'OrderCOverridesAB', '(C) Overrides order specified in A and B');
  AAdd('PlanningGUI', 'CoefficientData','Coefficient data (target drafts in ascending order) :');

  AAdd('Tabsheet', 'Properties','Properties');
  AAdd('Tabsheet', 'Coefficients','Coefficients');
  AAdd('Tabsheet', 'DemandSupport','Non-firm inter sub-system support');

  AAdd('ButtonCaption', 'BtnDelete','Delete');
  AAdd('ButtonCaption', 'BtnAdd',   'Add');
  AAdd('ButtonCaption', 'BtnMoveUp', 'Move up');
  AAdd('ButtonCaption', 'BtnMoveDown', 'Move down');

  AAdd('ButtonHint', 'BtnMoveUpSubSystem','Move sub-system up in solving order');
  AAdd('ButtonHint', 'BtnMoveDownSubSystem','Move sub-system down in solving order');
  AAdd('ButtonHint', 'BtnMoveUpDemandDef','Move demand definition up in resident sub-system');
  AAdd('ButtonHint', 'BtnMoveDownDemandDef','Move demand definition down in resident sub-system');
  AAdd('ButtonHint', 'AddFixedPosition','Add Sub-system to be solved in a fixed position');
  AAdd('ButtonHint', 'DeleteFixedPosition','Delete Sub-system to be solved in a fixed position');
  AAdd('ButtonHint', 'AddSpecificOrder','Add Sub-system to be solved in a specific sequence');
  AAdd('ButtonHint', 'DeleteSpecificOrder','Delete Sub-system to be solved in a specific sequence');
  AAdd('ButtonHint', 'AddRI', 'Add recurrence interval');
  AAdd('ButtonHint', 'DeleteRI', 'Delete recurrence interval');
  AAdd('ButtonHint', 'AddCategory', 'Add user category');
  AAdd('ButtonHint', 'DeleteCategory', 'Delete user category');
  AAdd('ButtonHint', 'AddAllocLevel', 'Add allocation level');
  AAdd('ButtonHint', 'DeleteAllocLevel', 'Delete allocation level');
  AAdd('ButtonHint', 'AddSubSystem','Add Sub-system');
  AAdd('ButtonHint', 'DeleteSubSystem','Delete Sub-system');
  AAdd('ButtonHint', 'AddSupportChannel','Add support channel');
  AAdd('ButtonHint', 'DeleteCntrlSubSystem','Delete controlling sub-system');
  AAdd('ButtonHint', 'DeleteSupportChannel','Delete support channel');
  AAdd('ButtonHint', 'AddCntrlSubSystem','Add controlling sub-system');
  AAdd('ButtonHint', 'AddDemandDef','Add Demand Definition');
  AAdd('ButtonHint', 'DeleteDemandDef','Delete Demand Definition');
  AAdd('ButtonHint', 'AddSupportSubSystem','Add Support Sub-System');
  AAdd('ButtonHint', 'DeleteSupportSubSystem','Delete Support Sub-System');

  AAdd('TTariffCalculationDataDialog','DialogCaption','Tariff Calculation');
  AAdd('TTariffCalculationDataDialog','Tariff','Channel Tariff');
  AAdd('TTariffCalculationDataDialog','EscalationFactors','Escalation Factors');
  AAdd('TMultiResChannelDialog', 'ChannelNoLabel','ChannelNo');
  AAdd('TMultiResChannelDialog', 'ReservoirNoLabel','Reservoir Number');
  AAdd('TMultiResChannelDialog', 'DecisionMonthLabel','Decision Month');
  AAdd('TMultiResChannelDialog', 'StartMonthLabel','Start Month');
  AAdd('TMultiResChannelValidator','Caption','Restriction');
  AAdd('CurtailRestrictionGrid','ElevationHeading','Elevation');
  AAdd('CurtailRestrictionGrid','FactorHeading','Factor');
  AAdd('TPlanningMineDialog','SaltWashoffNoLabel','Associated Salt Washoff Number');
  AAdd('TPlanningMineDialog','BuildUpRateLabel','Salt Build-up Rate');
  AAdd('TPlanningMineDialog','EfficiencyFactorLabel','Salt Washoff Efficiency Factor');
  AAdd('TPlanningMineDialog','InitialSaltStoreLabel','Initial Salt Store');
  AAdd('TPlanningMineDialog','RainfallLabel','Rainfall File Name');
  AAdd('TPlanningMineDialog','MeanAnnualPrecipitationLabel','Mean Annual Precipitation');
  AAdd('TPlanningMineGrowthFactorControl','Caption','GrowthFactors');
  AAdd('TPlanningMineGrowthFactorControl','NoOfPoints',' Number of points defining growth factor');
  AAdd('TPlanningMineGrowthFactorControl','FactorType','Factor Type');
  AAdd('TPlanningMineGrowthFactorControl','InterpolationMethod','Interpolation Method');
  AAdd('TPlanningMineGrowthFactorControl','LinearInterpolation','Not Checked = Linear Interpolation');
  AAdd('TPlanningMineGrowthFactorControl','ExponentialInterpolation','Checked = Exponential Interpolation');
  AAdd('FYearAndFactorGrid','YearCol','Year data Points');
  AAdd('FYearAndFactorGrid','GrowthFactorMine','Growth Factor(Benificiation)');
  AAdd('FYearAndFactorGrid','GrowthFactorOpenCastType1','Growth Factor(Working Area)');
  AAdd('FYearAndFactorGrid','GrowthFactorOpenCastType2','Growth Factor(Disturbed Working Area)');
  AAdd('FYearAndFactorGrid','GrowthFactorOpenCastType3','Growth Factor(Disturbed Rehabilitated Area)');
  AAdd('FYearAndFactorGrid','GrowthFactorOpenCastType4','Growth Factor(Evaporation Surface)');
  AAdd('FYearAndFactorGrid','GrowthFactorOpenCastType5','Growth Factor(Spoil Tank Volume-Decant)');
  AAdd('FYearAndFactorGrid','GrowthFactorOpenCastType6','Growth Factor(Spoil Tank Volume-Seepage)');
  AAdd('FYearAndFactorGrid','GrowthFactorUnderGroundType1','Growth Factor(Board and Pillar Mine)');
  AAdd('FYearAndFactorGrid','GrowthFactorUnderGroundType2','Growth Factor(High Extraction Mine)');
  AAdd('FYearAndFactorGrid','GrowthFactorSlurryDump','Growth Factor(Slurry Dump Area)');
  AAdd('PlanningMineOpenCastDialog','PCDIniConcentrationCol','PCD Initial Concentration');
  AAdd('PlanningMineOpenCastDialog','WorkingCommYearCol','Working Commissioning Year');
  AAdd('PlanningMineOpenCastDialog','WorkingCommMonthCol','Working Commissioning Month');
  AAdd('PlanningMineOpenCastDialog','WorkingDecommYearCol','Working Decommissioning Year');
  AAdd('PlanningMineOpenCastDialog','WorkingDecommMonthCol','Working Decommissioning Month');
  AAdd('PlanningMineOpenCastDialog','RunoffSaltWashoffEfficiencyFactorCol','Efficiency Factor(Disturbed Rehabilitated Area)');
  AAdd('PlanningMineOpenCastDialog','IniSaltStoreCol','Initial Salt Store(Disturbed Rehabilitated Area)');
  AAdd('PlanningMineOpenCastDialog','ReChargeRateCol','ReCharge Rate(Disturbed Rehabilitated Area)');
  AAdd('TLoadGenerationControl','Caption','Load Generation');
  AAdd('TLoadGenerationControl','LoadGenerationType','Load Generation Type');
  AAdd('TLoadGenerationControl','StandardDeviation','Standard Deviation');
  AAdd('FactorsAndMeanGrid','OpenCastTypeFlow1','Flow (Working Area)');
  AAdd('FactorsAndMeanGrid','OpenCastTypeFlow2','Flow (Seep And Decant)');
  AAdd('FactorsAndMeanGrid','OpenCastTypeMean1','Mean of Salt (Working Area)');
  AAdd('FactorsAndMeanGrid','OpenCastTypeMean2','Mean of Salt (Seep And Decant)');
  AAdd('FactorsAndMeanGrid','UndergroundFlow','Flow (Recharge to underground)');
  AAdd('FactorsAndMeanGrid','UndergroundMean','Mean of Salt(Recharge to underground)');
  AAdd('FactorsAndMeanGrid','SlurryDumpFlow','Flow (Spill from Slurry Pond)');
  AAdd('FactorsAndMeanGrid','SlurryDumpMean','Mean of Salt (Spill from Slurry Pond)');
  AAdd('AbstractionControl','Caption','Abstraction');
  AAdd('AbstractionControl','AbstractionLabel','Abstraction To');
  AAdd('AbstractionControl','EvapCol','Evaporation');
  AAdd('AbstractionControl','RiverCol','River');
  AAdd('AbstractionControl','PCDCol','PCD');
  AAdd('AbstractionControl','TotalCol','Total');
  AAdd('AbstractionControl','PortionCol','Portion');
  AAdd('AbstractionControl','TimeSeriesLabel','TimeSeries File');
  AAdd('PlanningMineValidator','CreateGrowthFactorButton','Create GrowthFactor');
  AAdd('PlanningSlurryDumpDialog','SaltConcentrationCol','SaltConcentration');
  AAdd('AbstractionTotalErrorString','','Abstraction Total must be 1');
  AAdd('NoTimeSeriesFilePrompt','','Choose a file');
end;

end.
