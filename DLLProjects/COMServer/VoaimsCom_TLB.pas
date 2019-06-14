unit VoaimsCom_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 2017/08/07 03:02:47 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: R:\WRMF\Source\DLLProjects\COMServer\VoaimsCom (1)
// LIBID: {F13D4EA3-3431-41BD-9AC3-B590D97717D6}
// LCID: 0
// Helpfile:
// HelpString: VoaimsCom Library
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: Symbol 'Type' renamed to 'type_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  VoaimsComMajorVersion = 1;
  VoaimsComMinorVersion = 0;

  LIBID_VoaimsCom: TGUID = '{F13D4EA3-3431-41BD-9AC3-B590D97717D6}';

  IID_IVoaimsComObject: TGUID = '{E32AC184-D23F-486C-B868-55DD59687AEF}';
  CLASS_VoaimsComObject: TGUID = '{A0FDB2AB-5B0B-4DB2-B209-154CDCE8F81F}';
  IID_IYieldModel: TGUID = '{17E8AE84-5FF8-4703-9131-EC8BC8D964B8}';
  IID_IYieldModelData: TGUID = '{6035552B-AA3B-4234-9154-F52D6552B2E4}';
  IID_IRunConfigurationData: TGUID = '{DD181D34-A59E-4F38-B728-CF5309A1A78D}';
  IID_INetworkElementData: TGUID = '{A91BF235-D16E-4A40-B6C3-33C7513B63BC}';
  IID_INetworkFeaturesData: TGUID = '{66B4A7FB-983F-4691-9004-C5714B4C7D02}';
  IID_IYieldModelCapability: TGUID = '{935785F3-EDB9-4B27-986A-42CF99AA4C04}';
  IID_IMinimumFlowConstraintList: TGUID = '{936AEC2C-69C6-411D-B593-46AE0CCFDD1B}';
  IID_ILossFeatureList: TGUID = '{94001A90-EEF1-44ED-8BDB-A041100A736C}';
  IID_ISpecifiedDemandFeatureList: TGUID = '{9652E861-EFF9-4A38-B996-7EB6F0D4D68E}';
  IID_IMinMaxFlowConstraintList: TGUID = '{A6A92273-082E-41FD-8B37-FB0199FB7CAB}';
  IID_IPumpingFeatureList: TGUID = '{E370AB03-D77F-4498-8A7D-6EA119D3AA24}';
  IID_IDiversionFeatureList: TGUID = '{700FF6AD-31BB-4181-B5E1-D4FDD15F183B}';
  IID_IPhysicalFlowConstraintList: TGUID = '{EA845874-F75A-48CF-9E71-966E4626F04D}';
  IID_IIFRFeatureList: TGUID = '{8A63409B-C04D-40FA-9067-3F88D6228E60}';
  IID_IIrrigationAreaList: TGUID = '{30DC71C6-A460-401E-8BB8-1508D2E1B95B}';
  IID_IPowerPlantList: TGUID = '{6E225ECE-89B4-433C-9267-761B32F7C055}';
  IID_IMasterControlFeatureList: TGUID = '{79D59912-D11C-483E-A095-428596DEB905}';
  IID_ISpecifiedInflowFeatureList: TGUID = '{6C8BE668-1D55-4E79-9D1F-FF8B752CC811}';
  IID_ISpecifiedInflowFeature: TGUID = '{97404534-4FBF-402F-9BF5-6724EED88E77}';
  IID_IChannelPenalty: TGUID = '{9D90233B-65E2-4BA0-AE93-2E428FCCAE9E}';
  IID_IChannelPenaltyList: TGUID = '{1FCC104E-A317-45AB-BF25-96ABB40136A6}';
  IID_IGeneralFlowChannel: TGUID = '{31DB8AEE-FDEE-4D25-8846-2FADD0714262}';
  IID_IChannelList: TGUID = '{89232BB2-43A8-4F08-8686-749CDE3DE005}';
  IID_IMinimumFlowConstraint: TGUID = '{9E8813BF-59B4-47E0-8F43-D1745B384369}';
  IID_IMinMaxFlowConstraint: TGUID = '{5FC99ABE-A8E7-4E12-B6B3-1A214B3569C7}';
  IID_IPumpingFeature: TGUID = '{1FCE9C4F-2A23-42BC-9D82-BBC4781798D4}';
  IID_ILossFeature: TGUID = '{ADE40F68-978B-4683-AA51-598EC11DB1CB}';
  IID_ISpecifiedDemandFeature: TGUID = '{6BD67D61-66D6-45E0-A7CA-D7E459F7C752}';
  IID_IDiversionFeature: TGUID = '{3B88DA42-F28C-442A-90B3-767E8F3971BB}';
  IID_IPhysicalFlowConstraint: TGUID = '{FCAE72BB-48EF-4C79-B693-4919AF7AADFC}';
  IID_IIFRFeature: TGUID = '{4D59DC3B-3DFA-47F1-8FD0-7BF3C097805A}';
  IID_IIrrigationArea: TGUID = '{088C51FC-BD08-4987-9CAD-6A611089DC94}';
  IID_IPowerPlant: TGUID = '{4DB4134C-C9C6-40E0-8A26-612EBB902A5A}';
  IID_IMasterControlFeature: TGUID = '{7249EDFC-CEF7-45A0-8516-FA78F7F16A27}';
  IID_IReservoirDataList: TGUID = '{82882783-20D9-4FC0-A87D-7B46979C76F9}';
  IID_IReservoirPenaltyList: TGUID = '{AC25D6EB-5451-4CA1-95A8-7683D42301A0}';
  IID_IReservoirPenaltyCounts: TGUID = '{35985C5F-AC92-4ED4-9C23-68BD84AC2419}';
  IID_IReservoirPenaltyZoneData: TGUID = '{23133903-E02B-4FDC-B858-0A64790D38F1}';
  IID_IReservoirZoneElevationsData: TGUID = '{160A912C-F2B2-4F8F-B871-8DA77FCE0C85}';
  IID_IFixedElevation: TGUID = '{DB609BCF-A6F9-4DED-9A68-F2370621C47F}';
  IID_IDrawDownElevation: TGUID = '{DE7A0E27-2489-4191-B1F3-06AD10B0C838}';
  IID_IInitialLevelsData: TGUID = '{EB9A0291-558B-4B7C-B25A-970067775E45}';
  IID_IReservoirPenalty: TGUID = '{1F40FCAB-92F0-41F8-86D9-CDCE587EDBED}';
  IID_IReservoirConfigurationData: TGUID = '{FA96F9C7-2D42-43C3-96EE-2742C396254C}';
  IID_IReservoirEvaporationsData: TGUID = '{4E0F062A-6733-4A9C-96AD-B9175382C027}';
  IID_IReservoirElevationsData: TGUID = '{083ABEBD-E5C8-4295-98E5-E7217C7266EA}';
  IID_IReservoirVolumeData: TGUID = '{0408D497-F57C-4BFB-AAD3-AEF30B2D798F}';
  IID_IReservoirAreaData: TGUID = '{99019087-B13E-44EA-A442-7471316E3F45}';
  IID_IReservoirData: TGUID = '{9CA99E70-F417-4B01-982D-E61AC8F4DBAA}';
  IID_IParamReference: TGUID = '{06BC50F1-039C-44F9-9FF7-56DF64F3AF47}';
  IID_IParamSetup: TGUID = '{BA9CEC23-E1B3-4F75-AEF1-17D1B0FFF340}';
  IID_IModelCalendar: TGUID = '{17AD05BB-10AE-4865-A135-16CB6FEBA859}';
  IID_IWaterDemandCategory: TGUID = '{4F1B0065-2F38-4771-9558-89F021CD56A8}';
  IID_IWaterDemandConfiguration: TGUID = '{3680A493-9DD2-445C-9BB2-D3FF32041AF8}';
  IID_IWaterDemandFeature: TGUID = '{009856A6-7FC7-4CC1-8EFE-D8BC00E852A0}';
  IID_IWaterDemandFeatureList: TGUID = '{39B5B8C0-60C8-444F-9373-42A144557E4D}';
  IID_IChangeList: TGUID = '{EF57D643-E162-4363-B32A-6654E2399B49}';
  IID_IParameterChange: TGUID = '{E9C04174-8297-4824-A5B9-86ACBBED4891}';
  IID_IMetaData: TGUID = '{2E5D1CA0-500C-45E3-A7FA-EBE07F1C8623}';
  IID_IConfiguration: TGUID = '{E5D0C300-5DBE-4205-A4D6-E3DEAB8F909C}';
  IID_IWeatherEvents: TGUID = '{B121E354-184D-4E47-B0C5-C962FAAC7715}';
  IID_IChannelArea: TGUID = '{DC54F70A-CB16-4ED5-8967-FBEB4F2E4F76}';
  IID_IChannelAreaList: TGUID = '{D300D1F5-35D3-48B1-96B6-CD4BE1CC3B21}';
  IID_IOutputData: TGUID = '{566134B3-D4A0-4036-9E10-D0B43FE60131}';
  IID_IWaterUseOutputProportion: TGUID = '{6811556E-DB56-4ACD-A03E-535B82C8D302}';
  IID_IOutputDataSelection: TGUID = '{F7500348-92A7-4351-8F8D-CE909D5264F8}';
  IID_IUserCategory: TGUID = '{1C26657F-E973-4983-B0A3-399EBD46247C}';
  IID_IAllocationLevel: TGUID = '{943AA3F2-FE5B-45D2-A0DB-126CA039D484}';
  IID_ICoefficient: TGUID = '{C29BCBFB-5CD5-4842-A4EA-3DC84BB2A7D3}';
  IID_ISubSystem: TGUID = '{6BD2ABF8-CB50-4B8E-8D13-D0EA1A66B340}';
  IID_ISupportChannel: TGUID = '{2D28477F-9E75-4A50-AD4A-6FFE0FA626FD}';
  IID_ISupportSubSystem: TGUID = '{DFABB0B2-5CB1-4185-8E85-E9ADF8DC53A8}';
  IID_IDemandDefinition: TGUID = '{343494D1-4D53-41BB-8198-5AD1ACFAEC6E}';
  IID_IAllocationDefinition: TGUID = '{CB6E149E-EC34-46CC-995A-328C095A16FB}';
  IID_IAllocationDefinitionsList: TGUID = '{DD7DB287-FE2C-4911-BBF8-C37E24E32F3E}';
  IID_IPlanningModelData: TGUID = '{BF29970A-47A7-474A-91AC-868AF9913203}';
  IID_IPreprocessor: TGUID = '{5EFEF2C0-6FB5-4DE0-9045-120BA75B0601}';
  IID_IPreProcessorData: TGUID = '{08484FED-6C2F-4CC5-A25B-EB29B4FA52CD}';
  IID_IFixedPosition: TGUID = '{96C94A5D-57CB-4557-A178-2D3C58DD4951}';
  IID_ISpecificOrder: TGUID = '{38074998-F70E-445C-9AF5-22BFB232B3C7}';
  IID_IReservoirTimeControl: TGUID = '{01BD48AF-CC96-437A-8336-56A1D9B7B085}';
  IID_ISwitchDefinition: TGUID = '{39CD48B8-8F01-466C-9C15-565E8ACA91CE}';
  IID_ISwitchDefinitionsList: TGUID = '{4FE7FF75-2A38-4E1A-B7CD-C2619501EBA6}';
  IID_IChannelTimeControl: TGUID = '{4A7D3EBC-944D-4228-9103-4D89035A17DA}';
  IID_IChannelSwitchControl: TGUID = '{A59B8604-3713-4026-98BB-D15FC12C1B76}';
  IID_IDataFilePaths: TGUID = '{BEB0FBD7-E770-450B-9010-E4146F4697BA}';
  IID_IChangeGroupElement: TGUID = '{23E6A510-D8F3-4CF2-8778-8314A14D92AC}';
  IID_IChangeGroup: TGUID = '{06EAC9C3-6137-4E37-9783-DD5C88682380}';
  IID_IDemandCentreGrowthFactors: TGUID = '{F7EDAF21-5D53-46CF-889E-BF1BAB85E10A}';
  IID_IMinMaxChannelGrowthFactors: TGUID = '{816AD665-1BF0-48B5-A820-98F4F7712D12}';
  IID_IHydrologyGrowthFactors: TGUID = '{0D8515C5-BAAB-4BDC-9C25-BEA6CC34F336}';
  IID_IGrowthFactors: TGUID = '{6A27BCD6-2C5B-4EC2-AAFA-0F217A932880}';
  IID_IDisbenefitFunctionDefinition: TGUID = '{4E79F3A5-BA49-48C7-B309-777A652733AD}';
  IID_ICorrespondingChannel: TGUID = '{137D8C62-B6D9-4E14-9E43-14514D84FE94}';
  IID_IReturnFlowChannel: TGUID = '{0BA634F1-4408-4031-BA15-5415D9143739}';
  IID_IReturnFlowChannelData: TGUID = '{888D1F6D-66A3-47BB-A347-103D19E39958}';
  IID_IWaterUsage: TGUID = '{7AFE47CC-ED8E-4B0B-B68A-1F08C3CE2350}';
  IID_IIrrigationBlock: TGUID = '{000F54E0-B287-4850-AAEF-F7150CEFF141}';
  IID_IIrrigationBlockList: TGUID = '{1EB11A40-22CB-4963-8671-E43007DF7ECE}';
  IID_IWetland: TGUID = '{654C7F3D-22C3-452D-9ECF-390DF6ADF242}';
  IID_IWetlandList: TGUID = '{446FFBE4-5803-402F-BEF4-140D81A256EA}';
  IID_IDischargeCurve: TGUID = '{D4A2C02D-7A9F-4A1A-8225-094B0E1B8C51}';
  IID_ISandAquifer: TGUID = '{65AEFFF7-6334-4C60-8E38-8F677FB6101D}';
  IID_IKFactors: TGUID = '{1F6FBC5C-3A16-474E-9BE9-3F56112394E5}';
  IID_ISubmergedOutlet: TGUID = '{DFA99D79-7271-4232-96E6-8085675FDFA4}';
  IID_IPumpStation: TGUID = '{9477C918-43C9-4A59-8896-8DEBB9860B94}';
  IID_IYMDemandCentreList: TGUID = '{C4FCFE02-5B63-48EF-B598-073309F09DBF}';
  IID_IYMDemandCentre: TGUID = '{0C70C527-39EE-4179-BD48-CD7C5BF39901}';
  IID_IStreamFlowReduction: TGUID = '{2C7B7AEC-91B4-4B89-BB33-E6DC5E130CE5}';
  IID_IStreamFlowReductionList: TGUID = '{EF13F588-AB78-4A0F-BF0C-C4168E8C8F35}';
  IID_IYMDemandCentreReturnFlowFeature: TGUID = '{C740A20C-B0F8-4F21-BCA3-D058A4C51016}';
  IID_IYMDemandCentreReturnFlowFeatureList: TGUID = '{E40D8681-53EE-42BE-B7BE-EE2BC1FCEA3D}';
  IID_IOpenCast: TGUID = '{5938EAE4-44E1-4C7B-9FFB-748C85FA03C5}';
  IID_IUnderground: TGUID = '{C6A51A66-97DA-4A20-B862-A727F2A0861E}';
  IID_ISlurryDump: TGUID = '{6880B920-1E02-4BDA-B71E-504328F65BB1}';
  IID_IMine: TGUID = '{0701352E-A9A4-4C99-A665-097099769D6F}';
  IID_IMineList: TGUID = '{444D81E8-F01B-4545-8D27-86D19E266DF6}';
  IID_IWRYMRunOptions: TGUID = '{33D69FE6-3CC2-4C88-B246-B95EA4A1A3C9}';
  IID_IYieldModelIterationTracker: TGUID = '{FE44BCBF-7DAC-41BC-A096-9633F83B9CB7}';
  IID_IIterationEventHandler: TGUID = '{6964220E-D400-4DD9-9318-6F45D7011980}';
  IID_ICurtailedChannel: TGUID = '{27CA6B7A-8A20-4BA2-83C6-BB94692D3556}';
  IID_IDroughtRestriction: TGUID = '{D19E4AF0-BED8-4B77-B674-91A65ACBF719}';
  IID_ICurtailmentAndDrought: TGUID = '{7802E932-40AC-448E-A2BF-0B446E6AD0D1}';
  IID_ISummaryOutputData: TGUID = '{563FEF84-230F-4F31-B59F-58A9F4F18788}';
  IID_ISumOutBlob: TGUID = '{05247B86-D2B2-4694-A346-4678D4AF6E6D}';
  IID_IGroundWater: TGUID = '{8AAB9F91-24FB-40F4-82F2-3324C658E25F}';
  IID_IGroundWaterList: TGUID = '{CEC4F1FD-7A19-4D8F-9857-93A63BEFE09A}';
  IID_IImplementedNetworkFeatures: TGUID = '{53AE96E0-3DF8-454F-840D-FC7AD7E13025}';
  IID_IReservoirAreaGroup: TGUID = '{1D915594-27DF-48D1-A410-8D28D1B2ADA6}';
  IID_IReservoirAreaGroupList: TGUID = '{BA43B71A-8542-45CE-B5D8-8D9D268DED94}';
  IID_IMineSubCatchmentList: TGUID = '{BF54785C-8D2E-4FCC-9BAC-ABFCC36FD27C}';
  IID_IMineSubCatchment: TGUID = '{4DC181F1-7FBE-4BEB-A3B2-C03468D45E9C}';
  IID_IChannelTariff: TGUID = '{2EB5ED1C-D4B5-4CA6-91BD-AC1EB88DF393}';
  IID_ITariffCalculationData: TGUID = '{14DF8EB9-A444-4CB0-87CE-9A27444D0F93}';
  IID_IPlanningModel: TGUID = '{F3EAE7EA-2F4C-490C-A455-60FD75B344B9}';
  IID_IMinMaxUpperBoundChannel: TGUID = '{D66F4E22-7296-48C7-9303-6FC166C8B9F2}';
  IID_IWQConstriantsChannel: TGUID = '{C7931A9B-780E-46CC-B0F1-B16847AC76DA}';
  IID_IWQConstraintData: TGUID = '{F9F7CE9B-61C1-45BD-84EA-B11041A13F4A}';
  IID_IMultiResMultiChannelCurtail: TGUID = '{9D444A57-3146-4842-AD63-F5AF097826D5}';
  IID_IMultiResMultiChannelCurtailList: TGUID = '{70CFCA36-BB7D-4834-BBCE-1ECBE036F9D9}';
  IID_IPlanningMine: TGUID = '{2D979DB3-2616-4BD0-B5D3-EE767B200305}';
  IID_IPlanningMineGrowthFactor: TGUID = '{6FF1E9C2-3BB7-42AF-B40B-51B44F56C6E7}';
  IID_IPlanningOpenCast: TGUID = '{342ED78F-8F33-4323-9DFD-FBCD4312B2B8}';
  IID_ILoadGeneration: TGUID = '{3B8AAE03-D9FD-42D6-9449-9C149FF7E411}';
  IID_IPlanningSlurryDump: TGUID = '{21806A78-10B8-459B-91E0-3F301315D060}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library
// *********************************************************************//
// Constants for enum TNodeType
type
  TNodeType = TOleEnum;
const
  ntUnknown = $00000000;
  ntReservoir = $00000001;
  ntNodeWithInflow = $00000002;
  ntNodeWithoutInflow = $00000003;
  ntIrrigationNode = $00000004;
  ntWetlandNode = $00000005;
  ntDemandCentreNode = $00000006;
  ntMinePolutionControlDam = $00000007;
  ntMineUndergroundDam = $00000008;
  ntMineNode = $00000009;
  ntGroundWater = $0000000A;
  ntAbstractionNode = $0000000B;
  ntCollectionNode = $0000000C;
  ntBaseFlowNode = $0000000D;

// Constants for enum TDeleteAction
type
  TDeleteAction = TOleEnum;
const
  daDeleteAll = $00000000;
  daClearData = $00000001;
  daContinue = $00000002;
  daCancel = $00000003;

// Constants for enum TOutputDataType
type
  TOutputDataType = TOleEnum;
const
  btNone = $00000000;
  btMonthEndReservoirVolume = $00000001;
  btMonthEndReservoirElevation = $00000002;
  btNetBasinRunoffIntoResArea = $00000003;
  btRainfallOnReservoirSurface = $00000004;
  btGrossEvaporationLossFromReservoir = $00000005;
  btMonthlyAveragePowerFlow = $00000006;
  btMonthlyAverageSpillFlow = $00000007;
  btMonthlyAverageStackedCapacity = $00000008;
  btMonthlyAverageStackedEnergy = $00000009;
  btMonthlyAverageIrrigationDeficits = $0000000A;
  btMonthlyAverageChannelFlow = $0000000B;
  btMonthlyPumpingEnergy = $0000000C;
  btYieldFailurePerYearPerSequence = $0000000D;
  btOutputSummary = $0000000E;
  btAnualFirmYieldDemands = $0000000F;
  btAnualFirmEnergyDemands = $00000010;
  btAnualFirmSelectedYieldDemands = $00000011;
  btAnualNonFirmYieldDemands = $00000012;
  btAnualSecondaryYieldDemands = $00000013;
  btAnualTotalSystemPumpingEnergy = $00000014;
  btAnualFullSystemSupplyVolume = $00000015;
  btAnualAverageInflow = $00000016;
  btFirmYieldRecurrenceInterval = $00000018;
  btNumberOfFailureSequence = $00000019;
  btCriticalPeriodsNumber = $0000001A;
  btCriticalPeriodsLength = $0000001B;
  btCriticalPeriodsDeficit = $0000001C;
  btCriticalPeriodsAvarage = $0000001D;
  btDeficitPropotion = $0000001E;
  btReservoirChannel = $0000001F;
  btReservoirStorage = $00000020;
  btChannelFlowDeficit = $00000021;
  btChannelSuppAndDeficitPerc = $00000022;
  btChannelSuppAndCompliencePerc = $00000023;
  btSequencesWithFailures = $00000017;
  btIFRRequirementAndSupply = $00000024;
  btIFRFlow = $00000025;
  btIFRRequirement = $00000026;
  btIFRRequirementAndFlow = $00000027;
  btRequirementAndSupplyDifference = $00000028;
  btDefinedAndSimulatedData = $00000029;
  btDefinedIFR = $0000002A;
  btDefinedReferenceflowVsIFRRelationship = $0000002B;
  btIFRStats = $0000002C;
  btIFRHistogram = $0000002D;
  btIFRData = $0000002E;
  btDefinedAndRequired = $0000002F;
  btReservoirWaterBalance = $00000030;
  btMonthlyChannelWaterBalance = $00000031;
  btAnnualChannelWaterBalancel = $00000032;
  btCurtailments = $00000033;
  btDemandSupply = $00000034;
  btSubSystemPlot = $00000035;
  btSupplyPlot = $00000036;
  btChannelPumping = $00000037;
  btReservoirPlot = $00000038;

// Constants for enum TOutputTimeStep
type
  TOutputTimeStep = TOleEnum;
const
  otsMonthly = $00000000;
  otsAnnual = $00000001;
  otsSequence = $00000002;
  otsMonthlyCumulative = $00000003;
  otsAnnualCumulative = $00000004;

// Constants for enum TOutputUnits
type
  TOutputUnits = TOleEnum;
const
  ouPerSecond = $00000000;
  ouMcmPerMonthOrYear = $00000001;
  ouPercentage = $00000002;
  ouMCM = $00000003;
  ouMeters = $00000004;
  ouMegaLitersPerDay = $00000005;
  ouTotalPercentage = $00000006;
  ouLivePercentage = $00000007;

// Constants for enum TOutputValueType
type
  TOutputValueType = TOleEnum;
const
  ovtNone = $00000000;
  ovtSupply = $00000001;
  ovtDemand = $00000002;
  ovtDeficits = $00000003;
  ovtDemandAndSupply = $00000004;
  ovtAllocated = $00000005;

// Constants for enum TChangeElementType
type
  TChangeElementType = TOleEnum;
const
  cetChangeList = $00000000;
  cetInactiveList = $00000001;
  cetActiveList = $00000002;
  cetInactiveGroup = $00000003;
  cetActiveGroup = $00000004;
  cetAlienList = $00000005;
  cetNone = $00000006;

// Constants for enum TPhysicalFlowConstraintGroup
type
  TPhysicalFlowConstraintGroup = TOleEnum;
const
  pfcgNone = $00000000;
  pfcgDischargeCurve = $00000001;
  pfcgKFactors = $00000002;
  pfcgSandAquifer = $00000003;
  pfcgSubmergedOutlet = $00000004;
  pfcgPumpStation = $00000005;

// Constants for enum TPhysicalFlowConstraintType
type
  TPhysicalFlowConstraintType = TOleEnum;
const
  pfcstNone = $00000000;
  pfcstElevation = $00000001;
  pfcstDischarge = $00000002;
  pfcstChannelNumber = $00000003;
  pfcstKFactor = $00000004;
  pfcstHeadDifference = $00000005;
  pfcstAquiferFlow = $00000006;
  pfcstDownStreamNodeInflow = $00000007;
  pfcstRiverDepth = $00000008;
  pfcstElevationDifference = $00000009;
  pfcstMonthlyAverageInflow = $0000000A;
  pfcstMonthlyAverageDivertedFlow = $0000000B;
  pfcstPumpingHead = $0000000C;
  pfcstPumpingDischarge = $0000000D;

// Constants for enum TIFRFeatureReferenceFlowType
type
  TIFRFeatureReferenceFlowType = TOleEnum;
const
  ifrtMonthly = $00000000;
  ifrrftAnnual = $00000001;
  ifrtNone = $FFFFFFFF;

// Constants for enum TChannelType
type
  TChannelType = TOleEnum;
const
  ctNone = $00000000;
  ctChannelPenaltyStructure = $00000001;
  ctMasterControlChannel = $00000002;
  ctPowerPlantChannel = $00000003;
  ctIrrigationChannel = $00000004;
  ctDiversionChannel = $00000005;
  ctMinimumFlowChannel = $00000006;
  ctLossChannel = $00000007;
  ctMinMaxChannel = $00000008;
  clPumpingChannel = $00000009;
  ctInflowChannel = $0000000A;
  ctDemandChannel = $0000000B;
  ctGeneralChannel = $0000000C;
  ctChannelCreateSummary = $0000000D;
  ctIrrigationBlockInflowChannel = $0000000E;
  ctIrrigationBlockReturnFlowChannel = $0000000F;
  ctWetlandInflowChannel = $00000010;
  ctWetlandOutflowChannel = $00000011;
  ctIFRChannel = $00000012;
  ctPhysicalFlowConstraintChannel = $00000013;
  ctDemandCentreReturnFlowChannel = $00000014;
  ctReclaimationPlantLossChannel = $00000015;
  ctMineToPCDChannel = $00000016;
  ctMineToRiverDChannel = $00000017;
  ctMineToUndergroundChannel = $00000018;
  ctAquiferInflowChannel = $00000019;
  ctAquiferExcessInterflowChannel = $0000001A;
  ctGroundWaterBaseflowChannel = $0000001B;
  ctAbstractionFromAquiferChannel = $0000001C;
  ctAbstractionFromBaseFlowChannel = $0000001D;
  ctGroundWaterBaseFlowRemainderChannel = $0000001E;
  ctOutFlowToDownstreamAquiferChannel = $0000001F;
  ctSurfaceRunoffChannel = $00000020;
  ctGroundWaterAbstractionChannel = $00000021;
  ctOutflowToNetworkChannel = $00000022;
  ctInflowFromUpstreamAquiferChannel = $00000023;

// Constants for enum TOutputAverageType
type
  TOutputAverageType = TOleEnum;
const
  oatMonthly = $00000000;
  oatAnnual = $00000001;
  oatSequence = $00000002;
  oatPeriod = $00000003;

// Constants for enum TOutputPlotOptions
type
  TOutputPlotOptions = TOleEnum;
const
  poNone = $00000000;
  poCondenced = $00000001;
  poNotCondenced = $00000002;
  poCumulative = $00000003;
  poNotCumulative = $00000004;

// Constants for enum TNetworkElementType
type
  TNetworkElementType = TOleEnum;
const
  votNone = $00000000;
  votMasterControl = $00000001;
  votReservoir = $00000002;
  votNodeWithInflow = $00000003;
  votNodeWithoutInflow = $00000004;
  votChannel = $00000005;
  votIrrigationArea = $00000006;
  votPowerPlant = $00000007;
  votWetland = $00000008;
  votChannelArea = $00000009;
  votIrrigationBlock = $0000000A;
  votReservoirAreaGroup = $0000000B;
  votReviewSubSystemCurtailment = $0000000C;
  votReviewDemandSupply = $0000000D;
  votReviewTotalSystemStorage = $0000000E;
  votReviewInterBasinSupport = $0000000F;
  votReviewMonthlyReservoirResult = $00000010;
  votReviewMonthlyChannelResult = $00000011;
  votReviewCollateOutputFiles = $00000012;
  votReviewDamStorage = $00000013;
  votReviewDemands = $00000014;
  votReviewSubSystems = $00000015;
  votReviewSubSystemStorage = $00000016;
  votTotalSystemCurtailment = $00000017;

// Constants for enum TOutputSourceDialog
type
  TOutputSourceDialog = TOleEnum;
const
  osdNone = $00000000;
  osdNetworkVisualiser = $00000001;
  osdGrid = $00000002;
  osdGraph = $00000003;
  osdWaterBalance = $00000004;
  osdComplianceGrid = $00000005;
  osdComplianceGraph = $00000006;
  osdDistributionCurve = $00000007;
  osdChannelDemands = $00000008;
  osdChannelComparison = $00000009;
  osdDeficitDuration = $0000000A;
  osdMonthlyDeficit = $0000000B;
  osdWaterUseComplianceGraph = $0000000C;
  osdWRPMBoxPlotGrid = $0000000D;
  osdWRPMGrid = $0000000E;
  osdWRPMGraph = $0000000F;
  osdComparisonReservoir = $00000010;

// Constants for enum TSensitivity
type
  TSensitivity = TOleEnum;
const
  stvNone = $00000000;
  stvAbsolute = $00000001;
  stvPercentage = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IVoaimsComObject = interface;
  IYieldModel = interface;
  IYieldModelData = interface;
  IYieldModelDataDisp = dispinterface;
  IRunConfigurationData = interface;
  IRunConfigurationDataDisp = dispinterface;
  INetworkElementData = interface;
  INetworkElementDataDisp = dispinterface;
  INetworkFeaturesData = interface;
  INetworkFeaturesDataDisp = dispinterface;
  IYieldModelCapability = interface;
  IYieldModelCapabilityDisp = dispinterface;
  IMinimumFlowConstraintList = interface;
  IMinimumFlowConstraintListDisp = dispinterface;
  ILossFeatureList = interface;
  ILossFeatureListDisp = dispinterface;
  ISpecifiedDemandFeatureList = interface;
  ISpecifiedDemandFeatureListDisp = dispinterface;
  IMinMaxFlowConstraintList = interface;
  IMinMaxFlowConstraintListDisp = dispinterface;
  IPumpingFeatureList = interface;
  IPumpingFeatureListDisp = dispinterface;
  IDiversionFeatureList = interface;
  IDiversionFeatureListDisp = dispinterface;
  IPhysicalFlowConstraintList = interface;
  IPhysicalFlowConstraintListDisp = dispinterface;
  IIFRFeatureList = interface;
  IIFRFeatureListDisp = dispinterface;
  IIrrigationAreaList = interface;
  IIrrigationAreaListDisp = dispinterface;
  IPowerPlantList = interface;
  IPowerPlantListDisp = dispinterface;
  IMasterControlFeatureList = interface;
  IMasterControlFeatureListDisp = dispinterface;
  ISpecifiedInflowFeatureList = interface;
  ISpecifiedInflowFeatureListDisp = dispinterface;
  ISpecifiedInflowFeature = interface;
  ISpecifiedInflowFeatureDisp = dispinterface;
  IChannelPenalty = interface;
  IChannelPenaltyDisp = dispinterface;
  IChannelPenaltyList = interface;
  IChannelPenaltyListDisp = dispinterface;
  IGeneralFlowChannel = interface;
  IGeneralFlowChannelDisp = dispinterface;
  IChannelList = interface;
  IChannelListDisp = dispinterface;
  IMinimumFlowConstraint = interface;
  IMinimumFlowConstraintDisp = dispinterface;
  IMinMaxFlowConstraint = interface;
  IMinMaxFlowConstraintDisp = dispinterface;
  IPumpingFeature = interface;
  IPumpingFeatureDisp = dispinterface;
  ILossFeature = interface;
  ILossFeatureDisp = dispinterface;
  ISpecifiedDemandFeature = interface;
  ISpecifiedDemandFeatureDisp = dispinterface;
  IDiversionFeature = interface;
  IDiversionFeatureDisp = dispinterface;
  IPhysicalFlowConstraint = interface;
  IPhysicalFlowConstraintDisp = dispinterface;
  IIFRFeature = interface;
  IIFRFeatureDisp = dispinterface;
  IIrrigationArea = interface;
  IIrrigationAreaDisp = dispinterface;
  IPowerPlant = interface;
  IPowerPlantDisp = dispinterface;
  IMasterControlFeature = interface;
  IMasterControlFeatureDisp = dispinterface;
  IReservoirDataList = interface;
  IReservoirDataListDisp = dispinterface;
  IReservoirPenaltyList = interface;
  IReservoirPenaltyListDisp = dispinterface;
  IReservoirPenaltyCounts = interface;
  IReservoirPenaltyCountsDisp = dispinterface;
  IReservoirPenaltyZoneData = interface;
  IReservoirPenaltyZoneDataDisp = dispinterface;
  IReservoirZoneElevationsData = interface;
  IReservoirZoneElevationsDataDisp = dispinterface;
  IFixedElevation = interface;
  IFixedElevationDisp = dispinterface;
  IDrawDownElevation = interface;
  IDrawDownElevationDisp = dispinterface;
  IInitialLevelsData = interface;
  IInitialLevelsDataDisp = dispinterface;
  IReservoirPenalty = interface;
  IReservoirPenaltyDisp = dispinterface;
  IReservoirConfigurationData = interface;
  IReservoirConfigurationDataDisp = dispinterface;
  IReservoirEvaporationsData = interface;
  IReservoirEvaporationsDataDisp = dispinterface;
  IReservoirElevationsData = interface;
  IReservoirElevationsDataDisp = dispinterface;
  IReservoirVolumeData = interface;
  IReservoirVolumeDataDisp = dispinterface;
  IReservoirAreaData = interface;
  IReservoirAreaDataDisp = dispinterface;
  IReservoirData = interface;
  IReservoirDataDisp = dispinterface;
  IParamReference = interface;
  IParamReferenceDisp = dispinterface;
  IParamSetup = interface;
  IParamSetupDisp = dispinterface;
  IModelCalendar = interface;
  IModelCalendarDisp = dispinterface;
  IWaterDemandCategory = interface;
  IWaterDemandCategoryDisp = dispinterface;
  IWaterDemandConfiguration = interface;
  IWaterDemandConfigurationDisp = dispinterface;
  IWaterDemandFeature = interface;
  IWaterDemandFeatureDisp = dispinterface;
  IWaterDemandFeatureList = interface;
  IWaterDemandFeatureListDisp = dispinterface;
  IChangeList = interface;
  IChangeListDisp = dispinterface;
  IParameterChange = interface;
  IParameterChangeDisp = dispinterface;
  IMetaData = interface;
  IMetaDataDisp = dispinterface;
  IConfiguration = interface;
  IConfigurationDisp = dispinterface;
  IWeatherEvents = interface;
  IWeatherEventsDisp = dispinterface;
  IChannelArea = interface;
  IChannelAreaDisp = dispinterface;
  IChannelAreaList = interface;
  IChannelAreaListDisp = dispinterface;
  IOutputData = interface;
  IOutputDataDisp = dispinterface;
  IWaterUseOutputProportion = interface;
  IWaterUseOutputProportionDisp = dispinterface;
  IOutputDataSelection = interface;
  IOutputDataSelectionDisp = dispinterface;
  IUserCategory = interface;
  IUserCategoryDisp = dispinterface;
  IAllocationLevel = interface;
  IAllocationLevelDisp = dispinterface;
  ICoefficient = interface;
  ICoefficientDisp = dispinterface;
  ISubSystem = interface;
  ISubSystemDisp = dispinterface;
  ISupportChannel = interface;
  ISupportChannelDisp = dispinterface;
  ISupportSubSystem = interface;
  ISupportSubSystemDisp = dispinterface;
  IDemandDefinition = interface;
  IDemandDefinitionDisp = dispinterface;
  IAllocationDefinition = interface;
  IAllocationDefinitionDisp = dispinterface;
  IAllocationDefinitionsList = interface;
  IAllocationDefinitionsListDisp = dispinterface;
  IPlanningModelData = interface;
  IPlanningModelDataDisp = dispinterface;
  IPreprocessor = interface;
  IPreprocessorDisp = dispinterface;
  IPreProcessorData = interface;
  IPreProcessorDataDisp = dispinterface;
  IFixedPosition = interface;
  IFixedPositionDisp = dispinterface;
  ISpecificOrder = interface;
  ISpecificOrderDisp = dispinterface;
  IReservoirTimeControl = interface;
  IReservoirTimeControlDisp = dispinterface;
  ISwitchDefinition = interface;
  ISwitchDefinitionDisp = dispinterface;
  ISwitchDefinitionsList = interface;
  ISwitchDefinitionsListDisp = dispinterface;
  IChannelTimeControl = interface;
  IChannelTimeControlDisp = dispinterface;
  IChannelSwitchControl = interface;
  IChannelSwitchControlDisp = dispinterface;
  IDataFilePaths = interface;
  IDataFilePathsDisp = dispinterface;
  IChangeGroupElement = interface;
  IChangeGroupElementDisp = dispinterface;
  IChangeGroup = interface;
  IChangeGroupDisp = dispinterface;
  IDemandCentreGrowthFactors = interface;
  IDemandCentreGrowthFactorsDisp = dispinterface;
  IMinMaxChannelGrowthFactors = interface;
  IMinMaxChannelGrowthFactorsDisp = dispinterface;
  IHydrologyGrowthFactors = interface;
  IHydrologyGrowthFactorsDisp = dispinterface;
  IGrowthFactors = interface;
  IGrowthFactorsDisp = dispinterface;
  IDisbenefitFunctionDefinition = interface;
  IDisbenefitFunctionDefinitionDisp = dispinterface;
  ICorrespondingChannel = interface;
  ICorrespondingChannelDisp = dispinterface;
  IReturnFlowChannel = interface;
  IReturnFlowChannelDisp = dispinterface;
  IReturnFlowChannelData = interface;
  IReturnFlowChannelDataDisp = dispinterface;
  IWaterUsage = interface;
  IWaterUsageDisp = dispinterface;
  IIrrigationBlock = interface;
  IIrrigationBlockDisp = dispinterface;
  IIrrigationBlockList = interface;
  IIrrigationBlockListDisp = dispinterface;
  IWetland = interface;
  IWetlandDisp = dispinterface;
  IWetlandList = interface;
  IWetlandListDisp = dispinterface;
  IDischargeCurve = interface;
  IDischargeCurveDisp = dispinterface;
  ISandAquifer = interface;
  ISandAquiferDisp = dispinterface;
  IKFactors = interface;
  IKFactorsDisp = dispinterface;
  ISubmergedOutlet = interface;
  ISubmergedOutletDisp = dispinterface;
  IPumpStation = interface;
  IPumpStationDisp = dispinterface;
  IYMDemandCentreList = interface;
  IYMDemandCentreListDisp = dispinterface;
  IYMDemandCentre = interface;
  IYMDemandCentreDisp = dispinterface;
  IStreamFlowReduction = interface;
  IStreamFlowReductionDisp = dispinterface;
  IStreamFlowReductionList = interface;
  IStreamFlowReductionListDisp = dispinterface;
  IYMDemandCentreReturnFlowFeature = interface;
  IYMDemandCentreReturnFlowFeatureDisp = dispinterface;
  IYMDemandCentreReturnFlowFeatureList = interface;
  IYMDemandCentreReturnFlowFeatureListDisp = dispinterface;
  IOpenCast = interface;
  IOpenCastDisp = dispinterface;
  IUnderground = interface;
  IUndergroundDisp = dispinterface;
  ISlurryDump = interface;
  ISlurryDumpDisp = dispinterface;
  IMine = interface;
  IMineDisp = dispinterface;
  IMineList = interface;
  IMineListDisp = dispinterface;
  IWRYMRunOptions = interface;
  IWRYMRunOptionsDisp = dispinterface;
  IYieldModelIterationTracker = interface;
  IYieldModelIterationTrackerDisp = dispinterface;
  IIterationEventHandler = interface;
  IIterationEventHandlerDisp = dispinterface;
  ICurtailedChannel = interface;
  ICurtailedChannelDisp = dispinterface;
  IDroughtRestriction = interface;
  IDroughtRestrictionDisp = dispinterface;
  ICurtailmentAndDrought = interface;
  ICurtailmentAndDroughtDisp = dispinterface;
  ISummaryOutputData = interface;
  ISummaryOutputDataDisp = dispinterface;
  ISumOutBlob = interface;
  ISumOutBlobDisp = dispinterface;
  IGroundWater = interface;
  IGroundWaterDisp = dispinterface;
  IGroundWaterList = interface;
  IGroundWaterListDisp = dispinterface;
  IImplementedNetworkFeatures = interface;
  IImplementedNetworkFeaturesDisp = dispinterface;
  IReservoirAreaGroup = interface;
  IReservoirAreaGroupDisp = dispinterface;
  IReservoirAreaGroupList = interface;
  IReservoirAreaGroupListDisp = dispinterface;
  IMineSubCatchmentList = interface;
  IMineSubCatchmentListDisp = dispinterface;
  IMineSubCatchment = interface;
  IMineSubCatchmentDisp = dispinterface;
  IChannelTariff = interface;
  IChannelTariffDisp = dispinterface;
  ITariffCalculationData = interface;
  ITariffCalculationDataDisp = dispinterface;
  IPlanningModel = interface;
  IPlanningModelDisp = dispinterface;
  IMinMaxUpperBoundChannel = interface;
  IMinMaxUpperBoundChannelDisp = dispinterface;
  IWQConstriantsChannel = interface;
  IWQConstriantsChannelDisp = dispinterface;
  IWQConstraintData = interface;
  IWQConstraintDataDisp = dispinterface;
  IMultiResMultiChannelCurtail = interface;
  IMultiResMultiChannelCurtailDisp = dispinterface;
  IMultiResMultiChannelCurtailList = interface;
  IMultiResMultiChannelCurtailListDisp = dispinterface;
  IPlanningMine = interface;
  IPlanningMineDisp = dispinterface;
  IPlanningMineGrowthFactor = interface;
  IPlanningMineGrowthFactorDisp = dispinterface;
  IPlanningOpenCast = interface;
  IPlanningOpenCastDisp = dispinterface;
  ILoadGeneration = interface;
  ILoadGenerationDisp = dispinterface;
  IPlanningSlurryDump = interface;
  IPlanningSlurryDumpDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  VoaimsComObject = IVoaimsComObject;


// *********************************************************************//
// Interface: IVoaimsComObject
// Flags:     (256) OleAutomation
// GUID:      {E32AC184-D23F-486C-B868-55DD59687AEF}
// *********************************************************************//
  IVoaimsComObject = interface(IUnknown)
    ['{E32AC184-D23F-486C-B868-55DD59687AEF}']
    function Logon(const AUserID: WideString; const APassword: WideString): WordBool; safecall;
    function SelectStudy(const AModel: WideString; const AStudy: WideString;
                         const ASubArea: WideString; const AScenario: WideString): WordBool; safecall;
    function Get_YieldModel: IYieldModel; safecall;
    function Initialise: WordBool; safecall;
    function Get_INIFileName: WideString; safecall;
    function HandleVNVEvent(const AVisioApp: IUnknown; const AVisioDoc: IUnknown;
                            AVisioEventCode: Integer; const ASourceObj: IUnknown;
                            AEventID: Integer; AEventSeqNum: Integer; const ASubjectObj: IUnknown;
                            AMoreInfo: OleVariant): WordBool; safecall;
    function ProcessVNVSpecial(const AParameter: WideString): WordBool; safecall;
    function IsServerInitialised: WordBool; safecall;
    function IsUserLoggedOn: WordBool; safecall;
    function IsStudySelected: WordBool; safecall;
    function LoggedOnUserName: WideString; safecall;
    function SelectedStudyKeys: WideString; safecall;
    function UnlockScenario(const AStudyAreaCode: WideString; const AModelCode: WideString;
                            const ASubAreaCode: WideString; const AScenarioCode: WideString): WordBool; safecall;
    function CloseScenario: WordBool; safecall;
    function Logoff: WordBool; safecall;
    function Get_PlanningModel: IPlanningModel; safecall;
    property YieldModel: IYieldModel read Get_YieldModel;
    property INIFileName: WideString read Get_INIFileName;
    property PlanningModel: IPlanningModel read Get_PlanningModel;
  end;

// *********************************************************************//
// Interface: IYieldModel
// Flags:     (256) OleAutomation
// GUID:      {17E8AE84-5FF8-4703-9131-EC8BC8D964B8}
// *********************************************************************//
  IYieldModel = interface(IUnknown)
    ['{17E8AE84-5FF8-4703-9131-EC8BC8D964B8}']
    function DoValidateAllFiles: WordBool; safecall;
    function DoExportAllFiles: WordBool; safecall;
    function DoImportAllFiles: WordBool; safecall;
    function DoClearModelData: WordBool; safecall;
    function DoWizardNewReservoir: WordBool; safecall;
    function DoWizardNewNodeWithInflow: WordBool; safecall;
    function DoInvokeWizard: WordBool; safecall;
    function DoWizardRunYieldHistoric: WordBool; safecall;
    function DoWizardRunYieldStochastic: WordBool; safecall;
    function DoWizardNewChannel(const AUpDownNodeNumbers: WideString): WordBool; safecall;
    function Get_YieldModelData: IYieldModelData; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function DoExportAllFilesAndRunModel(ASilent: WordBool): WordBool; safecall;
    function GetYieldChannelYield: Double; safecall;
    function DoRunModel: WordBool; safecall;
    function DoCreateReservoir: IReservoirData; safecall;
    function DoDeleteReservoir(AReservoirNumber: Integer): WordBool; safecall;
    function DoCreateNodeWithInflow: IReservoirData; safecall;
    function DoDeleteNodeWithInflow(ANodeNumber: Integer): WordBool; safecall;
    function DoCreateNodeWithoutInflow: IReservoirData; safecall;
    function DoDeleteNodeWithoutInflow(ANodeNumber: Integer): WordBool; safecall;
    function DoCreateChannel(AUpStreamNodeNumber: Integer; ADownStreamNodeNumber: Integer): IGeneralFlowChannel; safecall;
    function DoDeleteChannel(AChannelNumber: Integer): WordBool; safecall;
    function DoConvertChannel(AChannelNumber: Integer): WordBool; safecall;
    function DoCreateMinimumFlowFeature(AChannelNumber: Integer): IMinimumFlowConstraint; safecall;
    function DoCreateMinMaxFlowFeature(AChannelNumber: Integer): IMinMaxFlowConstraint; safecall;
    function DoCreatePumpingFeature(AChannelNumber: Integer): IPumpingFeature; safecall;
    function DoCreateLossFeature(AChannelNumber: Integer): ILossFeature; safecall;
    function DoCreateSpecifiedDemandFeature(AChannelNumber: Integer): ISpecifiedDemandFeature; safecall;
    function DoCreateDiversionFeature(AChannelNumber: Integer): IDiversionFeature; safecall;
    function DoCreateSpecifiedInflowFeature(AChannelNumber: Integer): ISpecifiedInflowFeature; safecall;
    function DoCreateIFRFeature(AChannelNumber: Integer; AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature; safecall;
    function DoCreatePhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint; safecall;
    function DoCreateIrrigationArea: IIrrigationArea; safecall;
    function DoCreatePowerPlant: IPowerPlant; safecall;
    function DoDeletePumpingFeature(AChannelNumber: Integer): WordBool; safecall;
    function DoDeleteIFRFeature(AChannelNumber: Integer): WordBool; safecall;
    function DoDeletePhysicalFlowConstraint(AChannelNumber: Integer): WordBool; safecall;
    function DoDeleteIrrigationArea(AFeatureID: Integer): WordBool; safecall;
    function DoDeletePowerPlant(AFeatureID: Integer): WordBool; safecall;
    function DoCreateMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature; safecall;
    function DoDeleteMasterControlFeature(AChannelNumber: Integer): WordBool; safecall;
    function DoCreateWaterDemandFeature(AChannelNumber: Integer): IWaterDemandFeature; safecall;
    function DoDeleteWaterDemandFeature(AChannelNumber: Integer): WordBool; safecall;
    function ViewInputReservoirDialog(AResevoirNumber: Integer): WordBool; safecall;
    function ViewInputNodeWithInflowDialog(ANodeNumber: Integer): WordBool; safecall;
    function ViewInputChannelDialog(AChannelNumber: Integer): WordBool; safecall;
    function HandleVNVEvent(const AVisioApp: IUnknown; const AVisioDoc: IUnknown;
                            AVisioEventCode: Integer; const ASourceObj: IUnknown;
                            AEventID: Integer; AEventSeqNum: Integer; const ASubjectObj: IUnknown;
                            AMoreInfo: OleVariant): WordBool; safecall;
    function ProcessVNVSpecial(const AParameter: WideString): WordBool; safecall;
    function DoCreateIrrigationBlock: IIrrigationBlock; safecall;
    function DoDeleteIrrigationBlock(AFeatureID: Integer): WordBool; safecall;
    function DoCreateWetland: IWetland; safecall;
    function DoDeleteWetland(AFeatureID: Integer): WordBool; safecall;
    function ViewInputIrrigationBlockDialog(AIrrigationBlockNr: Integer): WordBool; safecall;
    function ViewInputWetlandDialog(AWetlandNr: Integer): WordBool; safecall;
    function DoCreateYMDemandCentre: IYMDemandCentre; safecall;
    function DoDeleteYMDemandCentre(ANodeNumber: Integer): WordBool; safecall;
    function DoCreateSFRSubCatchment: IStreamFlowReduction; safecall;
    function DoDeleteSFRSubCatchment(AIdentifier: Integer): WordBool; safecall;
    function ViewInputDemandCentreDialog(AYMDemandCentreNr: Integer): WordBool; safecall;
    function ReadFirmYieldFromDebugFile: Double; safecall;
    function DoCreateMine: IMine; safecall;
    function DoDeleteMine(AMineNumber: Integer): WordBool; safecall;
    function DoCreateOpenCast(AMineNumber: Integer): IOpenCast; safecall;
    function DoDeleteOpenCast(AMineNumber: Integer; AOpenCastIdentifier: Integer): WordBool; safecall;
    function DoCreateUnderGround(AMineNumber: Integer): IUnderground; safecall;
    function DoDeleteUnderGround(AMineNumber: Integer; AUnderGroundIdentifier: Integer): WordBool; safecall;
    function DoCreateSlurryDump(AMineNumber: Integer): ISlurryDump; safecall;
    function DoDeleteSlurryDump(AMineNumber: Integer; ASlurryDumpIdentifier: Integer): WordBool; safecall;
    function ViewInputConfigurationDialog(const AViewName: WideString): WordBool; safecall;
    function ImportOutputFiles: WordBool; safecall;
    function ViewInputNodeWithoutInflowDialog(ANodeNumber: Integer): WordBool; safecall;
    function ViewInputPowerPlantDialog(APowerPlantNumber: Integer): WordBool; safecall;
    function ViewInputIrrigationAreaDialog(AIrrigationAreaNumber: Integer): WordBool; safecall;
    function ViewInputStreamFlowReductionDialog(AStreamFlowReductionNumber: Integer): WordBool; safecall;
    function ViewInputMineDialog(AMineNodeNr: Integer): WordBool; safecall;
    function ViewInputMinePCDDamDialog(APCDDamNr: Integer): WordBool; safecall;
    function ViewInputMineUndergroundDamDialog(AUndergroundDamNr: Integer): WordBool; safecall;
    function ViewInputMasterControlChannelDialog(AChannelNumber: Integer): WordBool; safecall;
    function ViewOutputMasterControlChannelDialog(AChannelNumber: Integer): WordBool; safecall;
    function ViewOutputReservoirDialog(AResevoirNumber: Integer): WordBool; safecall;
    function ViewOutputNodeWithInflowDialog(ANodeNumber: Integer): WordBool; safecall;
    function ViewOutputNodeWithoutInflowDialog(ANodeNumber: Integer): WordBool; safecall;
    function ViewOutputChannelDialog(AChannelNumber: Integer): WordBool; safecall;
    function ViewOutputPowerPlantDialog(APowerPlantNumber: Integer): WordBool; safecall;
    function ViewOutputIrrigationAreaDialog(AIrrigationAreaNumber: Integer): WordBool; safecall;
    function ViewOutputIrrigationBlockDialog(AIrrigationBlockNr: Integer): WordBool; safecall;
    function ViewOutputWetlandDialog(AWetlandNr: Integer): WordBool; safecall;
    function ViewOutputStreamFlowReductionDialog(AStreamFlowReductionNumber: Integer): WordBool; safecall;
    function ViewOutputDemandCentreDialog(AYMDemandCentreNr: Integer): WordBool; safecall;
    function ViewOutputMineDialog(AMineNodeNr: Integer): WordBool; safecall;
    function ViewOutputMinePCDDamDialog(APCDDamNr: Integer): WordBool; safecall;
    function ViewOutputMineUndergroundDamDialog(AUndergroundDamNr: Integer): WordBool; safecall;
    function Get_WRYMRunOptions: IWRYMRunOptions; safecall;
    function Get_YieldModelIterationTracker: IYieldModelIterationTracker; safecall;
    function DoCreateDroughtRestriction: IDroughtRestriction; safecall;
    function DoDeleteDroughtRestriction(AIdentifier: Integer): WordBool; safecall;
    function DoCopyIFRFeature(AChannelNumber: Integer): IIFRFeature; safecall;
    function DoCopyChannel(AChannelNumber: Integer): IGeneralFlowChannel; safecall;
    function DoCopyMinimumFlowFeature(AChannelNumber: Integer): IMinimumFlowConstraint; safecall;
    function DoCopyMinMaxFlowFeature(AChannelNumber: Integer): IMinMaxFlowConstraint; safecall;
    function DoCopyLossFeature(AChannelNumber: Integer): ILossFeature; safecall;
    function DoCopySpecifiedDemandFeature(AChannelNumber: Integer): ISpecifiedDemandFeature; safecall;
    function DoCopyDiversionFeature(AChannelNumber: Integer): IDiversionFeature; safecall;
    function DoCopySpecifiedInflowFeature(AChannelNumber: Integer): ISpecifiedInflowFeature; safecall;
    function CopyPhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint; safecall;
    function DoCopyMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature; safecall;
    function DoCopyPumpingFeature(AChannelNumber: Integer): IPumpingFeature; safecall;
    function DoCopyWaterDemandFeature(AChannelNumber: Integer): IWaterDemandFeature; safecall;
    function DoCopyPowerPlant(AChannelNumber: Integer): IPowerPlant; safecall;
    function DoCopyIrrigationArea(AFeatureID: Integer): IIrrigationArea; safecall;
    function DoCopyIrrigationBlock(AFeatureID: Integer): IIrrigationBlock; safecall;
    function DoCopyWetland(AWetlandID: Integer): IWetland; safecall;
    function DoCopySFRSubCatchment(AStreamFlowReductionID: Integer): IStreamFlowReduction; safecall;
    function DoCopyYMDemandCentre(ANodeNumber: Integer): IYMDemandCentre; safecall;
    function DoCopyMine(AMineNumber: Integer): IMine; safecall;
    function DoCopyReservoir(AReservoirNumber: Integer): IReservoirData; safecall;
    function DoRunStorageVsYield(AReservoirNumber: Integer;
                                 const AStartingStorageCommaText: WideString;
                                 var AMinTargetDraftCommaText: WideString;
                                 var AMaxTargetDraftCommaText: WideString;
                                 var AYieldCommaText: WideString): WordBool; safecall;
    function DoCreateGroundWater: IGroundWater; safecall;
    function DoDeleteGroundWater(AGroundWaterID: Integer): WordBool; safecall;
    function DoCopyReservoirFromScenario: WordBool; safecall;
    function DoCopyChannelFromScenario: WordBool; safecall;
    function DoCopyIrrigationAreaFromScenario: WordBool; safecall;
    function DoCopyPowerPlantFromScenario: WordBool; safecall;
    function DoCopyIrrigationBlockFromScenario: WordBool; safecall;
    function DoCopyWetlandFromScenario: WordBool; safecall;
    function DoCopyYMDemandCentreFromScenario: WordBool; safecall;
    function DoCopySFRFromScenario: WordBool; safecall;
    function DoCopyMineFromScenario: WordBool; safecall;
    function DoCopyGroundWaterFromScenario: WordBool; safecall;
    function ViewInputGroundwaterDialog(AAquiferNodeNr: Integer): WordBool; safecall;
    function ViewOutputGroundwaterDialog(AAquiferNodeNr: Integer): WordBool; safecall;
    function StudyPropertiesCommaText: WideString; safecall;
    property YieldModelData: IYieldModelData read Get_YieldModelData;
    property WRYMRunOptions: IWRYMRunOptions read Get_WRYMRunOptions;
    property YieldModelIterationTracker: IYieldModelIterationTracker read Get_YieldModelIterationTracker;
  end;

// *********************************************************************//
// Interface: IYieldModelData
// Flags:     (320) Dual OleAutomation
// GUID:      {6035552B-AA3B-4234-9154-F52D6552B2E4}
// *********************************************************************//
  IYieldModelData = interface(IUnknown)
    ['{6035552B-AA3B-4234-9154-F52D6552B2E4}']
    function Get_RunConfigurationData: IRunConfigurationData; safecall;
    function Get_NetworkElementData: INetworkElementData; safecall;
    function Get_NetworkFeaturesData: INetworkFeaturesData; safecall;
    function Get_YieldModelCapability: IYieldModelCapability; safecall;
    function Get_ParamSetup: IParamSetup; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ModelCalendar: IModelCalendar; safecall;
    procedure HydrologyFilesForCatchment(ACatchmentRef: Integer;
                                         var AFilesNamesContainer: WideString); safecall;
    function Get_DataFilePaths: IDataFilePaths; safecall;
    function Get_HydrologyFileData(const AFileName: WideString): WideString; safecall;
    function Get_DemandFileData(const AFileName: WideString): WideString; safecall;
    function Get_OutputData: IOutputData; safecall;
    function Get_ImplementedNetworkFeatures: IImplementedNetworkFeatures; safecall;
    property RunConfigurationData: IRunConfigurationData read Get_RunConfigurationData;
    property NetworkElementData: INetworkElementData read Get_NetworkElementData;
    property NetworkFeaturesData: INetworkFeaturesData read Get_NetworkFeaturesData;
    property YieldModelCapability: IYieldModelCapability read Get_YieldModelCapability;
    property ParamSetup: IParamSetup read Get_ParamSetup;
    property ModelCalendar: IModelCalendar read Get_ModelCalendar;
    property DataFilePaths: IDataFilePaths read Get_DataFilePaths;
    property HydrologyFileData[const AFileName: WideString]: WideString read Get_HydrologyFileData;
    property DemandFileData[const AFileName: WideString]: WideString read Get_DemandFileData;
    property OutputData: IOutputData read Get_OutputData;
    property ImplementedNetworkFeatures: IImplementedNetworkFeatures read Get_ImplementedNetworkFeatures;
  end;

// *********************************************************************//
// DispIntf:  IYieldModelDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6035552B-AA3B-4234-9154-F52D6552B2E4}
// *********************************************************************//
  IYieldModelDataDisp = dispinterface
    ['{6035552B-AA3B-4234-9154-F52D6552B2E4}']
    property RunConfigurationData: IRunConfigurationData readonly dispid 101;
    property NetworkElementData: INetworkElementData readonly dispid 102;
    property NetworkFeaturesData: INetworkFeaturesData readonly dispid 103;
    property YieldModelCapability: IYieldModelCapability readonly dispid 104;
    property ParamSetup: IParamSetup readonly dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    property ModelCalendar: IModelCalendar readonly dispid 107;
    procedure HydrologyFilesForCatchment(ACatchmentRef: Integer;
                                         var AFilesNamesContainer: WideString); dispid 108;
    property DataFilePaths: IDataFilePaths readonly dispid 110;
    property HydrologyFileData[const AFileName: WideString]: WideString readonly dispid 111;
    property DemandFileData[const AFileName: WideString]: WideString readonly dispid 112;
    property OutputData: IOutputData readonly dispid 109;
    property ImplementedNetworkFeatures: IImplementedNetworkFeatures readonly dispid 113;
  end;

// *********************************************************************//
// Interface: IRunConfigurationData
// Flags:     (320) Dual OleAutomation
// GUID:      {DD181D34-A59E-4F38-B728-CF5309A1A78D}
// *********************************************************************//
  IRunConfigurationData = interface(IUnknown)
    ['{DD181D34-A59E-4F38-B728-CF5309A1A78D}']
    function Get_MonthNameByIndex(Index: Integer): WideString; safecall;
    procedure Set_MonthNameByIndex(Index: Integer; const Value: WideString); safecall;
    function Get_MonthDaysByIndex(Index: Integer): Double; safecall;
    procedure Set_MonthDaysByIndex(Index: Integer; Value: Double); safecall;
    function Get_MonthIndexByName(const Name: WideString): Integer; safecall;
    procedure Set_MonthIndexByName(const Name: WideString; Value: Integer); safecall;
    function Get_MonthDaysByName(const Name: WideString): Double; safecall;
    procedure Set_MonthDaysByName(const Name: WideString; Value: Double); safecall;
    function Get_StartYearGregorian: Integer; safecall;
    procedure Set_StartYearGregorian(Value: Integer); safecall;
    function Get_StartYearOther: Integer; safecall;
    procedure Set_StartYearOther(Value: Integer); safecall;
    function Get_YearsInAnalysis: Integer; safecall;
    procedure Set_YearsInAnalysis(Value: Integer); safecall;
    function Get_PeriodsInAnalysis: Integer; safecall;
    procedure Set_PeriodsInAnalysis(Value: Integer); safecall;
    function Get_StartMonthNumber: Integer; safecall;
    procedure Set_StartMonthNumber(Value: Integer); safecall;
    function Get_CalculateHistoricFirmYield: Integer; safecall;
    procedure Set_CalculateHistoricFirmYield(Value: Integer); safecall;
    function Get_LimitOption: WordBool; safecall;
    procedure Set_LimitOption(Value: WordBool); safecall;
    function Get_RunSequenceType: WideString; safecall;
    procedure Set_RunSequenceType(const Value: WideString); safecall;
    function Get_MultiplePeriodLengths: WordBool; safecall;
    procedure Set_MultiplePeriodLengths(Value: WordBool); safecall;
    function Get_ReduceSequences: WordBool; safecall;
    procedure Set_ReduceSequences(Value: WordBool); safecall;
    function Get_HistoricSequenceStartYear: Integer; safecall;
    procedure Set_HistoricSequenceStartYear(Value: Integer); safecall;
    function Get_StartSequenceNumber: Integer; safecall;
    procedure Set_StartSequenceNumber(Value: Integer); safecall;
    function Get_NumberOfSequencesInAnalysis: Integer; safecall;
    procedure Set_NumberOfSequencesInAnalysis(Value: Integer); safecall;
    function Get_GeneratedFlowFlag: Integer; safecall;
    procedure Set_GeneratedFlowFlag(Value: Integer); safecall;
    function Get_SequenceToBeAnalysedByIndex(Index: Integer): Integer; safecall;
    procedure Set_SequenceToBeAnalysedByIndex(Index: Integer; Value: Integer); safecall;
    function Get_DebugLevel: Integer; safecall;
    procedure Set_DebugLevel(Value: Integer); safecall;
    function Get_StartDebugPeriod: Integer; safecall;
    procedure Set_StartDebugPeriod(Value: Integer); safecall;
    function Get_EndDebugPeriod: Integer; safecall;
    procedure Set_EndDebugPeriod(Value: Integer); safecall;
    function Get_StartDebugDate: TDateTime; safecall;
    procedure Set_StartDebugDate(Value: TDateTime); safecall;
    function Get_EndDebugDate: TDateTime; safecall;
    procedure Set_EndDebugDate(Value: TDateTime); safecall;
    function Get_OutputSummaryLevel: Integer; safecall;
    procedure Set_OutputSummaryLevel(Value: Integer); safecall;
    function Get_CreateDataFile: WordBool; safecall;
    procedure Set_CreateDataFile(Value: WordBool); safecall;
    function Get_CreateYieldFile: WordBool; safecall;
    procedure Set_CreateYieldFile(Value: WordBool); safecall;
    function Get_CreatePlotFile: WordBool; safecall;
    procedure Set_CreatePlotFile(Value: WordBool); safecall;
    function Get_TargetYieldByIndex(Index: Integer): Double; safecall;
    procedure Set_TargetYieldByIndex(Index: Integer; Value: Double); safecall;
    function Get_MaximumYieldByIndex(Index: Integer): Double; safecall;
    procedure Set_MaximumYieldByIndex(Index: Integer; Value: Double); safecall;
    function Get_TargetPowerByIndex(Index: Integer): Double; safecall;
    procedure Set_TargetPowerByIndex(Index: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_HasBeenPopulated: WordBool; safecall;
    function Get_OutputDataHasBeenPopulated: WordBool; safecall;
    function Get_TargetRecurrenceInterval: Integer; safecall;
    procedure Set_TargetRecurrenceInterval(Value: Integer); safecall;
    function Get_YieldRunTitle1: WideString; safecall;
    procedure Set_YieldRunTitle1(const Value: WideString); safecall;
    function Get_YieldRunTitle2: WideString; safecall;
    procedure Set_YieldRunTitle2(const Value: WideString); safecall;
    function Get_YieldRunTitle3: WideString; safecall;
    procedure Set_YieldRunTitle3(const Value: WideString); safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_NrOfActiveLoadCases: Integer; safecall;
    function Get_DetailedOption: WordBool; safecall;
    procedure Set_DetailedOption(Value: WordBool); safecall;
    function Get_SupplyOption: WordBool; safecall;
    procedure Set_SupplyOption(Value: WordBool); safecall;
    function Get_AnnualSummary: WideString; safecall;
    procedure Set_AnnualSummary(const Value: WideString); safecall;
    function Get_EconomicOption: WordBool; safecall;
    procedure Set_EconomicOption(Value: WordBool); safecall;
    function Get_PlanningSummary: WordBool; safecall;
    procedure Set_PlanningSummary(Value: WordBool); safecall;
    function Get_InputSummary: WordBool; safecall;
    procedure Set_InputSummary(Value: WordBool); safecall;
    function Get_WaterQualityOption: WordBool; safecall;
    procedure Set_WaterQualityOption(Value: WordBool); safecall;
    function Get_PeriodsPerYear: Integer; safecall;
    procedure Set_PeriodsPerYear(Value: Integer); safecall;
    function Get_CalendarStartMonth: Integer; safecall;
    procedure Set_CalendarStartMonth(Value: Integer); safecall;
    function Get_ShortTermPlanningOption: WideString; safecall;
    procedure Set_ShortTermPlanningOption(const Value: WideString); safecall;
    function Get_HydroPowerOption: WideString; safecall;
    procedure Set_HydroPowerOption(const Value: WideString); safecall;
    function Get_AllocationControlOption: WideString; safecall;
    procedure Set_AllocationControlOption(const Value: WideString); safecall;
    function Get_NrOfDecisionMonths: Integer; safecall;
    procedure Set_NrOfDecisionMonths(Value: Integer); safecall;
    function Get_DecisionMonthByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_DecisionMonthByIndex(AIndex: Integer; AValue: Integer); safecall;
    function Get_DecisionTypeByIndex(AIndex: Integer): WideString; safecall;
    procedure Set_DecisionTypeByIndex(AIndex: Integer; const AValue: WideString); safecall;
    function Get_HydroPowerIndicatorByIndex(AIndex: Integer): WideString; safecall;
    procedure Set_HydroPowerIndicatorByIndex(AIndex: Integer; const AValue: WideString); safecall;
    function Get_HydroUnitsCode: WideString; safecall;
    procedure Set_HydroUnitsCode(const Value: WideString); safecall;
    function Get_RandomNumberOption: Integer; safecall;
    procedure Set_RandomNumberOption(Value: Integer); safecall;
    function Get_ParamFileName: WideString; safecall;
    procedure Set_ParamFileName(const Value: WideString); safecall;
    property MonthNameByIndex[Index: Integer]: WideString read Get_MonthNameByIndex write Set_MonthNameByIndex;
    property MonthDaysByIndex[Index: Integer]: Double read Get_MonthDaysByIndex write Set_MonthDaysByIndex;
    property MonthIndexByName[const Name: WideString]: Integer read Get_MonthIndexByName write Set_MonthIndexByName;
    property MonthDaysByName[const Name: WideString]: Double read Get_MonthDaysByName write Set_MonthDaysByName;
    property StartYearGregorian: Integer read Get_StartYearGregorian write Set_StartYearGregorian;
    property StartYearOther: Integer read Get_StartYearOther write Set_StartYearOther;
    property YearsInAnalysis: Integer read Get_YearsInAnalysis write Set_YearsInAnalysis;
    property PeriodsInAnalysis: Integer read Get_PeriodsInAnalysis write Set_PeriodsInAnalysis;
    property StartMonthNumber: Integer read Get_StartMonthNumber write Set_StartMonthNumber;
    property CalculateHistoricFirmYield: Integer read Get_CalculateHistoricFirmYield write Set_CalculateHistoricFirmYield;
    property LimitOption: WordBool read Get_LimitOption write Set_LimitOption;
    property RunSequenceType: WideString read Get_RunSequenceType write Set_RunSequenceType;
    property MultiplePeriodLengths: WordBool read Get_MultiplePeriodLengths write Set_MultiplePeriodLengths;
    property ReduceSequences: WordBool read Get_ReduceSequences write Set_ReduceSequences;
    property HistoricSequenceStartYear: Integer read Get_HistoricSequenceStartYear write Set_HistoricSequenceStartYear;
    property StartSequenceNumber: Integer read Get_StartSequenceNumber write Set_StartSequenceNumber;
    property NumberOfSequencesInAnalysis: Integer read Get_NumberOfSequencesInAnalysis write Set_NumberOfSequencesInAnalysis;
    property GeneratedFlowFlag: Integer read Get_GeneratedFlowFlag write Set_GeneratedFlowFlag;
    property SequenceToBeAnalysedByIndex[Index: Integer]: Integer read Get_SequenceToBeAnalysedByIndex write Set_SequenceToBeAnalysedByIndex;
    property DebugLevel: Integer read Get_DebugLevel write Set_DebugLevel;
    property StartDebugPeriod: Integer read Get_StartDebugPeriod write Set_StartDebugPeriod;
    property EndDebugPeriod: Integer read Get_EndDebugPeriod write Set_EndDebugPeriod;
    property StartDebugDate: TDateTime read Get_StartDebugDate write Set_StartDebugDate;
    property EndDebugDate: TDateTime read Get_EndDebugDate write Set_EndDebugDate;
    property OutputSummaryLevel: Integer read Get_OutputSummaryLevel write Set_OutputSummaryLevel;
    property CreateDataFile: WordBool read Get_CreateDataFile write Set_CreateDataFile;
    property CreateYieldFile: WordBool read Get_CreateYieldFile write Set_CreateYieldFile;
    property CreatePlotFile: WordBool read Get_CreatePlotFile write Set_CreatePlotFile;
    property TargetYieldByIndex[Index: Integer]: Double read Get_TargetYieldByIndex write Set_TargetYieldByIndex;
    property MaximumYieldByIndex[Index: Integer]: Double read Get_MaximumYieldByIndex write Set_MaximumYieldByIndex;
    property TargetPowerByIndex[Index: Integer]: Double read Get_TargetPowerByIndex write Set_TargetPowerByIndex;
    property HasBeenPopulated: WordBool read Get_HasBeenPopulated;
    property OutputDataHasBeenPopulated: WordBool read Get_OutputDataHasBeenPopulated;
    property TargetRecurrenceInterval: Integer read Get_TargetRecurrenceInterval write Set_TargetRecurrenceInterval;
    property YieldRunTitle1: WideString read Get_YieldRunTitle1 write Set_YieldRunTitle1;
    property YieldRunTitle2: WideString read Get_YieldRunTitle2 write Set_YieldRunTitle2;
    property YieldRunTitle3: WideString read Get_YieldRunTitle3 write Set_YieldRunTitle3;
    property NrOfActiveLoadCases: Integer read Get_NrOfActiveLoadCases;
    property DetailedOption: WordBool read Get_DetailedOption write Set_DetailedOption;
    property SupplyOption: WordBool read Get_SupplyOption write Set_SupplyOption;
    property AnnualSummary: WideString read Get_AnnualSummary write Set_AnnualSummary;
    property EconomicOption: WordBool read Get_EconomicOption write Set_EconomicOption;
    property PlanningSummary: WordBool read Get_PlanningSummary write Set_PlanningSummary;
    property InputSummary: WordBool read Get_InputSummary write Set_InputSummary;
    property WaterQualityOption: WordBool read Get_WaterQualityOption write Set_WaterQualityOption;
    property PeriodsPerYear: Integer read Get_PeriodsPerYear write Set_PeriodsPerYear;
    property CalendarStartMonth: Integer read Get_CalendarStartMonth write Set_CalendarStartMonth;
    property ShortTermPlanningOption: WideString read Get_ShortTermPlanningOption write Set_ShortTermPlanningOption;
    property HydroPowerOption: WideString read Get_HydroPowerOption write Set_HydroPowerOption;
    property AllocationControlOption: WideString read Get_AllocationControlOption write Set_AllocationControlOption;
    property NrOfDecisionMonths: Integer read Get_NrOfDecisionMonths write Set_NrOfDecisionMonths;
    property DecisionMonthByIndex[AIndex: Integer]: Integer read Get_DecisionMonthByIndex write Set_DecisionMonthByIndex;
    property DecisionTypeByIndex[AIndex: Integer]: WideString read Get_DecisionTypeByIndex write Set_DecisionTypeByIndex;
    property HydroPowerIndicatorByIndex[AIndex: Integer]: WideString read Get_HydroPowerIndicatorByIndex write Set_HydroPowerIndicatorByIndex;
    property HydroUnitsCode: WideString read Get_HydroUnitsCode write Set_HydroUnitsCode;
    property RandomNumberOption: Integer read Get_RandomNumberOption write Set_RandomNumberOption;
    property ParamFileName: WideString read Get_ParamFileName write Set_ParamFileName;
  end;

// *********************************************************************//
// DispIntf:  IRunConfigurationDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DD181D34-A59E-4F38-B728-CF5309A1A78D}
// *********************************************************************//
  IRunConfigurationDataDisp = dispinterface
    ['{DD181D34-A59E-4F38-B728-CF5309A1A78D}']
    property MonthNameByIndex[Index: Integer]: WideString dispid 101;
    property MonthDaysByIndex[Index: Integer]: Double dispid 102;
    property MonthIndexByName[const Name: WideString]: Integer dispid 103;
    property MonthDaysByName[const Name: WideString]: Double dispid 104;
    property StartYearGregorian: Integer dispid 105;
    property StartYearOther: Integer dispid 106;
    property YearsInAnalysis: Integer dispid 107;
    property PeriodsInAnalysis: Integer dispid 108;
    property StartMonthNumber: Integer dispid 109;
    property CalculateHistoricFirmYield: Integer dispid 110;
    property LimitOption: WordBool dispid 111;
    property RunSequenceType: WideString dispid 112;
    property MultiplePeriodLengths: WordBool dispid 113;
    property ReduceSequences: WordBool dispid 114;
    property HistoricSequenceStartYear: Integer dispid 115;
    property StartSequenceNumber: Integer dispid 116;
    property NumberOfSequencesInAnalysis: Integer dispid 117;
    property GeneratedFlowFlag: Integer dispid 118;
    property SequenceToBeAnalysedByIndex[Index: Integer]: Integer dispid 119;
    property DebugLevel: Integer dispid 121;
    property StartDebugPeriod: Integer dispid 122;
    property EndDebugPeriod: Integer dispid 123;
    property StartDebugDate: TDateTime dispid 124;
    property EndDebugDate: TDateTime dispid 125;
    property OutputSummaryLevel: Integer dispid 126;
    property CreateDataFile: WordBool dispid 127;
    property CreateYieldFile: WordBool dispid 128;
    property CreatePlotFile: WordBool dispid 129;
    property TargetYieldByIndex[Index: Integer]: Double dispid 130;
    property MaximumYieldByIndex[Index: Integer]: Double dispid 131;
    property TargetPowerByIndex[Index: Integer]: Double dispid 132;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 133;
    property HasBeenPopulated: WordBool readonly dispid 134;
    property OutputDataHasBeenPopulated: WordBool readonly dispid 135;
    property TargetRecurrenceInterval: Integer dispid 136;
    property YieldRunTitle1: WideString dispid 138;
    property YieldRunTitle2: WideString dispid 137;
    property YieldRunTitle3: WideString dispid 139;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 140;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 141;
    property NrOfActiveLoadCases: Integer readonly dispid 142;
    property DetailedOption: WordBool dispid 120;
    property SupplyOption: WordBool dispid 143;
    property AnnualSummary: WideString dispid 144;
    property EconomicOption: WordBool dispid 145;
    property PlanningSummary: WordBool dispid 146;
    property InputSummary: WordBool dispid 147;
    property WaterQualityOption: WordBool dispid 148;
    property PeriodsPerYear: Integer dispid 149;
    property CalendarStartMonth: Integer dispid 150;
    property ShortTermPlanningOption: WideString dispid 151;
    property HydroPowerOption: WideString dispid 152;
    property AllocationControlOption: WideString dispid 153;
    property NrOfDecisionMonths: Integer dispid 154;
    property DecisionMonthByIndex[AIndex: Integer]: Integer dispid 155;
    property DecisionTypeByIndex[AIndex: Integer]: WideString dispid 156;
    property HydroPowerIndicatorByIndex[AIndex: Integer]: WideString dispid 157;
    property HydroUnitsCode: WideString dispid 158;
    property RandomNumberOption: Integer dispid 159;
    property ParamFileName: WideString dispid 160;
  end;

// *********************************************************************//
// Interface: INetworkElementData
// Flags:     (320) Dual OleAutomation
// GUID:      {A91BF235-D16E-4A40-B6C3-33C7513B63BC}
// *********************************************************************//
  INetworkElementData = interface(IUnknown)
    ['{A91BF235-D16E-4A40-B6C3-33C7513B63BC}']
    function Get_ReservoirList: IReservoirDataList; safecall;
    function Get_ReservoirPenaltyStructureList: IReservoirPenaltyList; safecall;
    function Get_ChannelList: IChannelList; safecall;
    function Get_ChannelPenaltyList: IChannelPenaltyList; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ReservoirAreaGroupList: IReservoirAreaGroupList; safecall;
    property ReservoirList: IReservoirDataList read Get_ReservoirList;
    property ReservoirPenaltyStructureList: IReservoirPenaltyList read Get_ReservoirPenaltyStructureList;
    property ChannelList: IChannelList read Get_ChannelList;
    property ChannelPenaltyList: IChannelPenaltyList read Get_ChannelPenaltyList;
    property ReservoirAreaGroupList: IReservoirAreaGroupList read Get_ReservoirAreaGroupList;
  end;

// *********************************************************************//
// DispIntf:  INetworkElementDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {A91BF235-D16E-4A40-B6C3-33C7513B63BC}
// *********************************************************************//
  INetworkElementDataDisp = dispinterface
    ['{A91BF235-D16E-4A40-B6C3-33C7513B63BC}']
    property ReservoirList: IReservoirDataList readonly dispid 101;
    property ReservoirPenaltyStructureList: IReservoirPenaltyList readonly dispid 102;
    property ChannelList: IChannelList readonly dispid 103;
    property ChannelPenaltyList: IChannelPenaltyList readonly dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
    property ReservoirAreaGroupList: IReservoirAreaGroupList readonly dispid 106;
  end;

// *********************************************************************//
// Interface: INetworkFeaturesData
// Flags:     (320) Dual OleAutomation
// GUID:      {66B4A7FB-983F-4691-9004-C5714B4C7D02}
// *********************************************************************//
  INetworkFeaturesData = interface(IUnknown)
    ['{66B4A7FB-983F-4691-9004-C5714B4C7D02}']
    function Get_MinimumFlowConstraintList: IMinimumFlowConstraintList; safecall;
    function Get_LossFeatureList: ILossFeatureList; safecall;
    function Get_SpecifiedDemandFeatureList: ISpecifiedDemandFeatureList; safecall;
    function Get_MinMaxFlowConstraintList: IMinMaxFlowConstraintList; safecall;
    function Get_PumpingFeatureList: IPumpingFeatureList; safecall;
    function Get_DiversionFeatureList: IDiversionFeatureList; safecall;
    function Get_PhysicalFlowConstraintList: IPhysicalFlowConstraintList; safecall;
    function Get_IFRFeatureList: IIFRFeatureList; safecall;
    function Get_IrrigationAreaList: IIrrigationAreaList; safecall;
    function Get_PowerPlantList: IPowerPlantList; safecall;
    function Get_MasterControlFeatureList: IMasterControlFeatureList; safecall;
    function Get_SpecifiedInflowFeatureList: ISpecifiedInflowFeatureList; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_WaterDemandFeatureList: IWaterDemandFeatureList; safecall;
    function Get_WaterDemandConfiguration: IWaterDemandConfiguration; safecall;
    function Get_ChannelAreaList: IChannelAreaList; safecall;
    function Get_IrrigationBlockList: IIrrigationBlockList; safecall;
    function Get_WetlandList: IWetlandList; safecall;
    function Get_YMDemandCentreList: IYMDemandCentreList; safecall;
    function Get_StreamFlowReductionList: IStreamFlowReductionList; safecall;
    function Get_MineList: IMineList; safecall;
    function Get_CurtailmentAndDrought: ICurtailmentAndDrought; safecall;
    function Get_GroundWaterList: IGroundWaterList; safecall;
    function Get_MineSubCatchmentList: IMineSubCatchmentList; safecall;
    property MinimumFlowConstraintList: IMinimumFlowConstraintList read Get_MinimumFlowConstraintList;
    property LossFeatureList: ILossFeatureList read Get_LossFeatureList;
    property SpecifiedDemandFeatureList: ISpecifiedDemandFeatureList read Get_SpecifiedDemandFeatureList;
    property MinMaxFlowConstraintList: IMinMaxFlowConstraintList read Get_MinMaxFlowConstraintList;
    property PumpingFeatureList: IPumpingFeatureList read Get_PumpingFeatureList;
    property DiversionFeatureList: IDiversionFeatureList read Get_DiversionFeatureList;
    property PhysicalFlowConstraintList: IPhysicalFlowConstraintList read Get_PhysicalFlowConstraintList;
    property IFRFeatureList: IIFRFeatureList read Get_IFRFeatureList;
    property IrrigationAreaList: IIrrigationAreaList read Get_IrrigationAreaList;
    property PowerPlantList: IPowerPlantList read Get_PowerPlantList;
    property MasterControlFeatureList: IMasterControlFeatureList read Get_MasterControlFeatureList;
    property SpecifiedInflowFeatureList: ISpecifiedInflowFeatureList read Get_SpecifiedInflowFeatureList;
    property WaterDemandFeatureList: IWaterDemandFeatureList read Get_WaterDemandFeatureList;
    property WaterDemandConfiguration: IWaterDemandConfiguration read Get_WaterDemandConfiguration;
    property ChannelAreaList: IChannelAreaList read Get_ChannelAreaList;
    property IrrigationBlockList: IIrrigationBlockList read Get_IrrigationBlockList;
    property WetlandList: IWetlandList read Get_WetlandList;
    property YMDemandCentreList: IYMDemandCentreList read Get_YMDemandCentreList;
    property StreamFlowReductionList: IStreamFlowReductionList read Get_StreamFlowReductionList;
    property MineList: IMineList read Get_MineList;
    property CurtailmentAndDrought: ICurtailmentAndDrought read Get_CurtailmentAndDrought;
    property GroundWaterList: IGroundWaterList read Get_GroundWaterList;
    property MineSubCatchmentList: IMineSubCatchmentList read Get_MineSubCatchmentList;
  end;

// *********************************************************************//
// DispIntf:  INetworkFeaturesDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {66B4A7FB-983F-4691-9004-C5714B4C7D02}
// *********************************************************************//
  INetworkFeaturesDataDisp = dispinterface
    ['{66B4A7FB-983F-4691-9004-C5714B4C7D02}']
    property MinimumFlowConstraintList: IMinimumFlowConstraintList readonly dispid 101;
    property LossFeatureList: ILossFeatureList readonly dispid 102;
    property SpecifiedDemandFeatureList: ISpecifiedDemandFeatureList readonly dispid 103;
    property MinMaxFlowConstraintList: IMinMaxFlowConstraintList readonly dispid 104;
    property PumpingFeatureList: IPumpingFeatureList readonly dispid 105;
    property DiversionFeatureList: IDiversionFeatureList readonly dispid 106;
    property PhysicalFlowConstraintList: IPhysicalFlowConstraintList readonly dispid 107;
    property IFRFeatureList: IIFRFeatureList readonly dispid 108;
    property IrrigationAreaList: IIrrigationAreaList readonly dispid 109;
    property PowerPlantList: IPowerPlantList readonly dispid 110;
    property MasterControlFeatureList: IMasterControlFeatureList readonly dispid 111;
    property SpecifiedInflowFeatureList: ISpecifiedInflowFeatureList readonly dispid 112;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 113;
    property WaterDemandFeatureList: IWaterDemandFeatureList readonly dispid 114;
    property WaterDemandConfiguration: IWaterDemandConfiguration readonly dispid 115;
    property ChannelAreaList: IChannelAreaList readonly dispid 116;
    property IrrigationBlockList: IIrrigationBlockList readonly dispid 117;
    property WetlandList: IWetlandList readonly dispid 118;
    property YMDemandCentreList: IYMDemandCentreList readonly dispid 119;
    property StreamFlowReductionList: IStreamFlowReductionList readonly dispid 120;
    property MineList: IMineList readonly dispid 121;
    property CurtailmentAndDrought: ICurtailmentAndDrought readonly dispid 122;
    property GroundWaterList: IGroundWaterList readonly dispid 123;
    property MineSubCatchmentList: IMineSubCatchmentList readonly dispid 124;
  end;

// *********************************************************************//
// Interface: IYieldModelCapability
// Flags:     (320) Dual OleAutomation
// GUID:      {935785F3-EDB9-4B27-986A-42CF99AA4C04}
// *********************************************************************//
  IYieldModelCapability = interface(IUnknown)
    ['{935785F3-EDB9-4B27-986A-42CF99AA4C04}']
    function Get_MaximumNumberOfChannels: Integer; safecall;
    function Get_MinimumNumberOfChannels: Integer; safecall;
    function Get_MaximumChannelNumberofLossChannels: Integer; safecall;
    function Get_MinimumChannelNumberofLossChannels: Integer; safecall;
    function Get_MaximumChannelNumberofMinMaxChannel: Integer; safecall;
    function Get_MinimumChannelNumberofMinMaxChannel: Integer; safecall;
    function Get_MaximumChannelNumberofPumpingChannels: Integer; safecall;
    function Get_MinimumChannelNumberofPumpingChannels: Integer; safecall;
    function Get_MaximumChannelsCount: Integer; safecall;
    function Get_MinimumChannelsCount: Integer; safecall;
    function Get_MaximumConstraintsChannelNumber: Integer; safecall;
    function Get_MinimumConstraintsChannelNumber: Integer; safecall;
    function Get_MaximumControlStructCount: Integer; safecall;
    function Get_MinimumControlStructCount: Integer; safecall;
    function Get_MaximumControlStructureCount: Integer; safecall;
    function Get_MinimumControlStructureCount: Integer; safecall;
    function Get_MaximumDemandCount: Integer; safecall;
    function Get_MinimumDemandCount: Integer; safecall;
    function Get_MaximumDiversionChannelCount: Integer; safecall;
    function Get_MinimumDiversionChannelCount: Integer; safecall;
    function Get_MaximumDiversionChannelNumber: Integer; safecall;
    function Get_MinimumDiversionChannelNumber: Integer; safecall;
    function Get_MaximumDownStreamPowerChannelCount: Integer; safecall;
    function Get_MinimumDownStreamPowerChannelCount: Integer; safecall;
    function Get_MaximumGeneralCount: Integer; safecall;
    function Get_MinimumGeneralCount: Integer; safecall;
    function Get_MaximumIFRPointsCount: Integer; safecall;
    function Get_MinimumIFRPointsCount: Integer; safecall;
    function Get_MaximumInflowCount: Integer; safecall;
    function Get_MinimumInflowCount: Integer; safecall;
    function Get_MaximumIrrigationCount: Integer; safecall;
    function Get_MinimumIrrigationCount: Integer; safecall;
    function Get_MaximumLossChannelNumber: Integer; safecall;
    function Get_MinimumLossChannelNumber: Integer; safecall;
    function Get_MaximumLossCount: Integer; safecall;
    function Get_MinimumLossCount: Integer; safecall;
    function Get_MaximumMasterControlChannelNumber: Integer; safecall;
    function Get_MinimumMasterControlChannelNumber: Integer; safecall;
    function Get_MaximumMinFlowChannelNumber: Integer; safecall;
    function Get_MinimumMinFlowChannelNumber: Integer; safecall;
    function Get_MaximumMinFlowCount: Integer; safecall;
    function Get_MinimumMinFlowCount: Integer; safecall;
    function Get_MaximumMultiPurposeCount: Integer; safecall;
    function Get_MinimumMultiPurposeCount: Integer; safecall;
    function Get_MaximumPenaltyCount: Integer; safecall;
    function Get_MinimumPenaltyCount: Integer; safecall;
    function Get_MaximumPenaltyStructureCount: Integer; safecall;
    function Get_MinimumPenaltyStructureCount: Integer; safecall;
    function Get_MaximumPowerCount: Integer; safecall;
    function Get_MinimumPowerCount: Integer; safecall;
    function Get_MaximumPowerGenerationChannelNumber: Integer; safecall;
    function Get_MinimumPowerGenerationChannelNumber: Integer; safecall;
    function Get_MaximumPumpingCount: Integer; safecall;
    function Get_MinimumPumpingCount: Integer; safecall;
    function Get_MaximumReferenceNodeCount: Integer; safecall;
    function Get_MinimumReferenceNodeCount: Integer; safecall;
    function Get_MaximumReservoirCount: Integer; safecall;
    function Get_MinimumReservoirCount: Integer; safecall;
    function Get_MaximumSpillChannelNumber: Integer; safecall;
    function Get_MinimumSpillChannelNumber: Integer; safecall;
    function Get_MaximumStorageZoneCount: Integer; safecall;
    function Get_MinimumStorageZoneCount: Integer; safecall;
    function Get_MaximumTailWaterCount: Integer; safecall;
    function Get_MinimumTailWaterCount: Integer; safecall;
    property MaximumNumberOfChannels: Integer read Get_MaximumNumberOfChannels;
    property MinimumNumberOfChannels: Integer read Get_MinimumNumberOfChannels;
    property MaximumChannelNumberofLossChannels: Integer read Get_MaximumChannelNumberofLossChannels;
    property MinimumChannelNumberofLossChannels: Integer read Get_MinimumChannelNumberofLossChannels;
    property MaximumChannelNumberofMinMaxChannel: Integer read Get_MaximumChannelNumberofMinMaxChannel;
    property MinimumChannelNumberofMinMaxChannel: Integer read Get_MinimumChannelNumberofMinMaxChannel;
    property MaximumChannelNumberofPumpingChannels: Integer read Get_MaximumChannelNumberofPumpingChannels;
    property MinimumChannelNumberofPumpingChannels: Integer read Get_MinimumChannelNumberofPumpingChannels;
    property MaximumChannelsCount: Integer read Get_MaximumChannelsCount;
    property MinimumChannelsCount: Integer read Get_MinimumChannelsCount;
    property MaximumConstraintsChannelNumber: Integer read Get_MaximumConstraintsChannelNumber;
    property MinimumConstraintsChannelNumber: Integer read Get_MinimumConstraintsChannelNumber;
    property MaximumControlStructCount: Integer read Get_MaximumControlStructCount;
    property MinimumControlStructCount: Integer read Get_MinimumControlStructCount;
    property MaximumControlStructureCount: Integer read Get_MaximumControlStructureCount;
    property MinimumControlStructureCount: Integer read Get_MinimumControlStructureCount;
    property MaximumDemandCount: Integer read Get_MaximumDemandCount;
    property MinimumDemandCount: Integer read Get_MinimumDemandCount;
    property MaximumDiversionChannelCount: Integer read Get_MaximumDiversionChannelCount;
    property MinimumDiversionChannelCount: Integer read Get_MinimumDiversionChannelCount;
    property MaximumDiversionChannelNumber: Integer read Get_MaximumDiversionChannelNumber;
    property MinimumDiversionChannelNumber: Integer read Get_MinimumDiversionChannelNumber;
    property MaximumDownStreamPowerChannelCount: Integer read Get_MaximumDownStreamPowerChannelCount;
    property MinimumDownStreamPowerChannelCount: Integer read Get_MinimumDownStreamPowerChannelCount;
    property MaximumGeneralCount: Integer read Get_MaximumGeneralCount;
    property MinimumGeneralCount: Integer read Get_MinimumGeneralCount;
    property MaximumIFRPointsCount: Integer read Get_MaximumIFRPointsCount;
    property MinimumIFRPointsCount: Integer read Get_MinimumIFRPointsCount;
    property MaximumInflowCount: Integer read Get_MaximumInflowCount;
    property MinimumInflowCount: Integer read Get_MinimumInflowCount;
    property MaximumIrrigationCount: Integer read Get_MaximumIrrigationCount;
    property MinimumIrrigationCount: Integer read Get_MinimumIrrigationCount;
    property MaximumLossChannelNumber: Integer read Get_MaximumLossChannelNumber;
    property MinimumLossChannelNumber: Integer read Get_MinimumLossChannelNumber;
    property MaximumLossCount: Integer read Get_MaximumLossCount;
    property MinimumLossCount: Integer read Get_MinimumLossCount;
    property MaximumMasterControlChannelNumber: Integer read Get_MaximumMasterControlChannelNumber;
    property MinimumMasterControlChannelNumber: Integer read Get_MinimumMasterControlChannelNumber;
    property MaximumMinFlowChannelNumber: Integer read Get_MaximumMinFlowChannelNumber;
    property MinimumMinFlowChannelNumber: Integer read Get_MinimumMinFlowChannelNumber;
    property MaximumMinFlowCount: Integer read Get_MaximumMinFlowCount;
    property MinimumMinFlowCount: Integer read Get_MinimumMinFlowCount;
    property MaximumMultiPurposeCount: Integer read Get_MaximumMultiPurposeCount;
    property MinimumMultiPurposeCount: Integer read Get_MinimumMultiPurposeCount;
    property MaximumPenaltyCount: Integer read Get_MaximumPenaltyCount;
    property MinimumPenaltyCount: Integer read Get_MinimumPenaltyCount;
    property MaximumPenaltyStructureCount: Integer read Get_MaximumPenaltyStructureCount;
    property MinimumPenaltyStructureCount: Integer read Get_MinimumPenaltyStructureCount;
    property MaximumPowerCount: Integer read Get_MaximumPowerCount;
    property MinimumPowerCount: Integer read Get_MinimumPowerCount;
    property MaximumPowerGenerationChannelNumber: Integer read Get_MaximumPowerGenerationChannelNumber;
    property MinimumPowerGenerationChannelNumber: Integer read Get_MinimumPowerGenerationChannelNumber;
    property MaximumPumpingCount: Integer read Get_MaximumPumpingCount;
    property MinimumPumpingCount: Integer read Get_MinimumPumpingCount;
    property MaximumReferenceNodeCount: Integer read Get_MaximumReferenceNodeCount;
    property MinimumReferenceNodeCount: Integer read Get_MinimumReferenceNodeCount;
    property MaximumReservoirCount: Integer read Get_MaximumReservoirCount;
    property MinimumReservoirCount: Integer read Get_MinimumReservoirCount;
    property MaximumSpillChannelNumber: Integer read Get_MaximumSpillChannelNumber;
    property MinimumSpillChannelNumber: Integer read Get_MinimumSpillChannelNumber;
    property MaximumStorageZoneCount: Integer read Get_MaximumStorageZoneCount;
    property MinimumStorageZoneCount: Integer read Get_MinimumStorageZoneCount;
    property MaximumTailWaterCount: Integer read Get_MaximumTailWaterCount;
    property MinimumTailWaterCount: Integer read Get_MinimumTailWaterCount;
  end;

// *********************************************************************//
// DispIntf:  IYieldModelCapabilityDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {935785F3-EDB9-4B27-986A-42CF99AA4C04}
// *********************************************************************//
  IYieldModelCapabilityDisp = dispinterface
    ['{935785F3-EDB9-4B27-986A-42CF99AA4C04}']
    property MaximumNumberOfChannels: Integer readonly dispid 201;
    property MinimumNumberOfChannels: Integer readonly dispid 202;
    property MaximumChannelNumberofLossChannels: Integer readonly dispid 203;
    property MinimumChannelNumberofLossChannels: Integer readonly dispid 204;
    property MaximumChannelNumberofMinMaxChannel: Integer readonly dispid 205;
    property MinimumChannelNumberofMinMaxChannel: Integer readonly dispid 206;
    property MaximumChannelNumberofPumpingChannels: Integer readonly dispid 207;
    property MinimumChannelNumberofPumpingChannels: Integer readonly dispid 208;
    property MaximumChannelsCount: Integer readonly dispid 209;
    property MinimumChannelsCount: Integer readonly dispid 210;
    property MaximumConstraintsChannelNumber: Integer readonly dispid 211;
    property MinimumConstraintsChannelNumber: Integer readonly dispid 212;
    property MaximumControlStructCount: Integer readonly dispid 213;
    property MinimumControlStructCount: Integer readonly dispid 214;
    property MaximumControlStructureCount: Integer readonly dispid 215;
    property MinimumControlStructureCount: Integer readonly dispid 216;
    property MaximumDemandCount: Integer readonly dispid 217;
    property MinimumDemandCount: Integer readonly dispid 218;
    property MaximumDiversionChannelCount: Integer readonly dispid 219;
    property MinimumDiversionChannelCount: Integer readonly dispid 220;
    property MaximumDiversionChannelNumber: Integer readonly dispid 221;
    property MinimumDiversionChannelNumber: Integer readonly dispid 222;
    property MaximumDownStreamPowerChannelCount: Integer readonly dispid 223;
    property MinimumDownStreamPowerChannelCount: Integer readonly dispid 224;
    property MaximumGeneralCount: Integer readonly dispid 225;
    property MinimumGeneralCount: Integer readonly dispid 226;
    property MaximumIFRPointsCount: Integer readonly dispid 227;
    property MinimumIFRPointsCount: Integer readonly dispid 228;
    property MaximumInflowCount: Integer readonly dispid 229;
    property MinimumInflowCount: Integer readonly dispid 230;
    property MaximumIrrigationCount: Integer readonly dispid 231;
    property MinimumIrrigationCount: Integer readonly dispid 232;
    property MaximumLossChannelNumber: Integer readonly dispid 233;
    property MinimumLossChannelNumber: Integer readonly dispid 234;
    property MaximumLossCount: Integer readonly dispid 235;
    property MinimumLossCount: Integer readonly dispid 236;
    property MaximumMasterControlChannelNumber: Integer readonly dispid 237;
    property MinimumMasterControlChannelNumber: Integer readonly dispid 238;
    property MaximumMinFlowChannelNumber: Integer readonly dispid 239;
    property MinimumMinFlowChannelNumber: Integer readonly dispid 240;
    property MaximumMinFlowCount: Integer readonly dispid 241;
    property MinimumMinFlowCount: Integer readonly dispid 242;
    property MaximumMultiPurposeCount: Integer readonly dispid 243;
    property MinimumMultiPurposeCount: Integer readonly dispid 244;
    property MaximumPenaltyCount: Integer readonly dispid 245;
    property MinimumPenaltyCount: Integer readonly dispid 246;
    property MaximumPenaltyStructureCount: Integer readonly dispid 247;
    property MinimumPenaltyStructureCount: Integer readonly dispid 248;
    property MaximumPowerCount: Integer readonly dispid 249;
    property MinimumPowerCount: Integer readonly dispid 250;
    property MaximumPowerGenerationChannelNumber: Integer readonly dispid 251;
    property MinimumPowerGenerationChannelNumber: Integer readonly dispid 252;
    property MaximumPumpingCount: Integer readonly dispid 253;
    property MinimumPumpingCount: Integer readonly dispid 254;
    property MaximumReferenceNodeCount: Integer readonly dispid 255;
    property MinimumReferenceNodeCount: Integer readonly dispid 256;
    property MaximumReservoirCount: Integer readonly dispid 257;
    property MinimumReservoirCount: Integer readonly dispid 258;
    property MaximumSpillChannelNumber: Integer readonly dispid 259;
    property MinimumSpillChannelNumber: Integer readonly dispid 260;
    property MaximumStorageZoneCount: Integer readonly dispid 261;
    property MinimumStorageZoneCount: Integer readonly dispid 262;
    property MaximumTailWaterCount: Integer readonly dispid 263;
    property MinimumTailWaterCount: Integer readonly dispid 264;
  end;

// *********************************************************************//
// Interface: IMinimumFlowConstraintList
// Flags:     (320) Dual OleAutomation
// GUID:      {936AEC2C-69C6-411D-B593-46AE0CCFDD1B}
// *********************************************************************//
  IMinimumFlowConstraintList = interface(IUnknown)
    ['{936AEC2C-69C6-411D-B593-46AE0CCFDD1B}']
    function Get_MinimumFlowConstraintCount: Integer; safecall;
    function Get_MinimumFlowConstraintByIndex(AIndex: Integer): IMinimumFlowConstraint; safecall;
    function Get_MinimumFlowConstraintByID(AFeatureID: Integer): IMinimumFlowConstraint; safecall;
    function CreateMinimumFlowConstraint: IMinimumFlowConstraint; safecall;
    function RemoveMinimumFlowConstraintWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CopyMinimumFlowConstraint(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IMinimumFlowConstraint; safecall;
    property MinimumFlowConstraintCount: Integer read Get_MinimumFlowConstraintCount;
    property MinimumFlowConstraintByIndex[AIndex: Integer]: IMinimumFlowConstraint read Get_MinimumFlowConstraintByIndex;
    property MinimumFlowConstraintByID[AFeatureID: Integer]: IMinimumFlowConstraint read Get_MinimumFlowConstraintByID;
  end;

// *********************************************************************//
// DispIntf:  IMinimumFlowConstraintListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {936AEC2C-69C6-411D-B593-46AE0CCFDD1B}
// *********************************************************************//
  IMinimumFlowConstraintListDisp = dispinterface
    ['{936AEC2C-69C6-411D-B593-46AE0CCFDD1B}']
    property MinimumFlowConstraintCount: Integer readonly dispid 101;
    property MinimumFlowConstraintByIndex[AIndex: Integer]: IMinimumFlowConstraint readonly dispid 102;
    property MinimumFlowConstraintByID[AFeatureID: Integer]: IMinimumFlowConstraint readonly dispid 103;
    function CreateMinimumFlowConstraint: IMinimumFlowConstraint; dispid 104;
    function RemoveMinimumFlowConstraintWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function CopyMinimumFlowConstraint(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IMinimumFlowConstraint; dispid 107;
  end;

// *********************************************************************//
// Interface: ILossFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {94001A90-EEF1-44ED-8BDB-A041100A736C}
// *********************************************************************//
  ILossFeatureList = interface(IUnknown)
    ['{94001A90-EEF1-44ED-8BDB-A041100A736C}']
    function Get_LossFeatureCount: Integer; safecall;
    function Get_LossFeatureByIndex(AIndex: Integer): ILossFeature; safecall;
    function Get_LossFeatureByID(AFeatureID: Integer): ILossFeature; safecall;
    function CreateLossFeature: ILossFeature; safecall;
    function RemoveLossFeatureWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CopyLossFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): ILossFeature; safecall;
    property LossFeatureCount: Integer read Get_LossFeatureCount;
    property LossFeatureByIndex[AIndex: Integer]: ILossFeature read Get_LossFeatureByIndex;
    property LossFeatureByID[AFeatureID: Integer]: ILossFeature read Get_LossFeatureByID;
  end;

// *********************************************************************//
// DispIntf:  ILossFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {94001A90-EEF1-44ED-8BDB-A041100A736C}
// *********************************************************************//
  ILossFeatureListDisp = dispinterface
    ['{94001A90-EEF1-44ED-8BDB-A041100A736C}']
    property LossFeatureCount: Integer readonly dispid 101;
    property LossFeatureByIndex[AIndex: Integer]: ILossFeature readonly dispid 102;
    property LossFeatureByID[AFeatureID: Integer]: ILossFeature readonly dispid 103;
    function CreateLossFeature: ILossFeature; dispid 104;
    function RemoveLossFeatureWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function CopyLossFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): ILossFeature; dispid 107;
  end;

// *********************************************************************//
// Interface: ISpecifiedDemandFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {9652E861-EFF9-4A38-B996-7EB6F0D4D68E}
// *********************************************************************//
  ISpecifiedDemandFeatureList = interface(IUnknown)
    ['{9652E861-EFF9-4A38-B996-7EB6F0D4D68E}']
    function Get_SpecifiedDemandFeatureCount: Integer; safecall;
    function Get_SpecifiedDemandFeatureByIndex(AIndex: Integer): ISpecifiedDemandFeature; safecall;
    function Get_SpecifiedDemandFeatureByID(AFeatureID: Integer): ISpecifiedDemandFeature; safecall;
    function RemoveSpecifiedDemandFeatureWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CopySpecifiedDemandFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): ISpecifiedDemandFeature; safecall;
    property SpecifiedDemandFeatureCount: Integer read Get_SpecifiedDemandFeatureCount;
    property SpecifiedDemandFeatureByIndex[AIndex: Integer]: ISpecifiedDemandFeature read Get_SpecifiedDemandFeatureByIndex;
    property SpecifiedDemandFeatureByID[AFeatureID: Integer]: ISpecifiedDemandFeature read Get_SpecifiedDemandFeatureByID;
  end;

// *********************************************************************//
// DispIntf:  ISpecifiedDemandFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {9652E861-EFF9-4A38-B996-7EB6F0D4D68E}
// *********************************************************************//
  ISpecifiedDemandFeatureListDisp = dispinterface
    ['{9652E861-EFF9-4A38-B996-7EB6F0D4D68E}']
    property SpecifiedDemandFeatureCount: Integer readonly dispid 101;
    property SpecifiedDemandFeatureByIndex[AIndex: Integer]: ISpecifiedDemandFeature readonly dispid 102;
    property SpecifiedDemandFeatureByID[AFeatureID: Integer]: ISpecifiedDemandFeature readonly dispid 103;
    function RemoveSpecifiedDemandFeatureWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function CopySpecifiedDemandFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): ISpecifiedDemandFeature; dispid 104;
  end;

// *********************************************************************//
// Interface: IMinMaxFlowConstraintList
// Flags:     (320) Dual OleAutomation
// GUID:      {A6A92273-082E-41FD-8B37-FB0199FB7CAB}
// *********************************************************************//
  IMinMaxFlowConstraintList = interface(IUnknown)
    ['{A6A92273-082E-41FD-8B37-FB0199FB7CAB}']
    function Get_MinMaxFlowConstraintCount: Integer; safecall;
    function Get_MinMaxFlowConstraintByIndex(AIndex: Integer): IMinMaxFlowConstraint; safecall;
    function Get_MinMaxFlowConstraintByID(AFeatureID: Integer): IMinMaxFlowConstraint; safecall;
    function CreateMinMaxFlowConstraint: IMinMaxFlowConstraint; safecall;
    function RemoveMinMaxFlowConstraintWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CopyMinMaxFlowConstraint(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IMinMaxFlowConstraint; safecall;
    property MinMaxFlowConstraintCount: Integer read Get_MinMaxFlowConstraintCount;
    property MinMaxFlowConstraintByIndex[AIndex: Integer]: IMinMaxFlowConstraint read Get_MinMaxFlowConstraintByIndex;
    property MinMaxFlowConstraintByID[AFeatureID: Integer]: IMinMaxFlowConstraint read Get_MinMaxFlowConstraintByID;
  end;

// *********************************************************************//
// DispIntf:  IMinMaxFlowConstraintListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {A6A92273-082E-41FD-8B37-FB0199FB7CAB}
// *********************************************************************//
  IMinMaxFlowConstraintListDisp = dispinterface
    ['{A6A92273-082E-41FD-8B37-FB0199FB7CAB}']
    property MinMaxFlowConstraintCount: Integer readonly dispid 101;
    property MinMaxFlowConstraintByIndex[AIndex: Integer]: IMinMaxFlowConstraint readonly dispid 102;
    property MinMaxFlowConstraintByID[AFeatureID: Integer]: IMinMaxFlowConstraint readonly dispid 104;
    function CreateMinMaxFlowConstraint: IMinMaxFlowConstraint; dispid 103;
    function RemoveMinMaxFlowConstraintWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function CopyMinMaxFlowConstraint(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IMinMaxFlowConstraint; dispid 107;
  end;

// *********************************************************************//
// Interface: IPumpingFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {E370AB03-D77F-4498-8A7D-6EA119D3AA24}
// *********************************************************************//
  IPumpingFeatureList = interface(IUnknown)
    ['{E370AB03-D77F-4498-8A7D-6EA119D3AA24}']
    function Get_PumpingFeatureCount: Integer; safecall;
    function Get_PumpingFeatureByIndex(AIndex: Integer): IPumpingFeature; safecall;
    function Get_PumpingFeatureByID(AFeatureID: Integer): IPumpingFeature; safecall;
    function CreatePumpingFeature: IPumpingFeature; safecall;
    function RemovePumpingFeatureWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CopyPumpingFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IPumpingFeature; safecall;
    property PumpingFeatureCount: Integer read Get_PumpingFeatureCount;
    property PumpingFeatureByIndex[AIndex: Integer]: IPumpingFeature read Get_PumpingFeatureByIndex;
    property PumpingFeatureByID[AFeatureID: Integer]: IPumpingFeature read Get_PumpingFeatureByID;
  end;

// *********************************************************************//
// DispIntf:  IPumpingFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {E370AB03-D77F-4498-8A7D-6EA119D3AA24}
// *********************************************************************//
  IPumpingFeatureListDisp = dispinterface
    ['{E370AB03-D77F-4498-8A7D-6EA119D3AA24}']
    property PumpingFeatureCount: Integer readonly dispid 101;
    property PumpingFeatureByIndex[AIndex: Integer]: IPumpingFeature readonly dispid 102;
    property PumpingFeatureByID[AFeatureID: Integer]: IPumpingFeature readonly dispid 103;
    function CreatePumpingFeature: IPumpingFeature; dispid 104;
    function RemovePumpingFeatureWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function CopyPumpingFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IPumpingFeature; dispid 107;
  end;

// *********************************************************************//
// Interface: IDiversionFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {700FF6AD-31BB-4181-B5E1-D4FDD15F183B}
// *********************************************************************//
  IDiversionFeatureList = interface(IUnknown)
    ['{700FF6AD-31BB-4181-B5E1-D4FDD15F183B}']
    function Get_DiversionFeatureCount: Integer; safecall;
    function Get_DiversionFeatureByIndex(AIndex: Integer): IDiversionFeature; safecall;
    function Get_DiversionFeatureByID(AFeatureID: Integer): IDiversionFeature; safecall;
    function CreateDiversionFeature: IDiversionFeature; safecall;
    function RemoveDiversionFeatureWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_DiversionGaugeList: WideString; safecall;
    function CopyDiversionFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IDiversionFeature; safecall;
    function GetStationIDByName(const AValue: WideString): Integer; safecall;
    property DiversionFeatureCount: Integer read Get_DiversionFeatureCount;
    property DiversionFeatureByIndex[AIndex: Integer]: IDiversionFeature read Get_DiversionFeatureByIndex;
    property DiversionFeatureByID[AFeatureID: Integer]: IDiversionFeature read Get_DiversionFeatureByID;
    property DiversionGaugeList: WideString read Get_DiversionGaugeList;
  end;

// *********************************************************************//
// DispIntf:  IDiversionFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {700FF6AD-31BB-4181-B5E1-D4FDD15F183B}
// *********************************************************************//
  IDiversionFeatureListDisp = dispinterface
    ['{700FF6AD-31BB-4181-B5E1-D4FDD15F183B}']
    property DiversionFeatureCount: Integer readonly dispid 101;
    property DiversionFeatureByIndex[AIndex: Integer]: IDiversionFeature readonly dispid 102;
    property DiversionFeatureByID[AFeatureID: Integer]: IDiversionFeature readonly dispid 103;
    function CreateDiversionFeature: IDiversionFeature; dispid 104;
    function RemoveDiversionFeatureWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    property DiversionGaugeList: WideString readonly dispid 107;
    function CopyDiversionFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IDiversionFeature; dispid 108;
    function GetStationIDByName(const AValue: WideString): Integer; dispid 109;
  end;

// *********************************************************************//
// Interface: IPhysicalFlowConstraintList
// Flags:     (320) Dual OleAutomation
// GUID:      {EA845874-F75A-48CF-9E71-966E4626F04D}
// *********************************************************************//
  IPhysicalFlowConstraintList = interface(IUnknown)
    ['{EA845874-F75A-48CF-9E71-966E4626F04D}']
    function Get_PhysicalFlowConstraintCount: Integer; safecall;
    function Get_PhysicalFlowConstraintByIndex(AIndex: Integer): IPhysicalFlowConstraint; safecall;
    function Get_PhysicalFlowConstraintByID(AFeatureID: Integer): IPhysicalFlowConstraint; safecall;
    function CreatePhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint; safecall;
    function RemovePhysicalFlowConstraintWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CopyPhysicalFlowConstraint(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IPhysicalFlowConstraint; safecall;
    property PhysicalFlowConstraintCount: Integer read Get_PhysicalFlowConstraintCount;
    property PhysicalFlowConstraintByIndex[AIndex: Integer]: IPhysicalFlowConstraint read Get_PhysicalFlowConstraintByIndex;
    property PhysicalFlowConstraintByID[AFeatureID: Integer]: IPhysicalFlowConstraint read Get_PhysicalFlowConstraintByID;
  end;

// *********************************************************************//
// DispIntf:  IPhysicalFlowConstraintListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {EA845874-F75A-48CF-9E71-966E4626F04D}
// *********************************************************************//
  IPhysicalFlowConstraintListDisp = dispinterface
    ['{EA845874-F75A-48CF-9E71-966E4626F04D}']
    property PhysicalFlowConstraintCount: Integer readonly dispid 101;
    property PhysicalFlowConstraintByIndex[AIndex: Integer]: IPhysicalFlowConstraint readonly dispid 102;
    property PhysicalFlowConstraintByID[AFeatureID: Integer]: IPhysicalFlowConstraint readonly dispid 103;
    function CreatePhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint; dispid 104;
    function RemovePhysicalFlowConstraintWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function CopyPhysicalFlowConstraint(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IPhysicalFlowConstraint; dispid 107;
  end;

// *********************************************************************//
// Interface: IIFRFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {8A63409B-C04D-40FA-9067-3F88D6228E60}
// *********************************************************************//
  IIFRFeatureList = interface(IUnknown)
    ['{8A63409B-C04D-40FA-9067-3F88D6228E60}']
    function Get_MonthlyIFRFeatureCount: Integer; safecall;
    function Get_MonthlyIFRFeatureByIndex(AIndex: Integer): IIFRFeature; safecall;
    function Get_MonthlyIFRFeatureByID(AFeatureID: Integer): IIFRFeature; safecall;
    function CreateIFRFeature(AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature; safecall;
    function RemoveIFRFeatureWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_InflowOption: Integer; safecall;
    procedure Set_InflowOption(Value: Integer); safecall;
    function Get_AnnualIFRFeatureCount: Integer; safecall;
    function Get_AnnualIFRFeatureByIndex(AIndex: Integer): IIFRFeature; safecall;
    function Get_AnnualIFRFeatureByID(AFeatureID: Integer): IIFRFeature; safecall;
    function CopyIFRFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer;
                            AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature; safecall;
    property MonthlyIFRFeatureCount: Integer read Get_MonthlyIFRFeatureCount;
    property MonthlyIFRFeatureByIndex[AIndex: Integer]: IIFRFeature read Get_MonthlyIFRFeatureByIndex;
    property MonthlyIFRFeatureByID[AFeatureID: Integer]: IIFRFeature read Get_MonthlyIFRFeatureByID;
    property InflowOption: Integer read Get_InflowOption write Set_InflowOption;
    property AnnualIFRFeatureCount: Integer read Get_AnnualIFRFeatureCount;
    property AnnualIFRFeatureByIndex[AIndex: Integer]: IIFRFeature read Get_AnnualIFRFeatureByIndex;
    property AnnualIFRFeatureByID[AFeatureID: Integer]: IIFRFeature read Get_AnnualIFRFeatureByID;
  end;

// *********************************************************************//
// DispIntf:  IIFRFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {8A63409B-C04D-40FA-9067-3F88D6228E60}
// *********************************************************************//
  IIFRFeatureListDisp = dispinterface
    ['{8A63409B-C04D-40FA-9067-3F88D6228E60}']
    property MonthlyIFRFeatureCount: Integer readonly dispid 101;
    property MonthlyIFRFeatureByIndex[AIndex: Integer]: IIFRFeature readonly dispid 102;
    property MonthlyIFRFeatureByID[AFeatureID: Integer]: IIFRFeature readonly dispid 103;
    function CreateIFRFeature(AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature; dispid 104;
    function RemoveIFRFeatureWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    property InflowOption: Integer dispid 107;
    property AnnualIFRFeatureCount: Integer readonly dispid 108;
    property AnnualIFRFeatureByIndex[AIndex: Integer]: IIFRFeature readonly dispid 109;
    property AnnualIFRFeatureByID[AFeatureID: Integer]: IIFRFeature readonly dispid 110;
    function CopyIFRFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer;
                            AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature; dispid 111;
  end;

// *********************************************************************//
// Interface: IIrrigationAreaList
// Flags:     (320) Dual OleAutomation
// GUID:      {30DC71C6-A460-401E-8BB8-1508D2E1B95B}
// *********************************************************************//
  IIrrigationAreaList = interface(IUnknown)
    ['{30DC71C6-A460-401E-8BB8-1508D2E1B95B}']
    function Get_IrrigationAreaCount: Integer; safecall;
    function Get_IrrigationAreaByIndex(AIndex: Integer): IIrrigationArea; safecall;
    function Get_IrrigationAreaByID(AFeatureID: Integer): IIrrigationArea; safecall;
    function CreateIrrigationArea: IIrrigationArea; safecall;
    function RemoveIrrigationAreaWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_IrrigationAreaByNodeNumber(ANodeNumber: Integer): IIrrigationArea; safecall;
    function CopyCreate(AFeatureID: Integer): IIrrigationArea; safecall;
    property IrrigationAreaCount: Integer read Get_IrrigationAreaCount;
    property IrrigationAreaByIndex[AIndex: Integer]: IIrrigationArea read Get_IrrigationAreaByIndex;
    property IrrigationAreaByID[AFeatureID: Integer]: IIrrigationArea read Get_IrrigationAreaByID;
    property IrrigationAreaByNodeNumber[ANodeNumber: Integer]: IIrrigationArea read Get_IrrigationAreaByNodeNumber;
  end;

// *********************************************************************//
// DispIntf:  IIrrigationAreaListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {30DC71C6-A460-401E-8BB8-1508D2E1B95B}
// *********************************************************************//
  IIrrigationAreaListDisp = dispinterface
    ['{30DC71C6-A460-401E-8BB8-1508D2E1B95B}']
    property IrrigationAreaCount: Integer readonly dispid 101;
    property IrrigationAreaByIndex[AIndex: Integer]: IIrrigationArea readonly dispid 102;
    property IrrigationAreaByID[AFeatureID: Integer]: IIrrigationArea readonly dispid 103;
    function CreateIrrigationArea: IIrrigationArea; dispid 104;
    function RemoveIrrigationAreaWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    property IrrigationAreaByNodeNumber[ANodeNumber: Integer]: IIrrigationArea readonly dispid 107;
    function CopyCreate(AFeatureID: Integer): IIrrigationArea; dispid 108;
  end;

// *********************************************************************//
// Interface: IPowerPlantList
// Flags:     (320) Dual OleAutomation
// GUID:      {6E225ECE-89B4-433C-9267-761B32F7C055}
// *********************************************************************//
  IPowerPlantList = interface(IUnknown)
    ['{6E225ECE-89B4-433C-9267-761B32F7C055}']
    function Get_PowerPlantCount: Integer; safecall;
    function Get_PowerPlantByIndex(AIndex: Integer): IPowerPlant; safecall;
    function Get_PowerPlantByID(AFeatureID: Integer): IPowerPlant; safecall;
    function CreatePowerPlant: IPowerPlant; safecall;
    function RemovePowerPlantWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CopyPowerPlant(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IPowerPlant; safecall;
    property PowerPlantCount: Integer read Get_PowerPlantCount;
    property PowerPlantByIndex[AIndex: Integer]: IPowerPlant read Get_PowerPlantByIndex;
    property PowerPlantByID[AFeatureID: Integer]: IPowerPlant read Get_PowerPlantByID;
  end;

// *********************************************************************//
// DispIntf:  IPowerPlantListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6E225ECE-89B4-433C-9267-761B32F7C055}
// *********************************************************************//
  IPowerPlantListDisp = dispinterface
    ['{6E225ECE-89B4-433C-9267-761B32F7C055}']
    property PowerPlantCount: Integer readonly dispid 101;
    property PowerPlantByIndex[AIndex: Integer]: IPowerPlant readonly dispid 102;
    property PowerPlantByID[AFeatureID: Integer]: IPowerPlant readonly dispid 103;
    function CreatePowerPlant: IPowerPlant; dispid 104;
    function RemovePowerPlantWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function CopyPowerPlant(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IPowerPlant; dispid 107;
  end;

// *********************************************************************//
// Interface: IMasterControlFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {79D59912-D11C-483E-A095-428596DEB905}
// *********************************************************************//
  IMasterControlFeatureList = interface(IUnknown)
    ['{79D59912-D11C-483E-A095-428596DEB905}']
    function Get_MasterControlFeatureCount: Integer; safecall;
    function Get_MasterControlFeatureByIndex(AIndex: Integer): IMasterControlFeature; safecall;
    function Get_MasterControlFeatureByID(AFeatureID: Integer): IMasterControlFeature; safecall;
    function Get_WaterControlFeature: IMasterControlFeature; safecall;
    function Get_PowerControlFeature: IMasterControlFeature; safecall;
    function CreateMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature; safecall;
    function RemoveMasterControlFeatureWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_DemandCentreByID(ADemandCentreID: Integer): IMasterControlFeature; safecall;
    function Get_ChannelByChannelNumber: Integer; safecall;
    function CopyMasterControlFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IMasterControlFeature; safecall;
    property MasterControlFeatureCount: Integer read Get_MasterControlFeatureCount;
    property MasterControlFeatureByIndex[AIndex: Integer]: IMasterControlFeature read Get_MasterControlFeatureByIndex;
    property MasterControlFeatureByID[AFeatureID: Integer]: IMasterControlFeature read Get_MasterControlFeatureByID;
    property WaterControlFeature: IMasterControlFeature read Get_WaterControlFeature;
    property PowerControlFeature: IMasterControlFeature read Get_PowerControlFeature;
    property DemandCentreByID[ADemandCentreID: Integer]: IMasterControlFeature read Get_DemandCentreByID;
    property ChannelByChannelNumber: Integer read Get_ChannelByChannelNumber;
  end;

// *********************************************************************//
// DispIntf:  IMasterControlFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {79D59912-D11C-483E-A095-428596DEB905}
// *********************************************************************//
  IMasterControlFeatureListDisp = dispinterface
    ['{79D59912-D11C-483E-A095-428596DEB905}']
    property MasterControlFeatureCount: Integer readonly dispid 101;
    property MasterControlFeatureByIndex[AIndex: Integer]: IMasterControlFeature readonly dispid 102;
    property MasterControlFeatureByID[AFeatureID: Integer]: IMasterControlFeature readonly dispid 103;
    property WaterControlFeature: IMasterControlFeature readonly dispid 104;
    property PowerControlFeature: IMasterControlFeature readonly dispid 105;
    function CreateMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature; dispid 106;
    function RemoveMasterControlFeatureWithID(AFeatureID: Integer): WordBool; dispid 107;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
    property DemandCentreByID[ADemandCentreID: Integer]: IMasterControlFeature readonly dispid 109;
    property ChannelByChannelNumber: Integer readonly dispid 110;
    function CopyMasterControlFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IMasterControlFeature; dispid 111;
  end;

// *********************************************************************//
// Interface: ISpecifiedInflowFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {6C8BE668-1D55-4E79-9D1F-FF8B752CC811}
// *********************************************************************//
  ISpecifiedInflowFeatureList = interface(IUnknown)
    ['{6C8BE668-1D55-4E79-9D1F-FF8B752CC811}']
    function Get_SpecifiedInflowFeatureCount: Integer; safecall;
    function Get_SpecifiedInflowFeatureByIndex(AIndex: Integer): ISpecifiedInflowFeature; safecall;
    function Get_SpecifiedInflowFeatureByID(AFeatureID: Integer): ISpecifiedInflowFeature; safecall;
    function CreateSpecifiedInflowFeature: ISpecifiedInflowFeature; safecall;
    function RemoveSpecifiedInflowFeatureWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CopySpecifiedInflowFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): ISpecifiedInflowFeature; safecall;
    property SpecifiedInflowFeatureCount: Integer read Get_SpecifiedInflowFeatureCount;
    property SpecifiedInflowFeatureByIndex[AIndex: Integer]: ISpecifiedInflowFeature read Get_SpecifiedInflowFeatureByIndex;
    property SpecifiedInflowFeatureByID[AFeatureID: Integer]: ISpecifiedInflowFeature read Get_SpecifiedInflowFeatureByID;
  end;

// *********************************************************************//
// DispIntf:  ISpecifiedInflowFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6C8BE668-1D55-4E79-9D1F-FF8B752CC811}
// *********************************************************************//
  ISpecifiedInflowFeatureListDisp = dispinterface
    ['{6C8BE668-1D55-4E79-9D1F-FF8B752CC811}']
    property SpecifiedInflowFeatureCount: Integer readonly dispid 101;
    property SpecifiedInflowFeatureByIndex[AIndex: Integer]: ISpecifiedInflowFeature readonly dispid 102;
    property SpecifiedInflowFeatureByID[AFeatureID: Integer]: ISpecifiedInflowFeature readonly dispid 103;
    function CreateSpecifiedInflowFeature: ISpecifiedInflowFeature; dispid 104;
    function RemoveSpecifiedInflowFeatureWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function CopySpecifiedInflowFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): ISpecifiedInflowFeature; dispid 107;
  end;

// *********************************************************************//
// Interface: ISpecifiedInflowFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {97404534-4FBF-402F-9BF5-6724EED88E77}
// *********************************************************************//
  ISpecifiedInflowFeature = interface(IUnknown)
    ['{97404534-4FBF-402F-9BF5-6724EED88E77}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_InflowFileName: WideString; safecall;
    procedure Set_InflowFileName(const AFileName: WideString); safecall;
    function Get_InflowNodeInflowFilesCommaText: WideString; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property InflowFileName: WideString read Get_InflowFileName write Set_InflowFileName;
    property InflowNodeInflowFilesCommaText: WideString read Get_InflowNodeInflowFilesCommaText;
  end;

// *********************************************************************//
// DispIntf:  ISpecifiedInflowFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {97404534-4FBF-402F-9BF5-6724EED88E77}
// *********************************************************************//
  ISpecifiedInflowFeatureDisp = dispinterface
    ['{97404534-4FBF-402F-9BF5-6724EED88E77}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 103;
    property Channel: IGeneralFlowChannel dispid 104;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 105;
    property InflowFileName: WideString dispid 106;
    property InflowNodeInflowFilesCommaText: WideString readonly dispid 107;
  end;

// *********************************************************************//
// Interface: IChannelPenalty
// Flags:     (320) Dual OleAutomation
// GUID:      {9D90233B-65E2-4BA0-AE93-2E428FCCAE9E}
// *********************************************************************//
  IChannelPenalty = interface(IUnknown)
    ['{9D90233B-65E2-4BA0-AE93-2E428FCCAE9E}']
    function Get_ChannelPenaltyID: Integer; safecall;
    function Get_ChannelPenaltyName: WideString; safecall;
    procedure Set_ChannelPenaltyName(const Value: WideString); safecall;
    function Get_ChannelPenaltyArcCount: Integer; safecall;
    function Get_ChannelPenaltyValueByIndex(Index: Integer): Double; safecall;
    procedure Set_ChannelPenaltyValueByIndex(Index: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property ChannelPenaltyID: Integer read Get_ChannelPenaltyID;
    property ChannelPenaltyName: WideString read Get_ChannelPenaltyName write Set_ChannelPenaltyName;
    property ChannelPenaltyArcCount: Integer read Get_ChannelPenaltyArcCount;
    property ChannelPenaltyValueByIndex[Index: Integer]: Double read Get_ChannelPenaltyValueByIndex write Set_ChannelPenaltyValueByIndex;
  end;

// *********************************************************************//
// DispIntf:  IChannelPenaltyDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {9D90233B-65E2-4BA0-AE93-2E428FCCAE9E}
// *********************************************************************//
  IChannelPenaltyDisp = dispinterface
    ['{9D90233B-65E2-4BA0-AE93-2E428FCCAE9E}']
    property ChannelPenaltyID: Integer readonly dispid 101;
    property ChannelPenaltyName: WideString dispid 102;
    property ChannelPenaltyArcCount: Integer readonly dispid 103;
    property ChannelPenaltyValueByIndex[Index: Integer]: Double dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 106;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 107;
  end;

// *********************************************************************//
// Interface: IChannelPenaltyList
// Flags:     (320) Dual OleAutomation
// GUID:      {1FCC104E-A317-45AB-BF25-96ABB40136A6}
// *********************************************************************//
  IChannelPenaltyList = interface(IUnknown)
    ['{1FCC104E-A317-45AB-BF25-96ABB40136A6}']
    function Get_ChannelPenaltyCount: Integer; safecall;
    function Get_ChannelPenaltyByIndex(Index: Integer): IChannelPenalty; safecall;
    function Get_ChannelPenaltyByIdentifier(Identifier: Integer): IChannelPenalty; safecall;
    function CreateChannelPenalty: IChannelPenalty; safecall;
    function RemoveChannelPenaltyWithID(PenaltyID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_InflowPenaltyNo: Integer; safecall;
    procedure Set_InflowPenaltyNo(Value: Integer); safecall;
    property ChannelPenaltyCount: Integer read Get_ChannelPenaltyCount;
    property ChannelPenaltyByIndex[Index: Integer]: IChannelPenalty read Get_ChannelPenaltyByIndex;
    property ChannelPenaltyByIdentifier[Identifier: Integer]: IChannelPenalty read Get_ChannelPenaltyByIdentifier;
    property InflowPenaltyNo: Integer read Get_InflowPenaltyNo write Set_InflowPenaltyNo;
  end;

// *********************************************************************//
// DispIntf:  IChannelPenaltyListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {1FCC104E-A317-45AB-BF25-96ABB40136A6}
// *********************************************************************//
  IChannelPenaltyListDisp = dispinterface
    ['{1FCC104E-A317-45AB-BF25-96ABB40136A6}']
    property ChannelPenaltyCount: Integer readonly dispid 101;
    property ChannelPenaltyByIndex[Index: Integer]: IChannelPenalty readonly dispid 102;
    property ChannelPenaltyByIdentifier[Identifier: Integer]: IChannelPenalty readonly dispid 103;
    function CreateChannelPenalty: IChannelPenalty; dispid 104;
    function RemoveChannelPenaltyWithID(PenaltyID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    property InflowPenaltyNo: Integer dispid 107;
  end;

// *********************************************************************//
// Interface: IGeneralFlowChannel
// Flags:     (320) Dual OleAutomation
// GUID:      {31DB8AEE-FDEE-4D25-8846-2FADD0714262}
// *********************************************************************//
  IGeneralFlowChannel = interface(IUnknown)
    ['{31DB8AEE-FDEE-4D25-8846-2FADD0714262}']
    function Get_ChannelID: Integer; safecall;
    function Get_ChannelNumber: Integer; safecall;
    function Get_ChannelName: WideString; safecall;
    procedure Set_ChannelName(const Value: WideString); safecall;
    function Get_ChannelType: Integer; safecall;
    procedure Set_ChannelType(Value: Integer); safecall;
    function Get_ChannelSubType: Integer; safecall;
    procedure Set_ChannelSubType(Value: Integer); safecall;
    function Get_UpStreamNodeNumber: Integer; safecall;
    procedure Set_UpStreamNodeNumber(Value: Integer); safecall;
    function Get_DownStreamNodeNumber: Integer; safecall;
    procedure Set_DownStreamNodeNumber(Value: Integer); safecall;
    function Get_SummaryOutputRequired: WideString; safecall;
    procedure Set_SummaryOutputRequired(const Value: WideString); safecall;
    function Get_RequiresFirmYieldAnalysis: WideString; safecall;
    procedure Set_RequiresFirmYieldAnalysis(const Value: WideString); safecall;
    function Get_ChannelPenaltyNumber: Integer; safecall;
    function Get_MinimumFlowConstraint: IMinimumFlowConstraint; safecall;
    procedure Set_MinimumFlowConstraint(const Value: IMinimumFlowConstraint); safecall;
    function Get_SourceName: WideString; safecall;
    function Get_SinkName: WideString; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_UpStreamNode: IReservoirData; safecall;
    function Get_DownStreamNode: IReservoirData; safecall;
    function Get_MinMaxFlowConstraint: IMinMaxFlowConstraint; safecall;
    procedure Set_MinMaxFlowConstraint(const Value: IMinMaxFlowConstraint); safecall;
    function Get_PumpingFeature: IPumpingFeature; safecall;
    procedure Set_PumpingFeature(const Value: IPumpingFeature); safecall;
    function Get_LossFeature: ILossFeature; safecall;
    procedure Set_LossFeature(const Value: ILossFeature); safecall;
    function Get_SpecifiedDemandFeature: ISpecifiedDemandFeature; safecall;
    procedure Set_SpecifiedDemandFeature(const Value: ISpecifiedDemandFeature); safecall;
    function Get_DiversionFeature: IDiversionFeature; safecall;
    procedure Set_DiversionFeature(const Value: IDiversionFeature); safecall;
    function Get_PhysicalFlowConstraint: IPhysicalFlowConstraint; safecall;
    procedure Set_PhysicalFlowConstraint(const Value: IPhysicalFlowConstraint); safecall;
    function Get_IFRFeature: IIFRFeature; safecall;
    procedure Set_IFRFeature(const Value: IIFRFeature); safecall;
    function Get_IrrigationArea: IIrrigationArea; safecall;
    procedure Set_IrrigationArea(const Value: IIrrigationArea); safecall;
    function Get_PowerPlant: IPowerPlant; safecall;
    procedure Set_PowerPlant(const Value: IPowerPlant); safecall;
    function Get_SpecifiedInflowFeature: ISpecifiedInflowFeature; safecall;
    procedure Set_SpecifiedInflowFeature(const Value: ISpecifiedInflowFeature); safecall;
    function Get_MasterControlFeature: IMasterControlFeature; safecall;
    procedure Set_MasterControlFeature(const Value: IMasterControlFeature); safecall;
    function Get_ChannelPenalty: IChannelPenalty; safecall;
    procedure Set_ChannelPenalty(const Value: IChannelPenalty); safecall;
    function Get_ValidArcCounts: WideString; safecall;
    function DeleteAllFeatures: WordBool; safecall;
    function DeletePumpingFeature: WordBool; safecall;
    function DeletePhysicalFlowConstraint: WordBool; safecall;
    function DeleteIFRFeature: WordBool; safecall;
    function DeleteMasterControlFeature: WordBool; safecall;
    function DeleteSpecifiedInflowFeature: WordBool; safecall;
    function Get_WaterDemandFeature: IWaterDemandFeature; safecall;
    procedure Set_WaterDemandFeature(const Value: IWaterDemandFeature); safecall;
    function DeleteWaterDemandFeature: WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_ChannelArea: Integer; safecall;
    procedure Set_ChannelArea(Value: Integer); safecall;
    function Get_TimeControl: IChannelTimeControl; safecall;
    function NewTimeControl: IChannelTimeControl; safecall;
    function RemoveTimeControl: WordBool; safecall;
    function Get_SwitchControlByID(AChannelSwitchID: Integer): IChannelSwitchControl; safecall;
    function NewSwitchControl: IChannelSwitchControl; safecall;
    function RemoveSwitchControl(AChannelSwitchID: Integer): WordBool; safecall;
    function Get_SwitchControlCount: Integer; safecall;
    function Get_SwitchControlByIndex(AIndex: Integer): IChannelSwitchControl; safecall;
    function Get_SelectedSwitchControlID: Integer; safecall;
    procedure Set_SelectedSwitchControlID(Value: Integer); safecall;
    function NewDisbenefitFunction: IDisbenefitFunctionDefinition; safecall;
    function RemoveDisbenefitFunction: WordBool; safecall;
    function Get_DisbenefitFunction: IDisbenefitFunctionDefinition; safecall;
    function NewReturnFlowChannel: IReturnFlowChannel; safecall;
    function RemoveReturnFlowChannel: WordBool; safecall;
    function Get_ReturnFlowChannel: IReturnFlowChannel; safecall;
    function Get_FlowOutput: WideString; safecall;
    procedure Set_FlowOutput(const Value: WideString); safecall;
    function Get_TariffCalculation: IChannelTariff; safecall;
    function Get_SwitchControlByChannelNumber(AChannelNumber: Integer): IChannelSwitchControl; safecall;
    function Get_MultiResChannelCurtailByChannelNo(AChannelNo: Integer): IMultiResMultiChannelCurtail; safecall;
    function RemoveMultiResChannelCurtail(AID: Integer): WordBool; safecall;
    function NewMultiResCurChannel: IMultiResMultiChannelCurtail; safecall;
    property ChannelID: Integer read Get_ChannelID;
    property ChannelNumber: Integer read Get_ChannelNumber;
    property ChannelName: WideString read Get_ChannelName write Set_ChannelName;
    property ChannelType: Integer read Get_ChannelType write Set_ChannelType;
    property ChannelSubType: Integer read Get_ChannelSubType write Set_ChannelSubType;
    property UpStreamNodeNumber: Integer read Get_UpStreamNodeNumber write Set_UpStreamNodeNumber;
    property DownStreamNodeNumber: Integer read Get_DownStreamNodeNumber write Set_DownStreamNodeNumber;
    property SummaryOutputRequired: WideString read Get_SummaryOutputRequired write Set_SummaryOutputRequired;
    property RequiresFirmYieldAnalysis: WideString read Get_RequiresFirmYieldAnalysis write Set_RequiresFirmYieldAnalysis;
    property ChannelPenaltyNumber: Integer read Get_ChannelPenaltyNumber;
    property MinimumFlowConstraint: IMinimumFlowConstraint read Get_MinimumFlowConstraint write Set_MinimumFlowConstraint;
    property SourceName: WideString read Get_SourceName;
    property SinkName: WideString read Get_SinkName;
    property UpStreamNode: IReservoirData read Get_UpStreamNode;
    property DownStreamNode: IReservoirData read Get_DownStreamNode;
    property MinMaxFlowConstraint: IMinMaxFlowConstraint read Get_MinMaxFlowConstraint write Set_MinMaxFlowConstraint;
    property PumpingFeature: IPumpingFeature read Get_PumpingFeature write Set_PumpingFeature;
    property LossFeature: ILossFeature read Get_LossFeature write Set_LossFeature;
    property SpecifiedDemandFeature: ISpecifiedDemandFeature read Get_SpecifiedDemandFeature write Set_SpecifiedDemandFeature;
    property DiversionFeature: IDiversionFeature read Get_DiversionFeature write Set_DiversionFeature;
    property PhysicalFlowConstraint: IPhysicalFlowConstraint read Get_PhysicalFlowConstraint write Set_PhysicalFlowConstraint;
    property IFRFeature: IIFRFeature read Get_IFRFeature write Set_IFRFeature;
    property IrrigationArea: IIrrigationArea read Get_IrrigationArea write Set_IrrigationArea;
    property PowerPlant: IPowerPlant read Get_PowerPlant write Set_PowerPlant;
    property SpecifiedInflowFeature: ISpecifiedInflowFeature read Get_SpecifiedInflowFeature write Set_SpecifiedInflowFeature;
    property MasterControlFeature: IMasterControlFeature read Get_MasterControlFeature write Set_MasterControlFeature;
    property ChannelPenalty: IChannelPenalty read Get_ChannelPenalty write Set_ChannelPenalty;
    property ValidArcCounts: WideString read Get_ValidArcCounts;
    property WaterDemandFeature: IWaterDemandFeature read Get_WaterDemandFeature write Set_WaterDemandFeature;
    property ChannelArea: Integer read Get_ChannelArea write Set_ChannelArea;
    property TimeControl: IChannelTimeControl read Get_TimeControl;
    property SwitchControlByID[AChannelSwitchID: Integer]: IChannelSwitchControl read Get_SwitchControlByID;
    property SwitchControlCount: Integer read Get_SwitchControlCount;
    property SwitchControlByIndex[AIndex: Integer]: IChannelSwitchControl read Get_SwitchControlByIndex;
    property SelectedSwitchControlID: Integer read Get_SelectedSwitchControlID write Set_SelectedSwitchControlID;
    property DisbenefitFunction: IDisbenefitFunctionDefinition read Get_DisbenefitFunction;
    property ReturnFlowChannel: IReturnFlowChannel read Get_ReturnFlowChannel;
    property FlowOutput: WideString read Get_FlowOutput write Set_FlowOutput;
    property TariffCalculation: IChannelTariff read Get_TariffCalculation;
    property SwitchControlByChannelNumber[AChannelNumber: Integer]: IChannelSwitchControl read Get_SwitchControlByChannelNumber;
    property MultiResChannelCurtailByChannelNo[AChannelNo: Integer]: IMultiResMultiChannelCurtail read Get_MultiResChannelCurtailByChannelNo;
  end;

// *********************************************************************//
// DispIntf:  IGeneralFlowChannelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {31DB8AEE-FDEE-4D25-8846-2FADD0714262}
// *********************************************************************//
  IGeneralFlowChannelDisp = dispinterface
    ['{31DB8AEE-FDEE-4D25-8846-2FADD0714262}']
    property ChannelID: Integer readonly dispid 201;
    property ChannelNumber: Integer readonly dispid 202;
    property ChannelName: WideString dispid 203;
    property ChannelType: Integer dispid 204;
    property ChannelSubType: Integer dispid 205;
    property UpStreamNodeNumber: Integer dispid 206;
    property DownStreamNodeNumber: Integer dispid 207;
    property SummaryOutputRequired: WideString dispid 209;
    property RequiresFirmYieldAnalysis: WideString dispid 210;
    property ChannelPenaltyNumber: Integer readonly dispid 211;
    property MinimumFlowConstraint: IMinimumFlowConstraint dispid 212;
    property SourceName: WideString readonly dispid 213;
    property SinkName: WideString readonly dispid 214;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 215;
    property UpStreamNode: IReservoirData readonly dispid 101;
    property DownStreamNode: IReservoirData readonly dispid 102;
    property MinMaxFlowConstraint: IMinMaxFlowConstraint dispid 103;
    property PumpingFeature: IPumpingFeature dispid 104;
    property LossFeature: ILossFeature dispid 105;
    property SpecifiedDemandFeature: ISpecifiedDemandFeature dispid 106;
    property DiversionFeature: IDiversionFeature dispid 107;
    property PhysicalFlowConstraint: IPhysicalFlowConstraint dispid 108;
    property IFRFeature: IIFRFeature dispid 109;
    property IrrigationArea: IIrrigationArea dispid 110;
    property PowerPlant: IPowerPlant dispid 111;
    property SpecifiedInflowFeature: ISpecifiedInflowFeature dispid 112;
    property MasterControlFeature: IMasterControlFeature dispid 113;
    property ChannelPenalty: IChannelPenalty dispid 114;
    property ValidArcCounts: WideString readonly dispid 115;
    function DeleteAllFeatures: WordBool; dispid 116;
    function DeletePumpingFeature: WordBool; dispid 117;
    function DeletePhysicalFlowConstraint: WordBool; dispid 118;
    function DeleteIFRFeature: WordBool; dispid 119;
    function DeleteMasterControlFeature: WordBool; dispid 120;
    function DeleteSpecifiedInflowFeature: WordBool; dispid 121;
    property WaterDemandFeature: IWaterDemandFeature dispid 122;
    function DeleteWaterDemandFeature: WordBool; dispid 123;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 124;
    property ChannelArea: Integer dispid 125;
    property TimeControl: IChannelTimeControl readonly dispid 126;
    function NewTimeControl: IChannelTimeControl; dispid 127;
    function RemoveTimeControl: WordBool; dispid 128;
    property SwitchControlByID[AChannelSwitchID: Integer]: IChannelSwitchControl readonly dispid 129;
    function NewSwitchControl: IChannelSwitchControl; dispid 130;
    function RemoveSwitchControl(AChannelSwitchID: Integer): WordBool; dispid 131;
    property SwitchControlCount: Integer readonly dispid 132;
    property SwitchControlByIndex[AIndex: Integer]: IChannelSwitchControl readonly dispid 133;
    property SelectedSwitchControlID: Integer dispid 134;
    function NewDisbenefitFunction: IDisbenefitFunctionDefinition; dispid 136;
    function RemoveDisbenefitFunction: WordBool; dispid 137;
    property DisbenefitFunction: IDisbenefitFunctionDefinition readonly dispid 135;
    function NewReturnFlowChannel: IReturnFlowChannel; dispid 138;
    function RemoveReturnFlowChannel: WordBool; dispid 139;
    property ReturnFlowChannel: IReturnFlowChannel readonly dispid 140;
    property FlowOutput: WideString dispid 141;
    property TariffCalculation: IChannelTariff readonly dispid 142;
    property SwitchControlByChannelNumber[AChannelNumber: Integer]: IChannelSwitchControl readonly dispid 143;
    property MultiResChannelCurtailByChannelNo[AChannelNo: Integer]: IMultiResMultiChannelCurtail readonly dispid 144;
    function RemoveMultiResChannelCurtail(AID: Integer): WordBool; dispid 145;
    function NewMultiResCurChannel: IMultiResMultiChannelCurtail; dispid 146;
  end;

// *********************************************************************//
// Interface: IChannelList
// Flags:     (320) Dual OleAutomation
// GUID:      {89232BB2-43A8-4F08-8686-749CDE3DE005}
// *********************************************************************//
  IChannelList = interface(IUnknown)
    ['{89232BB2-43A8-4F08-8686-749CDE3DE005}']
    function Get_ChannelCount: Integer; safecall;
    function Get_ChannelByIndex(Index: Integer): IGeneralFlowChannel; safecall;
    function Get_ChannelByChannelNumber(ChannelNumber: Integer): IGeneralFlowChannel; safecall;
    function Get_OutputChannelCount: Integer; safecall;
    function CreateChannel: IGeneralFlowChannel; safecall;
    function RemoveChannelWithID(AChannelID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function MayChangePenaltyArcCount(APenaltyNr: Integer; AArcCount: Integer): WordBool; safecall;
    function Get_ChannelByIdentifier(ChannelID: Integer): IGeneralFlowChannel; safecall;
    function RemoveChannelWithNumber(AChannelNumber: Integer): WordBool; safecall;
    function CopyChannel(AChannelNumber: Integer): IGeneralFlowChannel; safecall;
    function Get_ChannelByName(const AName: WideString): IGeneralFlowChannel; safecall;
    property ChannelCount: Integer read Get_ChannelCount;
    property ChannelByIndex[Index: Integer]: IGeneralFlowChannel read Get_ChannelByIndex;
    property ChannelByChannelNumber[ChannelNumber: Integer]: IGeneralFlowChannel read Get_ChannelByChannelNumber;
    property OutputChannelCount: Integer read Get_OutputChannelCount;
    property ChannelByIdentifier[ChannelID: Integer]: IGeneralFlowChannel read Get_ChannelByIdentifier;
    property ChannelByName[const AName: WideString]: IGeneralFlowChannel read Get_ChannelByName;
  end;

// *********************************************************************//
// DispIntf:  IChannelListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {89232BB2-43A8-4F08-8686-749CDE3DE005}
// *********************************************************************//
  IChannelListDisp = dispinterface
    ['{89232BB2-43A8-4F08-8686-749CDE3DE005}']
    property ChannelCount: Integer readonly dispid 101;
    property ChannelByIndex[Index: Integer]: IGeneralFlowChannel readonly dispid 102;
    property ChannelByChannelNumber[ChannelNumber: Integer]: IGeneralFlowChannel readonly dispid 103;
    property OutputChannelCount: Integer readonly dispid 104;
    function CreateChannel: IGeneralFlowChannel; dispid 105;
    function RemoveChannelWithID(AChannelID: Integer): WordBool; dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
    function MayChangePenaltyArcCount(APenaltyNr: Integer; AArcCount: Integer): WordBool; dispid 108;
    property ChannelByIdentifier[ChannelID: Integer]: IGeneralFlowChannel readonly dispid 109;
    function RemoveChannelWithNumber(AChannelNumber: Integer): WordBool; dispid 110;
    function CopyChannel(AChannelNumber: Integer): IGeneralFlowChannel; dispid 111;
    property ChannelByName[const AName: WideString]: IGeneralFlowChannel readonly dispid 112;
  end;

// *********************************************************************//
// Interface: IMinimumFlowConstraint
// Flags:     (320) Dual OleAutomation
// GUID:      {9E8813BF-59B4-47E0-8F43-D1745B384369}
// *********************************************************************//
  IMinimumFlowConstraint = interface(IUnknown)
    ['{9E8813BF-59B4-47E0-8F43-D1745B384369}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_MinimumFlowDemandByMonth(AMonth: Integer): Double; safecall;
    procedure Set_MinimumFlowDemandByMonth(AMonth: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property MinimumFlowDemandByMonth[AMonth: Integer]: Double read Get_MinimumFlowDemandByMonth write Set_MinimumFlowDemandByMonth;
  end;

// *********************************************************************//
// DispIntf:  IMinimumFlowConstraintDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {9E8813BF-59B4-47E0-8F43-D1745B384369}
// *********************************************************************//
  IMinimumFlowConstraintDisp = dispinterface
    ['{9E8813BF-59B4-47E0-8F43-D1745B384369}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property Channel: IGeneralFlowChannel dispid 103;
    property MinimumFlowDemandByMonth[AMonth: Integer]: Double dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 108;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 109;
  end;

// *********************************************************************//
// Interface: IMinMaxFlowConstraint
// Flags:     (320) Dual OleAutomation
// GUID:      {5FC99ABE-A8E7-4E12-B6B3-1A214B3569C7}
// *********************************************************************//
  IMinMaxFlowConstraint = interface(IUnknown)
    ['{5FC99ABE-A8E7-4E12-B6B3-1A214B3569C7}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_FlowConstraintCount: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CreateFlowConstraints: WordBool; safecall;
    function RemoveFlowConstraints: WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_FlowConstraintByArcMonth(AArc: Integer; AMonth: Integer): Double; safecall;
    procedure Set_FlowConstraintByArcMonth(AArc: Integer; AMonth: Integer; Value: Double); safecall;
    function Get_DistributionByArcMonth(AArc: Integer; AMonth: Integer): Double; safecall;
    procedure Set_DistributionByArcMonth(AArc: Integer; AMonth: Integer; Value: Double); safecall;
    procedure CalculateDistributionFactors; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FlowConstraintCount: Integer read Get_FlowConstraintCount;
    property FlowConstraintByArcMonth[AArc: Integer; AMonth: Integer]: Double read Get_FlowConstraintByArcMonth write Set_FlowConstraintByArcMonth;
    property DistributionByArcMonth[AArc: Integer; AMonth: Integer]: Double read Get_DistributionByArcMonth write Set_DistributionByArcMonth;
  end;

// *********************************************************************//
// DispIntf:  IMinMaxFlowConstraintDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {5FC99ABE-A8E7-4E12-B6B3-1A214B3569C7}
// *********************************************************************//
  IMinMaxFlowConstraintDisp = dispinterface
    ['{5FC99ABE-A8E7-4E12-B6B3-1A214B3569C7}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property Channel: IGeneralFlowChannel dispid 103;
    property FlowConstraintCount: Integer readonly dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
    function CreateFlowConstraints: WordBool; dispid 109;
    function RemoveFlowConstraints: WordBool; dispid 110;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 111;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 112;
    property FlowConstraintByArcMonth[AArc: Integer; AMonth: Integer]: Double dispid 107;
    property DistributionByArcMonth[AArc: Integer; AMonth: Integer]: Double dispid 113;
    procedure CalculateDistributionFactors; dispid 114;
  end;

// *********************************************************************//
// Interface: IPumpingFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {1FCE9C4F-2A23-42BC-9D82-BBC4781798D4}
// *********************************************************************//
  IPumpingFeature = interface(IUnknown)
    ['{1FCE9C4F-2A23-42BC-9D82-BBC4781798D4}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_PumpingHead: Double; safecall;
    procedure Set_PumpingHead(Value: Double); safecall;
    function Get_PumpingEfficiency: Double; safecall;
    procedure Set_PumpingEfficiency(Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property PumpingHead: Double read Get_PumpingHead write Set_PumpingHead;
    property PumpingEfficiency: Double read Get_PumpingEfficiency write Set_PumpingEfficiency;
  end;

// *********************************************************************//
// DispIntf:  IPumpingFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {1FCE9C4F-2A23-42BC-9D82-BBC4781798D4}
// *********************************************************************//
  IPumpingFeatureDisp = dispinterface
    ['{1FCE9C4F-2A23-42BC-9D82-BBC4781798D4}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property Channel: IGeneralFlowChannel dispid 103;
    property PumpingHead: Double dispid 106;
    property PumpingEfficiency: Double dispid 107;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 109;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 110;
  end;

// *********************************************************************//
// Interface: ILossFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {ADE40F68-978B-4683-AA51-598EC11DB1CB}
// *********************************************************************//
  ILossFeature = interface(IUnknown)
    ['{ADE40F68-978B-4683-AA51-598EC11DB1CB}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_WaterLossByMonth(AMonth: Integer): Double; safecall;
    procedure Set_WaterLossByMonth(AMonth: Integer; Value: Double); safecall;
    function Get_DivertedFlowByMonth(AMonth: Integer): Double; safecall;
    procedure Set_DivertedFlowByMonth(AMonth: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ReferenceNode: Integer; safecall;
    procedure Set_ReferenceNode(Value: Integer); safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property WaterLossByMonth[AMonth: Integer]: Double read Get_WaterLossByMonth write Set_WaterLossByMonth;
    property DivertedFlowByMonth[AMonth: Integer]: Double read Get_DivertedFlowByMonth write Set_DivertedFlowByMonth;
    property ReferenceNode: Integer read Get_ReferenceNode write Set_ReferenceNode;
  end;

// *********************************************************************//
// DispIntf:  ILossFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {ADE40F68-978B-4683-AA51-598EC11DB1CB}
// *********************************************************************//
  ILossFeatureDisp = dispinterface
    ['{ADE40F68-978B-4683-AA51-598EC11DB1CB}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property Channel: IGeneralFlowChannel dispid 103;
    property WaterLossByMonth[AMonth: Integer]: Double dispid 106;
    property DivertedFlowByMonth[AMonth: Integer]: Double dispid 107;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
    property ReferenceNode: Integer dispid 109;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 110;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 111;
  end;

// *********************************************************************//
// Interface: ISpecifiedDemandFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {6BD67D61-66D6-45E0-A7CA-D7E459F7C752}
// *********************************************************************//
  ISpecifiedDemandFeature = interface(IUnknown)
    ['{6BD67D61-66D6-45E0-A7CA-D7E459F7C752}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_CatchmentRefNumber: Integer; safecall;
    procedure Set_CatchmentRefNumber(Value: Integer); safecall;
    function Get_StochasticIndicator: WideString; safecall;
    procedure Set_StochasticIndicator(const Value: WideString); safecall;
    function Get_SpecifiedDemandFileName: WideString; safecall;
    procedure Set_SpecifiedDemandFileName(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetMonthlyDemand(ATimeStep: Integer; var ADemand: WideString): WordBool; safecall;
    function GetAnnualDemand(AYearIndex: Integer; var ADemand: WideString): WordBool; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property CatchmentRefNumber: Integer read Get_CatchmentRefNumber write Set_CatchmentRefNumber;
    property StochasticIndicator: WideString read Get_StochasticIndicator write Set_StochasticIndicator;
    property SpecifiedDemandFileName: WideString read Get_SpecifiedDemandFileName write Set_SpecifiedDemandFileName;
  end;

// *********************************************************************//
// DispIntf:  ISpecifiedDemandFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6BD67D61-66D6-45E0-A7CA-D7E459F7C752}
// *********************************************************************//
  ISpecifiedDemandFeatureDisp = dispinterface
    ['{6BD67D61-66D6-45E0-A7CA-D7E459F7C752}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property Channel: IGeneralFlowChannel dispid 103;
    property CatchmentRefNumber: Integer dispid 106;
    property StochasticIndicator: WideString dispid 107;
    property SpecifiedDemandFileName: WideString dispid 108;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 109;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 110;
    function GetMonthlyDemand(ATimeStep: Integer; var ADemand: WideString): WordBool; dispid 111;
    function GetAnnualDemand(AYearIndex: Integer; var ADemand: WideString): WordBool; dispid 112;
  end;

// *********************************************************************//
// Interface: IDiversionFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {3B88DA42-F28C-442A-90B3-767E8F3971BB}
// *********************************************************************//
  IDiversionFeature = interface(IUnknown)
    ['{3B88DA42-F28C-442A-90B3-767E8F3971BB}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_DiversionType: Integer; safecall;
    procedure Set_DiversionType(Value: Integer); safecall;
    function Get_DiversionDemandByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DiversionDemandByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_DivertedFlowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DivertedFlowByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_ReservoirElevationByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReservoirElevationByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_ReferenceFlowsCount: Integer; safecall;
    procedure Set_ReferenceFlowsCount(Value: Integer); safecall;
    function Get_ReferenceFlowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReferenceFlowByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_DivertedFlowProportion(AFlowIndex: Integer; AElevationIndex: Integer): Double; safecall;
    procedure Set_DivertedFlowProportion(AFlowIndex: Integer; AElevationIndex: Integer;
                                         Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ControllingReservoir: IReservoirData; safecall;
    procedure Set_ControllingReservoir(const Value: IReservoirData); safecall;
    function Get_ReservoirElevationsCount: Integer; safecall;
    procedure Set_ReservoirElevationsCount(Value: Integer); safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Type2and4RowCount: Integer; safecall;
    function InsertRow(AIndex: Integer): WordBool; safecall;
    function DeleteRow(AIndex: Integer): WordBool; safecall;
    function ImportedType2or4RelationshipFromPreProcessor(AStation: Integer): WordBool; safecall;
    function Get_Station: WideString; safecall;
    procedure Set_Station(const Value: WideString); safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property DiversionType: Integer read Get_DiversionType write Set_DiversionType;
    property DiversionDemandByIndex[AIndex: Integer]: Double read Get_DiversionDemandByIndex write Set_DiversionDemandByIndex;
    property DivertedFlowByIndex[AIndex: Integer]: Double read Get_DivertedFlowByIndex write Set_DivertedFlowByIndex;
    property ReservoirElevationByIndex[AIndex: Integer]: Double read Get_ReservoirElevationByIndex write Set_ReservoirElevationByIndex;
    property ReferenceFlowsCount: Integer read Get_ReferenceFlowsCount write Set_ReferenceFlowsCount;
    property ReferenceFlowByIndex[AIndex: Integer]: Double read Get_ReferenceFlowByIndex write Set_ReferenceFlowByIndex;
    property DivertedFlowProportion[AFlowIndex: Integer; AElevationIndex: Integer]: Double read Get_DivertedFlowProportion write Set_DivertedFlowProportion;
    property ControllingReservoir: IReservoirData read Get_ControllingReservoir write Set_ControllingReservoir;
    property ReservoirElevationsCount: Integer read Get_ReservoirElevationsCount write Set_ReservoirElevationsCount;
    property Station: WideString read Get_Station write Set_Station;
  end;

// *********************************************************************//
// DispIntf:  IDiversionFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {3B88DA42-F28C-442A-90B3-767E8F3971BB}
// *********************************************************************//
  IDiversionFeatureDisp = dispinterface
    ['{3B88DA42-F28C-442A-90B3-767E8F3971BB}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property Channel: IGeneralFlowChannel dispid 103;
    property DiversionType: Integer dispid 106;
    property DiversionDemandByIndex[AIndex: Integer]: Double dispid 107;
    property DivertedFlowByIndex[AIndex: Integer]: Double dispid 108;
    property ReservoirElevationByIndex[AIndex: Integer]: Double dispid 109;
    property ReferenceFlowsCount: Integer dispid 110;
    property ReferenceFlowByIndex[AIndex: Integer]: Double dispid 111;
    property DivertedFlowProportion[AFlowIndex: Integer; AElevationIndex: Integer]: Double dispid 112;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 113;
    property ControllingReservoir: IReservoirData dispid 114;
    property ReservoirElevationsCount: Integer dispid 115;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 116;
    function Type2and4RowCount: Integer; dispid 117;
    function InsertRow(AIndex: Integer): WordBool; dispid 118;
    function DeleteRow(AIndex: Integer): WordBool; dispid 119;
    function ImportedType2or4RelationshipFromPreProcessor(AStation: Integer): WordBool; dispid 120;
    property Station: WideString dispid 121;
  end;

// *********************************************************************//
// Interface: IPhysicalFlowConstraint
// Flags:     (320) Dual OleAutomation
// GUID:      {FCAE72BB-48EF-4C79-B693-4919AF7AADFC}
// *********************************************************************//
  IPhysicalFlowConstraint = interface(IUnknown)
    ['{FCAE72BB-48EF-4C79-B693-4919AF7AADFC}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_StructureType: Integer; safecall;
    procedure Set_StructureType(Value: Integer); safecall;
    function Get_ElevationOfSill: Double; safecall;
    procedure Set_ElevationOfSill(Value: Double); safecall;
    function Get_MaximumGateHeight: Double; safecall;
    procedure Set_MaximumGateHeight(Value: Double); safecall;
    function Get_DischargeCoefficient: Double; safecall;
    procedure Set_DischargeCoefficient(Value: Double); safecall;
    function Get_StructureLength: Double; safecall;
    procedure Set_StructureLength(Value: Double); safecall;
    function Get_NrOfPoints: Integer; safecall;
    procedure Set_NrOfPoints(Value: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_DischargeCurve: IDischargeCurve; safecall;
    function Get_KFactors: IKFactors; safecall;
    function Get_SandAquifer: ISandAquifer; safecall;
    function Get_SubmergedOutlet: ISubmergedOutlet; safecall;
    function Get_PumpStation: IPumpStation; safecall;
    function Get_ChannelNumber: Integer; safecall;
    function Get_UpstreamReservoirNr: Integer; safecall;
    procedure Set_UpstreamReservoirNr(Value: Integer); safecall;
    function Get_DownstreamReservoirNr: Integer; safecall;
    procedure Set_DownstreamReservoirNr(Value: Integer); safecall;
    function Get_WaterLevelAtDownstreamNode: Double; safecall;
    procedure Set_WaterLevelAtDownstreamNode(Value: Double); safecall;
    function Get_ReferenceElevation: Double; safecall;
    procedure Set_ReferenceElevation(Value: Double); safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property StructureType: Integer read Get_StructureType write Set_StructureType;
    property ElevationOfSill: Double read Get_ElevationOfSill write Set_ElevationOfSill;
    property MaximumGateHeight: Double read Get_MaximumGateHeight write Set_MaximumGateHeight;
    property DischargeCoefficient: Double read Get_DischargeCoefficient write Set_DischargeCoefficient;
    property StructureLength: Double read Get_StructureLength write Set_StructureLength;
    property NrOfPoints: Integer read Get_NrOfPoints write Set_NrOfPoints;
    property DischargeCurve: IDischargeCurve read Get_DischargeCurve;
    property KFactors: IKFactors read Get_KFactors;
    property SandAquifer: ISandAquifer read Get_SandAquifer;
    property SubmergedOutlet: ISubmergedOutlet read Get_SubmergedOutlet;
    property PumpStation: IPumpStation read Get_PumpStation;
    property ChannelNumber: Integer read Get_ChannelNumber;
    property UpstreamReservoirNr: Integer read Get_UpstreamReservoirNr write Set_UpstreamReservoirNr;
    property DownstreamReservoirNr: Integer read Get_DownstreamReservoirNr write Set_DownstreamReservoirNr;
    property WaterLevelAtDownstreamNode: Double read Get_WaterLevelAtDownstreamNode write Set_WaterLevelAtDownstreamNode;
    property ReferenceElevation: Double read Get_ReferenceElevation write Set_ReferenceElevation;
  end;

// *********************************************************************//
// DispIntf:  IPhysicalFlowConstraintDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {FCAE72BB-48EF-4C79-B693-4919AF7AADFC}
// *********************************************************************//
  IPhysicalFlowConstraintDisp = dispinterface
    ['{FCAE72BB-48EF-4C79-B693-4919AF7AADFC}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property StructureType: Integer dispid 108;
    property ElevationOfSill: Double dispid 109;
    property MaximumGateHeight: Double dispid 110;
    property DischargeCoefficient: Double dispid 111;
    property StructureLength: Double dispid 112;
    property NrOfPoints: Integer dispid 113;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 119;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 123;
    property DischargeCurve: IDischargeCurve readonly dispid 114;
    property KFactors: IKFactors readonly dispid 115;
    property SandAquifer: ISandAquifer readonly dispid 116;
    property SubmergedOutlet: ISubmergedOutlet readonly dispid 117;
    property PumpStation: IPumpStation readonly dispid 118;
    property ChannelNumber: Integer readonly dispid 103;
    property UpstreamReservoirNr: Integer dispid 104;
    property DownstreamReservoirNr: Integer dispid 105;
    property WaterLevelAtDownstreamNode: Double dispid 106;
    property ReferenceElevation: Double dispid 107;
  end;

// *********************************************************************//
// Interface: IIFRFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {4D59DC3B-3DFA-47F1-8FD0-7BF3C097805A}
// *********************************************************************//
  IIFRFeature = interface(IUnknown)
    ['{4D59DC3B-3DFA-47F1-8FD0-7BF3C097805A}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_LagMonths: Integer; safecall;
    procedure Set_LagMonths(Value: Integer); safecall;
    function Get_ReferenceNodeNumberByIndex(AIndex: Integer): Integer; safecall;
    function Get_ReferenceNodeNumbersCount: Integer; safecall;
    function Get_ReferenceNodeNumbers: WideString; safecall;
    procedure Set_ReferenceNodeNumbers(const Value: WideString); safecall;
    function Get_NrOfInflowIFRPoints: Integer; safecall;
    procedure Set_NrOfInflowIFRPoints(Value: Integer); safecall;
    function Get_ExceedencePercentageByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ExceedencePercentageByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_InflowByIndexAndMonth(AIndex: Integer; AMonth: Integer): Double; safecall;
    procedure Set_InflowByIndexAndMonth(AIndex: Integer; AMonth: Integer; Value: Double); safecall;
    function Get_ReleaseByIndexAndMonth(AIndex: Integer; AMonth: Integer): Double; safecall;
    procedure Set_ReleaseByIndexAndMonth(AIndex: Integer; AMonth: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_CalculationOption: Double; safecall;
    procedure Set_CalculationOption(Value: Double); safecall;
    function Get_ReferenceFlowType: TIFRFeatureReferenceFlowType; safecall;
    function Get_AnnualInflow(AIndex: Integer): Double; safecall;
    procedure Set_AnnualInflow(AIndex: Integer; AInflow: Double); safecall;
    function Get_IFRStatusIndicator: Integer; safecall;
    procedure Set_IFRStatusIndicator(Value: Integer); safecall;
    function Get_IFRLoss: Integer; safecall;
    procedure Set_IFRLoss(Value: Integer); safecall;
    function Get_MonthlyIFRLossByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyIFRLossByIndex(AIndex: Integer; Value: Double); safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property LagMonths: Integer read Get_LagMonths write Set_LagMonths;
    property ReferenceNodeNumberByIndex[AIndex: Integer]: Integer read Get_ReferenceNodeNumberByIndex;
    property ReferenceNodeNumbersCount: Integer read Get_ReferenceNodeNumbersCount;
    property ReferenceNodeNumbers: WideString read Get_ReferenceNodeNumbers write Set_ReferenceNodeNumbers;
    property NrOfInflowIFRPoints: Integer read Get_NrOfInflowIFRPoints write Set_NrOfInflowIFRPoints;
    property ExceedencePercentageByIndex[AIndex: Integer]: Double read Get_ExceedencePercentageByIndex write Set_ExceedencePercentageByIndex;
    property InflowByIndexAndMonth[AIndex: Integer; AMonth: Integer]: Double read Get_InflowByIndexAndMonth write Set_InflowByIndexAndMonth;
    property ReleaseByIndexAndMonth[AIndex: Integer; AMonth: Integer]: Double read Get_ReleaseByIndexAndMonth write Set_ReleaseByIndexAndMonth;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property CalculationOption: Double read Get_CalculationOption write Set_CalculationOption;
    property ReferenceFlowType: TIFRFeatureReferenceFlowType read Get_ReferenceFlowType;
    property AnnualInflow[AIndex: Integer]: Double read Get_AnnualInflow write Set_AnnualInflow;
    property IFRStatusIndicator: Integer read Get_IFRStatusIndicator write Set_IFRStatusIndicator;
    property IFRLoss: Integer read Get_IFRLoss write Set_IFRLoss;
    property MonthlyIFRLossByIndex[AIndex: Integer]: Double read Get_MonthlyIFRLossByIndex write Set_MonthlyIFRLossByIndex;
  end;

// *********************************************************************//
// DispIntf:  IIFRFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4D59DC3B-3DFA-47F1-8FD0-7BF3C097805A}
// *********************************************************************//
  IIFRFeatureDisp = dispinterface
    ['{4D59DC3B-3DFA-47F1-8FD0-7BF3C097805A}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property LagMonths: Integer dispid 103;
    property ReferenceNodeNumberByIndex[AIndex: Integer]: Integer readonly dispid 106;
    property ReferenceNodeNumbersCount: Integer readonly dispid 107;
    property ReferenceNodeNumbers: WideString dispid 108;
    property NrOfInflowIFRPoints: Integer dispid 109;
    property ExceedencePercentageByIndex[AIndex: Integer]: Double dispid 110;
    property InflowByIndexAndMonth[AIndex: Integer; AMonth: Integer]: Double dispid 111;
    property ReleaseByIndexAndMonth[AIndex: Integer; AMonth: Integer]: Double dispid 112;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 113;
    property Channel: IGeneralFlowChannel dispid 114;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 115;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 116;
    property CalculationOption: Double dispid 117;
    property ReferenceFlowType: TIFRFeatureReferenceFlowType readonly dispid 118;
    property AnnualInflow[AIndex: Integer]: Double dispid 119;
    property IFRStatusIndicator: Integer dispid 120;
    property IFRLoss: Integer dispid 121;
    property MonthlyIFRLossByIndex[AIndex: Integer]: Double dispid 122;
  end;

// *********************************************************************//
// Interface: IIrrigationArea
// Flags:     (320) Dual OleAutomation
// GUID:      {088C51FC-BD08-4987-9CAD-6A611089DC94}
// *********************************************************************//
  IIrrigationArea = interface(IUnknown)
    ['{088C51FC-BD08-4987-9CAD-6A611089DC94}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_DiversionFlowByMonth(AIndex: Integer): Double; safecall;
    procedure Set_DiversionFlowByMonth(AIndex: Integer; Value: Double); safecall;
    function Get_ReturnFlowByMonth(AIndex: Integer): Double; safecall;
    procedure Set_ReturnFlowByMonth(AIndex: Integer; Value: Double); safecall;
    function Get_DiversionChannel: IGeneralFlowChannel; safecall;
    function Get_ReturnFlowChannel: IGeneralFlowChannel; safecall;
    function Get_ConsumptiveChannel: IGeneralFlowChannel; safecall;
    function Get_IrrigationPolicy: Integer; safecall;
    procedure Set_IrrigationPolicy(Value: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_IrrigationNode: IReservoirData; safecall;
    function Get_IrrigationNodeNumber: Integer; safecall;
    function Get_ConsumptiveChannelNumber: Integer; safecall;
    function Get_DiversionChannelNumber: Integer; safecall;
    function Get_ReturnFlowChannelNumber: Integer; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property DiversionFlowByMonth[AIndex: Integer]: Double read Get_DiversionFlowByMonth write Set_DiversionFlowByMonth;
    property ReturnFlowByMonth[AIndex: Integer]: Double read Get_ReturnFlowByMonth write Set_ReturnFlowByMonth;
    property DiversionChannel: IGeneralFlowChannel read Get_DiversionChannel;
    property ReturnFlowChannel: IGeneralFlowChannel read Get_ReturnFlowChannel;
    property ConsumptiveChannel: IGeneralFlowChannel read Get_ConsumptiveChannel;
    property IrrigationPolicy: Integer read Get_IrrigationPolicy write Set_IrrigationPolicy;
    property IrrigationNode: IReservoirData read Get_IrrigationNode;
    property IrrigationNodeNumber: Integer read Get_IrrigationNodeNumber;
    property ConsumptiveChannelNumber: Integer read Get_ConsumptiveChannelNumber;
    property DiversionChannelNumber: Integer read Get_DiversionChannelNumber;
    property ReturnFlowChannelNumber: Integer read Get_ReturnFlowChannelNumber;
  end;

// *********************************************************************//
// DispIntf:  IIrrigationAreaDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {088C51FC-BD08-4987-9CAD-6A611089DC94}
// *********************************************************************//
  IIrrigationAreaDisp = dispinterface
    ['{088C51FC-BD08-4987-9CAD-6A611089DC94}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property DiversionFlowByMonth[AIndex: Integer]: Double dispid 103;
    property ReturnFlowByMonth[AIndex: Integer]: Double dispid 106;
    property DiversionChannel: IGeneralFlowChannel readonly dispid 107;
    property ReturnFlowChannel: IGeneralFlowChannel readonly dispid 108;
    property ConsumptiveChannel: IGeneralFlowChannel readonly dispid 109;
    property IrrigationPolicy: Integer dispid 110;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 111;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 112;
    property IrrigationNode: IReservoirData readonly dispid 113;
    property IrrigationNodeNumber: Integer readonly dispid 114;
    property ConsumptiveChannelNumber: Integer readonly dispid 115;
    property DiversionChannelNumber: Integer readonly dispid 116;
    property ReturnFlowChannelNumber: Integer readonly dispid 117;
  end;

// *********************************************************************//
// Interface: IPowerPlant
// Flags:     (320) Dual OleAutomation
// GUID:      {4DB4134C-C9C6-40E0-8A26-612EBB902A5A}
// *********************************************************************//
  IPowerPlant = interface(IUnknown)
    ['{4DB4134C-C9C6-40E0-8A26-612EBB902A5A}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_PowerChannel: IGeneralFlowChannel; safecall;
    function Get_SpillChannel: IGeneralFlowChannel; safecall;
    function Get_MaximumGeneratorCapacity: Double; safecall;
    procedure Set_MaximumGeneratorCapacity(Value: Double); safecall;
    function Get_MaximumTurbineCapacity: Double; safecall;
    procedure Set_MaximumTurbineCapacity(Value: Double); safecall;
    function Get_CombinedEfficiency: Double; safecall;
    procedure Set_CombinedEfficiency(Value: Double); safecall;
    function Get_PowerPlantStatus: WordBool; safecall;
    procedure Set_PowerPlantStatus(Value: WordBool); safecall;
    function Get_HeadLoss: Double; safecall;
    procedure Set_HeadLoss(Value: Double); safecall;
    function Get_DesignHead: Double; safecall;
    procedure Set_DesignHead(Value: Double); safecall;
    function Get_MaximumNetHead: Double; safecall;
    procedure Set_MaximumNetHead(Value: Double); safecall;
    function Get_MinimumNetHead: Double; safecall;
    procedure Set_MinimumNetHead(Value: Double); safecall;
    function Get_TailWaterType: Integer; safecall;
    procedure Set_TailWaterType(Value: Integer); safecall;
    function Get_EfficiencyFactorByIndex(Index: Integer): Double; safecall;
    procedure Set_EfficiencyFactorByIndex(Index: Integer; Value: Double); safecall;
    function Get_NetHeadFactorByIndex(Index: Integer): Double; safecall;
    procedure Set_NetHeadFactorByIndex(Index: Integer; Value: Double); safecall;
    function Get_DischargeByIndex(Index: Integer): Double; safecall;
    procedure Set_DischargeByIndex(Index: Integer; Value: Double); safecall;
    function Get_TailwaterElevationByIndex(Index: Integer): Double; safecall;
    procedure Set_TailwaterElevationByIndex(Index: Integer; Value: Double); safecall;
    function Get_MinimumPowerGenerationByMonth(Month: Integer): Double; safecall;
    procedure Set_MinimumPowerGenerationByMonth(Month: Integer; Value: Double); safecall;
    function Get_MinimumPowerReleaseByMonth(Month: Integer): Double; safecall;
    procedure Set_MinimumPowerReleaseByMonth(Month: Integer; Value: Double); safecall;
    function Get_DownstreamPowerChannelNrs: WideString; safecall;
    procedure Set_DownstreamPowerChannelNrs(const Value: WideString); safecall;
    function Get_DownstreamPowerChannelNrByIndex(AIndex: Integer): Integer; safecall;
    function Get_DownstreamPowerChannelNrsCount: Integer; safecall;
    procedure Set_DownstreamPowerChannelNrsCount(Value: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_NetHeadEfficiencyCount: Integer; safecall;
    procedure Set_NetHeadEfficiencyCount(Value: Integer); safecall;
    function Get_TailwaterElevationCount: Integer; safecall;
    procedure Set_TailwaterElevationCount(Value: Integer); safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property PowerChannel: IGeneralFlowChannel read Get_PowerChannel;
    property SpillChannel: IGeneralFlowChannel read Get_SpillChannel;
    property MaximumGeneratorCapacity: Double read Get_MaximumGeneratorCapacity write Set_MaximumGeneratorCapacity;
    property MaximumTurbineCapacity: Double read Get_MaximumTurbineCapacity write Set_MaximumTurbineCapacity;
    property CombinedEfficiency: Double read Get_CombinedEfficiency write Set_CombinedEfficiency;
    property PowerPlantStatus: WordBool read Get_PowerPlantStatus write Set_PowerPlantStatus;
    property HeadLoss: Double read Get_HeadLoss write Set_HeadLoss;
    property DesignHead: Double read Get_DesignHead write Set_DesignHead;
    property MaximumNetHead: Double read Get_MaximumNetHead write Set_MaximumNetHead;
    property MinimumNetHead: Double read Get_MinimumNetHead write Set_MinimumNetHead;
    property TailWaterType: Integer read Get_TailWaterType write Set_TailWaterType;
    property EfficiencyFactorByIndex[Index: Integer]: Double read Get_EfficiencyFactorByIndex write Set_EfficiencyFactorByIndex;
    property NetHeadFactorByIndex[Index: Integer]: Double read Get_NetHeadFactorByIndex write Set_NetHeadFactorByIndex;
    property DischargeByIndex[Index: Integer]: Double read Get_DischargeByIndex write Set_DischargeByIndex;
    property TailwaterElevationByIndex[Index: Integer]: Double read Get_TailwaterElevationByIndex write Set_TailwaterElevationByIndex;
    property MinimumPowerGenerationByMonth[Month: Integer]: Double read Get_MinimumPowerGenerationByMonth write Set_MinimumPowerGenerationByMonth;
    property MinimumPowerReleaseByMonth[Month: Integer]: Double read Get_MinimumPowerReleaseByMonth write Set_MinimumPowerReleaseByMonth;
    property DownstreamPowerChannelNrs: WideString read Get_DownstreamPowerChannelNrs write Set_DownstreamPowerChannelNrs;
    property DownstreamPowerChannelNrByIndex[AIndex: Integer]: Integer read Get_DownstreamPowerChannelNrByIndex;
    property DownstreamPowerChannelNrsCount: Integer read Get_DownstreamPowerChannelNrsCount write Set_DownstreamPowerChannelNrsCount;
    property NetHeadEfficiencyCount: Integer read Get_NetHeadEfficiencyCount write Set_NetHeadEfficiencyCount;
    property TailwaterElevationCount: Integer read Get_TailwaterElevationCount write Set_TailwaterElevationCount;
  end;

// *********************************************************************//
// DispIntf:  IPowerPlantDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4DB4134C-C9C6-40E0-8A26-612EBB902A5A}
// *********************************************************************//
  IPowerPlantDisp = dispinterface
    ['{4DB4134C-C9C6-40E0-8A26-612EBB902A5A}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property PowerChannel: IGeneralFlowChannel readonly dispid 106;
    property SpillChannel: IGeneralFlowChannel readonly dispid 107;
    property MaximumGeneratorCapacity: Double dispid 108;
    property MaximumTurbineCapacity: Double dispid 109;
    property CombinedEfficiency: Double dispid 110;
    property PowerPlantStatus: WordBool dispid 111;
    property HeadLoss: Double dispid 112;
    property DesignHead: Double dispid 113;
    property MaximumNetHead: Double dispid 114;
    property MinimumNetHead: Double dispid 115;
    property TailWaterType: Integer dispid 116;
    property EfficiencyFactorByIndex[Index: Integer]: Double dispid 122;
    property NetHeadFactorByIndex[Index: Integer]: Double dispid 123;
    property DischargeByIndex[Index: Integer]: Double dispid 125;
    property TailwaterElevationByIndex[Index: Integer]: Double dispid 126;
    property MinimumPowerGenerationByMonth[Month: Integer]: Double dispid 127;
    property MinimumPowerReleaseByMonth[Month: Integer]: Double dispid 128;
    property DownstreamPowerChannelNrs: WideString dispid 117;
    property DownstreamPowerChannelNrByIndex[AIndex: Integer]: Integer readonly dispid 118;
    property DownstreamPowerChannelNrsCount: Integer dispid 119;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 120;
    property NetHeadEfficiencyCount: Integer dispid 103;
    property TailwaterElevationCount: Integer dispid 121;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 124;
  end;

// *********************************************************************//
// Interface: IMasterControlFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {7249EDFC-CEF7-45A0-8516-FA78F7F16A27}
// *********************************************************************//
  IMasterControlFeature = interface(IUnknown)
    ['{7249EDFC-CEF7-45A0-8516-FA78F7F16A27}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_MasterControlType: WideString; safecall;
    procedure Set_MasterControlType(const Value: WideString); safecall;
    function Get_FactorByMonth(Month: Integer): Double; safecall;
    procedure Set_FactorByMonth(Month: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_DemandCentreType: WideString; safecall;
    procedure Set_DemandCentreType(const Value: WideString); safecall;
    function Get_DemandCentreID: Integer; safecall;
    procedure Set_DemandCentreID(Value: Integer); safecall;
    function Get_AnnualDemand: Double; safecall;
    procedure Set_AnnualDemand(Value: Double); safecall;
    function Get_MinimumDemand: Double; safecall;
    procedure Set_MinimumDemand(Value: Double); safecall;
    function Get_IncludeInOutput: WordBool; safecall;
    procedure Set_IncludeInOutput(Value: WordBool); safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property MasterControlType: WideString read Get_MasterControlType write Set_MasterControlType;
    property FactorByMonth[Month: Integer]: Double read Get_FactorByMonth write Set_FactorByMonth;
    property DemandCentreType: WideString read Get_DemandCentreType write Set_DemandCentreType;
    property DemandCentreID: Integer read Get_DemandCentreID write Set_DemandCentreID;
    property AnnualDemand: Double read Get_AnnualDemand write Set_AnnualDemand;
    property MinimumDemand: Double read Get_MinimumDemand write Set_MinimumDemand;
    property IncludeInOutput: WordBool read Get_IncludeInOutput write Set_IncludeInOutput;
  end;

// *********************************************************************//
// DispIntf:  IMasterControlFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {7249EDFC-CEF7-45A0-8516-FA78F7F16A27}
// *********************************************************************//
  IMasterControlFeatureDisp = dispinterface
    ['{7249EDFC-CEF7-45A0-8516-FA78F7F16A27}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property Channel: IGeneralFlowChannel dispid 103;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property MasterControlType: WideString dispid 106;
    property FactorByMonth[Month: Integer]: Double dispid 107;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 109;
    property DemandCentreType: WideString dispid 110;
    property DemandCentreID: Integer dispid 111;
    property AnnualDemand: Double dispid 112;
    property MinimumDemand: Double dispid 113;
    property IncludeInOutput: WordBool dispid 114;
  end;

// *********************************************************************//
// Interface: IReservoirDataList
// Flags:     (320) Dual OleAutomation
// GUID:      {82882783-20D9-4FC0-A87D-7B46979C76F9}
// *********************************************************************//
  IReservoirDataList = interface(IUnknown)
    ['{82882783-20D9-4FC0-A87D-7B46979C76F9}']
    function Get_ReservoirCount: Integer; safecall;
    function Get_ReservoirAndNodesCount: Integer; safecall;
    function Get_NodesWithInflowCount: Integer; safecall;
    function Get_NodesWithoutInflowCount: Integer; safecall;
    function Get_IrrigationNodesCount: Integer; safecall;
    function Get_ReservoirByIdentifier(AReservoirIdentifier: Integer): IReservoirData; safecall;
    function Get_ReservoirByIndex(AReservoirIndex: Integer): IReservoirData; safecall;
    function Get_NodeWithInflowByIdentifier(ANodeIdentifier: Integer): IReservoirData; safecall;
    function Get_NodeWithInflowByIndex(ANodeIndex: Integer): IReservoirData; safecall;
    function Get_NodeWithoutInflowByIdentifier(ANodeIdentifier: Integer): IReservoirData; safecall;
    function Get_NodeWithoutInflowByIndex(ANodeIndex: Integer): IReservoirData; safecall;
    function Get_IrrigationNodeByIdentifier(ANodeIdentifier: Integer): IReservoirData; safecall;
    function Get_IrrigationNodeByIndex(ANodeIndex: Integer): IReservoirData; safecall;
    function Get_ReservoirOrNodeByIdentifier(AReservoirOrNodeIdentifier: Integer): IReservoirData; safecall;
    function Get_ReservoirOrNodeByIndex(AReservoirOrNodeIndex: Integer): IReservoirData; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ReservoirOrNodeByID(AIdentifier: Integer): IReservoirData; safecall;
    function NodeHasInflow(ANodeNr: Integer): WordBool; safecall;
    function CreateReservoir(ANodeType: TNodeType): IReservoirData; safecall;
    function DeleteReservoir(AReservoirNumber: Integer): WordBool; safecall;
    function CreateNodeWithInflow(ANodeType: TNodeType): IReservoirData; safecall;
    function DeleteNodeWithInflow(ANodeNumber: Integer): WordBool; safecall;
    function CreateNodeWithoutInflow(ANodeType: TNodeType): IReservoirData; safecall;
    function DeleteNodeWithoutInflow(ANodeNumber: Integer): WordBool; safecall;
    function CopyCreate(AReservoirNumber: Integer): IReservoirData; safecall;
    function Get_ReservoirOrNodeByName(const AName: WideString): IReservoirData; safecall;
    property ReservoirCount: Integer read Get_ReservoirCount;
    property ReservoirAndNodesCount: Integer read Get_ReservoirAndNodesCount;
    property NodesWithInflowCount: Integer read Get_NodesWithInflowCount;
    property NodesWithoutInflowCount: Integer read Get_NodesWithoutInflowCount;
    property IrrigationNodesCount: Integer read Get_IrrigationNodesCount;
    property ReservoirByIdentifier[AReservoirIdentifier: Integer]: IReservoirData read Get_ReservoirByIdentifier;
    property ReservoirByIndex[AReservoirIndex: Integer]: IReservoirData read Get_ReservoirByIndex;
    property NodeWithInflowByIdentifier[ANodeIdentifier: Integer]: IReservoirData read Get_NodeWithInflowByIdentifier;
    property NodeWithInflowByIndex[ANodeIndex: Integer]: IReservoirData read Get_NodeWithInflowByIndex;
    property NodeWithoutInflowByIdentifier[ANodeIdentifier: Integer]: IReservoirData read Get_NodeWithoutInflowByIdentifier;
    property NodeWithoutInflowByIndex[ANodeIndex: Integer]: IReservoirData read Get_NodeWithoutInflowByIndex;
    property IrrigationNodeByIdentifier[ANodeIdentifier: Integer]: IReservoirData read Get_IrrigationNodeByIdentifier;
    property IrrigationNodeByIndex[ANodeIndex: Integer]: IReservoirData read Get_IrrigationNodeByIndex;
    property ReservoirOrNodeByIdentifier[AReservoirOrNodeIdentifier: Integer]: IReservoirData read Get_ReservoirOrNodeByIdentifier;
    property ReservoirOrNodeByIndex[AReservoirOrNodeIndex: Integer]: IReservoirData read Get_ReservoirOrNodeByIndex;
    property ReservoirOrNodeByID[AIdentifier: Integer]: IReservoirData read Get_ReservoirOrNodeByID;
    property ReservoirOrNodeByName[const AName: WideString]: IReservoirData read Get_ReservoirOrNodeByName;
  end;

// *********************************************************************//
// DispIntf:  IReservoirDataListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {82882783-20D9-4FC0-A87D-7B46979C76F9}
// *********************************************************************//
  IReservoirDataListDisp = dispinterface
    ['{82882783-20D9-4FC0-A87D-7B46979C76F9}']
    property ReservoirCount: Integer readonly dispid 101;
    property ReservoirAndNodesCount: Integer readonly dispid 102;
    property NodesWithInflowCount: Integer readonly dispid 103;
    property NodesWithoutInflowCount: Integer readonly dispid 104;
    property IrrigationNodesCount: Integer readonly dispid 105;
    property ReservoirByIdentifier[AReservoirIdentifier: Integer]: IReservoirData readonly dispid 106;
    property ReservoirByIndex[AReservoirIndex: Integer]: IReservoirData readonly dispid 107;
    property NodeWithInflowByIdentifier[ANodeIdentifier: Integer]: IReservoirData readonly dispid 108;
    property NodeWithInflowByIndex[ANodeIndex: Integer]: IReservoirData readonly dispid 109;
    property NodeWithoutInflowByIdentifier[ANodeIdentifier: Integer]: IReservoirData readonly dispid 110;
    property NodeWithoutInflowByIndex[ANodeIndex: Integer]: IReservoirData readonly dispid 111;
    property IrrigationNodeByIdentifier[ANodeIdentifier: Integer]: IReservoirData readonly dispid 112;
    property IrrigationNodeByIndex[ANodeIndex: Integer]: IReservoirData readonly dispid 113;
    property ReservoirOrNodeByIdentifier[AReservoirOrNodeIdentifier: Integer]: IReservoirData readonly dispid 114;
    property ReservoirOrNodeByIndex[AReservoirOrNodeIndex: Integer]: IReservoirData readonly dispid 115;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 116;
    property ReservoirOrNodeByID[AIdentifier: Integer]: IReservoirData readonly dispid 117;
    function NodeHasInflow(ANodeNr: Integer): WordBool; dispid 118;
    function CreateReservoir(ANodeType: TNodeType): IReservoirData; dispid 119;
    function DeleteReservoir(AReservoirNumber: Integer): WordBool; dispid 120;
    function CreateNodeWithInflow(ANodeType: TNodeType): IReservoirData; dispid 121;
    function DeleteNodeWithInflow(ANodeNumber: Integer): WordBool; dispid 122;
    function CreateNodeWithoutInflow(ANodeType: TNodeType): IReservoirData; dispid 123;
    function DeleteNodeWithoutInflow(ANodeNumber: Integer): WordBool; dispid 124;
    function CopyCreate(AReservoirNumber: Integer): IReservoirData; dispid 125;
    property ReservoirOrNodeByName[const AName: WideString]: IReservoirData readonly dispid 126;
  end;

// *********************************************************************//
// Interface: IReservoirPenaltyList
// Flags:     (320) Dual OleAutomation
// GUID:      {AC25D6EB-5451-4CA1-95A8-7683D42301A0}
// *********************************************************************//
  IReservoirPenaltyList = interface(IUnknown)
    ['{AC25D6EB-5451-4CA1-95A8-7683D42301A0}']
    function Get_PenaltyCount: Integer; safecall;
    function Get_PenaltyZoneCount: Integer; safecall;
    function Get_ReservoirPenaltyByIndex(APenaltyIndex: Integer): IReservoirPenalty; safecall;
    function Get_ReservoirPenaltyByIdentifier(AReservoirPenaltyStructureIdentifier: Integer): IReservoirPenalty; safecall;
    function Get_ReservoirPenaltyZoneByIndex(AZoneIndex: Integer): IReservoirPenaltyZoneData; safecall;
    function Get_ReservoirPenaltyCounts: IReservoirPenaltyCounts; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property PenaltyCount: Integer read Get_PenaltyCount;
    property PenaltyZoneCount: Integer read Get_PenaltyZoneCount;
    property ReservoirPenaltyByIndex[APenaltyIndex: Integer]: IReservoirPenalty read Get_ReservoirPenaltyByIndex;
    property ReservoirPenaltyByIdentifier[AReservoirPenaltyStructureIdentifier: Integer]: IReservoirPenalty read Get_ReservoirPenaltyByIdentifier;
    property ReservoirPenaltyZoneByIndex[AZoneIndex: Integer]: IReservoirPenaltyZoneData read Get_ReservoirPenaltyZoneByIndex;
    property ReservoirPenaltyCounts: IReservoirPenaltyCounts read Get_ReservoirPenaltyCounts;
  end;

// *********************************************************************//
// DispIntf:  IReservoirPenaltyListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {AC25D6EB-5451-4CA1-95A8-7683D42301A0}
// *********************************************************************//
  IReservoirPenaltyListDisp = dispinterface
    ['{AC25D6EB-5451-4CA1-95A8-7683D42301A0}']
    property PenaltyCount: Integer readonly dispid 101;
    property PenaltyZoneCount: Integer readonly dispid 102;
    property ReservoirPenaltyByIndex[APenaltyIndex: Integer]: IReservoirPenalty readonly dispid 103;
    property ReservoirPenaltyByIdentifier[AReservoirPenaltyStructureIdentifier: Integer]: IReservoirPenalty readonly dispid 104;
    property ReservoirPenaltyZoneByIndex[AZoneIndex: Integer]: IReservoirPenaltyZoneData readonly dispid 105;
    property ReservoirPenaltyCounts: IReservoirPenaltyCounts readonly dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
  end;

// *********************************************************************//
// Interface: IReservoirPenaltyCounts
// Flags:     (320) Dual OleAutomation
// GUID:      {35985C5F-AC92-4ED4-9C23-68BD84AC2419}
// *********************************************************************//
  IReservoirPenaltyCounts = interface(IUnknown)
    ['{35985C5F-AC92-4ED4-9C23-68BD84AC2419}']
    function Get_StorageZoneCount: Integer; safecall;
    function Get_PenaltyStructureCount: Integer; safecall;
    function Get_ZoneRuleCurve: Integer; safecall;
    procedure Set_ZoneRuleCurve(Value: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property StorageZoneCount: Integer read Get_StorageZoneCount;
    property PenaltyStructureCount: Integer read Get_PenaltyStructureCount;
    property ZoneRuleCurve: Integer read Get_ZoneRuleCurve write Set_ZoneRuleCurve;
  end;

// *********************************************************************//
// DispIntf:  IReservoirPenaltyCountsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {35985C5F-AC92-4ED4-9C23-68BD84AC2419}
// *********************************************************************//
  IReservoirPenaltyCountsDisp = dispinterface
    ['{35985C5F-AC92-4ED4-9C23-68BD84AC2419}']
    property StorageZoneCount: Integer readonly dispid 101;
    property PenaltyStructureCount: Integer readonly dispid 102;
    property ZoneRuleCurve: Integer dispid 103;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 104;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 105;
  end;

// *********************************************************************//
// Interface: IReservoirPenaltyZoneData
// Flags:     (320) Dual OleAutomation
// GUID:      {23133903-E02B-4FDC-B858-0A64790D38F1}
// *********************************************************************//
  IReservoirPenaltyZoneData = interface(IUnknown)
    ['{23133903-E02B-4FDC-B858-0A64790D38F1}']
    function Get_RecordIdentifier: Integer; safecall;
    function Get_ZoneName: WideString; safecall;
    procedure Set_ZoneName(const Value: WideString); safecall;
    function Get_StrategyIndicator: Integer; safecall;
    procedure Set_StrategyIndicator(Value: Integer); safecall;
    function Get_BalancingVariable: Integer; safecall;
    procedure Set_BalancingVariable(Value: Integer); safecall;
    function Get_BalancingPolicy: Integer; safecall;
    procedure Set_BalancingPolicy(Value: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property ZoneName: WideString read Get_ZoneName write Set_ZoneName;
    property StrategyIndicator: Integer read Get_StrategyIndicator write Set_StrategyIndicator;
    property BalancingVariable: Integer read Get_BalancingVariable write Set_BalancingVariable;
    property BalancingPolicy: Integer read Get_BalancingPolicy write Set_BalancingPolicy;
  end;

// *********************************************************************//
// DispIntf:  IReservoirPenaltyZoneDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {23133903-E02B-4FDC-B858-0A64790D38F1}
// *********************************************************************//
  IReservoirPenaltyZoneDataDisp = dispinterface
    ['{23133903-E02B-4FDC-B858-0A64790D38F1}']
    property RecordIdentifier: Integer readonly dispid 101;
    property ZoneName: WideString dispid 102;
    property StrategyIndicator: Integer dispid 103;
    property BalancingVariable: Integer dispid 104;
    property BalancingPolicy: Integer dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 107;
  end;

// *********************************************************************//
// Interface: IReservoirZoneElevationsData
// Flags:     (320) Dual OleAutomation
// GUID:      {160A912C-F2B2-4F8F-B871-8DA77FCE0C85}
// *********************************************************************//
  IReservoirZoneElevationsData = interface(IUnknown)
    ['{160A912C-F2B2-4F8F-B871-8DA77FCE0C85}']
    function Get_ReservoirZoneLevelsCount: Integer; safecall;
    function Get_ReservoirDrawDownLevelsCount: Integer; safecall;
    function Get_BottomOfReservoir: IFixedElevation; safecall;
    function Get_DeadStorageLevel: IFixedElevation; safecall;
    function Get_FullSupplyLevel: IFixedElevation; safecall;
    function Get_InitialLevelsData: IInitialLevelsData; safecall;
    function Get_DrawDownLevelByIndex(ALevelIndex: Integer): IDrawDownElevation; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    procedure UpdateDrawDownLevelFromDSL; safecall;
    property ReservoirZoneLevelsCount: Integer read Get_ReservoirZoneLevelsCount;
    property ReservoirDrawDownLevelsCount: Integer read Get_ReservoirDrawDownLevelsCount;
    property BottomOfReservoir: IFixedElevation read Get_BottomOfReservoir;
    property DeadStorageLevel: IFixedElevation read Get_DeadStorageLevel;
    property FullSupplyLevel: IFixedElevation read Get_FullSupplyLevel;
    property InitialLevelsData: IInitialLevelsData read Get_InitialLevelsData;
    property DrawDownLevelByIndex[ALevelIndex: Integer]: IDrawDownElevation read Get_DrawDownLevelByIndex;
  end;

// *********************************************************************//
// DispIntf:  IReservoirZoneElevationsDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {160A912C-F2B2-4F8F-B871-8DA77FCE0C85}
// *********************************************************************//
  IReservoirZoneElevationsDataDisp = dispinterface
    ['{160A912C-F2B2-4F8F-B871-8DA77FCE0C85}']
    property ReservoirZoneLevelsCount: Integer readonly dispid 101;
    property ReservoirDrawDownLevelsCount: Integer readonly dispid 102;
    property BottomOfReservoir: IFixedElevation readonly dispid 103;
    property DeadStorageLevel: IFixedElevation readonly dispid 104;
    property FullSupplyLevel: IFixedElevation readonly dispid 105;
    property InitialLevelsData: IInitialLevelsData readonly dispid 106;
    property DrawDownLevelByIndex[ALevelIndex: Integer]: IDrawDownElevation readonly dispid 108;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 109;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 107;
    procedure UpdateDrawDownLevelFromDSL; dispid 110;
  end;

// *********************************************************************//
// Interface: IFixedElevation
// Flags:     (320) Dual OleAutomation
// GUID:      {DB609BCF-A6F9-4DED-9A68-F2370621C47F}
// *********************************************************************//
  IFixedElevation = interface(IUnknown)
    ['{DB609BCF-A6F9-4DED-9A68-F2370621C47F}']
    function Get_Elevation: Double; safecall;
    procedure Set_Elevation(Value: Double); safecall;
    function Get_ElevationName: WideString; safecall;
    function Get_PenaltyValue: Double; safecall;
    function Get_ReservoirData: IReservoirData; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property Elevation: Double read Get_Elevation write Set_Elevation;
    property ElevationName: WideString read Get_ElevationName;
    property PenaltyValue: Double read Get_PenaltyValue;
    property ReservoirData: IReservoirData read Get_ReservoirData;
  end;

// *********************************************************************//
// DispIntf:  IFixedElevationDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DB609BCF-A6F9-4DED-9A68-F2370621C47F}
// *********************************************************************//
  IFixedElevationDisp = dispinterface
    ['{DB609BCF-A6F9-4DED-9A68-F2370621C47F}']
    property Elevation: Double dispid 101;
    property ElevationName: WideString readonly dispid 102;
    property PenaltyValue: Double readonly dispid 103;
    property ReservoirData: IReservoirData readonly dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
  end;

// *********************************************************************//
// Interface: IDrawDownElevation
// Flags:     (320) Dual OleAutomation
// GUID:      {DE7A0E27-2489-4191-B1F3-06AD10B0C838}
// *********************************************************************//
  IDrawDownElevation = interface(IUnknown)
    ['{DE7A0E27-2489-4191-B1F3-06AD10B0C838}']
    function Get_AverageElevations: Double; safecall;
    function Get_MonthlyElevationByIndex(AMonthIndex: Integer): Double; safecall;
    procedure Set_MonthlyElevationByIndex(AMonthIndex: Integer; Value: Double); safecall;
    function Get_ReservoirData: IReservoirData; safecall;
    function Get_LevelIdentifier: Integer; safecall;
    function Get_ReservoirIdentifier: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    procedure CalculateAvarageElevation; safecall;
    property AverageElevations: Double read Get_AverageElevations;
    property MonthlyElevationByIndex[AMonthIndex: Integer]: Double read Get_MonthlyElevationByIndex write Set_MonthlyElevationByIndex;
    property ReservoirData: IReservoirData read Get_ReservoirData;
    property LevelIdentifier: Integer read Get_LevelIdentifier;
    property ReservoirIdentifier: Integer read Get_ReservoirIdentifier;
  end;

// *********************************************************************//
// DispIntf:  IDrawDownElevationDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DE7A0E27-2489-4191-B1F3-06AD10B0C838}
// *********************************************************************//
  IDrawDownElevationDisp = dispinterface
    ['{DE7A0E27-2489-4191-B1F3-06AD10B0C838}']
    property AverageElevations: Double readonly dispid 101;
    property MonthlyElevationByIndex[AMonthIndex: Integer]: Double dispid 102;
    property ReservoirData: IReservoirData readonly dispid 103;
    property LevelIdentifier: Integer readonly dispid 104;
    property ReservoirIdentifier: Integer readonly dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 107;
    procedure CalculateAvarageElevation; dispid 108;
  end;

// *********************************************************************//
// Interface: IInitialLevelsData
// Flags:     (320) Dual OleAutomation
// GUID:      {EB9A0291-558B-4B7C-B25A-970067775E45}
// *********************************************************************//
  IInitialLevelsData = interface(IUnknown)
    ['{EB9A0291-558B-4B7C-B25A-970067775E45}']
    function Get_ReservoirIdentifier: Integer; safecall;
    function Get_InitialLevelsByIndex(ALevelIndex: Integer): Double; safecall;
    procedure Set_InitialLevelsByIndex(ALevelIndex: Integer; Value: Double); safecall;
    procedure UpdateInitialLevelValue(ALevelIndex: Integer; ANewValue: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property ReservoirIdentifier: Integer read Get_ReservoirIdentifier;
    property InitialLevelsByIndex[ALevelIndex: Integer]: Double read Get_InitialLevelsByIndex write Set_InitialLevelsByIndex;
  end;

// *********************************************************************//
// DispIntf:  IInitialLevelsDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {EB9A0291-558B-4B7C-B25A-970067775E45}
// *********************************************************************//
  IInitialLevelsDataDisp = dispinterface
    ['{EB9A0291-558B-4B7C-B25A-970067775E45}']
    property ReservoirIdentifier: Integer readonly dispid 101;
    property InitialLevelsByIndex[ALevelIndex: Integer]: Double dispid 102;
    procedure UpdateInitialLevelValue(ALevelIndex: Integer; ANewValue: Double); dispid 103;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 104;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 105;
  end;

// *********************************************************************//
// Interface: IReservoirPenalty
// Flags:     (320) Dual OleAutomation
// GUID:      {1F40FCAB-92F0-41F8-86D9-CDCE587EDBED}
// *********************************************************************//
  IReservoirPenalty = interface(IUnknown)
    ['{1F40FCAB-92F0-41F8-86D9-CDCE587EDBED}']
    function Get_PenaltyValueCount: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ReservoirPenaltyValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReservoirPenaltyValueByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_ReservoirPenaltyID: Integer; safecall;
    procedure Set_ReservoirPenaltyID(Value: Integer); safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property PenaltyValueCount: Integer read Get_PenaltyValueCount;
    property ReservoirPenaltyValueByIndex[AIndex: Integer]: Double read Get_ReservoirPenaltyValueByIndex write Set_ReservoirPenaltyValueByIndex;
    property ReservoirPenaltyID: Integer read Get_ReservoirPenaltyID write Set_ReservoirPenaltyID;
  end;

// *********************************************************************//
// DispIntf:  IReservoirPenaltyDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {1F40FCAB-92F0-41F8-86D9-CDCE587EDBED}
// *********************************************************************//
  IReservoirPenaltyDisp = dispinterface
    ['{1F40FCAB-92F0-41F8-86D9-CDCE587EDBED}']
    property PenaltyValueCount: Integer readonly dispid 101;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 104;
    property ReservoirPenaltyValueByIndex[AIndex: Integer]: Double dispid 103;
    property ReservoirPenaltyID: Integer dispid 102;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 105;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 106;
  end;

// *********************************************************************//
// Interface: IReservoirConfigurationData
// Flags:     (320) Dual OleAutomation
// GUID:      {FA96F9C7-2D42-43C3-96EE-2742C396254C}
// *********************************************************************//
  IReservoirConfigurationData = interface(IUnknown)
    ['{FA96F9C7-2D42-43C3-96EE-2742C396254C}']
    function Get_RecordIdentifier: Integer; safecall;
    function Get_NodeType: TNodeType; safecall;
    function Get_PenaltyStructIdentifier: Integer; safecall;
    procedure Set_PenaltyStructIdentifier(Value: Integer); safecall;
    function Get_PointsCount: Integer; safecall;
    function Get_StatusIndicator: Integer; safecall;
    procedure Set_StatusIndicator(Value: Integer); safecall;
    function Get_ReservoirName: WideString; safecall;
    procedure Set_ReservoirName(const Value: WideString); safecall;
    function Get_ReservoirIdentifier: Integer; safecall;
    function Get_IncludeSummary: WideString; safecall;
    procedure Set_IncludeSummary(const Value: WideString); safecall;
    function Get_DrainageScale: Double; safecall;
    procedure Set_DrainageScale(Value: Double); safecall;
    function Get_AfforestationScale: Double; safecall;
    procedure Set_AfforestationScale(Value: Double); safecall;
    function Get_IrrigationScale: Double; safecall;
    procedure Set_IrrigationScale(Value: Double); safecall;
    function Get_CatchmentRef: Integer; safecall;
    procedure Set_CatchmentRef(Value: Integer); safecall;
    function Get_MaxArea: Double; safecall;
    function Get_AreaWhenFull: Double; safecall;
    procedure Set_AreaWhenFull(Value: Double); safecall;
    function Get_MaxVolume: Double; safecall;
    function Get_Priority: Double; safecall;
    procedure Set_Priority(Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_RainCoef: Double; safecall;
    procedure Set_RainCoef(Value: Double); safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_DamLevelsFileName: WideString; safecall;
    procedure Set_DamLevelsFileName(const Value: WideString); safecall;
    function Get_VolumeWhenFull: Double; safecall;
    procedure Set_VolumeWhenFull(Value: Double); safecall;
    function Get_UrbanRunOff: Double; safecall;
    procedure Set_UrbanRunOff(Value: Double); safecall;
    function Get_XCoord: Double; safecall;
    procedure Set_XCoord(Value: Double); safecall;
    function Get_YCoord: Double; safecall;
    procedure Set_YCoord(Value: Double); safecall;
    function Get_GroupID: Integer; safecall;
    procedure Set_GroupID(Value: Integer); safecall;
    function Get_NaturalInflowChannel: Integer; safecall;
    procedure Set_NaturalInflowChannel(Value: Integer); safecall;
    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property NodeType: TNodeType read Get_NodeType;
    property PenaltyStructIdentifier: Integer read Get_PenaltyStructIdentifier write Set_PenaltyStructIdentifier;
    property PointsCount: Integer read Get_PointsCount;
    property StatusIndicator: Integer read Get_StatusIndicator write Set_StatusIndicator;
    property ReservoirName: WideString read Get_ReservoirName write Set_ReservoirName;
    property ReservoirIdentifier: Integer read Get_ReservoirIdentifier;
    property IncludeSummary: WideString read Get_IncludeSummary write Set_IncludeSummary;
    property DrainageScale: Double read Get_DrainageScale write Set_DrainageScale;
    property AfforestationScale: Double read Get_AfforestationScale write Set_AfforestationScale;
    property IrrigationScale: Double read Get_IrrigationScale write Set_IrrigationScale;
    property CatchmentRef: Integer read Get_CatchmentRef write Set_CatchmentRef;
    property MaxArea: Double read Get_MaxArea;
    property AreaWhenFull: Double read Get_AreaWhenFull write Set_AreaWhenFull;
    property MaxVolume: Double read Get_MaxVolume;
    property Priority: Double read Get_Priority write Set_Priority;
    property RainCoef: Double read Get_RainCoef write Set_RainCoef;
    property DamLevelsFileName: WideString read Get_DamLevelsFileName write Set_DamLevelsFileName;
    property VolumeWhenFull: Double read Get_VolumeWhenFull write Set_VolumeWhenFull;
    property UrbanRunOff: Double read Get_UrbanRunOff write Set_UrbanRunOff;
    property XCoord: Double read Get_XCoord write Set_XCoord;
    property YCoord: Double read Get_YCoord write Set_YCoord;
    property GroupID: Integer read Get_GroupID write Set_GroupID;
    property NaturalInflowChannel: Integer read Get_NaturalInflowChannel write Set_NaturalInflowChannel;
  end;

// *********************************************************************//
// DispIntf:  IReservoirConfigurationDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {FA96F9C7-2D42-43C3-96EE-2742C396254C}
// *********************************************************************//
  IReservoirConfigurationDataDisp = dispinterface
    ['{FA96F9C7-2D42-43C3-96EE-2742C396254C}']
    property RecordIdentifier: Integer readonly dispid 101;
    property NodeType: TNodeType readonly dispid 102;
    property PenaltyStructIdentifier: Integer dispid 103;
    property PointsCount: Integer readonly dispid 104;
    property StatusIndicator: Integer dispid 105;
    property ReservoirName: WideString dispid 106;
    property ReservoirIdentifier: Integer readonly dispid 107;
    property IncludeSummary: WideString dispid 108;
    property DrainageScale: Double dispid 109;
    property AfforestationScale: Double dispid 110;
    property IrrigationScale: Double dispid 111;
    property CatchmentRef: Integer dispid 112;
    property MaxArea: Double readonly dispid 113;
    property AreaWhenFull: Double dispid 114;
    property MaxVolume: Double readonly dispid 115;
    property Priority: Double dispid 116;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 117;
    property RainCoef: Double dispid 118;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 119;
    property DamLevelsFileName: WideString dispid 120;
    property VolumeWhenFull: Double dispid 121;
    property UrbanRunOff: Double dispid 122;
    property XCoord: Double dispid 123;
    property YCoord: Double dispid 124;
    property GroupID: Integer dispid 125;
    property NaturalInflowChannel: Integer dispid 126;
  end;

// *********************************************************************//
// Interface: IReservoirEvaporationsData
// Flags:     (320) Dual OleAutomation
// GUID:      {4E0F062A-6733-4A9C-96AD-B9175382C027}
// *********************************************************************//
  IReservoirEvaporationsData = interface(IUnknown)
    ['{4E0F062A-6733-4A9C-96AD-B9175382C027}']
    function Get_RecordIdentifier: Integer; safecall;
    function Get_MonthlyEvaporationsByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyEvaporationsByIndex(AIndex: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property MonthlyEvaporationsByIndex[AIndex: Integer]: Double read Get_MonthlyEvaporationsByIndex write Set_MonthlyEvaporationsByIndex;
  end;

// *********************************************************************//
// DispIntf:  IReservoirEvaporationsDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4E0F062A-6733-4A9C-96AD-B9175382C027}
// *********************************************************************//
  IReservoirEvaporationsDataDisp = dispinterface
    ['{4E0F062A-6733-4A9C-96AD-B9175382C027}']
    property RecordIdentifier: Integer readonly dispid 101;
    property MonthlyEvaporationsByIndex[AIndex: Integer]: Double dispid 102;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 103;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 104;
  end;

// *********************************************************************//
// Interface: IReservoirElevationsData
// Flags:     (320) Dual OleAutomation
// GUID:      {083ABEBD-E5C8-4295-98E5-E7217C7266EA}
// *********************************************************************//
  IReservoirElevationsData = interface(IUnknown)
    ['{083ABEBD-E5C8-4295-98E5-E7217C7266EA}']
    function Get_RecordIdentifier: Integer; safecall;
    function Get_StartElevation: Integer; safecall;
    function Get_ReservoirElevationsByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReservoirElevationsByIndex(AIndex: Integer; Value: Double); safecall;
    function DeleteElevationValue(AIndex: Integer): WordBool; safecall;
    function InsertElevationValue(AIndex: Integer; AValue: Double): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property StartElevation: Integer read Get_StartElevation;
    property ReservoirElevationsByIndex[AIndex: Integer]: Double read Get_ReservoirElevationsByIndex write Set_ReservoirElevationsByIndex;
  end;

// *********************************************************************//
// DispIntf:  IReservoirElevationsDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {083ABEBD-E5C8-4295-98E5-E7217C7266EA}
// *********************************************************************//
  IReservoirElevationsDataDisp = dispinterface
    ['{083ABEBD-E5C8-4295-98E5-E7217C7266EA}']
    property RecordIdentifier: Integer readonly dispid 101;
    property StartElevation: Integer readonly dispid 102;
    property ReservoirElevationsByIndex[AIndex: Integer]: Double dispid 103;
    function DeleteElevationValue(AIndex: Integer): WordBool; dispid 105;
    function InsertElevationValue(AIndex: Integer; AValue: Double): WordBool; dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 104;
  end;

// *********************************************************************//
// Interface: IReservoirVolumeData
// Flags:     (320) Dual OleAutomation
// GUID:      {0408D497-F57C-4BFB-AAD3-AEF30B2D798F}
// *********************************************************************//
  IReservoirVolumeData = interface(IUnknown)
    ['{0408D497-F57C-4BFB-AAD3-AEF30B2D798F}']
    function Get_RecordIdentifier: Integer; safecall;
    function Get_StartVolume: Integer; safecall;
    function Get_ReservoirVolumesByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReservoirVolumesByIndex(AIndex: Integer; Value: Double); safecall;
    function DeleteVolumeValue(AIndex: Integer): WordBool; safecall;
    function InsertVolumeValue(AIndex: Integer; AValue: Double): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_MaxReservoirVolume: Double; safecall;
    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property StartVolume: Integer read Get_StartVolume;
    property ReservoirVolumesByIndex[AIndex: Integer]: Double read Get_ReservoirVolumesByIndex write Set_ReservoirVolumesByIndex;
    property MaxReservoirVolume: Double read Get_MaxReservoirVolume;
  end;

// *********************************************************************//
// DispIntf:  IReservoirVolumeDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {0408D497-F57C-4BFB-AAD3-AEF30B2D798F}
// *********************************************************************//
  IReservoirVolumeDataDisp = dispinterface
    ['{0408D497-F57C-4BFB-AAD3-AEF30B2D798F}']
    property RecordIdentifier: Integer readonly dispid 101;
    property StartVolume: Integer readonly dispid 102;
    property ReservoirVolumesByIndex[AIndex: Integer]: Double dispid 103;
    function DeleteVolumeValue(AIndex: Integer): WordBool; dispid 104;
    function InsertVolumeValue(AIndex: Integer; AValue: Double): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 107;
    property MaxReservoirVolume: Double readonly dispid 108;
  end;

// *********************************************************************//
// Interface: IReservoirAreaData
// Flags:     (320) Dual OleAutomation
// GUID:      {99019087-B13E-44EA-A442-7471316E3F45}
// *********************************************************************//
  IReservoirAreaData = interface(IUnknown)
    ['{99019087-B13E-44EA-A442-7471316E3F45}']
    function Get_RecordIdentifier: Integer; safecall;
    function Get_StartArea: Integer; safecall;
    function Get_ReservoirAreasByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReservoirAreasByIndex(AIndex: Integer; Value: Double); safecall;
    function DeleteAreaValue(AIndex: Integer): WordBool; safecall;
    function InsertAreaValue(AIndex: Integer; AValue: Double): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property StartArea: Integer read Get_StartArea;
    property ReservoirAreasByIndex[AIndex: Integer]: Double read Get_ReservoirAreasByIndex write Set_ReservoirAreasByIndex;
  end;

// *********************************************************************//
// DispIntf:  IReservoirAreaDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {99019087-B13E-44EA-A442-7471316E3F45}
// *********************************************************************//
  IReservoirAreaDataDisp = dispinterface
    ['{99019087-B13E-44EA-A442-7471316E3F45}']
    property RecordIdentifier: Integer readonly dispid 101;
    property StartArea: Integer readonly dispid 102;
    property ReservoirAreasByIndex[AIndex: Integer]: Double dispid 103;
    function DeleteAreaValue(AIndex: Integer): WordBool; dispid 104;
    function InsertAreaValue(AIndex: Integer; AValue: Double): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 107;
  end;

// *********************************************************************//
// Interface: IReservoirData
// Flags:     (320) Dual OleAutomation
// GUID:      {9CA99E70-F417-4B01-982D-E61AC8F4DBAA}
// *********************************************************************//
  IReservoirData = interface(IUnknown)
    ['{9CA99E70-F417-4B01-982D-E61AC8F4DBAA}']
    function Get_ReservoirConfigurationData: IReservoirConfigurationData; safecall;
    function Get_ReservoirPenaltyStructureData: IReservoirPenalty; safecall;
    function Get_ReservoirZoneElevationsData: IReservoirZoneElevationsData; safecall;
    function Get_ReservoirEvaporationsData: IReservoirEvaporationsData; safecall;
    function Get_ReservoirElevationsData: IReservoirElevationsData; safecall;
    function Get_ReservoirVolumesData: IReservoirVolumeData; safecall;
    function Get_ReservoirAreasData: IReservoirAreaData; safecall;
    function DeletePhysicalCharacteristicsRow(AIndex: Integer): WordBool; safecall;
    function InsertPhysicalCharacteristicsRow(AIndex: Integer; AElevation: Double; AVolume: Double;
                                              AArea: Double): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function RecalculateAreaWhenFull: WordBool; safecall;
    function Get_TimeControl: IReservoirTimeControl; safecall;
    function NewTimeControl: IReservoirTimeControl; safecall;
    function RemoveTimeControl: WordBool; safecall;
    function Clone: IReservoirData; safecall;
    function DownStreamPowerChannels: WideString; safecall;
    function DownStreamPowerChannelCount: Integer; safecall;
    function GetReservoirVolumeByElevation(AElevation: Double): Double; safecall;
    function GetReservoirElevationByVolume(AVolume: Double): Double; safecall;
    function GetReservoirElevationBySurfaceArea(AArea: Double): Double; safecall;
    function GetReservoirSurfaceAreaByElevation(AElevation: Double): Double; safecall;
    function GetReservoirSurfaceAreaByVolume(AVolume: Double): Double; safecall;
    function GetReservoirVolumeBySurfaceArea(AArea: Double): Double; safecall;
    property ReservoirConfigurationData: IReservoirConfigurationData read Get_ReservoirConfigurationData;
    property ReservoirPenaltyStructureData: IReservoirPenalty read Get_ReservoirPenaltyStructureData;
    property ReservoirZoneElevationsData: IReservoirZoneElevationsData read Get_ReservoirZoneElevationsData;
    property ReservoirEvaporationsData: IReservoirEvaporationsData read Get_ReservoirEvaporationsData;
    property ReservoirElevationsData: IReservoirElevationsData read Get_ReservoirElevationsData;
    property ReservoirVolumesData: IReservoirVolumeData read Get_ReservoirVolumesData;
    property ReservoirAreasData: IReservoirAreaData read Get_ReservoirAreasData;
    property TimeControl: IReservoirTimeControl read Get_TimeControl;
  end;

// *********************************************************************//
// DispIntf:  IReservoirDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {9CA99E70-F417-4B01-982D-E61AC8F4DBAA}
// *********************************************************************//
  IReservoirDataDisp = dispinterface
    ['{9CA99E70-F417-4B01-982D-E61AC8F4DBAA}']
    property ReservoirConfigurationData: IReservoirConfigurationData readonly dispid 101;
    property ReservoirPenaltyStructureData: IReservoirPenalty readonly dispid 102;
    property ReservoirZoneElevationsData: IReservoirZoneElevationsData readonly dispid 103;
    property ReservoirEvaporationsData: IReservoirEvaporationsData readonly dispid 104;
    property ReservoirElevationsData: IReservoirElevationsData readonly dispid 105;
    property ReservoirVolumesData: IReservoirVolumeData readonly dispid 106;
    property ReservoirAreasData: IReservoirAreaData readonly dispid 107;
    function DeletePhysicalCharacteristicsRow(AIndex: Integer): WordBool; dispid 108;
    function InsertPhysicalCharacteristicsRow(AIndex: Integer; AElevation: Double; AVolume: Double;
                                              AArea: Double): WordBool; dispid 109;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 110;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 111;
    function RecalculateAreaWhenFull: WordBool; dispid 112;
    property TimeControl: IReservoirTimeControl readonly dispid 113;
    function NewTimeControl: IReservoirTimeControl; dispid 114;
    function RemoveTimeControl: WordBool; dispid 115;
    function Clone: IReservoirData; dispid 116;
    function DownStreamPowerChannels: WideString; dispid 117;
    function DownStreamPowerChannelCount: Integer; dispid 118;
    function GetReservoirVolumeByElevation(AElevation: Double): Double; dispid 119;
    function GetReservoirElevationByVolume(AVolume: Double): Double; dispid 120;
    function GetReservoirElevationBySurfaceArea(AArea: Double): Double; dispid 121;
    function GetReservoirSurfaceAreaByElevation(AElevation: Double): Double; dispid 122;
    function GetReservoirSurfaceAreaByVolume(AVolume: Double): Double; dispid 123;
    function GetReservoirVolumeBySurfaceArea(AArea: Double): Double; dispid 124;
  end;

// *********************************************************************//
// Interface: IParamReference
// Flags:     (320) Dual OleAutomation
// GUID:      {06BC50F1-039C-44F9-9FF7-56DF64F3AF47}
// *********************************************************************//
  IParamReference = interface(IUnknown)
    ['{06BC50F1-039C-44F9-9FF7-56DF64F3AF47}']
    function Get_CatchReference: Integer; safecall;
    function Get_FileReference: WideString; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_CatchmentArea: Double; safecall;
    procedure Set_CatchmentArea(Value: Double); safecall;
    function Get_NumberOfYears: Integer; safecall;
    procedure Set_NumberOfYears(Value: Integer); safecall;
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(Value: Integer); safecall;
    function Get_Residual1: Double; safecall;
    procedure Set_Residual1(Value: Double); safecall;
    function Get_Residual2: Double; safecall;
    procedure Set_Residual2(Value: Double); safecall;
    function Get_Variate1: Double; safecall;
    procedure Set_Variate1(Value: Double); safecall;
    function Get_Variate2: Double; safecall;
    procedure Set_Variate2(Value: Double); safecall;
    function Get_TransformType: Integer; safecall;
    procedure Set_TransformType(Value: Integer); safecall;
    function Get_TransformGamma: Double; safecall;
    procedure Set_TransformGamma(Value: Double); safecall;
    function Get_TransformDelta: Double; safecall;
    procedure Set_TransformDelta(Value: Double); safecall;
    function Get_TransformXlam: Double; safecall;
    procedure Set_TransformXlam(Value: Double); safecall;
    function Get_TransformXi: Double; safecall;
    procedure Set_TransformXi(Value: Double); safecall;
    function Get_ResidualMean: Double; safecall;
    procedure Set_ResidualMean(Value: Double); safecall;
    function Get_ResidualStdDev: Double; safecall;
    procedure Set_ResidualStdDev(Value: Double); safecall;
    function Get_ArmaPhi1: Double; safecall;
    procedure Set_ArmaPhi1(Value: Double); safecall;
    function Get_ArmaPhi2: Double; safecall;
    procedure Set_ArmaPhi2(Value: Double); safecall;
    function Get_ArmaTheta1: Double; safecall;
    procedure Set_ArmaTheta1(Value: Double); safecall;
    function Get_ArmaTheta2: Double; safecall;
    procedure Set_ArmaTheta2(Value: Double); safecall;
    function Get_PhiZero: Double; safecall;
    procedure Set_PhiZero(Value: Double); safecall;
    function Get_ZTVariates: Integer; safecall;
    procedure Set_ZTVariates(Value: Integer); safecall;
    function Get_ParamXa: Double; safecall;
    procedure Set_ParamXa(Value: Double); safecall;
    function Get_ParamXSD: Double; safecall;
    procedure Set_ParamXSD(Value: Double); safecall;
    function Get_ParamAIC: Double; safecall;
    procedure Set_ParamAIC(Value: Double); safecall;
    function Get_ParamANC: Double; safecall;
    procedure Set_ParamANC(Value: Double); safecall;
    function Get_GaugeName: WideString; safecall;
    procedure Set_GaugeName(const Value: WideString); safecall;
    property CatchReference: Integer read Get_CatchReference;
    property FileReference: WideString read Get_FileReference;
    property CatchmentArea: Double read Get_CatchmentArea write Set_CatchmentArea;
    property NumberOfYears: Integer read Get_NumberOfYears write Set_NumberOfYears;
    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property Residual1: Double read Get_Residual1 write Set_Residual1;
    property Residual2: Double read Get_Residual2 write Set_Residual2;
    property Variate1: Double read Get_Variate1 write Set_Variate1;
    property Variate2: Double read Get_Variate2 write Set_Variate2;
    property TransformType: Integer read Get_TransformType write Set_TransformType;
    property TransformGamma: Double read Get_TransformGamma write Set_TransformGamma;
    property TransformDelta: Double read Get_TransformDelta write Set_TransformDelta;
    property TransformXlam: Double read Get_TransformXlam write Set_TransformXlam;
    property TransformXi: Double read Get_TransformXi write Set_TransformXi;
    property ResidualMean: Double read Get_ResidualMean write Set_ResidualMean;
    property ResidualStdDev: Double read Get_ResidualStdDev write Set_ResidualStdDev;
    property ArmaPhi1: Double read Get_ArmaPhi1 write Set_ArmaPhi1;
    property ArmaPhi2: Double read Get_ArmaPhi2 write Set_ArmaPhi2;
    property ArmaTheta1: Double read Get_ArmaTheta1 write Set_ArmaTheta1;
    property ArmaTheta2: Double read Get_ArmaTheta2 write Set_ArmaTheta2;
    property PhiZero: Double read Get_PhiZero write Set_PhiZero;
    property ZTVariates: Integer read Get_ZTVariates write Set_ZTVariates;
    property ParamXa: Double read Get_ParamXa write Set_ParamXa;
    property ParamXSD: Double read Get_ParamXSD write Set_ParamXSD;
    property ParamAIC: Double read Get_ParamAIC write Set_ParamAIC;
    property ParamANC: Double read Get_ParamANC write Set_ParamANC;
    property GaugeName: WideString read Get_GaugeName write Set_GaugeName;
  end;

// *********************************************************************//
// DispIntf:  IParamReferenceDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {06BC50F1-039C-44F9-9FF7-56DF64F3AF47}
// *********************************************************************//
  IParamReferenceDisp = dispinterface
    ['{06BC50F1-039C-44F9-9FF7-56DF64F3AF47}']
    property CatchReference: Integer readonly dispid 101;
    property FileReference: WideString readonly dispid 102;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 103;
    property CatchmentArea: Double dispid 104;
    property NumberOfYears: Integer dispid 105;
    property StartYear: Integer dispid 106;
    property Residual1: Double dispid 107;
    property Residual2: Double dispid 108;
    property Variate1: Double dispid 109;
    property Variate2: Double dispid 110;
    property TransformType: Integer dispid 111;
    property TransformGamma: Double dispid 112;
    property TransformDelta: Double dispid 113;
    property TransformXlam: Double dispid 114;
    property TransformXi: Double dispid 115;
    property ResidualMean: Double dispid 116;
    property ResidualStdDev: Double dispid 117;
    property ArmaPhi1: Double dispid 118;
    property ArmaPhi2: Double dispid 119;
    property ArmaTheta1: Double dispid 120;
    property ArmaTheta2: Double dispid 121;
    property PhiZero: Double dispid 122;
    property ZTVariates: Integer dispid 123;
    property ParamXa: Double dispid 124;
    property ParamXSD: Double dispid 125;
    property ParamAIC: Double dispid 126;
    property ParamANC: Double dispid 127;
    property GaugeName: WideString dispid 128;
  end;

// *********************************************************************//
// Interface: IParamSetup
// Flags:     (320) Dual OleAutomation
// GUID:      {BA9CEC23-E1B3-4F75-AEF1-17D1B0FFF340}
// *********************************************************************//
  IParamSetup = interface(IUnknown)
    ['{BA9CEC23-E1B3-4F75-AEF1-17D1B0FFF340}']
    function ReferenceNumberValid(ANumber: Integer): WordBool; safecall;
    function Get_ReferenceCount: Integer; safecall;
    function Get_ReferenceDataByIndex(AIndex: Integer): IParamReference; safecall;
    function Get_ReferenceDataByCatchNumber(ARefNumber: Integer): IParamReference; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_MatrixB(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_MatrixB0(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_MatrixB1(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_MatrixA(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_MatrixC(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_KeyGaugeCount: Integer; safecall;
    function Get_KeyGaugeNoByIndex(AIndex: Integer): Integer; safecall;
    property ReferenceCount: Integer read Get_ReferenceCount;
    property ReferenceDataByIndex[AIndex: Integer]: IParamReference read Get_ReferenceDataByIndex;
    property ReferenceDataByCatchNumber[ARefNumber: Integer]: IParamReference read Get_ReferenceDataByCatchNumber;
    property MatrixB[ARow: Integer; ACol: Integer]: Double read Get_MatrixB;
    property MatrixB0[ARow: Integer; ACol: Integer]: Double read Get_MatrixB0;
    property MatrixB1[ARow: Integer; ACol: Integer]: Double read Get_MatrixB1;
    property MatrixA[ARow: Integer; ACol: Integer]: Double read Get_MatrixA;
    property MatrixC[ARow: Integer; ACol: Integer]: Double read Get_MatrixC;
    property KeyGaugeCount: Integer read Get_KeyGaugeCount;
    property KeyGaugeNoByIndex[AIndex: Integer]: Integer read Get_KeyGaugeNoByIndex;
  end;

// *********************************************************************//
// DispIntf:  IParamSetupDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {BA9CEC23-E1B3-4F75-AEF1-17D1B0FFF340}
// *********************************************************************//
  IParamSetupDisp = dispinterface
    ['{BA9CEC23-E1B3-4F75-AEF1-17D1B0FFF340}']
    function ReferenceNumberValid(ANumber: Integer): WordBool; dispid 101;
    property ReferenceCount: Integer readonly dispid 102;
    property ReferenceDataByIndex[AIndex: Integer]: IParamReference readonly dispid 103;
    property ReferenceDataByCatchNumber[ARefNumber: Integer]: IParamReference readonly dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
    property MatrixB[ARow: Integer; ACol: Integer]: Double readonly dispid 106;
    property MatrixB0[ARow: Integer; ACol: Integer]: Double readonly dispid 107;
    property MatrixB1[ARow: Integer; ACol: Integer]: Double readonly dispid 108;
    property MatrixA[ARow: Integer; ACol: Integer]: Double readonly dispid 109;
    property MatrixC[ARow: Integer; ACol: Integer]: Double readonly dispid 110;
    property KeyGaugeCount: Integer readonly dispid 111;
    property KeyGaugeNoByIndex[AIndex: Integer]: Integer readonly dispid 112;
  end;

// *********************************************************************//
// Interface: IModelCalendar
// Flags:     (320) Dual OleAutomation
// GUID:      {17AD05BB-10AE-4865-A135-16CB6FEBA859}
// *********************************************************************//
  IModelCalendar = interface(IUnknown)
    ['{17AD05BB-10AE-4865-A135-16CB6FEBA859}']
    function Get_CalenderStartDate(AYear: Integer): TDateTime; safecall;
    function Get_CalenderDateByMonthIndex(AModelMonthIndex: Integer; AYear: Integer): TDateTime; safecall;
    function Get_CalenderDateByMonthName(const AModelMonthName: WideString; AYear: Integer): TDateTime; safecall;
    function Get_ModelMonthNameByIndex(AModelMonthIndex: Integer): WideString; safecall;
    function Get_ModelMonthIndexByName(const AModelMonthName: WideString): Integer; safecall;
    function Get_ModelMonthDaysByIndex(AModelMonthIndex: Integer): Double; safecall;
    function Get_ModelMonthDaysByName(const AModelMonthName: WideString): Double; safecall;
    function Get_ModelMonthPeriodElapsed(AEndDate: TDateTime): Integer; safecall;
    function Get_CalenderDateFromPeriodElapsed(APeriodElapsed: Integer): TDateTime; safecall;
    property CalenderStartDate[AYear: Integer]: TDateTime read Get_CalenderStartDate;
    property CalenderDateByMonthIndex[AModelMonthIndex: Integer; AYear: Integer]: TDateTime read Get_CalenderDateByMonthIndex;
    property CalenderDateByMonthName[const AModelMonthName: WideString; AYear: Integer]: TDateTime read Get_CalenderDateByMonthName;
    property ModelMonthNameByIndex[AModelMonthIndex: Integer]: WideString read Get_ModelMonthNameByIndex;
    property ModelMonthIndexByName[const AModelMonthName: WideString]: Integer read Get_ModelMonthIndexByName;
    property ModelMonthDaysByIndex[AModelMonthIndex: Integer]: Double read Get_ModelMonthDaysByIndex;
    property ModelMonthDaysByName[const AModelMonthName: WideString]: Double read Get_ModelMonthDaysByName;
    property ModelMonthPeriodElapsed[AEndDate: TDateTime]: Integer read Get_ModelMonthPeriodElapsed;
    property CalenderDateFromPeriodElapsed[APeriodElapsed: Integer]: TDateTime read Get_CalenderDateFromPeriodElapsed;
  end;

// *********************************************************************//
// DispIntf:  IModelCalendarDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {17AD05BB-10AE-4865-A135-16CB6FEBA859}
// *********************************************************************//
  IModelCalendarDisp = dispinterface
    ['{17AD05BB-10AE-4865-A135-16CB6FEBA859}']
    property CalenderStartDate[AYear: Integer]: TDateTime readonly dispid 101;
    property CalenderDateByMonthIndex[AModelMonthIndex: Integer; AYear: Integer]: TDateTime readonly dispid 102;
    property CalenderDateByMonthName[const AModelMonthName: WideString; AYear: Integer]: TDateTime readonly dispid 103;
    property ModelMonthNameByIndex[AModelMonthIndex: Integer]: WideString readonly dispid 104;
    property ModelMonthIndexByName[const AModelMonthName: WideString]: Integer readonly dispid 105;
    property ModelMonthDaysByIndex[AModelMonthIndex: Integer]: Double readonly dispid 106;
    property ModelMonthDaysByName[const AModelMonthName: WideString]: Double readonly dispid 107;
    property ModelMonthPeriodElapsed[AEndDate: TDateTime]: Integer readonly dispid 108;
    property CalenderDateFromPeriodElapsed[APeriodElapsed: Integer]: TDateTime readonly dispid 109;
  end;

// *********************************************************************//
// Interface: IWaterDemandCategory
// Flags:     (320) Dual OleAutomation
// GUID:      {4F1B0065-2F38-4771-9558-89F021CD56A8}
// *********************************************************************//
  IWaterDemandCategory = interface(IUnknown)
    ['{4F1B0065-2F38-4771-9558-89F021CD56A8}']
    function Get_CategoryID: Integer; safecall;
    function Get_CategoryName: WideString; safecall;
    procedure Set_CategoryName(const Value: WideString); safecall;
    function Get_DemandPortionByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DemandPortionByIndex(AIndex: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_PortionTotal(AIndex: Integer): Double; safecall;
    property CategoryID: Integer read Get_CategoryID;
    property CategoryName: WideString read Get_CategoryName write Set_CategoryName;
    property DemandPortionByIndex[AIndex: Integer]: Double read Get_DemandPortionByIndex write Set_DemandPortionByIndex;
    property PortionTotal[AIndex: Integer]: Double read Get_PortionTotal;
  end;

// *********************************************************************//
// DispIntf:  IWaterDemandCategoryDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4F1B0065-2F38-4771-9558-89F021CD56A8}
// *********************************************************************//
  IWaterDemandCategoryDisp = dispinterface
    ['{4F1B0065-2F38-4771-9558-89F021CD56A8}']
    property CategoryID: Integer readonly dispid 101;
    property CategoryName: WideString dispid 102;
    property DemandPortionByIndex[AIndex: Integer]: Double dispid 103;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 104;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 105;
    property PortionTotal[AIndex: Integer]: Double readonly dispid 106;
  end;

// *********************************************************************//
// Interface: IWaterDemandConfiguration
// Flags:     (320) Dual OleAutomation
// GUID:      {3680A493-9DD2-445C-9BB2-D3FF32041AF8}
// *********************************************************************//
  IWaterDemandConfiguration = interface(IUnknown)
    ['{3680A493-9DD2-445C-9BB2-D3FF32041AF8}']
    function Get_DemandCategoryCount: Integer; safecall;
    function Get_RecurrenceIntervalByIndex(AIndex: Integer): Double; safecall;
    procedure Set_RecurrenceIntervalByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_DemandCategoryByIndex(AIndex: Integer): IWaterDemandCategory; safecall;
    function Get_DemandCategoryByID(ACategoryID: Integer): IWaterDemandCategory; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CreateWaterDemandCategory: IWaterDemandCategory; safecall;
    function RemoveWaterDemandCategoryWithID(ACategoryID: Integer): WordBool; safecall;
    function Get_DemandCategoryByName(const AName: WideString): IWaterDemandCategory; safecall;
    function Get_RiskCriteriaCount: Integer; safecall;
    procedure Set_RiskCriteriaCount(Value: Integer); safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function CreateNewWaterUseOutputProportion(AChannelNumber: Integer): IWaterUseOutputProportion; safecall;
    function Get_WaterUseOutputProportionByIndex(AIndex: Integer): IWaterUseOutputProportion; safecall;
    function Get_WaterUseOutputProportionByChannelNumber(AChannelNumber: Integer): IWaterUseOutputProportion; safecall;
    function Get_WaterUseOutputProportionCount: Integer; safecall;
    procedure CreateYieldWaterUseOutputProportion; safecall;
    function DeleteWaterUseOutputProportion(AChannelNumber: Integer): WordBool; safecall;
    procedure UpdateWaterUseOutputProportions(ANewCount: Integer); safecall;
    function Get_ImplementReconciliation: WordBool; safecall;
    procedure Set_ImplementReconciliation(Value: WordBool); safecall;
    property DemandCategoryCount: Integer read Get_DemandCategoryCount;
    property RecurrenceIntervalByIndex[AIndex: Integer]: Double read Get_RecurrenceIntervalByIndex write Set_RecurrenceIntervalByIndex;
    property DemandCategoryByIndex[AIndex: Integer]: IWaterDemandCategory read Get_DemandCategoryByIndex;
    property DemandCategoryByID[ACategoryID: Integer]: IWaterDemandCategory read Get_DemandCategoryByID;
    property DemandCategoryByName[const AName: WideString]: IWaterDemandCategory read Get_DemandCategoryByName;
    property RiskCriteriaCount: Integer read Get_RiskCriteriaCount write Set_RiskCriteriaCount;
    property WaterUseOutputProportionByIndex[AIndex: Integer]: IWaterUseOutputProportion read Get_WaterUseOutputProportionByIndex;
    property WaterUseOutputProportionByChannelNumber[AChannelNumber: Integer]: IWaterUseOutputProportion read Get_WaterUseOutputProportionByChannelNumber;
    property WaterUseOutputProportionCount: Integer read Get_WaterUseOutputProportionCount;
    property ImplementReconciliation: WordBool read Get_ImplementReconciliation write Set_ImplementReconciliation;
  end;

// *********************************************************************//
// DispIntf:  IWaterDemandConfigurationDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {3680A493-9DD2-445C-9BB2-D3FF32041AF8}
// *********************************************************************//
  IWaterDemandConfigurationDisp = dispinterface
    ['{3680A493-9DD2-445C-9BB2-D3FF32041AF8}']
    property DemandCategoryCount: Integer readonly dispid 101;
    property RecurrenceIntervalByIndex[AIndex: Integer]: Double dispid 104;
    property DemandCategoryByIndex[AIndex: Integer]: IWaterDemandCategory readonly dispid 105;
    property DemandCategoryByID[ACategoryID: Integer]: IWaterDemandCategory readonly dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
    function CreateWaterDemandCategory: IWaterDemandCategory; dispid 108;
    function RemoveWaterDemandCategoryWithID(ACategoryID: Integer): WordBool; dispid 109;
    property DemandCategoryByName[const AName: WideString]: IWaterDemandCategory readonly dispid 103;
    property RiskCriteriaCount: Integer dispid 102;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 110;
    function CreateNewWaterUseOutputProportion(AChannelNumber: Integer): IWaterUseOutputProportion; dispid 111;
    property WaterUseOutputProportionByIndex[AIndex: Integer]: IWaterUseOutputProportion readonly dispid 113;
    property WaterUseOutputProportionByChannelNumber[AChannelNumber: Integer]: IWaterUseOutputProportion readonly dispid 114;
    property WaterUseOutputProportionCount: Integer readonly dispid 112;
    procedure CreateYieldWaterUseOutputProportion; dispid 117;
    function DeleteWaterUseOutputProportion(AChannelNumber: Integer): WordBool; dispid 115;
    procedure UpdateWaterUseOutputProportions(ANewCount: Integer); dispid 116;
    property ImplementReconciliation: WordBool dispid 118;
  end;

// *********************************************************************//
// Interface: IWaterDemandFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {009856A6-7FC7-4CC1-8EFE-D8BC00E852A0}
// *********************************************************************//
  IWaterDemandFeature = interface(IUnknown)
    ['{009856A6-7FC7-4CC1-8EFE-D8BC00E852A0}']
    function Get_FeatureID: Integer; safecall;
    function Get_FeatureName: WideString; safecall;
    procedure Set_FeatureName(const Value: WideString); safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_FeatureType: Integer; safecall;
    procedure Set_FeatureType(Value: Integer); safecall;
    function Get_FeatureSubType: Integer; safecall;
    procedure Set_FeatureSubType(Value: Integer); safecall;
    function Get_WaterDemandCategory: Integer; safecall;
    procedure Set_WaterDemandCategory(Value: Integer); safecall;
    function Get_ScenarioPortionByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ScenarioPortionByIndex(AIndex: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property FeatureName: WideString read Get_FeatureName write Set_FeatureName;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType: Integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType: Integer read Get_FeatureSubType write Set_FeatureSubType;
    property WaterDemandCategory: Integer read Get_WaterDemandCategory write Set_WaterDemandCategory;
    property ScenarioPortionByIndex[AIndex: Integer]: Double read Get_ScenarioPortionByIndex write Set_ScenarioPortionByIndex;
  end;

// *********************************************************************//
// DispIntf:  IWaterDemandFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {009856A6-7FC7-4CC1-8EFE-D8BC00E852A0}
// *********************************************************************//
  IWaterDemandFeatureDisp = dispinterface
    ['{009856A6-7FC7-4CC1-8EFE-D8BC00E852A0}']
    property FeatureID: Integer readonly dispid 101;
    property FeatureName: WideString dispid 102;
    property Channel: IGeneralFlowChannel dispid 103;
    property FeatureType: Integer dispid 104;
    property FeatureSubType: Integer dispid 105;
    property WaterDemandCategory: Integer dispid 106;
    property ScenarioPortionByIndex[AIndex: Integer]: Double dispid 107;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 109;
  end;

// *********************************************************************//
// Interface: IWaterDemandFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {39B5B8C0-60C8-444F-9373-42A144557E4D}
// *********************************************************************//
  IWaterDemandFeatureList = interface(IUnknown)
    ['{39B5B8C0-60C8-444F-9373-42A144557E4D}']
    function Get_WaterDemandFeatureCount: Integer; safecall;
    function Get_WaterDemandFeatureByIndex(AIndex: Integer): IWaterDemandFeature; safecall;
    function Get_WaterDemandFeatureByID(AFeatureID: Integer): IWaterDemandFeature; safecall;
    function CreateWaterDemandFeature: IWaterDemandFeature; safecall;
    function RemoveWaterDemandFeatureWithID(AFeatureID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ScenarioCount: Integer; safecall;
    procedure Set_ScenarioCount(Value: Integer); safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function CopyWaterDemandFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IWaterDemandFeature; safecall;
    property WaterDemandFeatureCount: Integer read Get_WaterDemandFeatureCount;
    property WaterDemandFeatureByIndex[AIndex: Integer]: IWaterDemandFeature read Get_WaterDemandFeatureByIndex;
    property WaterDemandFeatureByID[AFeatureID: Integer]: IWaterDemandFeature read Get_WaterDemandFeatureByID;
    property ScenarioCount: Integer read Get_ScenarioCount write Set_ScenarioCount;
  end;

// *********************************************************************//
// DispIntf:  IWaterDemandFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {39B5B8C0-60C8-444F-9373-42A144557E4D}
// *********************************************************************//
  IWaterDemandFeatureListDisp = dispinterface
    ['{39B5B8C0-60C8-444F-9373-42A144557E4D}']
    property WaterDemandFeatureCount: Integer readonly dispid 101;
    property WaterDemandFeatureByIndex[AIndex: Integer]: IWaterDemandFeature readonly dispid 102;
    property WaterDemandFeatureByID[AFeatureID: Integer]: IWaterDemandFeature readonly dispid 103;
    function CreateWaterDemandFeature: IWaterDemandFeature; dispid 104;
    function RemoveWaterDemandFeatureWithID(AFeatureID: Integer): WordBool; dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    property ScenarioCount: Integer dispid 107;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 108;
    function CopyWaterDemandFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IWaterDemandFeature; dispid 109;
  end;

// *********************************************************************//
// Interface: IChangeList
// Flags:     (320) Dual OleAutomation
// GUID:      {EF57D643-E162-4363-B32A-6654E2399B49}
// *********************************************************************//
  IChangeList = interface(IUnknown)
    ['{EF57D643-E162-4363-B32A-6654E2399B49}']
    function Get_DateCreated: TDateTime; safecall;
    function Get_CreatedBy: WideString; safecall;
    procedure Set_CreatedBy(const Value: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_ChangeListName: WideString; safecall;
    procedure Set_ChangeListName(const Value: WideString); safecall;
    function Get_ChangeListID: Integer; safecall;
    function FindParamChange(const AParamField: WideString; const AKeyValues: WideString;
                             const AFieldIndex: WideString): IParameterChange; safecall;
    function CreateNewParamChange(const AParamField: WideString; const AKeyValues: WideString;
                                  const AFieldIndex: WideString; const AAbsolut: WideString;
                                  const AChange: WideString; const AParamDescr: WideString;
                                  AFiltered: WordBool): IParameterChange; safecall;
    procedure DeleteParamChange(const AParamField: WideString; const AKeyValues: WideString;
                                const AFieldIndex: WideString); safecall;
    function ParamChangeByIndex(AIndex: Integer): IParameterChange; safecall;
    function ParamChangeCount: Integer; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function Get_IsResident: WordBool; safecall;
    property DateCreated: TDateTime read Get_DateCreated;
    property CreatedBy: WideString read Get_CreatedBy write Set_CreatedBy;
    property Description: WideString read Get_Description write Set_Description;
    property ChangeListName: WideString read Get_ChangeListName write Set_ChangeListName;
    property ChangeListID: Integer read Get_ChangeListID;
    property IsResident: WordBool read Get_IsResident;
  end;

// *********************************************************************//
// DispIntf:  IChangeListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {EF57D643-E162-4363-B32A-6654E2399B49}
// *********************************************************************//
  IChangeListDisp = dispinterface
    ['{EF57D643-E162-4363-B32A-6654E2399B49}']
    property DateCreated: TDateTime readonly dispid 102;
    property CreatedBy: WideString dispid 103;
    property Description: WideString dispid 104;
    property ChangeListName: WideString dispid 101;
    property ChangeListID: Integer readonly dispid 107;
    function FindParamChange(const AParamField: WideString; const AKeyValues: WideString;
                             const AFieldIndex: WideString): IParameterChange; dispid 108;
    function CreateNewParamChange(const AParamField: WideString; const AKeyValues: WideString;
                                  const AFieldIndex: WideString; const AAbsolut: WideString;
                                  const AChange: WideString; const AParamDescr: WideString;
                                  AFiltered: WordBool): IParameterChange; dispid 109;
    procedure DeleteParamChange(const AParamField: WideString; const AKeyValues: WideString;
                                const AFieldIndex: WideString); dispid 110;
    function ParamChangeByIndex(AIndex: Integer): IParameterChange; dispid 111;
    function ParamChangeCount: Integer; dispid 112;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 113;
    property IsResident: WordBool readonly dispid 105;
  end;

// *********************************************************************//
// Interface: IParameterChange
// Flags:     (320) Dual OleAutomation
// GUID:      {E9C04174-8297-4824-A5B9-86ACBBED4891}
// *********************************************************************//
  IParameterChange = interface(IUnknown)
    ['{E9C04174-8297-4824-A5B9-86ACBBED4891}']
    function Get_ChangeListID: Integer; safecall;
    function Get_ParamField: WideString; safecall;
    function Get_Absolut: WordBool; safecall;
    procedure Set_Absolut(Value: WordBool); safecall;
    function Get_Change: WideString; safecall;
    procedure Set_Change(const Value: WideString); safecall;
    function Get_FieldIndex: WideString; safecall;
    function Get_KeyValues: WideString; safecall;
    function Get_ParamDescr: WideString; safecall;
    procedure Set_ParamDescr(const Value: WideString); safecall;
    property ChangeListID: Integer read Get_ChangeListID;
    property ParamField: WideString read Get_ParamField;
    property Absolut: WordBool read Get_Absolut write Set_Absolut;
    property Change: WideString read Get_Change write Set_Change;
    property FieldIndex: WideString read Get_FieldIndex;
    property KeyValues: WideString read Get_KeyValues;
    property ParamDescr: WideString read Get_ParamDescr write Set_ParamDescr;
  end;

// *********************************************************************//
// DispIntf:  IParameterChangeDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {E9C04174-8297-4824-A5B9-86ACBBED4891}
// *********************************************************************//
  IParameterChangeDisp = dispinterface
    ['{E9C04174-8297-4824-A5B9-86ACBBED4891}']
    property ChangeListID: Integer readonly dispid 101;
    property ParamField: WideString readonly dispid 102;
    property Absolut: WordBool dispid 104;
    property Change: WideString dispid 105;
    property FieldIndex: WideString readonly dispid 106;
    property KeyValues: WideString readonly dispid 108;
    property ParamDescr: WideString dispid 103;
  end;

// *********************************************************************//
// Interface: IMetaData
// Flags:     (320) Dual OleAutomation
// GUID:      {2E5D1CA0-500C-45E3-A7FA-EBE07F1C8623}
// *********************************************************************//
  IMetaData = interface(IUnknown)
    ['{2E5D1CA0-500C-45E3-A7FA-EBE07F1C8623}']
    function Get_ParamField: WideString; safecall;
    function Get_FieldIndex: WideString; safecall;
    function Get_KeyValues: WideString; safecall;
    function Get_DateCreated: TDateTime; safecall;
    procedure Set_DateCreated(Value: TDateTime); safecall;
    function Get_CreatedBy: WideString; safecall;
    procedure Set_CreatedBy(const Value: WideString); safecall;
    function Get_Comment: WideString; safecall;
    procedure Set_Comment(const Value: WideString); safecall;
    property ParamField: WideString read Get_ParamField;
    property FieldIndex: WideString read Get_FieldIndex;
    property KeyValues: WideString read Get_KeyValues;
    property DateCreated: TDateTime read Get_DateCreated write Set_DateCreated;
    property CreatedBy: WideString read Get_CreatedBy write Set_CreatedBy;
    property Comment: WideString read Get_Comment write Set_Comment;
  end;

// *********************************************************************//
// DispIntf:  IMetaDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {2E5D1CA0-500C-45E3-A7FA-EBE07F1C8623}
// *********************************************************************//
  IMetaDataDisp = dispinterface
    ['{2E5D1CA0-500C-45E3-A7FA-EBE07F1C8623}']
    property ParamField: WideString readonly dispid 101;
    property FieldIndex: WideString readonly dispid 102;
    property KeyValues: WideString readonly dispid 103;
    property DateCreated: TDateTime dispid 104;
    property CreatedBy: WideString dispid 105;
    property Comment: WideString dispid 106;
  end;

// *********************************************************************//
// Interface: IConfiguration
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E5D0C300-5DBE-4205-A4D6-E3DEAB8F909C}
// *********************************************************************//
  IConfiguration = interface(IDispatch)
    ['{E5D0C300-5DBE-4205-A4D6-E3DEAB8F909C}']
    function Get_INIFileName: WideString; safecall;
    function Get_COMServerFileName: WideString; safecall;
    property INIFileName: WideString read Get_INIFileName;
    property COMServerFileName: WideString read Get_COMServerFileName;
  end;

// *********************************************************************//
// DispIntf:  IConfigurationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E5D0C300-5DBE-4205-A4D6-E3DEAB8F909C}
// *********************************************************************//
  IConfigurationDisp = dispinterface
    ['{E5D0C300-5DBE-4205-A4D6-E3DEAB8F909C}']
    property INIFileName: WideString readonly dispid 201;
    property COMServerFileName: WideString readonly dispid 202;
  end;

// *********************************************************************//
// Interface: IWeatherEvents
// Flags:     (320) Dual OleAutomation
// GUID:      {B121E354-184D-4E47-B0C5-C962FAAC7715}
// *********************************************************************//
  IWeatherEvents = interface(IUnknown)
    ['{B121E354-184D-4E47-B0C5-C962FAAC7715}']
    function FindWeatherEvents(AStartDateTime: TDateTime; AEndDateTime: TDateTime;
                               const AArea: WideString): WideString; safecall;
    function GetAreas: WideString; safecall;
    function EarliestDate: TDateTime; safecall;
    function LatestDate: TDateTime; safecall;
  end;

// *********************************************************************//
// DispIntf:  IWeatherEventsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {B121E354-184D-4E47-B0C5-C962FAAC7715}
// *********************************************************************//
  IWeatherEventsDisp = dispinterface
    ['{B121E354-184D-4E47-B0C5-C962FAAC7715}']
    function FindWeatherEvents(AStartDateTime: TDateTime; AEndDateTime: TDateTime;
                               const AArea: WideString): WideString; dispid 101;
    function GetAreas: WideString; dispid 102;
    function EarliestDate: TDateTime; dispid 103;
    function LatestDate: TDateTime; dispid 104;
  end;

// *********************************************************************//
// Interface: IChannelArea
// Flags:     (320) Dual OleAutomation
// GUID:      {DC54F70A-CB16-4ED5-8967-FBEB4F2E4F76}
// *********************************************************************//
  IChannelArea = interface(IUnknown)
    ['{DC54F70A-CB16-4ED5-8967-FBEB4F2E4F76}']
    function Get_AreaID: Integer; safecall;
    function Get_AreaName: WideString; safecall;
    procedure Set_AreaName(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property AreaID: Integer read Get_AreaID;
    property AreaName: WideString read Get_AreaName write Set_AreaName;
  end;

// *********************************************************************//
// DispIntf:  IChannelAreaDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DC54F70A-CB16-4ED5-8967-FBEB4F2E4F76}
// *********************************************************************//
  IChannelAreaDisp = dispinterface
    ['{DC54F70A-CB16-4ED5-8967-FBEB4F2E4F76}']
    property AreaID: Integer readonly dispid 101;
    property AreaName: WideString dispid 102;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
  end;

// *********************************************************************//
// Interface: IChannelAreaList
// Flags:     (320) Dual OleAutomation
// GUID:      {D300D1F5-35D3-48B1-96B6-CD4BE1CC3B21}
// *********************************************************************//
  IChannelAreaList = interface(IUnknown)
    ['{D300D1F5-35D3-48B1-96B6-CD4BE1CC3B21}']
    function Get_AreaCount: Integer; safecall;
    function ChannelAreaByIndex(AIndex: Integer): IChannelArea; safecall;
    function ChannelAreaByID(AChannelAreaID: Integer): IChannelArea; safecall;
    function ChannelAreaByName(const AName: WideString): IChannelArea; safecall;
    function CreateChannelArea: IChannelArea; safecall;
    function RemoveChannelArea(AChannelAreaID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property AreaCount: Integer read Get_AreaCount;
  end;

// *********************************************************************//
// DispIntf:  IChannelAreaListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {D300D1F5-35D3-48B1-96B6-CD4BE1CC3B21}
// *********************************************************************//
  IChannelAreaListDisp = dispinterface
    ['{D300D1F5-35D3-48B1-96B6-CD4BE1CC3B21}']
    property AreaCount: Integer readonly dispid 101;
    function ChannelAreaByIndex(AIndex: Integer): IChannelArea; dispid 102;
    function ChannelAreaByID(AChannelAreaID: Integer): IChannelArea; dispid 103;
    function ChannelAreaByName(const AName: WideString): IChannelArea; dispid 104;
    function CreateChannelArea: IChannelArea; dispid 105;
    function RemoveChannelArea(AChannelAreaID: Integer): WordBool; dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
  end;

// *********************************************************************//
// Interface: IOutputData
// Flags:     (320) Dual OleAutomation
// GUID:      {566134B3-D4A0-4036-9E10-D0B43FE60131}
// *********************************************************************//
  IOutputData = interface(IUnknown)
    ['{566134B3-D4A0-4036-9E10-D0B43FE60131}']
    function ShowDataSelectionDialog(AIdentifier: Integer;
                                     ANetworkElementType: TNetworkElementType;
                                     AOutputSourceDialog: TOutputSourceDialog;
                                     AOutputDataType: TOutputDataType;
                                     AOutputValueType: TOutputValueType): WordBool; safecall;
    function Get_GetSelection: IOutputDataSelection; safecall;
    function Get_SummaryOutputData: ISummaryOutputData; safecall;
    property GetSelection: IOutputDataSelection read Get_GetSelection;
    property SummaryOutputData: ISummaryOutputData read Get_SummaryOutputData;
  end;

// *********************************************************************//
// DispIntf:  IOutputDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {566134B3-D4A0-4036-9E10-D0B43FE60131}
// *********************************************************************//
  IOutputDataDisp = dispinterface
    ['{566134B3-D4A0-4036-9E10-D0B43FE60131}']
    function ShowDataSelectionDialog(AIdentifier: Integer;
                                     ANetworkElementType: TNetworkElementType;
                                     AOutputSourceDialog: TOutputSourceDialog;
                                     AOutputDataType: TOutputDataType;
                                     AOutputValueType: TOutputValueType): WordBool; dispid 104;
    property GetSelection: IOutputDataSelection readonly dispid 101;
    property SummaryOutputData: ISummaryOutputData readonly dispid 103;
  end;

// *********************************************************************//
// Interface: IWaterUseOutputProportion
// Flags:     (320) Dual OleAutomation
// GUID:      {6811556E-DB56-4ACD-A03E-535B82C8D302}
// *********************************************************************//
  IWaterUseOutputProportion = interface(IUnknown)
    ['{6811556E-DB56-4ACD-A03E-535B82C8D302}']
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(Value: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ProportionByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ProportionByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_Total: Double; safecall;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property ProportionByIndex[AIndex: Integer]: Double read Get_ProportionByIndex write Set_ProportionByIndex;
    property Total: Double read Get_Total;
  end;

// *********************************************************************//
// DispIntf:  IWaterUseOutputProportionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6811556E-DB56-4ACD-A03E-535B82C8D302}
// *********************************************************************//
  IWaterUseOutputProportionDisp = dispinterface
    ['{6811556E-DB56-4ACD-A03E-535B82C8D302}']
    property ChannelNumber: Integer dispid 101;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 103;
    property ProportionByIndex[AIndex: Integer]: Double dispid 104;
    property Total: Double readonly dispid 105;
  end;

// *********************************************************************//
// Interface: IOutputDataSelection
// Flags:     (320) Dual OleAutomation
// GUID:      {F7500348-92A7-4351-8F8D-CE909D5264F8}
// *********************************************************************//
  IOutputDataSelection = interface(IUnknown)
    ['{F7500348-92A7-4351-8F8D-CE909D5264F8}']
    function Get_Month: Integer; safecall;
    function Get_Units: TOutputUnits; safecall;
    function Get_ValueType: TOutputValueType; safecall;
    function Get_Highlight: WordBool; safecall;
    function Get_DisplayMonth: Integer; safecall;
    function Get_AverageType: TOutputAverageType; safecall;
    function Get_AverageStartDate: TDateTime; safecall;
    function Get_AverageEndDate: TDateTime; safecall;
    function Get_DecisionMonth: Integer; safecall;
    function Get_PlotOption: TOutputPlotOptions; safecall;
    function Get_YearsToSkip: Integer; safecall;
    function Get_LoadCase: Integer; safecall;
    procedure Set_LoadCase(Value: Integer); safecall;
    function Get_Sequence: Integer; safecall;
    procedure Set_Sequence(Value: Integer); safecall;
    function Get_ApplySensitivity: TSensitivity; safecall;
    procedure Set_ApplySensitivity(AValue: TSensitivity); safecall;
    function Get_Sensitivity: Double; safecall;
    procedure Set_Sensitivity(Value: Double); safecall;
    function Get_PercSensitivity: Double; safecall;
    procedure Set_PercSensitivity(Value: Double); safecall;
    function Get_TimeStep: TOutputTimeStep; safecall;
    procedure Set_TimeStep(Value: TOutputTimeStep); safecall;
    property Month: Integer read Get_Month;
    property Units: TOutputUnits read Get_Units;
    property ValueType: TOutputValueType read Get_ValueType;
    property Highlight: WordBool read Get_Highlight;
    property DisplayMonth: Integer read Get_DisplayMonth;
    property AverageType: TOutputAverageType read Get_AverageType;
    property AverageStartDate: TDateTime read Get_AverageStartDate;
    property AverageEndDate: TDateTime read Get_AverageEndDate;
    property DecisionMonth: Integer read Get_DecisionMonth;
    property PlotOption: TOutputPlotOptions read Get_PlotOption;
    property YearsToSkip: Integer read Get_YearsToSkip;
    property LoadCase: Integer read Get_LoadCase write Set_LoadCase;
    property Sequence: Integer read Get_Sequence write Set_Sequence;
    property ApplySensitivity: TSensitivity read Get_ApplySensitivity write Set_ApplySensitivity;
    property Sensitivity: Double read Get_Sensitivity write Set_Sensitivity;
    property PercSensitivity: Double read Get_PercSensitivity write Set_PercSensitivity;
    property TimeStep: TOutputTimeStep read Get_TimeStep write Set_TimeStep;
  end;

// *********************************************************************//
// DispIntf:  IOutputDataSelectionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {F7500348-92A7-4351-8F8D-CE909D5264F8}
// *********************************************************************//
  IOutputDataSelectionDisp = dispinterface
    ['{F7500348-92A7-4351-8F8D-CE909D5264F8}']
    property Month: Integer readonly dispid 103;
    property Units: TOutputUnits readonly dispid 104;
    property ValueType: TOutputValueType readonly dispid 105;
    property Highlight: WordBool readonly dispid 107;
    property DisplayMonth: Integer readonly dispid 108;
    property AverageType: TOutputAverageType readonly dispid 109;
    property AverageStartDate: TDateTime readonly dispid 110;
    property AverageEndDate: TDateTime readonly dispid 111;
    property DecisionMonth: Integer readonly dispid 112;
    property PlotOption: TOutputPlotOptions readonly dispid 113;
    property YearsToSkip: Integer readonly dispid 114;
    property LoadCase: Integer dispid 115;
    property Sequence: Integer dispid 102;
    property ApplySensitivity: TSensitivity dispid 101;
    property Sensitivity: Double dispid 116;
    property PercSensitivity: Double dispid 117;
    property TimeStep: TOutputTimeStep dispid 106;
  end;

// *********************************************************************//
// Interface: IUserCategory
// Flags:     (320) Dual OleAutomation
// GUID:      {1C26657F-E973-4983-B0A3-399EBD46247C}
// *********************************************************************//
  IUserCategory = interface(IUnknown)
    ['{1C26657F-E973-4983-B0A3-399EBD46247C}']
    function Get_CategoryID: Integer; safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const AValue: WideString); safecall;
    function Get_DistributionByIndex(AInteger: Integer): Double; safecall;
    procedure Set_DistributionByIndex(AInteger: Integer; AValue: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property CategoryID: Integer read Get_CategoryID;
    property Description: WideString read Get_Description write Set_Description;
    property DistributionByIndex[AInteger: Integer]: Double read Get_DistributionByIndex write Set_DistributionByIndex;
  end;

// *********************************************************************//
// DispIntf:  IUserCategoryDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {1C26657F-E973-4983-B0A3-399EBD46247C}
// *********************************************************************//
  IUserCategoryDisp = dispinterface
    ['{1C26657F-E973-4983-B0A3-399EBD46247C}']
    property CategoryID: Integer readonly dispid 201;
    property Description: WideString dispid 202;
    property DistributionByIndex[AInteger: Integer]: Double dispid 203;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 111;
  end;

// *********************************************************************//
// Interface: IAllocationLevel
// Flags:     (320) Dual OleAutomation
// GUID:      {943AA3F2-FE5B-45D2-A0DB-126CA039D484}
// *********************************************************************//
  IAllocationLevel = interface(IUnknown)
    ['{943AA3F2-FE5B-45D2-A0DB-126CA039D484}']
    function Get_AllocationLevelID: Integer; safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const AValue: WideString); safecall;
    function Get_CurtailmentByIndex(AIndex: Integer): Double; safecall;
    procedure Set_CurtailmentByIndex(AIndex: Integer; AValue: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property AllocationLevelID: Integer read Get_AllocationLevelID;
    property Description: WideString read Get_Description write Set_Description;
    property CurtailmentByIndex[AIndex: Integer]: Double read Get_CurtailmentByIndex write Set_CurtailmentByIndex;
  end;

// *********************************************************************//
// DispIntf:  IAllocationLevelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {943AA3F2-FE5B-45D2-A0DB-126CA039D484}
// *********************************************************************//
  IAllocationLevelDisp = dispinterface
    ['{943AA3F2-FE5B-45D2-A0DB-126CA039D484}']
    property AllocationLevelID: Integer readonly dispid 101;
    property Description: WideString dispid 102;
    property CurtailmentByIndex[AIndex: Integer]: Double dispid 103;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 111;
  end;

// *********************************************************************//
// Interface: ICoefficient
// Flags:     (320) Dual OleAutomation
// GUID:      {C29BCBFB-5CD5-4842-A4EA-3DC84BB2A7D3}
// *********************************************************************//
  ICoefficient = interface(IUnknown)
    ['{C29BCBFB-5CD5-4842-A4EA-3DC84BB2A7D3}']
    function Get_TargetDraft: Double; safecall;
    procedure Set_TargetDraft(AValue: Double); safecall;
    function Get_CoefficientA: Double; safecall;
    procedure Set_CoefficientA(AValue: Double); safecall;
    function Get_CoefficientB: Double; safecall;
    procedure Set_CoefficientB(AValue: Double); safecall;
    function Get_CoefficientC: Double; safecall;
    procedure Set_CoefficientC(AValue: Double); safecall;
    function Get_CoefficientD: Double; safecall;
    procedure Set_CoefficientD(AValue: Double); safecall;
    function Get_Risk: Double; safecall;
    procedure Set_Risk(AValue: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property TargetDraft: Double read Get_TargetDraft write Set_TargetDraft;
    property CoefficientA: Double read Get_CoefficientA write Set_CoefficientA;
    property CoefficientB: Double read Get_CoefficientB write Set_CoefficientB;
    property CoefficientC: Double read Get_CoefficientC write Set_CoefficientC;
    property CoefficientD: Double read Get_CoefficientD write Set_CoefficientD;
    property Risk: Double read Get_Risk write Set_Risk;
  end;

// *********************************************************************//
// DispIntf:  ICoefficientDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {C29BCBFB-5CD5-4842-A4EA-3DC84BB2A7D3}
// *********************************************************************//
  ICoefficientDisp = dispinterface
    ['{C29BCBFB-5CD5-4842-A4EA-3DC84BB2A7D3}']
    property TargetDraft: Double dispid 201;
    property CoefficientA: Double dispid 202;
    property CoefficientB: Double dispid 203;
    property CoefficientC: Double dispid 204;
    property CoefficientD: Double dispid 205;
    property Risk: Double dispid 206;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 101;
  end;

// *********************************************************************//
// Interface: ISubSystem
// Flags:     (320) Dual OleAutomation
// GUID:      {6BD2ABF8-CB50-4B8E-8D13-D0EA1A66B340}
// *********************************************************************//
  ISubSystem = interface(IUnknown)
    ['{6BD2ABF8-CB50-4B8E-8D13-D0EA1A66B340}']
    function Get_SubSystemID: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const AValue: WideString); safecall;
    function Get_StartMonth: Integer; safecall;
    procedure Set_StartMonth(AValue: Integer); safecall;
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(AValue: Integer); safecall;
    function Get_EndMonth: Integer; safecall;
    procedure Set_EndMonth(AValue: Integer); safecall;
    function Get_EndYear: Integer; safecall;
    procedure Set_EndYear(AValue: Integer); safecall;
    function Get_SubtractID: Integer; safecall;
    procedure Set_SubtractID(AValue: Integer); safecall;
    function Get_SupportID: Integer; safecall;
    procedure Set_SupportID(AValue: Integer); safecall;
    function Get_ShortTermYield: Double; safecall;
    procedure Set_ShortTermYield(AValue: Double); safecall;
    function Get_LongTermYield: Double; safecall;
    procedure Set_LongTermYield(AValue: Double); safecall;
    function Get_LowestStreamFlow: Double; safecall;
    procedure Set_LowestStreamFlow(AValue: Double); safecall;
    function Get_FirmYield: WordBool; safecall;
    procedure Set_FirmYield(AValue: WordBool); safecall;
    function Get_ReservoirNrs: WideString; safecall;
    procedure Set_ReservoirNrs(const AValue: WideString); safecall;
    function Get_SupportCalcType: Integer; safecall;
    procedure Set_SupportCalcType(AValue: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_CoefficientByPercCurveCase(APercIndex: Integer; ACurveIndex: Integer;
                                            ACaseIndex: Integer): ICoefficient; safecall;
    procedure Set_CoefficientByPercCurveCase(APercIndex: Integer; ACurveIndex: Integer;
                                             ACaseIndex: Integer; const Value: ICoefficient); safecall;
    function Get_RoutingChannelNrByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_RoutingChannelNrByIndex(AIndex: Integer; AValue: Integer); safecall;
    function Get_SupportChannelNr: Integer; safecall;
    procedure Set_SupportChannelNr(AValue: Integer); safecall;
    function Get_Order: Integer; safecall;
    procedure Set_Order(AValue: Integer); safecall;
    property SubSystemID: Integer read Get_SubSystemID;
    property Name: WideString read Get_Name write Set_Name;
    property StartMonth: Integer read Get_StartMonth write Set_StartMonth;
    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property EndMonth: Integer read Get_EndMonth write Set_EndMonth;
    property EndYear: Integer read Get_EndYear write Set_EndYear;
    property SubtractID: Integer read Get_SubtractID write Set_SubtractID;
    property SupportID: Integer read Get_SupportID write Set_SupportID;
    property ShortTermYield: Double read Get_ShortTermYield write Set_ShortTermYield;
    property LongTermYield: Double read Get_LongTermYield write Set_LongTermYield;
    property LowestStreamFlow: Double read Get_LowestStreamFlow write Set_LowestStreamFlow;
    property FirmYield: WordBool read Get_FirmYield write Set_FirmYield;
    property ReservoirNrs: WideString read Get_ReservoirNrs write Set_ReservoirNrs;
    property SupportCalcType: Integer read Get_SupportCalcType write Set_SupportCalcType;
    property CoefficientByPercCurveCase[APercIndex: Integer; ACurveIndex: Integer;
                                        ACaseIndex: Integer]: ICoefficient read Get_CoefficientByPercCurveCase write Set_CoefficientByPercCurveCase;
    property RoutingChannelNrByIndex[AIndex: Integer]: Integer read Get_RoutingChannelNrByIndex write Set_RoutingChannelNrByIndex;
    property SupportChannelNr: Integer read Get_SupportChannelNr write Set_SupportChannelNr;
    property Order: Integer read Get_Order write Set_Order;
  end;

// *********************************************************************//
// DispIntf:  ISubSystemDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6BD2ABF8-CB50-4B8E-8D13-D0EA1A66B340}
// *********************************************************************//
  ISubSystemDisp = dispinterface
    ['{6BD2ABF8-CB50-4B8E-8D13-D0EA1A66B340}']
    property SubSystemID: Integer readonly dispid 201;
    property Name: WideString dispid 202;
    property StartMonth: Integer dispid 203;
    property StartYear: Integer dispid 204;
    property EndMonth: Integer dispid 205;
    property EndYear: Integer dispid 101;
    property SubtractID: Integer dispid 102;
    property SupportID: Integer dispid 103;
    property ShortTermYield: Double dispid 104;
    property LongTermYield: Double dispid 105;
    property LowestStreamFlow: Double dispid 106;
    property FirmYield: WordBool dispid 107;
    property ReservoirNrs: WideString dispid 108;
    property SupportCalcType: Integer dispid 110;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 111;
    property CoefficientByPercCurveCase[APercIndex: Integer; ACurveIndex: Integer;
                                        ACaseIndex: Integer]: ICoefficient dispid 112;
    property RoutingChannelNrByIndex[AIndex: Integer]: Integer dispid 109;
    property SupportChannelNr: Integer dispid 113;
    property Order: Integer dispid 114;
  end;

// *********************************************************************//
// Interface: ISupportChannel
// Flags:     (320) Dual OleAutomation
// GUID:      {2D28477F-9E75-4A50-AD4A-6FFE0FA626FD}
// *********************************************************************//
  ISupportChannel = interface(IUnknown)
    ['{2D28477F-9E75-4A50-AD4A-6FFE0FA626FD}']
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(AValue: Integer); safecall;
    function Get_NrOfCntrlSubSystems: Integer; safecall;
    function Get_SubSystemIDByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_SubSystemIDByIndex(AIndex: Integer; AValue: Integer); safecall;
    function Get_SubSystemFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_SubSystemFactorByIndex(AIndex: Integer; AValue: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_SupportChannelID: Integer; safecall;
    function NewControllingSubSystem: Integer; safecall;
    function RemoveControllingSubSystem(AIndex: Integer): WordBool; safecall;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property NrOfCntrlSubSystems: Integer read Get_NrOfCntrlSubSystems;
    property SubSystemIDByIndex[AIndex: Integer]: Integer read Get_SubSystemIDByIndex write Set_SubSystemIDByIndex;
    property SubSystemFactorByIndex[AIndex: Integer]: Double read Get_SubSystemFactorByIndex write Set_SubSystemFactorByIndex;
    property SupportChannelID: Integer read Get_SupportChannelID;
  end;

// *********************************************************************//
// DispIntf:  ISupportChannelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {2D28477F-9E75-4A50-AD4A-6FFE0FA626FD}
// *********************************************************************//
  ISupportChannelDisp = dispinterface
    ['{2D28477F-9E75-4A50-AD4A-6FFE0FA626FD}']
    property ChannelNumber: Integer dispid 103;
    property NrOfCntrlSubSystems: Integer readonly dispid 104;
    property SubSystemIDByIndex[AIndex: Integer]: Integer dispid 105;
    property SubSystemFactorByIndex[AIndex: Integer]: Double dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
    property SupportChannelID: Integer readonly dispid 101;
    function NewControllingSubSystem: Integer; dispid 102;
    function RemoveControllingSubSystem(AIndex: Integer): WordBool; dispid 108;
  end;

// *********************************************************************//
// Interface: ISupportSubSystem
// Flags:     (320) Dual OleAutomation
// GUID:      {DFABB0B2-5CB1-4185-8E85-E9ADF8DC53A8}
// *********************************************************************//
  ISupportSubSystem = interface(IUnknown)
    ['{DFABB0B2-5CB1-4185-8E85-E9ADF8DC53A8}']
    function Get_SupportSubSystemID: Integer; safecall;
    function Get_SubSystemID: Integer; safecall;
    procedure Set_SubSystemID(AValue: Integer); safecall;
    function Get_SupportChannelNrByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_SupportChannelNrByIndex(AIndex: Integer; AValue: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property SupportSubSystemID: Integer read Get_SupportSubSystemID;
    property SubSystemID: Integer read Get_SubSystemID write Set_SubSystemID;
    property SupportChannelNrByIndex[AIndex: Integer]: Integer read Get_SupportChannelNrByIndex write Set_SupportChannelNrByIndex;
  end;

// *********************************************************************//
// DispIntf:  ISupportSubSystemDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DFABB0B2-5CB1-4185-8E85-E9ADF8DC53A8}
// *********************************************************************//
  ISupportSubSystemDisp = dispinterface
    ['{DFABB0B2-5CB1-4185-8E85-E9ADF8DC53A8}']
    property SupportSubSystemID: Integer readonly dispid 101;
    property SubSystemID: Integer dispid 102;
    property SupportChannelNrByIndex[AIndex: Integer]: Integer dispid 103;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 104;
  end;

// *********************************************************************//
// Interface: IDemandDefinition
// Flags:     (320) Dual OleAutomation
// GUID:      {343494D1-4D53-41BB-8198-5AD1ACFAEC6E}
// *********************************************************************//
  IDemandDefinition = interface(IUnknown)
    ['{343494D1-4D53-41BB-8198-5AD1ACFAEC6E}']
    function Get_DemandDefID: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const AValue: WideString); safecall;
    function Get_ParentSubSystemID: Integer; safecall;
    procedure Set_ParentSubSystemID(AValue: Integer); safecall;
    function Get_GrowthType: Integer; safecall;
    procedure Set_GrowthType(AValue: Integer); safecall;
    function Get_TargetDemand: Double; safecall;
    procedure Set_TargetDemand(AValue: Double); safecall;
    function Get_UserCategoryID: Integer; safecall;
    procedure Set_UserCategoryID(AValue: Integer); safecall;
    function Get_SupportArc1: Integer; safecall;
    procedure Set_SupportArc1(AValue: Integer); safecall;
    function Get_SupportArc2: Integer; safecall;
    procedure Set_SupportArc2(AValue: Integer); safecall;
    function Get_NrOfSupportSubSystems: Integer; safecall;
    function Get_SupportSubSystemByID(AID: Integer): ISupportSubSystem; safecall;
    function Get_SupportSubSystemByIndex(AIndex: Integer): ISupportSubSystem; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function NewSupportSubSystem: ISupportSubSystem; safecall;
    function RemoveSupportSubSystem(AID: Integer): WordBool; safecall;
    function Get_Order: Integer; safecall;
    procedure Set_Order(AValue: Integer); safecall;
    function Get_DemandCentreID: Integer; safecall;
    procedure Set_DemandCentreID(Value: Integer); safecall;
    property DemandDefID: Integer read Get_DemandDefID;
    property Name: WideString read Get_Name write Set_Name;
    property ParentSubSystemID: Integer read Get_ParentSubSystemID write Set_ParentSubSystemID;
    property GrowthType: Integer read Get_GrowthType write Set_GrowthType;
    property TargetDemand: Double read Get_TargetDemand write Set_TargetDemand;
    property UserCategoryID: Integer read Get_UserCategoryID write Set_UserCategoryID;
    property SupportArc1: Integer read Get_SupportArc1 write Set_SupportArc1;
    property SupportArc2: Integer read Get_SupportArc2 write Set_SupportArc2;
    property NrOfSupportSubSystems: Integer read Get_NrOfSupportSubSystems;
    property SupportSubSystemByID[AID: Integer]: ISupportSubSystem read Get_SupportSubSystemByID;
    property SupportSubSystemByIndex[AIndex: Integer]: ISupportSubSystem read Get_SupportSubSystemByIndex;
    property Order: Integer read Get_Order write Set_Order;
    property DemandCentreID: Integer read Get_DemandCentreID write Set_DemandCentreID;
  end;

// *********************************************************************//
// DispIntf:  IDemandDefinitionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {343494D1-4D53-41BB-8198-5AD1ACFAEC6E}
// *********************************************************************//
  IDemandDefinitionDisp = dispinterface
    ['{343494D1-4D53-41BB-8198-5AD1ACFAEC6E}']
    property DemandDefID: Integer readonly dispid 103;
    property Name: WideString dispid 104;
    property ParentSubSystemID: Integer dispid 105;
    property GrowthType: Integer dispid 106;
    property TargetDemand: Double dispid 107;
    property UserCategoryID: Integer dispid 108;
    property SupportArc1: Integer dispid 109;
    property SupportArc2: Integer dispid 110;
    property NrOfSupportSubSystems: Integer readonly dispid 111;
    property SupportSubSystemByID[AID: Integer]: ISupportSubSystem readonly dispid 112;
    property SupportSubSystemByIndex[AIndex: Integer]: ISupportSubSystem readonly dispid 113;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 101;
    function NewSupportSubSystem: ISupportSubSystem; dispid 102;
    function RemoveSupportSubSystem(AID: Integer): WordBool; dispid 114;
    property Order: Integer dispid 115;
    property DemandCentreID: Integer dispid 116;
  end;

// *********************************************************************//
// Interface: IAllocationDefinition
// Flags:     (320) Dual OleAutomation
// GUID:      {CB6E149E-EC34-46CC-995A-328C095A16FB}
// *********************************************************************//
  IAllocationDefinition = interface(IUnknown)
    ['{CB6E149E-EC34-46CC-995A-328C095A16FB}']
    function Get_AllocationDefinitionID: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const AValue: WideString); safecall;
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(AValue: Integer); safecall;
    function Get_StartMonth: Integer; safecall;
    procedure Set_StartMonth(AValue: Integer); safecall;
    function Get_NrOfCategories: Integer; safecall;
    function Get_NrOfReliabilityClasses: Integer; safecall;
    procedure Set_NrOfReliabilityClasses(AValue: Integer); safecall;
    function Get_NrOfAllocationLevels: Integer; safecall;
    function Get_PeriodLength: Integer; safecall;
    procedure Set_PeriodLength(AValue: Integer); safecall;
    function Get_NrOfLoadCases: Integer; safecall;
    procedure Set_NrOfLoadCases(AValue: Integer); safecall;
    function Get_NrOfStartingPercentages: Integer; safecall;
    procedure Set_NrOfStartingPercentages(AValue: Integer); safecall;
    function Get_NrOfSubSystems: Integer; safecall;
    function Get_NrOfCurveSets: Integer; safecall;
    procedure Set_NrOfCurveSets(AValue: Integer); safecall;
    function Get_RecurrenceIntervalByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_RecurrenceIntervalByIndex(AIndex: Integer; AValue: Integer); safecall;
    function Get_RILabelByIndex(AIndex: Integer): WideString; safecall;
    procedure Set_RILabelByIndex(AIndex: Integer; const AValue: WideString); safecall;
    function Get_CategoryByIndex(AIndex: Integer): IUserCategory; safecall;
    function Get_CategoryByID(AID: Integer): IUserCategory; safecall;
    function Get_AllocationLevelByIndex(AIndex: Integer): IAllocationLevel; safecall;
    function Get_SubSystemByIndex(AIndex: Integer): ISubSystem; safecall;
    function Get_SubSystemByID(AIndex: Integer): ISubSystem; safecall;
    function Get_DecisionCurveSetByMonth(AMonth: Integer): Integer; safecall;
    procedure Set_DecisionCurveSetByMonth(AMonth: Integer; AValue: Integer); safecall;
    function Get_StartingPercentageByIndex(AIndex: Integer): Double; safecall;
    procedure Set_StartingPercentageByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_NrInFixedPosition: Integer; safecall;
    function Get_FixedPositionByIndex(AIndex: Integer): IFixedPosition; safecall;
    function Get_NrInSpecificOrder: Integer; safecall;
    function Get_NrOfSupportChannels: Integer; safecall;
    function Get_SupportChannelByIndex(AIndex: Integer): ISupportChannel; safecall;
    function Get_NrOfDemandDefinitions: Integer; safecall;
    function Get_DemandDefinitionByIndex(AIndex: Integer): IDemandDefinition; safecall;
    function NewUserCategory: IUserCategory; safecall;
    function RemoveUserCategory(AID: Integer): WordBool; safecall;
    function NewAllocationLevel: IAllocationLevel; safecall;
    function RemoveAllocationLevel(AID: Integer): WordBool; safecall;
    function NewSubSystem: ISubSystem; safecall;
    function RemoveSubSystem(AID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function NewFixedPosition: IFixedPosition; safecall;
    function RemoveFixedPosition(AID: Integer): WordBool; safecall;
    function NewSpecificOrder: ISpecificOrder; safecall;
    function RemoveSpecificOrder(AID: Integer): WordBool; safecall;
    function Get_SupportChannelByID(AID: Integer): ISupportChannel; safecall;
    function NewSupportChannel: ISupportChannel; safecall;
    function RemoveSupportChannel(AID: Integer): WordBool; safecall;
    function Get_SupportStrategy: Integer; safecall;
    procedure Set_SupportStrategy(Value: Integer); safecall;
    function Get_BalancingOption: Integer; safecall;
    procedure Set_BalancingOption(Value: Integer); safecall;
    function Get_DemandDefinitionByID(AID: Integer): IDemandDefinition; safecall;
    function NewDemandDefinition: IDemandDefinition; safecall;
    function RemoveDemandDefinition(AID: Integer): WordBool; safecall;
    function Get_AllocDefFileName: WideString; safecall;
    procedure Set_AllocDefFileName(const Value: WideString); safecall;
    function Get_FixedPositionByID(AID: Integer): IFixedPosition; safecall;
    function Get_SpecificOrderByIndex(AIndex: Integer): ISpecificOrder; safecall;
    function Get_SpecificOrderByID(AID: Integer): ISpecificOrder; safecall;
    property AllocationDefinitionID: Integer read Get_AllocationDefinitionID;
    property Name: WideString read Get_Name write Set_Name;
    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property StartMonth: Integer read Get_StartMonth write Set_StartMonth;
    property NrOfCategories: Integer read Get_NrOfCategories;
    property NrOfReliabilityClasses: Integer read Get_NrOfReliabilityClasses write Set_NrOfReliabilityClasses;
    property NrOfAllocationLevels: Integer read Get_NrOfAllocationLevels;
    property PeriodLength: Integer read Get_PeriodLength write Set_PeriodLength;
    property NrOfLoadCases: Integer read Get_NrOfLoadCases write Set_NrOfLoadCases;
    property NrOfStartingPercentages: Integer read Get_NrOfStartingPercentages write Set_NrOfStartingPercentages;
    property NrOfSubSystems: Integer read Get_NrOfSubSystems;
    property NrOfCurveSets: Integer read Get_NrOfCurveSets write Set_NrOfCurveSets;
    property RecurrenceIntervalByIndex[AIndex: Integer]: Integer read Get_RecurrenceIntervalByIndex write Set_RecurrenceIntervalByIndex;
    property RILabelByIndex[AIndex: Integer]: WideString read Get_RILabelByIndex write Set_RILabelByIndex;
    property CategoryByIndex[AIndex: Integer]: IUserCategory read Get_CategoryByIndex;
    property CategoryByID[AID: Integer]: IUserCategory read Get_CategoryByID;
    property AllocationLevelByIndex[AIndex: Integer]: IAllocationLevel read Get_AllocationLevelByIndex;
    property SubSystemByIndex[AIndex: Integer]: ISubSystem read Get_SubSystemByIndex;
    property SubSystemByID[AIndex: Integer]: ISubSystem read Get_SubSystemByID;
    property DecisionCurveSetByMonth[AMonth: Integer]: Integer read Get_DecisionCurveSetByMonth write Set_DecisionCurveSetByMonth;
    property StartingPercentageByIndex[AIndex: Integer]: Double read Get_StartingPercentageByIndex write Set_StartingPercentageByIndex;
    property NrInFixedPosition: Integer read Get_NrInFixedPosition;
    property FixedPositionByIndex[AIndex: Integer]: IFixedPosition read Get_FixedPositionByIndex;
    property NrInSpecificOrder: Integer read Get_NrInSpecificOrder;
    property NrOfSupportChannels: Integer read Get_NrOfSupportChannels;
    property SupportChannelByIndex[AIndex: Integer]: ISupportChannel read Get_SupportChannelByIndex;
    property NrOfDemandDefinitions: Integer read Get_NrOfDemandDefinitions;
    property DemandDefinitionByIndex[AIndex: Integer]: IDemandDefinition read Get_DemandDefinitionByIndex;
    property SupportChannelByID[AID: Integer]: ISupportChannel read Get_SupportChannelByID;
    property SupportStrategy: Integer read Get_SupportStrategy write Set_SupportStrategy;
    property BalancingOption: Integer read Get_BalancingOption write Set_BalancingOption;
    property DemandDefinitionByID[AID: Integer]: IDemandDefinition read Get_DemandDefinitionByID;
    property AllocDefFileName: WideString read Get_AllocDefFileName write Set_AllocDefFileName;
    property FixedPositionByID[AID: Integer]: IFixedPosition read Get_FixedPositionByID;
    property SpecificOrderByIndex[AIndex: Integer]: ISpecificOrder read Get_SpecificOrderByIndex;
    property SpecificOrderByID[AID: Integer]: ISpecificOrder read Get_SpecificOrderByID;
  end;

// *********************************************************************//
// DispIntf:  IAllocationDefinitionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {CB6E149E-EC34-46CC-995A-328C095A16FB}
// *********************************************************************//
  IAllocationDefinitionDisp = dispinterface
    ['{CB6E149E-EC34-46CC-995A-328C095A16FB}']
    property AllocationDefinitionID: Integer readonly dispid 101;
    property Name: WideString dispid 102;
    property StartYear: Integer dispid 103;
    property StartMonth: Integer dispid 104;
    property NrOfCategories: Integer readonly dispid 107;
    property NrOfReliabilityClasses: Integer dispid 108;
    property NrOfAllocationLevels: Integer readonly dispid 109;
    property PeriodLength: Integer dispid 110;
    property NrOfLoadCases: Integer dispid 111;
    property NrOfStartingPercentages: Integer dispid 112;
    property NrOfSubSystems: Integer readonly dispid 113;
    property NrOfCurveSets: Integer dispid 114;
    property RecurrenceIntervalByIndex[AIndex: Integer]: Integer dispid 115;
    property RILabelByIndex[AIndex: Integer]: WideString dispid 116;
    property CategoryByIndex[AIndex: Integer]: IUserCategory readonly dispid 117;
    property CategoryByID[AID: Integer]: IUserCategory readonly dispid 118;
    property AllocationLevelByIndex[AIndex: Integer]: IAllocationLevel readonly dispid 119;
    property SubSystemByIndex[AIndex: Integer]: ISubSystem readonly dispid 120;
    property SubSystemByID[AIndex: Integer]: ISubSystem readonly dispid 121;
    property DecisionCurveSetByMonth[AMonth: Integer]: Integer dispid 122;
    property StartingPercentageByIndex[AIndex: Integer]: Double dispid 123;
    property NrInFixedPosition: Integer readonly dispid 124;
    property FixedPositionByIndex[AIndex: Integer]: IFixedPosition readonly dispid 126;
    property NrInSpecificOrder: Integer readonly dispid 127;
    property NrOfSupportChannels: Integer readonly dispid 130;
    property SupportChannelByIndex[AIndex: Integer]: ISupportChannel readonly dispid 131;
    property NrOfDemandDefinitions: Integer readonly dispid 132;
    property DemandDefinitionByIndex[AIndex: Integer]: IDemandDefinition readonly dispid 133;
    function NewUserCategory: IUserCategory; dispid 134;
    function RemoveUserCategory(AID: Integer): WordBool; dispid 135;
    function NewAllocationLevel: IAllocationLevel; dispid 136;
    function RemoveAllocationLevel(AID: Integer): WordBool; dispid 137;
    function NewSubSystem: ISubSystem; dispid 138;
    function RemoveSubSystem(AID: Integer): WordBool; dispid 139;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 140;
    function NewFixedPosition: IFixedPosition; dispid 141;
    function RemoveFixedPosition(AID: Integer): WordBool; dispid 142;
    function NewSpecificOrder: ISpecificOrder; dispid 143;
    function RemoveSpecificOrder(AID: Integer): WordBool; dispid 144;
    property SupportChannelByID[AID: Integer]: ISupportChannel readonly dispid 145;
    function NewSupportChannel: ISupportChannel; dispid 146;
    function RemoveSupportChannel(AID: Integer): WordBool; dispid 147;
    property SupportStrategy: Integer dispid 148;
    property BalancingOption: Integer dispid 149;
    property DemandDefinitionByID[AID: Integer]: IDemandDefinition readonly dispid 150;
    function NewDemandDefinition: IDemandDefinition; dispid 151;
    function RemoveDemandDefinition(AID: Integer): WordBool; dispid 152;
    property AllocDefFileName: WideString dispid 105;
    property FixedPositionByID[AID: Integer]: IFixedPosition readonly dispid 106;
    property SpecificOrderByIndex[AIndex: Integer]: ISpecificOrder readonly dispid 125;
    property SpecificOrderByID[AID: Integer]: ISpecificOrder readonly dispid 128;
  end;

// *********************************************************************//
// Interface: IAllocationDefinitionsList
// Flags:     (320) Dual OleAutomation
// GUID:      {DD7DB287-FE2C-4911-BBF8-C37E24E32F3E}
// *********************************************************************//
  IAllocationDefinitionsList = interface(IUnknown)
    ['{DD7DB287-FE2C-4911-BBF8-C37E24E32F3E}']
    function Get_AllocationDefinitionCount: Integer; safecall;
    function Get_AllocationDefinitionByIndex(AIndex: Integer): IAllocationDefinition; safecall;
    function Get_AllocationDefinitionByID(AID: Integer): IAllocationDefinition; safecall;
    function NewAllocationDefinition: IAllocationDefinition; safecall;
    function RemoveAllocationDefinitionWithID(AID: Integer): WordBool; safecall;
    property AllocationDefinitionCount: Integer read Get_AllocationDefinitionCount;
    property AllocationDefinitionByIndex[AIndex: Integer]: IAllocationDefinition read Get_AllocationDefinitionByIndex;
    property AllocationDefinitionByID[AID: Integer]: IAllocationDefinition read Get_AllocationDefinitionByID;
  end;

// *********************************************************************//
// DispIntf:  IAllocationDefinitionsListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DD7DB287-FE2C-4911-BBF8-C37E24E32F3E}
// *********************************************************************//
  IAllocationDefinitionsListDisp = dispinterface
    ['{DD7DB287-FE2C-4911-BBF8-C37E24E32F3E}']
    property AllocationDefinitionCount: Integer readonly dispid 101;
    property AllocationDefinitionByIndex[AIndex: Integer]: IAllocationDefinition readonly dispid 102;
    property AllocationDefinitionByID[AID: Integer]: IAllocationDefinition readonly dispid 103;
    function NewAllocationDefinition: IAllocationDefinition; dispid 104;
    function RemoveAllocationDefinitionWithID(AID: Integer): WordBool; dispid 105;
  end;

// *********************************************************************//
// Interface: IPlanningModelData
// Flags:     (320) Dual OleAutomation
// GUID:      {BF29970A-47A7-474A-91AC-868AF9913203}
// *********************************************************************//
  IPlanningModelData = interface(IUnknown)
    ['{BF29970A-47A7-474A-91AC-868AF9913203}']
    function Get_AllocationDefinitionsList: IAllocationDefinitionsList; safecall;
    function Get_SwitchDefinitionsList: ISwitchDefinitionsList; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_TariffCalculationData: ITariffCalculationData; safecall;
    property AllocationDefinitionsList: IAllocationDefinitionsList read Get_AllocationDefinitionsList;
    property SwitchDefinitionsList: ISwitchDefinitionsList read Get_SwitchDefinitionsList;
    property TariffCalculationData: ITariffCalculationData read Get_TariffCalculationData;
  end;

// *********************************************************************//
// DispIntf:  IPlanningModelDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {BF29970A-47A7-474A-91AC-868AF9913203}
// *********************************************************************//
  IPlanningModelDataDisp = dispinterface
    ['{BF29970A-47A7-474A-91AC-868AF9913203}']
    property AllocationDefinitionsList: IAllocationDefinitionsList readonly dispid 201;
    property SwitchDefinitionsList: ISwitchDefinitionsList readonly dispid 101;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    property TariffCalculationData: ITariffCalculationData readonly dispid 102;
  end;

// *********************************************************************//
// Interface: IPreprocessor
// Flags:     (320) Dual OleAutomation
// GUID:      {5EFEF2C0-6FB5-4DE0-9045-120BA75B0601}
// *********************************************************************//
  IPreprocessor = interface(IUnknown)
    ['{5EFEF2C0-6FB5-4DE0-9045-120BA75B0601}']
  end;

// *********************************************************************//
// DispIntf:  IPreprocessorDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {5EFEF2C0-6FB5-4DE0-9045-120BA75B0601}
// *********************************************************************//
  IPreprocessorDisp = dispinterface
    ['{5EFEF2C0-6FB5-4DE0-9045-120BA75B0601}']
  end;

// *********************************************************************//
// Interface: IPreProcessorData
// Flags:     (320) Dual OleAutomation
// GUID:      {08484FED-6C2F-4CC5-A25B-EB29B4FA52CD}
// *********************************************************************//
  IPreProcessorData = interface(IUnknown)
    ['{08484FED-6C2F-4CC5-A25B-EB29B4FA52CD}']
  end;

// *********************************************************************//
// DispIntf:  IPreProcessorDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {08484FED-6C2F-4CC5-A25B-EB29B4FA52CD}
// *********************************************************************//
  IPreProcessorDataDisp = dispinterface
    ['{08484FED-6C2F-4CC5-A25B-EB29B4FA52CD}']
  end;

// *********************************************************************//
// Interface: IFixedPosition
// Flags:     (320) Dual OleAutomation
// GUID:      {96C94A5D-57CB-4557-A178-2D3C58DD4951}
// *********************************************************************//
  IFixedPosition = interface(IUnknown)
    ['{96C94A5D-57CB-4557-A178-2D3C58DD4951}']
    function Get_FixedPositionNr: Integer; safecall;
    procedure Set_FixedPositionNr(AValue: Integer); safecall;
    function Get_FixedPosSubSystemID: Integer; safecall;
    procedure Set_FixedPosSubSystemID(AValue: Integer); safecall;
    function Get_FixedPositionID: Integer; safecall;
    property FixedPositionNr: Integer read Get_FixedPositionNr write Set_FixedPositionNr;
    property FixedPosSubSystemID: Integer read Get_FixedPosSubSystemID write Set_FixedPosSubSystemID;
    property FixedPositionID: Integer read Get_FixedPositionID;
  end;

// *********************************************************************//
// DispIntf:  IFixedPositionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {96C94A5D-57CB-4557-A178-2D3C58DD4951}
// *********************************************************************//
  IFixedPositionDisp = dispinterface
    ['{96C94A5D-57CB-4557-A178-2D3C58DD4951}']
    property FixedPositionNr: Integer dispid 201;
    property FixedPosSubSystemID: Integer dispid 202;
    property FixedPositionID: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: ISpecificOrder
// Flags:     (320) Dual OleAutomation
// GUID:      {38074998-F70E-445C-9AF5-22BFB232B3C7}
// *********************************************************************//
  ISpecificOrder = interface(IUnknown)
    ['{38074998-F70E-445C-9AF5-22BFB232B3C7}']
    function Get_BeforeSubSystemID: Integer; safecall;
    procedure Set_BeforeSubSystemID(AValue: Integer); safecall;
    function Get_AfterSubSystemID: Integer; safecall;
    procedure Set_AfterSubSystemID(AValue: Integer); safecall;
    function Get_SpecificOrderID: Integer; safecall;
    property BeforeSubSystemID: Integer read Get_BeforeSubSystemID write Set_BeforeSubSystemID;
    property AfterSubSystemID: Integer read Get_AfterSubSystemID write Set_AfterSubSystemID;
    property SpecificOrderID: Integer read Get_SpecificOrderID;
  end;

// *********************************************************************//
// DispIntf:  ISpecificOrderDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {38074998-F70E-445C-9AF5-22BFB232B3C7}
// *********************************************************************//
  ISpecificOrderDisp = dispinterface
    ['{38074998-F70E-445C-9AF5-22BFB232B3C7}']
    property BeforeSubSystemID: Integer dispid 201;
    property AfterSubSystemID: Integer dispid 202;
    property SpecificOrderID: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: IReservoirTimeControl
// Flags:     (320) Dual OleAutomation
// GUID:      {01BD48AF-CC96-437A-8336-56A1D9B7B085}
// *********************************************************************//
  IReservoirTimeControl = interface(IUnknown)
    ['{01BD48AF-CC96-437A-8336-56A1D9B7B085}']
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(Value: Integer); safecall;
    function Get_StartMonth: Integer; safecall;
    procedure Set_StartMonth(Value: Integer); safecall;
    function Get_EndYear: Integer; safecall;
    procedure Set_EndYear(Value: Integer); safecall;
    function Get_EndMonth: Integer; safecall;
    procedure Set_EndMonth(Value: Integer); safecall;
    function Get_EconomicLife: Integer; safecall;
    procedure Set_EconomicLife(Value: Integer); safecall;
    function Get_CapitalCost: Double; safecall;
    procedure Set_CapitalCost(Value: Double); safecall;
    function Get_OMCost: Double; safecall;
    procedure Set_OMCost(Value: Double); safecall;
    function Get_YearsToConstruct: Integer; safecall;
    procedure Set_YearsToConstruct(Value: Integer); safecall;
    function Get_CostScheduleByIndex(AIndex: Integer): Double; safecall;
    procedure Set_CostScheduleByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_ReservoirNumber: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_Replacements: WideString; safecall;
    procedure AddReplacement(AResNr: Integer); safecall;
    procedure DeleteReplacement(AResNr: Integer); safecall;
    function Get_CostSchedule: WideString; safecall;
    procedure Set_CostSchedule(const Value: WideString); safecall;
    function Get_BaseNodeNumber: Integer; safecall;
    procedure Set_BaseNodeNumber(AValue: Integer); safecall;
    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property StartMonth: Integer read Get_StartMonth write Set_StartMonth;
    property EndYear: Integer read Get_EndYear write Set_EndYear;
    property EndMonth: Integer read Get_EndMonth write Set_EndMonth;
    property EconomicLife: Integer read Get_EconomicLife write Set_EconomicLife;
    property CapitalCost: Double read Get_CapitalCost write Set_CapitalCost;
    property OMCost: Double read Get_OMCost write Set_OMCost;
    property YearsToConstruct: Integer read Get_YearsToConstruct write Set_YearsToConstruct;
    property CostScheduleByIndex[AIndex: Integer]: Double read Get_CostScheduleByIndex write Set_CostScheduleByIndex;
    property ReservoirNumber: Integer read Get_ReservoirNumber;
    property Replacements: WideString read Get_Replacements;
    property CostSchedule: WideString read Get_CostSchedule write Set_CostSchedule;
    property BaseNodeNumber: Integer read Get_BaseNodeNumber write Set_BaseNodeNumber;
  end;

// *********************************************************************//
// DispIntf:  IReservoirTimeControlDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {01BD48AF-CC96-437A-8336-56A1D9B7B085}
// *********************************************************************//
  IReservoirTimeControlDisp = dispinterface
    ['{01BD48AF-CC96-437A-8336-56A1D9B7B085}']
    property StartYear: Integer dispid 201;
    property StartMonth: Integer dispid 202;
    property EndYear: Integer dispid 203;
    property EndMonth: Integer dispid 204;
    property EconomicLife: Integer dispid 205;
    property CapitalCost: Double dispid 206;
    property OMCost: Double dispid 207;
    property YearsToConstruct: Integer dispid 208;
    property CostScheduleByIndex[AIndex: Integer]: Double dispid 209;
    property ReservoirNumber: Integer readonly dispid 211;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 212;
    property Replacements: WideString readonly dispid 101;
    procedure AddReplacement(AResNr: Integer); dispid 102;
    procedure DeleteReplacement(AResNr: Integer); dispid 103;
    property CostSchedule: WideString dispid 104;
    property BaseNodeNumber: Integer dispid 105;
  end;

// *********************************************************************//
// Interface: ISwitchDefinition
// Flags:     (320) Dual OleAutomation
// GUID:      {39CD48B8-8F01-466C-9C15-565E8ACA91CE}
// *********************************************************************//
  ISwitchDefinition = interface(IUnknown)
    ['{39CD48B8-8F01-466C-9C15-565E8ACA91CE}']
    function Get_SwitchDefID: Integer; safecall;
    function Get_SwitchDefStartYear: Integer; safecall;
    procedure Set_SwitchDefStartYear(Value: Integer); safecall;
    function Get_SwitchDefStartMonth: Integer; safecall;
    procedure Set_SwitchDefStartMonth(Value: Integer); safecall;
    function Get_SwitchDefFileName: WideString; safecall;
    procedure Set_SwitchDefFileName(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property SwitchDefID: Integer read Get_SwitchDefID;
    property SwitchDefStartYear: Integer read Get_SwitchDefStartYear write Set_SwitchDefStartYear;
    property SwitchDefStartMonth: Integer read Get_SwitchDefStartMonth write Set_SwitchDefStartMonth;
    property SwitchDefFileName: WideString read Get_SwitchDefFileName write Set_SwitchDefFileName;
  end;

// *********************************************************************//
// DispIntf:  ISwitchDefinitionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {39CD48B8-8F01-466C-9C15-565E8ACA91CE}
// *********************************************************************//
  ISwitchDefinitionDisp = dispinterface
    ['{39CD48B8-8F01-466C-9C15-565E8ACA91CE}']
    property SwitchDefID: Integer readonly dispid 101;
    property SwitchDefStartYear: Integer dispid 102;
    property SwitchDefStartMonth: Integer dispid 103;
    property SwitchDefFileName: WideString dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
  end;

// *********************************************************************//
// Interface: ISwitchDefinitionsList
// Flags:     (320) Dual OleAutomation
// GUID:      {4FE7FF75-2A38-4E1A-B7CD-C2619501EBA6}
// *********************************************************************//
  ISwitchDefinitionsList = interface(IUnknown)
    ['{4FE7FF75-2A38-4E1A-B7CD-C2619501EBA6}']
    function NewSwitchDefinition: ISwitchDefinition; safecall;
    function RemoveSwitchDefinitionWithID(AID: Integer): WordBool; safecall;
    function Get_SwitchDefinitionCount: Integer; safecall;
    function Get_SwitchDefinitionByID(AID: Integer): ISwitchDefinition; safecall;
    function Get_SwitchDefinitionByIndex(AIndex: Integer): ISwitchDefinition; safecall;
    property SwitchDefinitionCount: Integer read Get_SwitchDefinitionCount;
    property SwitchDefinitionByID[AID: Integer]: ISwitchDefinition read Get_SwitchDefinitionByID;
    property SwitchDefinitionByIndex[AIndex: Integer]: ISwitchDefinition read Get_SwitchDefinitionByIndex;
  end;

// *********************************************************************//
// DispIntf:  ISwitchDefinitionsListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4FE7FF75-2A38-4E1A-B7CD-C2619501EBA6}
// *********************************************************************//
  ISwitchDefinitionsListDisp = dispinterface
    ['{4FE7FF75-2A38-4E1A-B7CD-C2619501EBA6}']
    function NewSwitchDefinition: ISwitchDefinition; dispid 101;
    function RemoveSwitchDefinitionWithID(AID: Integer): WordBool; dispid 102;
    property SwitchDefinitionCount: Integer readonly dispid 103;
    property SwitchDefinitionByID[AID: Integer]: ISwitchDefinition readonly dispid 104;
    property SwitchDefinitionByIndex[AIndex: Integer]: ISwitchDefinition readonly dispid 105;
  end;

// *********************************************************************//
// Interface: IChannelTimeControl
// Flags:     (320) Dual OleAutomation
// GUID:      {4A7D3EBC-944D-4228-9103-4D89035A17DA}
// *********************************************************************//
  IChannelTimeControl = interface(IUnknown)
    ['{4A7D3EBC-944D-4228-9103-4D89035A17DA}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ChannelNumber: Integer; safecall;
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(Value: Integer); safecall;
    function Get_StartMonth: Integer; safecall;
    procedure Set_StartMonth(Value: Integer); safecall;
    function Get_EndYear: Integer; safecall;
    procedure Set_EndYear(Value: Integer); safecall;
    function Get_EndMonth: Integer; safecall;
    procedure Set_EndMonth(Value: Integer); safecall;
    function Get_EconomicLife: Integer; safecall;
    procedure Set_EconomicLife(Value: Integer); safecall;
    function Get_CapitalCost: Double; safecall;
    procedure Set_CapitalCost(Value: Double); safecall;
    function Get_FixedOMCost: Double; safecall;
    procedure Set_FixedOMCost(Value: Double); safecall;
    function Get_VariableOMCost: Double; safecall;
    procedure Set_VariableOMCost(Value: Double); safecall;
    function Get_YearsToConstruct: Integer; safecall;
    procedure Set_YearsToConstruct(Value: Integer); safecall;
    function Get_CostSchedule: WideString; safecall;
    procedure Set_CostSchedule(const Value: WideString); safecall;
    function Get_EscalationCost: WideString; safecall;
    procedure Set_EscalationCost(const Value: WideString); safecall;
    function Get_CostScheduleByIndex(AIndex: Integer): Double; safecall;
    procedure Set_CostScheduleByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_EscalationCostValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_EscalationCostValueByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_YearsInAnalysis: Integer; safecall;
    procedure Set_YearsInAnalysis(AValue: Integer); safecall;
    property ChannelNumber: Integer read Get_ChannelNumber;
    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property StartMonth: Integer read Get_StartMonth write Set_StartMonth;
    property EndYear: Integer read Get_EndYear write Set_EndYear;
    property EndMonth: Integer read Get_EndMonth write Set_EndMonth;
    property EconomicLife: Integer read Get_EconomicLife write Set_EconomicLife;
    property CapitalCost: Double read Get_CapitalCost write Set_CapitalCost;
    property FixedOMCost: Double read Get_FixedOMCost write Set_FixedOMCost;
    property VariableOMCost: Double read Get_VariableOMCost write Set_VariableOMCost;
    property YearsToConstruct: Integer read Get_YearsToConstruct write Set_YearsToConstruct;
    property CostSchedule: WideString read Get_CostSchedule write Set_CostSchedule;
    property EscalationCost: WideString read Get_EscalationCost write Set_EscalationCost;
    property CostScheduleByIndex[AIndex: Integer]: Double read Get_CostScheduleByIndex write Set_CostScheduleByIndex;
    property EscalationCostValueByIndex[AIndex: Integer]: Double read Get_EscalationCostValueByIndex write Set_EscalationCostValueByIndex;
    property YearsInAnalysis: Integer read Get_YearsInAnalysis write Set_YearsInAnalysis;
  end;

// *********************************************************************//
// DispIntf:  IChannelTimeControlDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4A7D3EBC-944D-4228-9103-4D89035A17DA}
// *********************************************************************//
  IChannelTimeControlDisp = dispinterface
    ['{4A7D3EBC-944D-4228-9103-4D89035A17DA}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 201;
    property ChannelNumber: Integer readonly dispid 202;
    property StartYear: Integer dispid 204;
    property StartMonth: Integer dispid 205;
    property EndYear: Integer dispid 206;
    property EndMonth: Integer dispid 207;
    property EconomicLife: Integer dispid 208;
    property CapitalCost: Double dispid 209;
    property FixedOMCost: Double dispid 210;
    property VariableOMCost: Double dispid 211;
    property YearsToConstruct: Integer dispid 212;
    property CostSchedule: WideString dispid 213;
    property EscalationCost: WideString dispid 214;
    property CostScheduleByIndex[AIndex: Integer]: Double dispid 101;
    property EscalationCostValueByIndex[AIndex: Integer]: Double dispid 103;
    property YearsInAnalysis: Integer dispid 107;
  end;

// *********************************************************************//
// Interface: IChannelSwitchControl
// Flags:     (320) Dual OleAutomation
// GUID:      {A59B8604-3713-4026-98BB-D15FC12C1B76}
// *********************************************************************//
  IChannelSwitchControl = interface(IUnknown)
    ['{A59B8604-3713-4026-98BB-D15FC12C1B76}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ChannelNumber: Integer; safecall;
    function Get_AssociatedNodeNr: Integer; safecall;
    procedure Set_AssociatedNodeNr(Value: Integer); safecall;
    function Get_WaterLevel: Double; safecall;
    procedure Set_WaterLevel(Value: Double); safecall;
    function Get_SwitchType: Integer; safecall;
    procedure Set_SwitchType(Value: Integer); safecall;
    function Get_InitialStatus: Integer; safecall;
    procedure Set_InitialStatus(Value: Integer); safecall;
    function Get_SwitchDefinitionID: Integer; safecall;
    procedure Set_SwitchDefinitionID(Value: Integer); safecall;
    function Get_ChannelSwitchID: Integer; safecall;
    property ChannelNumber: Integer read Get_ChannelNumber;
    property AssociatedNodeNr: Integer read Get_AssociatedNodeNr write Set_AssociatedNodeNr;
    property WaterLevel: Double read Get_WaterLevel write Set_WaterLevel;
    property SwitchType: Integer read Get_SwitchType write Set_SwitchType;
    property InitialStatus: Integer read Get_InitialStatus write Set_InitialStatus;
    property SwitchDefinitionID: Integer read Get_SwitchDefinitionID write Set_SwitchDefinitionID;
    property ChannelSwitchID: Integer read Get_ChannelSwitchID;
  end;

// *********************************************************************//
// DispIntf:  IChannelSwitchControlDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {A59B8604-3713-4026-98BB-D15FC12C1B76}
// *********************************************************************//
  IChannelSwitchControlDisp = dispinterface
    ['{A59B8604-3713-4026-98BB-D15FC12C1B76}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 201;
    property ChannelNumber: Integer readonly dispid 202;
    property AssociatedNodeNr: Integer dispid 204;
    property WaterLevel: Double dispid 205;
    property SwitchType: Integer dispid 206;
    property InitialStatus: Integer dispid 207;
    property SwitchDefinitionID: Integer dispid 101;
    property ChannelSwitchID: Integer readonly dispid 102;
  end;

// *********************************************************************//
// Interface: IDataFilePaths
// Flags:     (320) Dual OleAutomation
// GUID:      {BEB0FBD7-E770-450B-9010-E4146F4697BA}
// *********************************************************************//
  IDataFilePaths = interface(IUnknown)
    ['{BEB0FBD7-E770-450B-9010-E4146F4697BA}']
    function Get_DataFilePrefix: WideString; safecall;
    function Get_DataFilePath: WideString; safecall;
    function Get_ParamFileName: WideString; safecall;
    function Get_OutputFilePath: WideString; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_HydrologyFilePath: WideString; safecall;
    function Get_DemandFilePath: WideString; safecall;
    property DataFilePrefix: WideString read Get_DataFilePrefix;
    property DataFilePath: WideString read Get_DataFilePath;
    property ParamFileName: WideString read Get_ParamFileName;
    property OutputFilePath: WideString read Get_OutputFilePath;
    property HydrologyFilePath: WideString read Get_HydrologyFilePath;
    property DemandFilePath: WideString read Get_DemandFilePath;
  end;

// *********************************************************************//
// DispIntf:  IDataFilePathsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {BEB0FBD7-E770-450B-9010-E4146F4697BA}
// *********************************************************************//
  IDataFilePathsDisp = dispinterface
    ['{BEB0FBD7-E770-450B-9010-E4146F4697BA}']
    property DataFilePrefix: WideString readonly dispid 101;
    property DataFilePath: WideString readonly dispid 102;
    property ParamFileName: WideString readonly dispid 103;
    property OutputFilePath: WideString readonly dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
    property HydrologyFilePath: WideString readonly dispid 106;
    property DemandFilePath: WideString readonly dispid 107;
  end;

// *********************************************************************//
// Interface: IChangeGroupElement
// Flags:     (320) Dual OleAutomation
// GUID:      {23E6A510-D8F3-4CF2-8778-8314A14D92AC}
// *********************************************************************//
  IChangeGroupElement = interface(IUnknown)
    ['{23E6A510-D8F3-4CF2-8778-8314A14D92AC}']
    function Get_GroupID: Integer; safecall;
    function Get_ElementID: Integer; safecall;
    function Get_IsElementGroup: WordBool; safecall;
    function Get_ElementOrder: Integer; safecall;
    procedure Set_ElementOrder(Value: Integer); safecall;
    function Get_ElementActive: WordBool; safecall;
    procedure Set_ElementActive(Value: WordBool); safecall;
    property GroupID: Integer read Get_GroupID;
    property ElementID: Integer read Get_ElementID;
    property IsElementGroup: WordBool read Get_IsElementGroup;
    property ElementOrder: Integer read Get_ElementOrder write Set_ElementOrder;
    property ElementActive: WordBool read Get_ElementActive write Set_ElementActive;
  end;

// *********************************************************************//
// DispIntf:  IChangeGroupElementDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {23E6A510-D8F3-4CF2-8778-8314A14D92AC}
// *********************************************************************//
  IChangeGroupElementDisp = dispinterface
    ['{23E6A510-D8F3-4CF2-8778-8314A14D92AC}']
    property GroupID: Integer readonly dispid 101;
    property ElementID: Integer readonly dispid 102;
    property IsElementGroup: WordBool readonly dispid 103;
    property ElementOrder: Integer dispid 104;
    property ElementActive: WordBool dispid 105;
  end;

// *********************************************************************//
// Interface: IChangeGroup
// Flags:     (320) Dual OleAutomation
// GUID:      {06EAC9C3-6137-4E37-9783-DD5C88682380}
// *********************************************************************//
  IChangeGroup = interface(IUnknown)
    ['{06EAC9C3-6137-4E37-9783-DD5C88682380}']
    function Get_GroupID: Integer; safecall;
    function Get_GroupName: WideString; safecall;
    procedure Set_GroupName(const Value: WideString); safecall;
    function NewChangeGroupElement(AElementID: Integer; AIsElementGroup: WordBool): IChangeGroupElement; safecall;
    function RemoveChangeGroupElementByID(AElementID: Integer; AIsElementGroup: WordBool): WordBool; safecall;
    function RemoveChangeGroupElementByIndex(AIndex: Integer): WordBool; safecall;
    function ChangeGroupElementByID(AElementID: Integer; AIsElementGroup: WordBool): IChangeGroupElement; safecall;
    function ChangeGroupElementByIndex(AIndex: Integer): IChangeGroupElement; safecall;
    function ContainsChangeLists: WordBool; safecall;
    function ElementCount: Integer; safecall;
    function Get_ParentGroupID: Integer; safecall;
    procedure Set_ParentGroupID(Value: Integer); safecall;
    function MoveUpChangeGroupElement(AElementID: Integer; AIsElementGroup: WordBool): WordBool; safecall;
    function MoveDownChangeGroupElement(AElementID: Integer; AIsElementGroup: WordBool): WordBool; safecall;
    property GroupID: Integer read Get_GroupID;
    property GroupName: WideString read Get_GroupName write Set_GroupName;
    property ParentGroupID: Integer read Get_ParentGroupID write Set_ParentGroupID;
  end;

// *********************************************************************//
// DispIntf:  IChangeGroupDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {06EAC9C3-6137-4E37-9783-DD5C88682380}
// *********************************************************************//
  IChangeGroupDisp = dispinterface
    ['{06EAC9C3-6137-4E37-9783-DD5C88682380}']
    property GroupID: Integer readonly dispid 101;
    property GroupName: WideString dispid 102;
    function NewChangeGroupElement(AElementID: Integer; AIsElementGroup: WordBool): IChangeGroupElement; dispid 103;
    function RemoveChangeGroupElementByID(AElementID: Integer; AIsElementGroup: WordBool): WordBool; dispid 104;
    function RemoveChangeGroupElementByIndex(AIndex: Integer): WordBool; dispid 105;
    function ChangeGroupElementByID(AElementID: Integer; AIsElementGroup: WordBool): IChangeGroupElement; dispid 106;
    function ChangeGroupElementByIndex(AIndex: Integer): IChangeGroupElement; dispid 107;
    function ContainsChangeLists: WordBool; dispid 108;
    function ElementCount: Integer; dispid 109;
    property ParentGroupID: Integer dispid 110;
    function MoveUpChangeGroupElement(AElementID: Integer; AIsElementGroup: WordBool): WordBool; dispid 111;
    function MoveDownChangeGroupElement(AElementID: Integer; AIsElementGroup: WordBool): WordBool; dispid 112;
  end;

// *********************************************************************//
// Interface: IDemandCentreGrowthFactors
// Flags:     (320) Dual OleAutomation
// GUID:      {F7EDAF21-5D53-46CF-889E-BF1BAB85E10A}
// *********************************************************************//
  IDemandCentreGrowthFactors = interface(IUnknown)
    ['{F7EDAF21-5D53-46CF-889E-BF1BAB85E10A}']
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(AValue: Integer); safecall;
    function Get_GrowthFactors: WideString; safecall;
    procedure Set_GrowthFactors(const AValue: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_GrowthFactorsCount: Integer; safecall;
    function Get_GrowthFactorsValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_GrowthFactorsValueByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_ValidFactors: WordBool; safecall;
    procedure Set_ValidFactors(AValue: WordBool); safecall;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property GrowthFactors: WideString read Get_GrowthFactors write Set_GrowthFactors;
    property GrowthFactorsCount: Integer read Get_GrowthFactorsCount;
    property GrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_GrowthFactorsValueByIndex write Set_GrowthFactorsValueByIndex;
    property ValidFactors: WordBool read Get_ValidFactors write Set_ValidFactors;
  end;

// *********************************************************************//
// DispIntf:  IDemandCentreGrowthFactorsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {F7EDAF21-5D53-46CF-889E-BF1BAB85E10A}
// *********************************************************************//
  IDemandCentreGrowthFactorsDisp = dispinterface
    ['{F7EDAF21-5D53-46CF-889E-BF1BAB85E10A}']
    property ChannelNumber: Integer dispid 101;
    property GrowthFactors: WideString dispid 102;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 103;
    property GrowthFactorsCount: Integer readonly dispid 104;
    property GrowthFactorsValueByIndex[AIndex: Integer]: Double dispid 105;
    property ValidFactors: WordBool dispid 106;
  end;

// *********************************************************************//
// Interface: IMinMaxChannelGrowthFactors
// Flags:     (320) Dual OleAutomation
// GUID:      {816AD665-1BF0-48B5-A820-98F4F7712D12}
// *********************************************************************//
  IMinMaxChannelGrowthFactors = interface(IUnknown)
    ['{816AD665-1BF0-48B5-A820-98F4F7712D12}']
    function Get_MinMaxChannel: Integer; safecall;
    procedure Set_MinMaxChannel(AValue: Integer); safecall;
    function Get_ArcNumber: Integer; safecall;
    procedure Set_ArcNumber(AValue: Integer); safecall;
    function Get_GrowthFactors: WideString; safecall;
    procedure Set_GrowthFactors(const AValue: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_GrowthFactorsValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_GrowthFactorsValueByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_GrowthFactorsCount: Integer; safecall;
    function Get_ValidFactors: WordBool; safecall;
    procedure Set_ValidFactors(AValue: WordBool); safecall;
    property MinMaxChannel: Integer read Get_MinMaxChannel write Set_MinMaxChannel;
    property ArcNumber: Integer read Get_ArcNumber write Set_ArcNumber;
    property GrowthFactors: WideString read Get_GrowthFactors write Set_GrowthFactors;
    property GrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_GrowthFactorsValueByIndex write Set_GrowthFactorsValueByIndex;
    property GrowthFactorsCount: Integer read Get_GrowthFactorsCount;
    property ValidFactors: WordBool read Get_ValidFactors write Set_ValidFactors;
  end;

// *********************************************************************//
// DispIntf:  IMinMaxChannelGrowthFactorsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {816AD665-1BF0-48B5-A820-98F4F7712D12}
// *********************************************************************//
  IMinMaxChannelGrowthFactorsDisp = dispinterface
    ['{816AD665-1BF0-48B5-A820-98F4F7712D12}']
    property MinMaxChannel: Integer dispid 101;
    property ArcNumber: Integer dispid 102;
    property GrowthFactors: WideString dispid 103;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 104;
    property GrowthFactorsValueByIndex[AIndex: Integer]: Double dispid 105;
    property GrowthFactorsCount: Integer readonly dispid 106;
    property ValidFactors: WordBool dispid 107;
  end;

// *********************************************************************//
// Interface: IHydrologyGrowthFactors
// Flags:     (320) Dual OleAutomation
// GUID:      {0D8515C5-BAAB-4BDC-9C25-BEA6CC34F336}
// *********************************************************************//
  IHydrologyGrowthFactors = interface(IUnknown)
    ['{0D8515C5-BAAB-4BDC-9C25-BEA6CC34F336}']
    function Get_GaugeNumber: Integer; safecall;
    procedure Set_GaugeNumber(AValue: Integer); safecall;
    function Get_AFFGrowthFactors: WideString; safecall;
    procedure Set_AFFGrowthFactors(const AValue: WideString); safecall;
    function Get_IRRGrowthFactors: WideString; safecall;
    procedure Set_IRRGrowthFactors(const AValue: WideString); safecall;
    function Get_URBGrowthFactors: WideString; safecall;
    procedure Set_URBGrowthFactors(const AValue: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_AFFGrowthFactorsValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AFFGrowthFactorsValueByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_IRRGrowthFactorsValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_IRRGrowthFactorsValueByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_URBGrowthFactorsValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_URBGrowthFactorsValueByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_AFFGrowthFactorsCount: Integer; safecall;
    function Get_IRRGrowthFactorsCount: Integer; safecall;
    function Get_URBGrowthFactorsCount: Integer; safecall;
    property GaugeNumber: Integer read Get_GaugeNumber write Set_GaugeNumber;
    property AFFGrowthFactors: WideString read Get_AFFGrowthFactors write Set_AFFGrowthFactors;
    property IRRGrowthFactors: WideString read Get_IRRGrowthFactors write Set_IRRGrowthFactors;
    property URBGrowthFactors: WideString read Get_URBGrowthFactors write Set_URBGrowthFactors;
    property AFFGrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_AFFGrowthFactorsValueByIndex write Set_AFFGrowthFactorsValueByIndex;
    property IRRGrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_IRRGrowthFactorsValueByIndex write Set_IRRGrowthFactorsValueByIndex;
    property URBGrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_URBGrowthFactorsValueByIndex write Set_URBGrowthFactorsValueByIndex;
    property AFFGrowthFactorsCount: Integer read Get_AFFGrowthFactorsCount;
    property IRRGrowthFactorsCount: Integer read Get_IRRGrowthFactorsCount;
    property URBGrowthFactorsCount: Integer read Get_URBGrowthFactorsCount;
  end;

// *********************************************************************//
// DispIntf:  IHydrologyGrowthFactorsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {0D8515C5-BAAB-4BDC-9C25-BEA6CC34F336}
// *********************************************************************//
  IHydrologyGrowthFactorsDisp = dispinterface
    ['{0D8515C5-BAAB-4BDC-9C25-BEA6CC34F336}']
    property GaugeNumber: Integer dispid 101;
    property AFFGrowthFactors: WideString dispid 102;
    property IRRGrowthFactors: WideString dispid 103;
    property URBGrowthFactors: WideString dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
    property AFFGrowthFactorsValueByIndex[AIndex: Integer]: Double dispid 106;
    property IRRGrowthFactorsValueByIndex[AIndex: Integer]: Double dispid 107;
    property URBGrowthFactorsValueByIndex[AIndex: Integer]: Double dispid 108;
    property AFFGrowthFactorsCount: Integer readonly dispid 109;
    property IRRGrowthFactorsCount: Integer readonly dispid 110;
    property URBGrowthFactorsCount: Integer readonly dispid 111;
  end;

// *********************************************************************//
// Interface: IGrowthFactors
// Flags:     (320) Dual OleAutomation
// GUID:      {6A27BCD6-2C5B-4EC2-AAFA-0F217A932880}
// *********************************************************************//
  IGrowthFactors = interface(IUnknown)
    ['{6A27BCD6-2C5B-4EC2-AAFA-0F217A932880}']
    function Get_DemandCentresGrowthByIndex(AIndex: Integer): IDemandCentreGrowthFactors; safecall;
    function Get_MinMaxChannelGrowthFactorByIndex(Param2: Integer): IMinMaxChannelGrowthFactors; safecall;
    function Get_HydrologyGrowthFactorByIndex(AIndex: Integer): IHydrologyGrowthFactors; safecall;
    function AddDemandCentresGrowthFactor(AChannelNumber: Integer): IDemandCentreGrowthFactors; safecall;
    function AddMinMaxChannelGrowthFactor(AChannelNumber: Integer): IMinMaxChannelGrowthFactors; safecall;
    function AddHydrologyGrowthFactor(AGaugeNumber: Integer): IHydrologyGrowthFactors; safecall;
    function RemoveDemandCentresGrowthFactor(AChannelNumber: Integer): WordBool; safecall;
    function RemoveMinMaxChannelGrowthFactor(AMinMaxChannel: Integer): WordBool; safecall;
    function RemoveHydrologyGrowthFactor(AGaugeNumber: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_NumberOfYears: Integer; safecall;
    procedure Set_NumberOfYears(Value: Integer); safecall;
    function Get_DemandGrowthFactorsByChannel(AChannelNumber: Integer): IDemandCentreGrowthFactors; safecall;
    function Get_MinMaxChannelGrowthFactorsByMinMaxChannel(AMinMaxChannel: Integer): IMinMaxChannelGrowthFactors; safecall;
    function Get_HydrologyGrowthFactorsByGaugeNumber(AGaugeNumber: Integer): IHydrologyGrowthFactors; safecall;
    property DemandCentresGrowthByIndex[AIndex: Integer]: IDemandCentreGrowthFactors read Get_DemandCentresGrowthByIndex;
    property MinMaxChannelGrowthFactorByIndex[Param2: Integer]: IMinMaxChannelGrowthFactors read Get_MinMaxChannelGrowthFactorByIndex;
    property HydrologyGrowthFactorByIndex[AIndex: Integer]: IHydrologyGrowthFactors read Get_HydrologyGrowthFactorByIndex;
    property NumberOfYears: Integer read Get_NumberOfYears write Set_NumberOfYears;
    property DemandGrowthFactorsByChannel[AChannelNumber: Integer]: IDemandCentreGrowthFactors read Get_DemandGrowthFactorsByChannel;
    property MinMaxChannelGrowthFactorsByMinMaxChannel[AMinMaxChannel: Integer]: IMinMaxChannelGrowthFactors read Get_MinMaxChannelGrowthFactorsByMinMaxChannel;
    property HydrologyGrowthFactorsByGaugeNumber[AGaugeNumber: Integer]: IHydrologyGrowthFactors read Get_HydrologyGrowthFactorsByGaugeNumber;
  end;

// *********************************************************************//
// DispIntf:  IGrowthFactorsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6A27BCD6-2C5B-4EC2-AAFA-0F217A932880}
// *********************************************************************//
  IGrowthFactorsDisp = dispinterface
    ['{6A27BCD6-2C5B-4EC2-AAFA-0F217A932880}']
    property DemandCentresGrowthByIndex[AIndex: Integer]: IDemandCentreGrowthFactors readonly dispid 202;
    property MinMaxChannelGrowthFactorByIndex[Param2: Integer]: IMinMaxChannelGrowthFactors readonly dispid 203;
    property HydrologyGrowthFactorByIndex[AIndex: Integer]: IHydrologyGrowthFactors readonly dispid 204;
    function AddDemandCentresGrowthFactor(AChannelNumber: Integer): IDemandCentreGrowthFactors; dispid 205;
    function AddMinMaxChannelGrowthFactor(AChannelNumber: Integer): IMinMaxChannelGrowthFactors; dispid 206;
    function AddHydrologyGrowthFactor(AGaugeNumber: Integer): IHydrologyGrowthFactors; dispid 207;
    function RemoveDemandCentresGrowthFactor(AChannelNumber: Integer): WordBool; dispid 208;
    function RemoveMinMaxChannelGrowthFactor(AMinMaxChannel: Integer): WordBool; dispid 209;
    function RemoveHydrologyGrowthFactor(AGaugeNumber: Integer): WordBool; dispid 210;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 211;
    property NumberOfYears: Integer dispid 101;
    property DemandGrowthFactorsByChannel[AChannelNumber: Integer]: IDemandCentreGrowthFactors readonly dispid 102;
    property MinMaxChannelGrowthFactorsByMinMaxChannel[AMinMaxChannel: Integer]: IMinMaxChannelGrowthFactors readonly dispid 103;
    property HydrologyGrowthFactorsByGaugeNumber[AGaugeNumber: Integer]: IHydrologyGrowthFactors readonly dispid 104;
  end;

// *********************************************************************//
// Interface: IDisbenefitFunctionDefinition
// Flags:     (320) Dual OleAutomation
// GUID:      {4E79F3A5-BA49-48C7-B309-777A652733AD}
// *********************************************************************//
  IDisbenefitFunctionDefinition = interface(IUnknown)
    ['{4E79F3A5-BA49-48C7-B309-777A652733AD}']
    function Get_NrOfEconomicYears: Integer; safecall;
    procedure Set_NrOfEconomicYears(Value: Integer); safecall;
    function Get_EquationDisbenefitX: Double; safecall;
    procedure Set_EquationDisbenefitX(Value: Double); safecall;
    function Get_EquationDisbenefitY: Double; safecall;
    procedure Set_EquationDisbenefitY(Value: Double); safecall;
    function Get_EquationDisbenefitNonSupply: Double; safecall;
    procedure Set_EquationDisbenefitNonSupply(Value: Double); safecall;
    function Get_EquationDisbenefitCost: Double; safecall;
    procedure Set_EquationDisbenefitCost(Value: Double); safecall;
    function Get_EscalationRate: WideString; safecall;
    procedure Set_EscalationRate(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_FeatureID: Integer; safecall;
    function Get_EscalationRateByIndex(AIndex: Integer): Double; safecall;
    procedure Set_EscalationRateByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_YearActive: Integer; safecall;
    procedure Set_YearActive(Value: Integer); safecall;
    function Get_MonthActive: Integer; safecall;
    procedure Set_MonthActive(Value: Integer); safecall;
    function Get_YearObsolete: Integer; safecall;
    procedure Set_YearObsolete(Value: Integer); safecall;
    function Get_MonthObsolete: Integer; safecall;
    procedure Set_MonthObsolete(Value: Integer); safecall;
    function Get_WQConstraint: Double; safecall;
    procedure Set_WQConstraint(Value: Double); safecall;
    function Get_TDSConcentrationByIndex(AIndex: Integer): Double; safecall;
    procedure Set_TDSConcentrationByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_EscalationFactors: WideString; safecall;
    procedure Set_EscalationFactors(const Value: WideString); safecall;
    property NrOfEconomicYears: Integer read Get_NrOfEconomicYears write Set_NrOfEconomicYears;
    property EquationDisbenefitX: Double read Get_EquationDisbenefitX write Set_EquationDisbenefitX;
    property EquationDisbenefitY: Double read Get_EquationDisbenefitY write Set_EquationDisbenefitY;
    property EquationDisbenefitNonSupply: Double read Get_EquationDisbenefitNonSupply write Set_EquationDisbenefitNonSupply;
    property EquationDisbenefitCost: Double read Get_EquationDisbenefitCost write Set_EquationDisbenefitCost;
    property EscalationRate: WideString read Get_EscalationRate write Set_EscalationRate;
    property FeatureID: Integer read Get_FeatureID;
    property EscalationRateByIndex[AIndex: Integer]: Double read Get_EscalationRateByIndex write Set_EscalationRateByIndex;
    property YearActive: Integer read Get_YearActive write Set_YearActive;
    property MonthActive: Integer read Get_MonthActive write Set_MonthActive;
    property YearObsolete: Integer read Get_YearObsolete write Set_YearObsolete;
    property MonthObsolete: Integer read Get_MonthObsolete write Set_MonthObsolete;
    property WQConstraint: Double read Get_WQConstraint write Set_WQConstraint;
    property TDSConcentrationByIndex[AIndex: Integer]: Double read Get_TDSConcentrationByIndex write Set_TDSConcentrationByIndex;
    property EscalationFactors: WideString read Get_EscalationFactors write Set_EscalationFactors;
  end;

// *********************************************************************//
// DispIntf:  IDisbenefitFunctionDefinitionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4E79F3A5-BA49-48C7-B309-777A652733AD}
// *********************************************************************//
  IDisbenefitFunctionDefinitionDisp = dispinterface
    ['{4E79F3A5-BA49-48C7-B309-777A652733AD}']
    property NrOfEconomicYears: Integer dispid 101;
    property EquationDisbenefitX: Double dispid 102;
    property EquationDisbenefitY: Double dispid 103;
    property EquationDisbenefitNonSupply: Double dispid 104;
    property EquationDisbenefitCost: Double dispid 105;
    property EscalationRate: WideString dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
    property FeatureID: Integer readonly dispid 108;
    property EscalationRateByIndex[AIndex: Integer]: Double dispid 109;
    property YearActive: Integer dispid 110;
    property MonthActive: Integer dispid 111;
    property YearObsolete: Integer dispid 112;
    property MonthObsolete: Integer dispid 113;
    property WQConstraint: Double dispid 114;
    property TDSConcentrationByIndex[AIndex: Integer]: Double dispid 115;
    property EscalationFactors: WideString dispid 116;
  end;

// *********************************************************************//
// Interface: ICorrespondingChannel
// Flags:     (320) Dual OleAutomation
// GUID:      {137D8C62-B6D9-4E14-9E43-14514D84FE94}
// *********************************************************************//
  ICorrespondingChannel = interface(IUnknown)
    ['{137D8C62-B6D9-4E14-9E43-14514D84FE94}']
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(AValue: Integer); safecall;
    function Get_AbstractionChannel: Integer; safecall;
    procedure Set_AbstractionChannel(AValue: Integer); safecall;
    function Get_AssumedFactor: Double; safecall;
    procedure Set_AssumedFactor(AValue: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property AbstractionChannel: Integer read Get_AbstractionChannel write Set_AbstractionChannel;
    property AssumedFactor: Double read Get_AssumedFactor write Set_AssumedFactor;
  end;

// *********************************************************************//
// DispIntf:  ICorrespondingChannelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {137D8C62-B6D9-4E14-9E43-14514D84FE94}
// *********************************************************************//
  ICorrespondingChannelDisp = dispinterface
    ['{137D8C62-B6D9-4E14-9E43-14514D84FE94}']
    property ChannelNumber: Integer dispid 201;
    property AbstractionChannel: Integer dispid 202;
    property AssumedFactor: Double dispid 203;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 204;
  end;

// *********************************************************************//
// Interface: IReturnFlowChannel
// Flags:     (320) Dual OleAutomation
// GUID:      {0BA634F1-4408-4031-BA15-5415D9143739}
// *********************************************************************//
  IReturnFlowChannel = interface(IUnknown)
    ['{0BA634F1-4408-4031-BA15-5415D9143739}']
    function NewCorrespondingChannel(ACorrespondingChannel: Integer): ICorrespondingChannel; safecall;
    function RemoveCorrespondingChannelByChannel(AChannel: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_DemandChannel: Integer; safecall;
    procedure Set_DemandChannel(AValue: Integer); safecall;
    function Get_NumOfCorrespondingChannels: Integer; safecall;
    procedure Set_NumOfCorrespondingChannels(AValue: Integer); safecall;
    function Get_GaugeNumber: Integer; safecall;
    procedure Set_GaugeNumber(AValue: Integer); safecall;
    function Get_MonthlyAvrgFactor: Double; safecall;
    procedure Set_MonthlyAvrgFactor(AValue: Double); safecall;
    function Get_CalibrationFactor: Double; safecall;
    procedure Set_CalibrationFactor(AValue: Double); safecall;
    function Get_MonthlyAvrgNetEvap: Double; safecall;
    procedure Set_MonthlyAvrgNetEvap(AValue: Double); safecall;
    function Get_RoutingConstant: Double; safecall;
    procedure Set_RoutingConstant(AValue: Double); safecall;
    function Get_MultiplicationFactor: Double; safecall;
    procedure Set_MultiplicationFactor(AValue: Double); safecall;
    function Get_CurtailmentFactor: Double; safecall;
    procedure Set_CurtailmentFactor(AValue: Double); safecall;
    function Get_MonthlyPotentialEvapByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyPotentialEvapByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_CorrespondingChannelByIndex(AIndex: Integer): ICorrespondingChannel; safecall;
    property DemandChannel: Integer read Get_DemandChannel write Set_DemandChannel;
    property NumOfCorrespondingChannels: Integer read Get_NumOfCorrespondingChannels write Set_NumOfCorrespondingChannels;
    property GaugeNumber: Integer read Get_GaugeNumber write Set_GaugeNumber;
    property MonthlyAvrgFactor: Double read Get_MonthlyAvrgFactor write Set_MonthlyAvrgFactor;
    property CalibrationFactor: Double read Get_CalibrationFactor write Set_CalibrationFactor;
    property MonthlyAvrgNetEvap: Double read Get_MonthlyAvrgNetEvap write Set_MonthlyAvrgNetEvap;
    property RoutingConstant: Double read Get_RoutingConstant write Set_RoutingConstant;
    property MultiplicationFactor: Double read Get_MultiplicationFactor write Set_MultiplicationFactor;
    property CurtailmentFactor: Double read Get_CurtailmentFactor write Set_CurtailmentFactor;
    property MonthlyPotentialEvapByIndex[AIndex: Integer]: Double read Get_MonthlyPotentialEvapByIndex write Set_MonthlyPotentialEvapByIndex;
    property CorrespondingChannelByIndex[AIndex: Integer]: ICorrespondingChannel read Get_CorrespondingChannelByIndex;
  end;

// *********************************************************************//
// DispIntf:  IReturnFlowChannelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {0BA634F1-4408-4031-BA15-5415D9143739}
// *********************************************************************//
  IReturnFlowChannelDisp = dispinterface
    ['{0BA634F1-4408-4031-BA15-5415D9143739}']
    function NewCorrespondingChannel(ACorrespondingChannel: Integer): ICorrespondingChannel; dispid 101;
    function RemoveCorrespondingChannelByChannel(AChannel: Integer): WordBool; dispid 102;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 103;
    property DemandChannel: Integer dispid 104;
    property NumOfCorrespondingChannels: Integer dispid 105;
    property GaugeNumber: Integer dispid 106;
    property MonthlyAvrgFactor: Double dispid 107;
    property CalibrationFactor: Double dispid 108;
    property MonthlyAvrgNetEvap: Double dispid 109;
    property RoutingConstant: Double dispid 110;
    property MultiplicationFactor: Double dispid 112;
    property CurtailmentFactor: Double dispid 111;
    property MonthlyPotentialEvapByIndex[AIndex: Integer]: Double dispid 113;
    property CorrespondingChannelByIndex[AIndex: Integer]: ICorrespondingChannel readonly dispid 114;
  end;

// *********************************************************************//
// Interface: IReturnFlowChannelData
// Flags:     (320) Dual OleAutomation
// GUID:      {888D1F6D-66A3-47BB-A347-103D19E39958}
// *********************************************************************//
  IReturnFlowChannelData = interface(IUnknown)
    ['{888D1F6D-66A3-47BB-A347-103D19E39958}']
    function NewReturnFlowChannel(ADemandChannel: Integer): IReturnFlowChannel; safecall;
    function RemoveReturnFlowByChannel(ADemandChannel: Integer): WordBool; safecall;
    function Get_ReturnFlowChannelCount: Integer; safecall;
    function Get_ReturnFlowChannelByIndex(AIndex: Integer): IReturnFlowChannel; safecall;
    function Get_ReturnFlowChannelByChannel(ADemandChannel: Integer): IReturnFlowChannel; safecall;
    property ReturnFlowChannelCount: Integer read Get_ReturnFlowChannelCount;
    property ReturnFlowChannelByIndex[AIndex: Integer]: IReturnFlowChannel read Get_ReturnFlowChannelByIndex;
    property ReturnFlowChannelByChannel[ADemandChannel: Integer]: IReturnFlowChannel read Get_ReturnFlowChannelByChannel;
  end;

// *********************************************************************//
// DispIntf:  IReturnFlowChannelDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {888D1F6D-66A3-47BB-A347-103D19E39958}
// *********************************************************************//
  IReturnFlowChannelDataDisp = dispinterface
    ['{888D1F6D-66A3-47BB-A347-103D19E39958}']
    function NewReturnFlowChannel(ADemandChannel: Integer): IReturnFlowChannel; dispid 101;
    function RemoveReturnFlowByChannel(ADemandChannel: Integer): WordBool; dispid 102;
    property ReturnFlowChannelCount: Integer readonly dispid 103;
    property ReturnFlowChannelByIndex[AIndex: Integer]: IReturnFlowChannel readonly dispid 104;
    property ReturnFlowChannelByChannel[ADemandChannel: Integer]: IReturnFlowChannel readonly dispid 105;
  end;

// *********************************************************************//
// Interface: IWaterUsage
// Flags:     (320) Dual OleAutomation
// GUID:      {7AFE47CC-ED8E-4B0B-B68A-1F08C3CE2350}
// *********************************************************************//
  IWaterUsage = interface(IUnknown)
    ['{7AFE47CC-ED8E-4B0B-B68A-1F08C3CE2350}']
    function Get_BlockIdentifier: Integer; safecall;
    procedure Set_BlockIdentifier(Value: Integer); safecall;
    function Get_Identifier: Integer; safecall;
    procedure Set_Identifier(Value: Integer); safecall;
    function Get_PercAreaUnderCropType: Double; safecall;
    procedure Set_PercAreaUnderCropType(Value: Double); safecall;
    function Get_CropName: WideString; safecall;
    procedure Set_CropName(const Value: WideString); safecall;
    function Get_MonthlyWaterUse(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyWaterUse(AIndex: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property BlockIdentifier: Integer read Get_BlockIdentifier write Set_BlockIdentifier;
    property Identifier: Integer read Get_Identifier write Set_Identifier;
    property PercAreaUnderCropType: Double read Get_PercAreaUnderCropType write Set_PercAreaUnderCropType;
    property CropName: WideString read Get_CropName write Set_CropName;
    property MonthlyWaterUse[AIndex: Integer]: Double read Get_MonthlyWaterUse write Set_MonthlyWaterUse;
  end;

// *********************************************************************//
// DispIntf:  IWaterUsageDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {7AFE47CC-ED8E-4B0B-B68A-1F08C3CE2350}
// *********************************************************************//
  IWaterUsageDisp = dispinterface
    ['{7AFE47CC-ED8E-4B0B-B68A-1F08C3CE2350}']
    property BlockIdentifier: Integer dispid 201;
    property Identifier: Integer dispid 202;
    property PercAreaUnderCropType: Double dispid 203;
    property CropName: WideString dispid 204;
    property MonthlyWaterUse[AIndex: Integer]: Double dispid 205;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 101;
  end;

// *********************************************************************//
// Interface: IIrrigationBlock
// Flags:     (320) Dual OleAutomation
// GUID:      {000F54E0-B287-4850-AAEF-F7150CEFF141}
// *********************************************************************//
  IIrrigationBlock = interface(IUnknown)
    ['{000F54E0-B287-4850-AAEF-F7150CEFF141}']
    function Get_Identifier: Integer; safecall;
    function Get_BlockNodeNumber: Integer; safecall;
    procedure Set_BlockNodeNumber(Value: Integer); safecall;
    function Get_BlockName: WideString; safecall;
    procedure Set_BlockName(const Value: WideString); safecall;
    function Get_BlockDescription: WideString; safecall;
    procedure Set_BlockDescription(const Value: WideString); safecall;
    function Get_MaxWaterAllocation: Double; safecall;
    procedure Set_MaxWaterAllocation(Value: Double); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Get_HydrologyNodeNumber: Integer; safecall;
    procedure Set_HydrologyNodeNumber(Value: Integer); safecall;
    function Get_CanalTransportLoss: Double; safecall;
    procedure Set_CanalTransportLoss(Value: Double); safecall;
    function Get_EfficiencyFactor: Double; safecall;
    procedure Set_EfficiencyFactor(Value: Double); safecall;
    function Get_ReturnFlowFactor: Double; safecall;
    procedure Set_ReturnFlowFactor(Value: Double); safecall;
    function Get_NumberOfCropTypes: Integer; safecall;
    procedure Set_NumberOfCropTypes(Value: Integer); safecall;
    function Get_RainAboveRainFactorSpecValue: Double; safecall;
    procedure Set_RainAboveRainFactorSpecValue(Value: Double); safecall;
    function Get_RainBelowRainFactor: Double; safecall;
    procedure Set_RainBelowRainFactor(Value: Double); safecall;
    function Get_RainCatchmentScalingFactor: Double; safecall;
    procedure Set_RainCatchmentScalingFactor(Value: Double); safecall;
    function Get_AllocatedIrrigationArea: Double; safecall;
    procedure Set_AllocatedIrrigationArea(Value: Double); safecall;
    function Get_UpperZoneReturnFlow: Double; safecall;
    procedure Set_UpperZoneReturnFlow(Value: Double); safecall;
    function Get_LowerZoneReturnFlow: Double; safecall;
    procedure Set_LowerZoneReturnFlow(Value: Double); safecall;
    function Get_ReturnFlowLoss: Double; safecall;
    procedure Set_ReturnFlowLoss(Value: Double); safecall;
    function Get_UpperZoneSoilMoistureCapacity: Double; safecall;
    procedure Set_UpperZoneSoilMoistureCapacity(Value: Double); safecall;
    function Get_LowerZoneSoilMoistureCapacity: Double; safecall;
    procedure Set_LowerZoneSoilMoistureCapacity(Value: Double); safecall;
    function Get_UpperZoneSoilMoistureTarget: Double; safecall;
    procedure Set_UpperZoneSoilMoistureTarget(Value: Double); safecall;
    function Get_InitialSoilMoistureStorage: Double; safecall;
    procedure Set_InitialSoilMoistureStorage(Value: Double); safecall;
    function Get_RainfallFactor(AMonth: Integer): Double; safecall;
    procedure Set_RainfallFactor(AMonth: Integer; Value: Double); safecall;
    function Get_PanEvaporation(AMonth: Integer): Double; safecall;
    procedure Set_PanEvaporation(AMonth: Integer; Value: Double); safecall;
    function Get_APanConvFactor(AMonth: Integer): Double; safecall;
    procedure Set_APanConvFactor(AMonth: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CreateWaterUse: IWaterUsage; safecall;
    function RemoveWaterUse(AIrrigationBlockWaterUsageID: Integer): WordBool; safecall;
    function WaterUsageFactorByName(const AName: WideString): IWaterUsage; safecall;
    function Get_DiversionChannel: IGeneralFlowChannel; safecall;
    function Get_ReturnFlowChannel: IGeneralFlowChannel; safecall;
    function Get_WaterUsageCount: Integer; safecall;
    function Get_WaterUsageFactorByIndex(AIndex: Integer): IWaterUsage; safecall;
    function Get_WaterUsageFactorByID(AIrrigationBlockWaterUsageID: Integer): IWaterUsage; safecall;
    function Get_DroughtApplicable: Integer; safecall;
    procedure Set_DroughtApplicable(Value: Integer); safecall;
    function Get_CropWaterUseType: Integer; safecall;
    procedure Set_CropWaterUseType(Value: Integer); safecall;
    function Get_HydrologyNode: IReservoirData; safecall;
    function Get_BlockNode: IReservoirData; safecall;
    function Get_IrrigationBlockType: Integer; safecall;
    procedure Set_IrrigationBlockType(Value: Integer); safecall;
    function Get_CurtailIrrigationAbstraction: Integer; safecall;
    procedure Set_CurtailIrrigationAbstraction(Value: Integer); safecall;
    function Get_CanalSeepageLoss: Double; safecall;
    procedure Set_CanalSeepageLoss(Value: Double); safecall;
    function Get_CanalTransmissionLoss: Double; safecall;
    procedure Set_CanalTransmissionLoss(Value: Double); safecall;
    function Get_UpperSoilOutflow: Double; safecall;
    procedure Set_UpperSoilOutflow(Value: Double); safecall;
    function Get_MaxUpperZoneMoisture: Double; safecall;
    procedure Set_MaxUpperZoneMoisture(Value: Double); safecall;
    function Get_MinUpperZoneMoisture: Double; safecall;
    procedure Set_MinUpperZoneMoisture(Value: Double); safecall;
    function Get_CropTypesCount: Integer; safecall;
    procedure Set_CropTypesCount(Value: Integer); safecall;
    function Get_IrrigationSupplyCapacity: Double; safecall;
    procedure Set_IrrigationSupplyCapacity(Value: Double); safecall;
    function Get_AllocatedAreaPointsCount: Integer; safecall;
    procedure Set_AllocatedAreaPointsCount(Value: Integer); safecall;
    function Get_MethodIrrigatedAreas: Integer; safecall;
    procedure Set_MethodIrrigatedAreas(Value: Integer); safecall;
    function Get_MaxWaterAllocationCount: Integer; safecall;
    procedure Set_MaxWaterAllocationCount(Value: Integer); safecall;
    function Get_MethodMaxWaterAllocation: Integer; safecall;
    procedure Set_MethodMaxWaterAllocation(Value: Integer); safecall;
    function Get_ReturnFlowVolumePointsCount: Integer; safecall;
    procedure Set_ReturnFlowVolumePointsCount(Value: Integer); safecall;
    function Get_MethodReturnFlowVolume: Integer; safecall;
    procedure Set_MethodReturnFlowVolume(Value: Integer); safecall;
    function Get_SupplyCapacityPointsCount: Integer; safecall;
    procedure Set_SupplyCapacityPointsCount(Value: Integer); safecall;
    function Get_MethodSupplyCapacity: Integer; safecall;
    procedure Set_MethodSupplyCapacity(Value: Integer); safecall;
    function Get_MethodIrrigationEfficiencies: Integer; safecall;
    procedure Set_MethodIrrigationEfficiencies(Value: Integer); safecall;
    function Get_ReturnFlowFactorsCount: Integer; safecall;
    procedure Set_ReturnFlowFactorsCount(Value: Integer); safecall;
    function Get_MethodReturnFlowFactors: Integer; safecall;
    procedure Set_MethodReturnFlowFactors(Value: Integer); safecall;
    function Get_IrrigatedAreasBreakPointCount: Integer; safecall;
    procedure Set_IrrigatedAreasBreakPointCount(Value: Integer); safecall;
    function Get_MaximumWaterAllocationBreakPointCount: Integer; safecall;
    procedure Set_MaximumWaterAllocationBreakPointCount(Value: Integer); safecall;
    function Get_ReturnFlowVolumeBreakPointsCount: Integer; safecall;
    procedure Set_ReturnFlowVolumeBreakPointsCount(Value: Integer); safecall;
    function Get_SupplyCapacityBreakPointsCount: Integer; safecall;
    procedure Set_SupplyCapacityBreakPointsCount(Value: Integer); safecall;
    function Get_IrrigationEfficiencyBreakPointsCount: Integer; safecall;
    procedure Set_IrrigationEfficiencyBreakPointsCount(Value: Integer); safecall;
    function Get_ReturnFlowFactorBreakPointsCount: Integer; safecall;
    procedure Set_ReturnFlowFactorBreakPointsCount(Value: Integer); safecall;
    function Get_IrrigatedAreasBreakPointYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_IrrigatedAreasBreakPointYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_IrrigatedAreaByIndex(AIndex: Integer): Double; safecall;
    procedure Set_IrrigatedAreaByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_MaximumWaterAllocationBreakPointYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_MaximumWaterAllocationBreakPointYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_MaximumWaterAllocationByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MaximumWaterAllocationByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_MaximumWaterAllocationGrowthByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MaximumWaterAllocationGrowthByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_ReturnFlowVolumeBreakpointYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_ReturnFlowVolumeBreakpointYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_ReturnFlowVolumeByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReturnFlowVolumeByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_SupplyCapacityBreakpointYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_SupplyCapacityBreakpointYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_SupplyCapacityByIndex(AIndex: Integer): Double; safecall;
    procedure Set_SupplyCapacityByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_IrrigationEfficiencyBreakpointYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_IrrigationEfficiencyBreakpointYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_IrrigationEfficiencyByIndex(AIndex: Integer): Double; safecall;
    procedure Set_IrrigationEfficiencyByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_ReturnFlowFactorBreakpointYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_ReturnFlowFactorBreakpointYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_ReturnFlowFactorsByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReturnFlowFactorsByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_MultiplicationFactor: Double; safecall;
    procedure Set_MultiplicationFactor(Value: Double); safecall;
    property Identifier: Integer read Get_Identifier;
    property BlockNodeNumber: Integer read Get_BlockNodeNumber write Set_BlockNodeNumber;
    property BlockName: WideString read Get_BlockName write Set_BlockName;
    property BlockDescription: WideString read Get_BlockDescription write Set_BlockDescription;
    property MaxWaterAllocation: Double read Get_MaxWaterAllocation write Set_MaxWaterAllocation;
    property FileName: WideString read Get_FileName write Set_FileName;
    property HydrologyNodeNumber: Integer read Get_HydrologyNodeNumber write Set_HydrologyNodeNumber;
    property CanalTransportLoss: Double read Get_CanalTransportLoss write Set_CanalTransportLoss;
    property EfficiencyFactor: Double read Get_EfficiencyFactor write Set_EfficiencyFactor;
    property ReturnFlowFactor: Double read Get_ReturnFlowFactor write Set_ReturnFlowFactor;
    property NumberOfCropTypes: Integer read Get_NumberOfCropTypes write Set_NumberOfCropTypes;
    property RainAboveRainFactorSpecValue: Double read Get_RainAboveRainFactorSpecValue write Set_RainAboveRainFactorSpecValue;
    property RainBelowRainFactor: Double read Get_RainBelowRainFactor write Set_RainBelowRainFactor;
    property RainCatchmentScalingFactor: Double read Get_RainCatchmentScalingFactor write Set_RainCatchmentScalingFactor;
    property AllocatedIrrigationArea: Double read Get_AllocatedIrrigationArea write Set_AllocatedIrrigationArea;
    property UpperZoneReturnFlow: Double read Get_UpperZoneReturnFlow write Set_UpperZoneReturnFlow;
    property LowerZoneReturnFlow: Double read Get_LowerZoneReturnFlow write Set_LowerZoneReturnFlow;
    property ReturnFlowLoss: Double read Get_ReturnFlowLoss write Set_ReturnFlowLoss;
    property UpperZoneSoilMoistureCapacity: Double read Get_UpperZoneSoilMoistureCapacity write Set_UpperZoneSoilMoistureCapacity;
    property LowerZoneSoilMoistureCapacity: Double read Get_LowerZoneSoilMoistureCapacity write Set_LowerZoneSoilMoistureCapacity;
    property UpperZoneSoilMoistureTarget: Double read Get_UpperZoneSoilMoistureTarget write Set_UpperZoneSoilMoistureTarget;
    property InitialSoilMoistureStorage: Double read Get_InitialSoilMoistureStorage write Set_InitialSoilMoistureStorage;
    property RainfallFactor[AMonth: Integer]: Double read Get_RainfallFactor write Set_RainfallFactor;
    property PanEvaporation[AMonth: Integer]: Double read Get_PanEvaporation write Set_PanEvaporation;
    property APanConvFactor[AMonth: Integer]: Double read Get_APanConvFactor write Set_APanConvFactor;
    property DiversionChannel: IGeneralFlowChannel read Get_DiversionChannel;
    property ReturnFlowChannel: IGeneralFlowChannel read Get_ReturnFlowChannel;
    property WaterUsageCount: Integer read Get_WaterUsageCount;
    property WaterUsageFactorByIndex[AIndex: Integer]: IWaterUsage read Get_WaterUsageFactorByIndex;
    property WaterUsageFactorByID[AIrrigationBlockWaterUsageID: Integer]: IWaterUsage read Get_WaterUsageFactorByID;
    property DroughtApplicable: Integer read Get_DroughtApplicable write Set_DroughtApplicable;
    property CropWaterUseType: Integer read Get_CropWaterUseType write Set_CropWaterUseType;
    property HydrologyNode: IReservoirData read Get_HydrologyNode;
    property BlockNode: IReservoirData read Get_BlockNode;
    property IrrigationBlockType: Integer read Get_IrrigationBlockType write Set_IrrigationBlockType;
    property CurtailIrrigationAbstraction: Integer read Get_CurtailIrrigationAbstraction write Set_CurtailIrrigationAbstraction;
    property CanalSeepageLoss: Double read Get_CanalSeepageLoss write Set_CanalSeepageLoss;
    property CanalTransmissionLoss: Double read Get_CanalTransmissionLoss write Set_CanalTransmissionLoss;
    property UpperSoilOutflow: Double read Get_UpperSoilOutflow write Set_UpperSoilOutflow;
    property MaxUpperZoneMoisture: Double read Get_MaxUpperZoneMoisture write Set_MaxUpperZoneMoisture;
    property MinUpperZoneMoisture: Double read Get_MinUpperZoneMoisture write Set_MinUpperZoneMoisture;
    property CropTypesCount: Integer read Get_CropTypesCount write Set_CropTypesCount;
    property IrrigationSupplyCapacity: Double read Get_IrrigationSupplyCapacity write Set_IrrigationSupplyCapacity;
    property AllocatedAreaPointsCount: Integer read Get_AllocatedAreaPointsCount write Set_AllocatedAreaPointsCount;
    property MethodIrrigatedAreas: Integer read Get_MethodIrrigatedAreas write Set_MethodIrrigatedAreas;
    property MaxWaterAllocationCount: Integer read Get_MaxWaterAllocationCount write Set_MaxWaterAllocationCount;
    property MethodMaxWaterAllocation: Integer read Get_MethodMaxWaterAllocation write Set_MethodMaxWaterAllocation;
    property ReturnFlowVolumePointsCount: Integer read Get_ReturnFlowVolumePointsCount write Set_ReturnFlowVolumePointsCount;
    property MethodReturnFlowVolume: Integer read Get_MethodReturnFlowVolume write Set_MethodReturnFlowVolume;
    property SupplyCapacityPointsCount: Integer read Get_SupplyCapacityPointsCount write Set_SupplyCapacityPointsCount;
    property MethodSupplyCapacity: Integer read Get_MethodSupplyCapacity write Set_MethodSupplyCapacity;
    property MethodIrrigationEfficiencies: Integer read Get_MethodIrrigationEfficiencies write Set_MethodIrrigationEfficiencies;
    property ReturnFlowFactorsCount: Integer read Get_ReturnFlowFactorsCount write Set_ReturnFlowFactorsCount;
    property MethodReturnFlowFactors: Integer read Get_MethodReturnFlowFactors write Set_MethodReturnFlowFactors;
    property IrrigatedAreasBreakPointCount: Integer read Get_IrrigatedAreasBreakPointCount write Set_IrrigatedAreasBreakPointCount;
    property MaximumWaterAllocationBreakPointCount: Integer read Get_MaximumWaterAllocationBreakPointCount write Set_MaximumWaterAllocationBreakPointCount;
    property ReturnFlowVolumeBreakPointsCount: Integer read Get_ReturnFlowVolumeBreakPointsCount write Set_ReturnFlowVolumeBreakPointsCount;
    property SupplyCapacityBreakPointsCount: Integer read Get_SupplyCapacityBreakPointsCount write Set_SupplyCapacityBreakPointsCount;
    property IrrigationEfficiencyBreakPointsCount: Integer read Get_IrrigationEfficiencyBreakPointsCount write Set_IrrigationEfficiencyBreakPointsCount;
    property ReturnFlowFactorBreakPointsCount: Integer read Get_ReturnFlowFactorBreakPointsCount write Set_ReturnFlowFactorBreakPointsCount;
    property IrrigatedAreasBreakPointYearByIndex[AIndex: Integer]: Integer read Get_IrrigatedAreasBreakPointYearByIndex write Set_IrrigatedAreasBreakPointYearByIndex;
    property IrrigatedAreaByIndex[AIndex: Integer]: Double read Get_IrrigatedAreaByIndex write Set_IrrigatedAreaByIndex;
    property MaximumWaterAllocationBreakPointYearByIndex[AIndex: Integer]: Integer read Get_MaximumWaterAllocationBreakPointYearByIndex write Set_MaximumWaterAllocationBreakPointYearByIndex;
    property MaximumWaterAllocationByIndex[AIndex: Integer]: Double read Get_MaximumWaterAllocationByIndex write Set_MaximumWaterAllocationByIndex;
    property MaximumWaterAllocationGrowthByIndex[AIndex: Integer]: Double read Get_MaximumWaterAllocationGrowthByIndex write Set_MaximumWaterAllocationGrowthByIndex;
    property ReturnFlowVolumeBreakpointYearByIndex[AIndex: Integer]: Integer read Get_ReturnFlowVolumeBreakpointYearByIndex write Set_ReturnFlowVolumeBreakpointYearByIndex;
    property ReturnFlowVolumeByIndex[AIndex: Integer]: Double read Get_ReturnFlowVolumeByIndex write Set_ReturnFlowVolumeByIndex;
    property SupplyCapacityBreakpointYearByIndex[AIndex: Integer]: Integer read Get_SupplyCapacityBreakpointYearByIndex write Set_SupplyCapacityBreakpointYearByIndex;
    property SupplyCapacityByIndex[AIndex: Integer]: Double read Get_SupplyCapacityByIndex write Set_SupplyCapacityByIndex;
    property IrrigationEfficiencyBreakpointYearByIndex[AIndex: Integer]: Integer read Get_IrrigationEfficiencyBreakpointYearByIndex write Set_IrrigationEfficiencyBreakpointYearByIndex;
    property IrrigationEfficiencyByIndex[AIndex: Integer]: Double read Get_IrrigationEfficiencyByIndex write Set_IrrigationEfficiencyByIndex;
    property ReturnFlowFactorBreakpointYearByIndex[AIndex: Integer]: Integer read Get_ReturnFlowFactorBreakpointYearByIndex write Set_ReturnFlowFactorBreakpointYearByIndex;
    property ReturnFlowFactorsByIndex[AIndex: Integer]: Double read Get_ReturnFlowFactorsByIndex write Set_ReturnFlowFactorsByIndex;
    property MultiplicationFactor: Double read Get_MultiplicationFactor write Set_MultiplicationFactor;
  end;

// *********************************************************************//
// DispIntf:  IIrrigationBlockDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {000F54E0-B287-4850-AAEF-F7150CEFF141}
// *********************************************************************//
  IIrrigationBlockDisp = dispinterface
    ['{000F54E0-B287-4850-AAEF-F7150CEFF141}']
    property Identifier: Integer readonly dispid 101;
    property BlockNodeNumber: Integer dispid 102;
    property BlockName: WideString dispid 103;
    property BlockDescription: WideString dispid 104;
    property MaxWaterAllocation: Double dispid 105;
    property FileName: WideString dispid 106;
    property HydrologyNodeNumber: Integer dispid 107;
    property CanalTransportLoss: Double dispid 108;
    property EfficiencyFactor: Double dispid 109;
    property ReturnFlowFactor: Double dispid 110;
    property NumberOfCropTypes: Integer dispid 111;
    property RainAboveRainFactorSpecValue: Double dispid 112;
    property RainBelowRainFactor: Double dispid 113;
    property RainCatchmentScalingFactor: Double dispid 114;
    property AllocatedIrrigationArea: Double dispid 115;
    property UpperZoneReturnFlow: Double dispid 116;
    property LowerZoneReturnFlow: Double dispid 117;
    property ReturnFlowLoss: Double dispid 118;
    property UpperZoneSoilMoistureCapacity: Double dispid 119;
    property LowerZoneSoilMoistureCapacity: Double dispid 120;
    property UpperZoneSoilMoistureTarget: Double dispid 121;
    property InitialSoilMoistureStorage: Double dispid 122;
    property RainfallFactor[AMonth: Integer]: Double dispid 123;
    property PanEvaporation[AMonth: Integer]: Double dispid 124;
    property APanConvFactor[AMonth: Integer]: Double dispid 125;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 126;
    function CreateWaterUse: IWaterUsage; dispid 127;
    function RemoveWaterUse(AIrrigationBlockWaterUsageID: Integer): WordBool; dispid 129;
    function WaterUsageFactorByName(const AName: WideString): IWaterUsage; dispid 131;
    property DiversionChannel: IGeneralFlowChannel readonly dispid 132;
    property ReturnFlowChannel: IGeneralFlowChannel readonly dispid 133;
    property WaterUsageCount: Integer readonly dispid 135;
    property WaterUsageFactorByIndex[AIndex: Integer]: IWaterUsage readonly dispid 128;
    property WaterUsageFactorByID[AIrrigationBlockWaterUsageID: Integer]: IWaterUsage readonly dispid 130;
    property DroughtApplicable: Integer dispid 134;
    property CropWaterUseType: Integer dispid 136;
    property HydrologyNode: IReservoirData readonly dispid 137;
    property BlockNode: IReservoirData readonly dispid 138;
    property IrrigationBlockType: Integer dispid 139;
    property CurtailIrrigationAbstraction: Integer dispid 140;
    property CanalSeepageLoss: Double dispid 141;
    property CanalTransmissionLoss: Double dispid 142;
    property UpperSoilOutflow: Double dispid 143;
    property MaxUpperZoneMoisture: Double dispid 144;
    property MinUpperZoneMoisture: Double dispid 145;
    property CropTypesCount: Integer dispid 146;
    property IrrigationSupplyCapacity: Double dispid 147;
    property AllocatedAreaPointsCount: Integer dispid 148;
    property MethodIrrigatedAreas: Integer dispid 149;
    property MaxWaterAllocationCount: Integer dispid 150;
    property MethodMaxWaterAllocation: Integer dispid 151;
    property ReturnFlowVolumePointsCount: Integer dispid 152;
    property MethodReturnFlowVolume: Integer dispid 153;
    property SupplyCapacityPointsCount: Integer dispid 154;
    property MethodSupplyCapacity: Integer dispid 155;
    property MethodIrrigationEfficiencies: Integer dispid 156;
    property ReturnFlowFactorsCount: Integer dispid 157;
    property MethodReturnFlowFactors: Integer dispid 158;
    property IrrigatedAreasBreakPointCount: Integer dispid 159;
    property MaximumWaterAllocationBreakPointCount: Integer dispid 160;
    property ReturnFlowVolumeBreakPointsCount: Integer dispid 161;
    property SupplyCapacityBreakPointsCount: Integer dispid 162;
    property IrrigationEfficiencyBreakPointsCount: Integer dispid 163;
    property ReturnFlowFactorBreakPointsCount: Integer dispid 164;
    property IrrigatedAreasBreakPointYearByIndex[AIndex: Integer]: Integer dispid 165;
    property IrrigatedAreaByIndex[AIndex: Integer]: Double dispid 166;
    property MaximumWaterAllocationBreakPointYearByIndex[AIndex: Integer]: Integer dispid 167;
    property MaximumWaterAllocationByIndex[AIndex: Integer]: Double dispid 168;
    property MaximumWaterAllocationGrowthByIndex[AIndex: Integer]: Double dispid 169;
    property ReturnFlowVolumeBreakpointYearByIndex[AIndex: Integer]: Integer dispid 170;
    property ReturnFlowVolumeByIndex[AIndex: Integer]: Double dispid 171;
    property SupplyCapacityBreakpointYearByIndex[AIndex: Integer]: Integer dispid 172;
    property SupplyCapacityByIndex[AIndex: Integer]: Double dispid 173;
    property IrrigationEfficiencyBreakpointYearByIndex[AIndex: Integer]: Integer dispid 174;
    property IrrigationEfficiencyByIndex[AIndex: Integer]: Double dispid 175;
    property ReturnFlowFactorBreakpointYearByIndex[AIndex: Integer]: Integer dispid 176;
    property ReturnFlowFactorsByIndex[AIndex: Integer]: Double dispid 177;
    property MultiplicationFactor: Double dispid 178;
  end;

// *********************************************************************//
// Interface: IIrrigationBlockList
// Flags:     (320) Dual OleAutomation
// GUID:      {1EB11A40-22CB-4963-8671-E43007DF7ECE}
// *********************************************************************//
  IIrrigationBlockList = interface(IUnknown)
    ['{1EB11A40-22CB-4963-8671-E43007DF7ECE}']
    function Get_IrrigationBlockCount: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function IrrigationBlockByName(const AName: WideString): IIrrigationBlock; safecall;
    function Get_IrrigationBlockByIndex(AIndex: Integer): IIrrigationBlock; safecall;
    function Get_IrrigationBlockByID(AIndex: Integer): IIrrigationBlock; safecall;
    function Get_IrrigationBlockByBlockNodeNumber(AIrrigationBlockNumber: Integer): IIrrigationBlock; safecall;
    function RemoveIrrigationBlock(AIrrigationBlockID: Integer): WordBool; safecall;
    function CreateIrrigationBlock(AType: Integer): IIrrigationBlock; safecall;
    function CopyCreate(ABlockID: Integer): IIrrigationBlock; safecall;
    property IrrigationBlockCount: Integer read Get_IrrigationBlockCount;
    property IrrigationBlockByIndex[AIndex: Integer]: IIrrigationBlock read Get_IrrigationBlockByIndex;
    property IrrigationBlockByID[AIndex: Integer]: IIrrigationBlock read Get_IrrigationBlockByID;
    property IrrigationBlockByBlockNodeNumber[AIrrigationBlockNumber: Integer]: IIrrigationBlock read Get_IrrigationBlockByBlockNodeNumber;
  end;

// *********************************************************************//
// DispIntf:  IIrrigationBlockListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {1EB11A40-22CB-4963-8671-E43007DF7ECE}
// *********************************************************************//
  IIrrigationBlockListDisp = dispinterface
    ['{1EB11A40-22CB-4963-8671-E43007DF7ECE}']
    property IrrigationBlockCount: Integer readonly dispid 101;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
    function IrrigationBlockByName(const AName: WideString): IIrrigationBlock; dispid 107;
    property IrrigationBlockByIndex[AIndex: Integer]: IIrrigationBlock readonly dispid 102;
    property IrrigationBlockByID[AIndex: Integer]: IIrrigationBlock readonly dispid 103;
    property IrrigationBlockByBlockNodeNumber[AIrrigationBlockNumber: Integer]: IIrrigationBlock readonly dispid 104;
    function RemoveIrrigationBlock(AIrrigationBlockID: Integer): WordBool; dispid 105;
    function CreateIrrigationBlock(AType: Integer): IIrrigationBlock; dispid 108;
    function CopyCreate(ABlockID: Integer): IIrrigationBlock; dispid 109;
  end;

// *********************************************************************//
// Interface: IWetland
// Flags:     (320) Dual OleAutomation
// GUID:      {654C7F3D-22C3-452D-9ECF-390DF6ADF242}
// *********************************************************************//
  IWetland = interface(IUnknown)
    ['{654C7F3D-22C3-452D-9ECF-390DF6ADF242}']
    function Get_Identifier: Integer; safecall;
    function Get_NodeNumber: Integer; safecall;
    procedure Set_NodeNumber(Value: Integer); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_StorageVolume: Double; safecall;
    procedure Set_StorageVolume(Value: Double); safecall;
    function Get_InflowProportion: Double; safecall;
    procedure Set_InflowProportion(Value: Double); safecall;
    function Get_OutflowProportion: Double; safecall;
    procedure Set_OutflowProportion(Value: Double); safecall;
    function Get_UpstreamThreshold: Double; safecall;
    procedure Set_UpstreamThreshold(Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_InflowChannel: IGeneralFlowChannel; safecall;
    function Get_OutflowChannel: IGeneralFlowChannel; safecall;
    function Get_ReservoirDetails: IReservoirData; safecall;
    property Identifier: Integer read Get_Identifier;
    property NodeNumber: Integer read Get_NodeNumber write Set_NodeNumber;
    property Name: WideString read Get_Name write Set_Name;
    property StorageVolume: Double read Get_StorageVolume write Set_StorageVolume;
    property InflowProportion: Double read Get_InflowProportion write Set_InflowProportion;
    property OutflowProportion: Double read Get_OutflowProportion write Set_OutflowProportion;
    property UpstreamThreshold: Double read Get_UpstreamThreshold write Set_UpstreamThreshold;
    property InflowChannel: IGeneralFlowChannel read Get_InflowChannel;
    property OutflowChannel: IGeneralFlowChannel read Get_OutflowChannel;
    property ReservoirDetails: IReservoirData read Get_ReservoirDetails;
  end;

// *********************************************************************//
// DispIntf:  IWetlandDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {654C7F3D-22C3-452D-9ECF-390DF6ADF242}
// *********************************************************************//
  IWetlandDisp = dispinterface
    ['{654C7F3D-22C3-452D-9ECF-390DF6ADF242}']
    property Identifier: Integer readonly dispid 101;
    property NodeNumber: Integer dispid 102;
    property Name: WideString dispid 103;
    property StorageVolume: Double dispid 104;
    property InflowProportion: Double dispid 105;
    property OutflowProportion: Double dispid 106;
    property UpstreamThreshold: Double dispid 107;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
    property InflowChannel: IGeneralFlowChannel readonly dispid 109;
    property OutflowChannel: IGeneralFlowChannel readonly dispid 110;
    property ReservoirDetails: IReservoirData readonly dispid 111;
  end;

// *********************************************************************//
// Interface: IWetlandList
// Flags:     (320) Dual OleAutomation
// GUID:      {446FFBE4-5803-402F-BEF4-140D81A256EA}
// *********************************************************************//
  IWetlandList = interface(IUnknown)
    ['{446FFBE4-5803-402F-BEF4-140D81A256EA}']
    function Get_WetLandCount: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function WetlandByName(const AName: WideString): IWetland; safecall;
    function Get_WetlandByIndex(AIndex: Integer): IWetland; safecall;
    function Get_WetlandByID(AWetlandID: Integer): IWetland; safecall;
    function Get_WetlandByNodeNumber(AWetlandNumber: Integer): IWetland; safecall;
    function RemoveWetland(ANodeNumber: Integer): WordBool; safecall;
    function CreateWetland: IWetland; safecall;
    function CopyCreate(AWetlandID: Integer): IWetland; safecall;
    property WetLandCount: Integer read Get_WetLandCount;
    property WetlandByIndex[AIndex: Integer]: IWetland read Get_WetlandByIndex;
    property WetlandByID[AWetlandID: Integer]: IWetland read Get_WetlandByID;
    property WetlandByNodeNumber[AWetlandNumber: Integer]: IWetland read Get_WetlandByNodeNumber;
  end;

// *********************************************************************//
// DispIntf:  IWetlandListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {446FFBE4-5803-402F-BEF4-140D81A256EA}
// *********************************************************************//
  IWetlandListDisp = dispinterface
    ['{446FFBE4-5803-402F-BEF4-140D81A256EA}']
    property WetLandCount: Integer readonly dispid 101;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 104;
    function WetlandByName(const AName: WideString): IWetland; dispid 107;
    property WetlandByIndex[AIndex: Integer]: IWetland readonly dispid 102;
    property WetlandByID[AWetlandID: Integer]: IWetland readonly dispid 103;
    property WetlandByNodeNumber[AWetlandNumber: Integer]: IWetland readonly dispid 105;
    function RemoveWetland(ANodeNumber: Integer): WordBool; dispid 106;
    function CreateWetland: IWetland; dispid 108;
    function CopyCreate(AWetlandID: Integer): IWetland; dispid 109;
  end;

// *********************************************************************//
// Interface: IDischargeCurve
// Flags:     (320) Dual OleAutomation
// GUID:      {D4A2C02D-7A9F-4A1A-8225-094B0E1B8C51}
// *********************************************************************//
  IDischargeCurve = interface(IUnknown)
    ['{D4A2C02D-7A9F-4A1A-8225-094B0E1B8C51}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ElevationByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ElevationByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_DischargeByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DischargeByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    property ElevationByIndex[AIndex: Integer]: Double read Get_ElevationByIndex write Set_ElevationByIndex;
    property DischargeByIndex[AIndex: Integer]: Double read Get_DischargeByIndex write Set_DischargeByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

// *********************************************************************//
// DispIntf:  IDischargeCurveDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {D4A2C02D-7A9F-4A1A-8225-094B0E1B8C51}
// *********************************************************************//
  IDischargeCurveDisp = dispinterface
    ['{D4A2C02D-7A9F-4A1A-8225-094B0E1B8C51}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 201;
    property ElevationByIndex[AIndex: Integer]: Double dispid 202;
    property DischargeByIndex[AIndex: Integer]: Double dispid 203;
    property CountNrOfPoints: Integer readonly dispid 204;
  end;

// *********************************************************************//
// Interface: ISandAquifer
// Flags:     (320) Dual OleAutomation
// GUID:      {65AEFFF7-6334-4C60-8E38-8F677FB6101D}
// *********************************************************************//
  ISandAquifer = interface(IUnknown)
    ['{65AEFFF7-6334-4C60-8E38-8F677FB6101D}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_HeadDifferenceByIndex(AIndex: Integer): Double; safecall;
    procedure Set_HeadDifferenceByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_AquiferFlowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AquiferFlowByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_DownStreamNodeInflowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DownStreamNodeInflowByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_RiverDepthByIndex(AIndex: Integer): Double; safecall;
    procedure Set_RiverDepthByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    property HeadDifferenceByIndex[AIndex: Integer]: Double read Get_HeadDifferenceByIndex write Set_HeadDifferenceByIndex;
    property AquiferFlowByIndex[AIndex: Integer]: Double read Get_AquiferFlowByIndex write Set_AquiferFlowByIndex;
    property DownStreamNodeInflowByIndex[AIndex: Integer]: Double read Get_DownStreamNodeInflowByIndex write Set_DownStreamNodeInflowByIndex;
    property RiverDepthByIndex[AIndex: Integer]: Double read Get_RiverDepthByIndex write Set_RiverDepthByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

// *********************************************************************//
// DispIntf:  ISandAquiferDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {65AEFFF7-6334-4C60-8E38-8F677FB6101D}
// *********************************************************************//
  ISandAquiferDisp = dispinterface
    ['{65AEFFF7-6334-4C60-8E38-8F677FB6101D}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 201;
    property HeadDifferenceByIndex[AIndex: Integer]: Double dispid 202;
    property AquiferFlowByIndex[AIndex: Integer]: Double dispid 203;
    property DownStreamNodeInflowByIndex[AIndex: Integer]: Double dispid 204;
    property RiverDepthByIndex[AIndex: Integer]: Double dispid 205;
    property CountNrOfPoints: Integer readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IKFactors
// Flags:     (320) Dual OleAutomation
// GUID:      {1F6FBC5C-3A16-474E-9BE9-3F56112394E5}
// *********************************************************************//
  IKFactors = interface(IUnknown)
    ['{1F6FBC5C-3A16-474E-9BE9-3F56112394E5}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ChannelNumberByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ChannelNumberByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_KFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_KFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    property ChannelNumberByIndex[AIndex: Integer]: Double read Get_ChannelNumberByIndex write Set_ChannelNumberByIndex;
    property KFactorByIndex[AIndex: Integer]: Double read Get_KFactorByIndex write Set_KFactorByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

// *********************************************************************//
// DispIntf:  IKFactorsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {1F6FBC5C-3A16-474E-9BE9-3F56112394E5}
// *********************************************************************//
  IKFactorsDisp = dispinterface
    ['{1F6FBC5C-3A16-474E-9BE9-3F56112394E5}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 201;
    property ChannelNumberByIndex[AIndex: Integer]: Double dispid 202;
    property KFactorByIndex[AIndex: Integer]: Double dispid 203;
    property CountNrOfPoints: Integer readonly dispid 204;
  end;

// *********************************************************************//
// Interface: ISubmergedOutlet
// Flags:     (320) Dual OleAutomation
// GUID:      {DFA99D79-7271-4232-96E6-8085675FDFA4}
// *********************************************************************//
  ISubmergedOutlet = interface(IUnknown)
    ['{DFA99D79-7271-4232-96E6-8085675FDFA4}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ElevationDifferenceByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ElevationDifferenceByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_MonthlyAverageInflowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyAverageInflowByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_MonthlyAverageDivertedFlowByIndex(ARow: Integer; ACol: Integer): Double; safecall;
    procedure Set_MonthlyAverageDivertedFlowByIndex(ARow: Integer; ACol: Integer; Value: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    property ElevationDifferenceByIndex[AIndex: Integer]: Double read Get_ElevationDifferenceByIndex write Set_ElevationDifferenceByIndex;
    property MonthlyAverageInflowByIndex[AIndex: Integer]: Double read Get_MonthlyAverageInflowByIndex write Set_MonthlyAverageInflowByIndex;
    property MonthlyAverageDivertedFlowByIndex[ARow: Integer; ACol: Integer]: Double read Get_MonthlyAverageDivertedFlowByIndex write Set_MonthlyAverageDivertedFlowByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

// *********************************************************************//
// DispIntf:  ISubmergedOutletDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DFA99D79-7271-4232-96E6-8085675FDFA4}
// *********************************************************************//
  ISubmergedOutletDisp = dispinterface
    ['{DFA99D79-7271-4232-96E6-8085675FDFA4}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 201;
    property ElevationDifferenceByIndex[AIndex: Integer]: Double dispid 202;
    property MonthlyAverageInflowByIndex[AIndex: Integer]: Double dispid 203;
    property MonthlyAverageDivertedFlowByIndex[ARow: Integer; ACol: Integer]: Double dispid 204;
    property CountNrOfPoints: Integer readonly dispid 205;
  end;

// *********************************************************************//
// Interface: IPumpStation
// Flags:     (320) Dual OleAutomation
// GUID:      {9477C918-43C9-4A59-8896-8DEBB9860B94}
// *********************************************************************//
  IPumpStation = interface(IUnknown)
    ['{9477C918-43C9-4A59-8896-8DEBB9860B94}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_PumpingHeadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_PumpingHeadByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_PumpingDischargeByIndex(AIndex: Integer): Double; safecall;
    procedure Set_PumpingDischargeByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    property PumpingHeadByIndex[AIndex: Integer]: Double read Get_PumpingHeadByIndex write Set_PumpingHeadByIndex;
    property PumpingDischargeByIndex[AIndex: Integer]: Double read Get_PumpingDischargeByIndex write Set_PumpingDischargeByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

// *********************************************************************//
// DispIntf:  IPumpStationDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {9477C918-43C9-4A59-8896-8DEBB9860B94}
// *********************************************************************//
  IPumpStationDisp = dispinterface
    ['{9477C918-43C9-4A59-8896-8DEBB9860B94}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 201;
    property PumpingHeadByIndex[AIndex: Integer]: Double dispid 202;
    property PumpingDischargeByIndex[AIndex: Integer]: Double dispid 203;
    property CountNrOfPoints: Integer readonly dispid 204;
  end;

// *********************************************************************//
// Interface: IYMDemandCentreList
// Flags:     (320) Dual OleAutomation
// GUID:      {C4FCFE02-5B63-48EF-B598-073309F09DBF}
// *********************************************************************//
  IYMDemandCentreList = interface(IUnknown)
    ['{C4FCFE02-5B63-48EF-B598-073309F09DBF}']
    function Get_YMDemandCentreCount: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function YMDemandCentreByName(const AName: WideString): IYMDemandCentre; safecall;
    function Get_YMDemandCentreByIndex(AIndex: Integer): IYMDemandCentre; safecall;
    function Get_YMDemandCentreByID(AYMDemandCentreID: Integer): IYMDemandCentre; safecall;
    function Get_YMDemandCentreByNodeNumber(AYMDemandCentreNodeNr: Integer): IYMDemandCentre; safecall;
    function RemoveYMDemandCentre(AYMDemandCentreNodeNr: Integer): WordBool; safecall;
    function CreateYMDemandCentre: IYMDemandCentre; safecall;
    function CopyCreate(ANodeNumber: Integer): IYMDemandCentre; safecall;
    property YMDemandCentreCount: Integer read Get_YMDemandCentreCount;
    property YMDemandCentreByIndex[AIndex: Integer]: IYMDemandCentre read Get_YMDemandCentreByIndex;
    property YMDemandCentreByID[AYMDemandCentreID: Integer]: IYMDemandCentre read Get_YMDemandCentreByID;
    property YMDemandCentreByNodeNumber[AYMDemandCentreNodeNr: Integer]: IYMDemandCentre read Get_YMDemandCentreByNodeNumber;
  end;

// *********************************************************************//
// DispIntf:  IYMDemandCentreListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {C4FCFE02-5B63-48EF-B598-073309F09DBF}
// *********************************************************************//
  IYMDemandCentreListDisp = dispinterface
    ['{C4FCFE02-5B63-48EF-B598-073309F09DBF}']
    property YMDemandCentreCount: Integer readonly dispid 101;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 102;
    function YMDemandCentreByName(const AName: WideString): IYMDemandCentre; dispid 103;
    property YMDemandCentreByIndex[AIndex: Integer]: IYMDemandCentre readonly dispid 104;
    property YMDemandCentreByID[AYMDemandCentreID: Integer]: IYMDemandCentre readonly dispid 105;
    property YMDemandCentreByNodeNumber[AYMDemandCentreNodeNr: Integer]: IYMDemandCentre readonly dispid 106;
    function RemoveYMDemandCentre(AYMDemandCentreNodeNr: Integer): WordBool; dispid 107;
    function CreateYMDemandCentre: IYMDemandCentre; dispid 108;
    function CopyCreate(ANodeNumber: Integer): IYMDemandCentre; dispid 109;
  end;

// *********************************************************************//
// Interface: IYMDemandCentre
// Flags:     (320) Dual OleAutomation
// GUID:      {0C70C527-39EE-4179-BD48-CD7C5BF39901}
// *********************************************************************//
  IYMDemandCentre = interface(IUnknown)
    ['{0C70C527-39EE-4179-BD48-CD7C5BF39901}']
    function Get_Identifier: Integer; safecall;
    function Get_NodeNumber: Integer; safecall;
    procedure Set_NodeNumber(Value: Integer); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_NodeRefNr: Integer; safecall;
    procedure Set_NodeRefNr(Value: Integer); safecall;
    function Get_AveReturnFlowFactor: Double; safecall;
    procedure Set_AveReturnFlowFactor(Value: Double); safecall;
    function Get_AveEvaporation: Double; safecall;
    procedure Set_AveEvaporation(Value: Double); safecall;
    function Get_StdDeviationFactor: Double; safecall;
    procedure Set_StdDeviationFactor(Value: Double); safecall;
    function Get_RoutingConstant: Double; safecall;
    procedure Set_RoutingConstant(Value: Double); safecall;
    function Get_RainfallScalingFactor: Double; safecall;
    procedure Set_RainfallScalingFactor(Value: Double); safecall;
    function Get_TotalFlowLost: Double; safecall;
    procedure Set_TotalFlowLost(Value: Double); safecall;
    function Get_EvapoTranspiration(AMonth: Integer): Double; safecall;
    procedure Set_EvapoTranspiration(AMonth: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ConsumptiveUseChannel: IGeneralFlowChannel; safecall;
    function Get_ReclaimationChannel: IGeneralFlowChannel; safecall;
    function Get_ConsumptiveUseChannelNr: Integer; safecall;
    procedure Set_ConsumptiveUseChannelNr(Value: Integer); safecall;
    function Get_ReclaimationPlantExists: WordBool; safecall;
    procedure Set_ReclaimationPlantExists(Value: WordBool); safecall;
    function Get_ReclaimationChannelNr: Integer; safecall;
    procedure Set_ReclaimationChannelNr(Value: Integer); safecall;
    function Get_ReturnFlowFeatureList: IYMDemandCentreReturnFlowFeatureList; safecall;
    function Get_SupplyChannelNrs: WideString; safecall;
    function Get_SupplyChannelCount: Integer; safecall;
    function Get_SupplyChannelByIndex(AIndex: Integer): IGeneralFlowChannel; safecall;
    property Identifier: Integer read Get_Identifier;
    property NodeNumber: Integer read Get_NodeNumber write Set_NodeNumber;
    property Name: WideString read Get_Name write Set_Name;
    property Description: WideString read Get_Description write Set_Description;
    property NodeRefNr: Integer read Get_NodeRefNr write Set_NodeRefNr;
    property AveReturnFlowFactor: Double read Get_AveReturnFlowFactor write Set_AveReturnFlowFactor;
    property AveEvaporation: Double read Get_AveEvaporation write Set_AveEvaporation;
    property StdDeviationFactor: Double read Get_StdDeviationFactor write Set_StdDeviationFactor;
    property RoutingConstant: Double read Get_RoutingConstant write Set_RoutingConstant;
    property RainfallScalingFactor: Double read Get_RainfallScalingFactor write Set_RainfallScalingFactor;
    property TotalFlowLost: Double read Get_TotalFlowLost write Set_TotalFlowLost;
    property EvapoTranspiration[AMonth: Integer]: Double read Get_EvapoTranspiration write Set_EvapoTranspiration;
    property ConsumptiveUseChannel: IGeneralFlowChannel read Get_ConsumptiveUseChannel;
    property ReclaimationChannel: IGeneralFlowChannel read Get_ReclaimationChannel;
    property ConsumptiveUseChannelNr: Integer read Get_ConsumptiveUseChannelNr write Set_ConsumptiveUseChannelNr;
    property ReclaimationPlantExists: WordBool read Get_ReclaimationPlantExists write Set_ReclaimationPlantExists;
    property ReclaimationChannelNr: Integer read Get_ReclaimationChannelNr write Set_ReclaimationChannelNr;
    property ReturnFlowFeatureList: IYMDemandCentreReturnFlowFeatureList read Get_ReturnFlowFeatureList;
    property SupplyChannelNrs: WideString read Get_SupplyChannelNrs;
    property SupplyChannelCount: Integer read Get_SupplyChannelCount;
    property SupplyChannelByIndex[AIndex: Integer]: IGeneralFlowChannel read Get_SupplyChannelByIndex;
  end;

// *********************************************************************//
// DispIntf:  IYMDemandCentreDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {0C70C527-39EE-4179-BD48-CD7C5BF39901}
// *********************************************************************//
  IYMDemandCentreDisp = dispinterface
    ['{0C70C527-39EE-4179-BD48-CD7C5BF39901}']
    property Identifier: Integer readonly dispid 201;
    property NodeNumber: Integer dispid 202;
    property Name: WideString dispid 203;
    property Description: WideString dispid 204;
    property NodeRefNr: Integer dispid 205;
    property AveReturnFlowFactor: Double dispid 206;
    property AveEvaporation: Double dispid 207;
    property StdDeviationFactor: Double dispid 208;
    property RoutingConstant: Double dispid 209;
    property RainfallScalingFactor: Double dispid 210;
    property TotalFlowLost: Double dispid 211;
    property EvapoTranspiration[AMonth: Integer]: Double dispid 212;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 101;
    property ConsumptiveUseChannel: IGeneralFlowChannel readonly dispid 102;
    property ReclaimationChannel: IGeneralFlowChannel readonly dispid 103;
    property ConsumptiveUseChannelNr: Integer dispid 104;
    property ReclaimationPlantExists: WordBool dispid 105;
    property ReclaimationChannelNr: Integer dispid 106;
    property ReturnFlowFeatureList: IYMDemandCentreReturnFlowFeatureList readonly dispid 107;
    property SupplyChannelNrs: WideString readonly dispid 108;
    property SupplyChannelCount: Integer readonly dispid 109;
    property SupplyChannelByIndex[AIndex: Integer]: IGeneralFlowChannel readonly dispid 110;
  end;

// *********************************************************************//
// Interface: IStreamFlowReduction
// Flags:     (320) Dual OleAutomation
// GUID:      {2C7B7AEC-91B4-4B89-BB33-E6DC5E130CE5}
// *********************************************************************//
  IStreamFlowReduction = interface(IUnknown)
    ['{2C7B7AEC-91B4-4B89-BB33-E6DC5E130CE5}']
    function Get_Identifier: Integer; safecall;
    function Get_InflowNodeNumber: Integer; safecall;
    procedure Set_InflowNodeNumber(Value: Integer); safecall;
    function Get_CoveredArea: Double; safecall;
    procedure Set_CoveredArea(Value: Double); safecall;
    function Get_UnitRunoffFileName: WideString; safecall;
    procedure Set_UnitRunoffFileName(const Value: WideString); safecall;
    function Get_SoilMoistureFileName: WideString; safecall;
    procedure Set_SoilMoistureFileName(const Value: WideString); safecall;
    function Get_SFRName: WideString; safecall;
    procedure Set_SFRName(const Value: WideString); safecall;
    function Get_SFRDescription: WideString; safecall;
    procedure Set_SFRDescription(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_UnitRunoffFileData: WideString; safecall;
    function Get_SoilMoistureFileData: WideString; safecall;
    function Get_InflowNodeDetails: IReservoirData; safecall;
    property Identifier: Integer read Get_Identifier;
    property InflowNodeNumber: Integer read Get_InflowNodeNumber write Set_InflowNodeNumber;
    property CoveredArea: Double read Get_CoveredArea write Set_CoveredArea;
    property UnitRunoffFileName: WideString read Get_UnitRunoffFileName write Set_UnitRunoffFileName;
    property SoilMoistureFileName: WideString read Get_SoilMoistureFileName write Set_SoilMoistureFileName;
    property SFRName: WideString read Get_SFRName write Set_SFRName;
    property SFRDescription: WideString read Get_SFRDescription write Set_SFRDescription;
    property UnitRunoffFileData: WideString read Get_UnitRunoffFileData;
    property SoilMoistureFileData: WideString read Get_SoilMoistureFileData;
    property InflowNodeDetails: IReservoirData read Get_InflowNodeDetails;
  end;

// *********************************************************************//
// DispIntf:  IStreamFlowReductionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {2C7B7AEC-91B4-4B89-BB33-E6DC5E130CE5}
// *********************************************************************//
  IStreamFlowReductionDisp = dispinterface
    ['{2C7B7AEC-91B4-4B89-BB33-E6DC5E130CE5}']
    property Identifier: Integer readonly dispid 201;
    property InflowNodeNumber: Integer dispid 202;
    property CoveredArea: Double dispid 203;
    property UnitRunoffFileName: WideString dispid 204;
    property SoilMoistureFileName: WideString dispid 205;
    property SFRName: WideString dispid 206;
    property SFRDescription: WideString dispid 207;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 101;
    property UnitRunoffFileData: WideString readonly dispid 102;
    property SoilMoistureFileData: WideString readonly dispid 103;
    property InflowNodeDetails: IReservoirData readonly dispid 104;
  end;

// *********************************************************************//
// Interface: IStreamFlowReductionList
// Flags:     (320) Dual OleAutomation
// GUID:      {EF13F588-AB78-4A0F-BF0C-C4168E8C8F35}
// *********************************************************************//
  IStreamFlowReductionList = interface(IUnknown)
    ['{EF13F588-AB78-4A0F-BF0C-C4168E8C8F35}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_StreamFlowReductionCount: Integer; safecall;
    function Get_StreamFlowReductionByIndex(AIndex: Integer): IStreamFlowReduction; safecall;
    function Get_StreamFlowReductionByID(AStreamFlowReductionID: Integer): IStreamFlowReduction; safecall;
    function StreamFlowReductionIDsPerInflowNode(AInflowNodeNumber: Integer): WideString; safecall;
    function CopyCreate(AStreamFlowReductionID: Integer): IStreamFlowReduction; safecall;
    property StreamFlowReductionCount: Integer read Get_StreamFlowReductionCount;
    property StreamFlowReductionByIndex[AIndex: Integer]: IStreamFlowReduction read Get_StreamFlowReductionByIndex;
    property StreamFlowReductionByID[AStreamFlowReductionID: Integer]: IStreamFlowReduction read Get_StreamFlowReductionByID;
  end;

// *********************************************************************//
// DispIntf:  IStreamFlowReductionListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {EF13F588-AB78-4A0F-BF0C-C4168E8C8F35}
// *********************************************************************//
  IStreamFlowReductionListDisp = dispinterface
    ['{EF13F588-AB78-4A0F-BF0C-C4168E8C8F35}']
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 110;
    property StreamFlowReductionCount: Integer readonly dispid 201;
    property StreamFlowReductionByIndex[AIndex: Integer]: IStreamFlowReduction readonly dispid 202;
    property StreamFlowReductionByID[AStreamFlowReductionID: Integer]: IStreamFlowReduction readonly dispid 203;
    function StreamFlowReductionIDsPerInflowNode(AInflowNodeNumber: Integer): WideString; dispid 101;
    function CopyCreate(AStreamFlowReductionID: Integer): IStreamFlowReduction; dispid 102;
  end;

// *********************************************************************//
// Interface: IYMDemandCentreReturnFlowFeature
// Flags:     (320) Dual OleAutomation
// GUID:      {C740A20C-B0F8-4F21-BCA3-D058A4C51016}
// *********************************************************************//
  IYMDemandCentreReturnFlowFeature = interface(IUnknown)
    ['{C740A20C-B0F8-4F21-BCA3-D058A4C51016}']
    function Get_FeatureID: Integer; safecall;
    function Get_Channel: IGeneralFlowChannel; safecall;
    procedure Set_Channel(const Value: IGeneralFlowChannel); safecall;
    function Get_DemandCentreID: Integer; safecall;
    procedure Set_DemandCentreID(Value: Integer); safecall;
    function Get_ChannelNr: Integer; safecall;
    procedure Set_ChannelNr(Value: Integer); safecall;
    function Get_TotalReturnFlow: Double; safecall;
    procedure Set_TotalReturnFlow(Value: Double); safecall;
    function Get_FlowDiversion: Double; safecall;
    procedure Set_FlowDiversion(Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property FeatureID: Integer read Get_FeatureID;
    property Channel: IGeneralFlowChannel read Get_Channel write Set_Channel;
    property DemandCentreID: Integer read Get_DemandCentreID write Set_DemandCentreID;
    property ChannelNr: Integer read Get_ChannelNr write Set_ChannelNr;
    property TotalReturnFlow: Double read Get_TotalReturnFlow write Set_TotalReturnFlow;
    property FlowDiversion: Double read Get_FlowDiversion write Set_FlowDiversion;
  end;

// *********************************************************************//
// DispIntf:  IYMDemandCentreReturnFlowFeatureDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {C740A20C-B0F8-4F21-BCA3-D058A4C51016}
// *********************************************************************//
  IYMDemandCentreReturnFlowFeatureDisp = dispinterface
    ['{C740A20C-B0F8-4F21-BCA3-D058A4C51016}']
    property FeatureID: Integer readonly dispid 101;
    property Channel: IGeneralFlowChannel dispid 103;
    property DemandCentreID: Integer dispid 104;
    property ChannelNr: Integer dispid 105;
    property TotalReturnFlow: Double dispid 106;
    property FlowDiversion: Double dispid 107;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
  end;

// *********************************************************************//
// Interface: IYMDemandCentreReturnFlowFeatureList
// Flags:     (320) Dual OleAutomation
// GUID:      {E40D8681-53EE-42BE-B7BE-EE2BC1FCEA3D}
// *********************************************************************//
  IYMDemandCentreReturnFlowFeatureList = interface(IUnknown)
    ['{E40D8681-53EE-42BE-B7BE-EE2BC1FCEA3D}']
    function Get_ReturnFlowFeatureCount: Integer; safecall;
    function Get_ReturnFlowFeatureByIndex(AIndex: Integer): IYMDemandCentreReturnFlowFeature; safecall;
    function Get_ReturnFlowFeatureByID(AFeatureID: Integer): IYMDemandCentreReturnFlowFeature; safecall;
    function CreateReturnFlowFeature(ADemandCentreID: Integer): IYMDemandCentreReturnFlowFeature; safecall;
    function RemoveReturnFlowFeatureWithNr(ADemandCentreID: Integer; AChannelNr: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ReturnFlowFeatureCount: Integer read Get_ReturnFlowFeatureCount;
    property ReturnFlowFeatureByIndex[AIndex: Integer]: IYMDemandCentreReturnFlowFeature read Get_ReturnFlowFeatureByIndex;
    property ReturnFlowFeatureByID[AFeatureID: Integer]: IYMDemandCentreReturnFlowFeature read Get_ReturnFlowFeatureByID;
  end;

// *********************************************************************//
// DispIntf:  IYMDemandCentreReturnFlowFeatureListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {E40D8681-53EE-42BE-B7BE-EE2BC1FCEA3D}
// *********************************************************************//
  IYMDemandCentreReturnFlowFeatureListDisp = dispinterface
    ['{E40D8681-53EE-42BE-B7BE-EE2BC1FCEA3D}']
    property ReturnFlowFeatureCount: Integer readonly dispid 201;
    property ReturnFlowFeatureByIndex[AIndex: Integer]: IYMDemandCentreReturnFlowFeature readonly dispid 202;
    property ReturnFlowFeatureByID[AFeatureID: Integer]: IYMDemandCentreReturnFlowFeature readonly dispid 203;
    function CreateReturnFlowFeature(ADemandCentreID: Integer): IYMDemandCentreReturnFlowFeature; dispid 204;
    function RemoveReturnFlowFeatureWithNr(ADemandCentreID: Integer; AChannelNr: Integer): WordBool; dispid 205;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 206;
  end;

// *********************************************************************//
// Interface: IOpenCast
// Flags:     (320) Dual OleAutomation
// GUID:      {5938EAE4-44E1-4C7B-9FFB-748C85FA03C5}
// *********************************************************************//
  IOpenCast = interface(IUnknown)
    ['{5938EAE4-44E1-4C7B-9FFB-748C85FA03C5}']
    function Get_Identifier: Integer; safecall;
    function Get_PitName: WideString; safecall;
    procedure Set_PitName(const Value: WideString); safecall;
    function Get_CoalReserveArea: Double; safecall;
    procedure Set_CoalReserveArea(Value: Double); safecall;
    function Get_WorkingsArea: Double; safecall;
    procedure Set_WorkingsArea(Value: Double); safecall;
    function Get_DisturbedWorkingsArea: Double; safecall;
    procedure Set_DisturbedWorkingsArea(Value: Double); safecall;
    function Get_DisturbedArea: Double; safecall;
    procedure Set_DisturbedArea(Value: Double); safecall;
    function Get_WaterSurfaceEvapArea: Double; safecall;
    procedure Set_WaterSurfaceEvapArea(Value: Double); safecall;
    function Get_DisturbedAreaRunoff: Double; safecall;
    procedure Set_DisturbedAreaRunoff(Value: Double); safecall;
    function Get_DecantVolume: Double; safecall;
    procedure Set_DecantVolume(Value: Double); safecall;
    function Get_SeepageVolume: Double; safecall;
    procedure Set_SeepageVolume(Value: Double); safecall;
    function Get_AnalysisStartVolume: Double; safecall;
    procedure Set_AnalysisStartVolume(Value: Double); safecall;
    function Get_MaximumSeepageRate: Double; safecall;
    procedure Set_MaximumSeepageRate(Value: Double); safecall;
    function Get_SeepageExponent: Double; safecall;
    procedure Set_SeepageExponent(Value: Double); safecall;
    function Get_PCDSurfaceArea: Double; safecall;
    procedure Set_PCDSurfaceArea(Value: Double); safecall;
    function Get_PCDStorageCapacity: Double; safecall;
    procedure Set_PCDStorageCapacity(Value: Double); safecall;
    function Get_PCDAnalysisStartVolume: Double; safecall;
    procedure Set_PCDAnalysisStartVolume(Value: Double); safecall;
    function Get_DisturbedWorkingsAreaRunoff: Double; safecall;
    procedure Set_DisturbedWorkingsAreaRunoff(Value: Double); safecall;
    function Get_DisturbedRechargeFactor(AIndex: Integer): Double; safecall;
    procedure Set_DisturbedRechargeFactor(AIndex: Integer; Value: Double); safecall;
    function Get_WorkingAreaRechargeFactor(AIndex: Integer): Double; safecall;
    procedure Set_WorkingAreaRechargeFactor(AIndex: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property Identifier: Integer read Get_Identifier;
    property PitName: WideString read Get_PitName write Set_PitName;
    property CoalReserveArea: Double read Get_CoalReserveArea write Set_CoalReserveArea;
    property WorkingsArea: Double read Get_WorkingsArea write Set_WorkingsArea;
    property DisturbedWorkingsArea: Double read Get_DisturbedWorkingsArea write Set_DisturbedWorkingsArea;
    property DisturbedArea: Double read Get_DisturbedArea write Set_DisturbedArea;
    property WaterSurfaceEvapArea: Double read Get_WaterSurfaceEvapArea write Set_WaterSurfaceEvapArea;
    property DisturbedAreaRunoff: Double read Get_DisturbedAreaRunoff write Set_DisturbedAreaRunoff;
    property DecantVolume: Double read Get_DecantVolume write Set_DecantVolume;
    property SeepageVolume: Double read Get_SeepageVolume write Set_SeepageVolume;
    property AnalysisStartVolume: Double read Get_AnalysisStartVolume write Set_AnalysisStartVolume;
    property MaximumSeepageRate: Double read Get_MaximumSeepageRate write Set_MaximumSeepageRate;
    property SeepageExponent: Double read Get_SeepageExponent write Set_SeepageExponent;
    property PCDSurfaceArea: Double read Get_PCDSurfaceArea write Set_PCDSurfaceArea;
    property PCDStorageCapacity: Double read Get_PCDStorageCapacity write Set_PCDStorageCapacity;
    property PCDAnalysisStartVolume: Double read Get_PCDAnalysisStartVolume write Set_PCDAnalysisStartVolume;
    property DisturbedWorkingsAreaRunoff: Double read Get_DisturbedWorkingsAreaRunoff write Set_DisturbedWorkingsAreaRunoff;
    property DisturbedRechargeFactor[AIndex: Integer]: Double read Get_DisturbedRechargeFactor write Set_DisturbedRechargeFactor;
    property WorkingAreaRechargeFactor[AIndex: Integer]: Double read Get_WorkingAreaRechargeFactor write Set_WorkingAreaRechargeFactor;
  end;

// *********************************************************************//
// DispIntf:  IOpenCastDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {5938EAE4-44E1-4C7B-9FFB-748C85FA03C5}
// *********************************************************************//
  IOpenCastDisp = dispinterface
    ['{5938EAE4-44E1-4C7B-9FFB-748C85FA03C5}']
    property Identifier: Integer readonly dispid 201;
    property PitName: WideString dispid 202;
    property CoalReserveArea: Double dispid 203;
    property WorkingsArea: Double dispid 204;
    property DisturbedWorkingsArea: Double dispid 205;
    property DisturbedArea: Double dispid 206;
    property WaterSurfaceEvapArea: Double dispid 207;
    property DisturbedAreaRunoff: Double dispid 208;
    property DecantVolume: Double dispid 209;
    property SeepageVolume: Double dispid 210;
    property AnalysisStartVolume: Double dispid 211;
    property MaximumSeepageRate: Double dispid 212;
    property SeepageExponent: Double dispid 213;
    property PCDSurfaceArea: Double dispid 214;
    property PCDStorageCapacity: Double dispid 215;
    property PCDAnalysisStartVolume: Double dispid 216;
    property DisturbedWorkingsAreaRunoff: Double dispid 217;
    property DisturbedRechargeFactor[AIndex: Integer]: Double dispid 218;
    property WorkingAreaRechargeFactor[AIndex: Integer]: Double dispid 219;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 220;
  end;

// *********************************************************************//
// Interface: IUnderground
// Flags:     (320) Dual OleAutomation
// GUID:      {C6A51A66-97DA-4A20-B862-A727F2A0861E}
// *********************************************************************//
  IUnderground = interface(IUnknown)
    ['{C6A51A66-97DA-4A20-B862-A727F2A0861E}']
    function Get_Identifier: Integer; safecall;
    function Get_UndergroundSectionName: WideString; safecall;
    procedure Set_UndergroundSectionName(const Value: WideString); safecall;
    function Get_ChannelNumberToUGDam: Integer; safecall;
    procedure Set_ChannelNumberToUGDam(Value: Integer); safecall;
    function Get_UpstreamCatchmentArea: Double; safecall;
    procedure Set_UpstreamCatchmentArea(Value: Double); safecall;
    function Get_BoardPillarCatchmentArea: Double; safecall;
    procedure Set_BoardPillarCatchmentArea(Value: Double); safecall;
    function Get_HighExtractionCatchmentArea: Double; safecall;
    procedure Set_HighExtractionCatchmentArea(Value: Double); safecall;
    function Get_HighExtractionAreaRunoffFactor: Double; safecall;
    procedure Set_HighExtractionAreaRunoffFactor(Value: Double); safecall;
    function Get_UpstreamRunoffPortion(AIndex: Integer): Double; safecall;
    procedure Set_UpstreamRunoffPortion(AIndex: Integer; Value: Double); safecall;
    function Get_BoardAndPilarRechargeFactor(AIndex: Integer): Double; safecall;
    procedure Set_BoardAndPilarRechargeFactor(AIndex: Integer; Value: Double); safecall;
    function Get_HighExtractionRechargeFactor(AIndex: Integer): Double; safecall;
    procedure Set_HighExtractionRechargeFactor(AIndex: Integer; Value: Double); safecall;
    function Get_ChannelToUnderGroundDam: IGeneralFlowChannel; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_UndergroundDam: IReservoirData; safecall;
    property Identifier: Integer read Get_Identifier;
    property UndergroundSectionName: WideString read Get_UndergroundSectionName write Set_UndergroundSectionName;
    property ChannelNumberToUGDam: Integer read Get_ChannelNumberToUGDam write Set_ChannelNumberToUGDam;
    property UpstreamCatchmentArea: Double read Get_UpstreamCatchmentArea write Set_UpstreamCatchmentArea;
    property BoardPillarCatchmentArea: Double read Get_BoardPillarCatchmentArea write Set_BoardPillarCatchmentArea;
    property HighExtractionCatchmentArea: Double read Get_HighExtractionCatchmentArea write Set_HighExtractionCatchmentArea;
    property HighExtractionAreaRunoffFactor: Double read Get_HighExtractionAreaRunoffFactor write Set_HighExtractionAreaRunoffFactor;
    property UpstreamRunoffPortion[AIndex: Integer]: Double read Get_UpstreamRunoffPortion write Set_UpstreamRunoffPortion;
    property BoardAndPilarRechargeFactor[AIndex: Integer]: Double read Get_BoardAndPilarRechargeFactor write Set_BoardAndPilarRechargeFactor;
    property HighExtractionRechargeFactor[AIndex: Integer]: Double read Get_HighExtractionRechargeFactor write Set_HighExtractionRechargeFactor;
    property ChannelToUnderGroundDam: IGeneralFlowChannel read Get_ChannelToUnderGroundDam;
    property UndergroundDam: IReservoirData read Get_UndergroundDam;
  end;

// *********************************************************************//
// DispIntf:  IUndergroundDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {C6A51A66-97DA-4A20-B862-A727F2A0861E}
// *********************************************************************//
  IUndergroundDisp = dispinterface
    ['{C6A51A66-97DA-4A20-B862-A727F2A0861E}']
    property Identifier: Integer readonly dispid 201;
    property UndergroundSectionName: WideString dispid 202;
    property ChannelNumberToUGDam: Integer dispid 203;
    property UpstreamCatchmentArea: Double dispid 204;
    property BoardPillarCatchmentArea: Double dispid 205;
    property HighExtractionCatchmentArea: Double dispid 206;
    property HighExtractionAreaRunoffFactor: Double dispid 207;
    property UpstreamRunoffPortion[AIndex: Integer]: Double dispid 208;
    property BoardAndPilarRechargeFactor[AIndex: Integer]: Double dispid 209;
    property HighExtractionRechargeFactor[AIndex: Integer]: Double dispid 210;
    property ChannelToUnderGroundDam: IGeneralFlowChannel readonly dispid 211;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 212;
    property UndergroundDam: IReservoirData readonly dispid 213;
  end;

// *********************************************************************//
// Interface: ISlurryDump
// Flags:     (320) Dual OleAutomation
// GUID:      {6880B920-1E02-4BDA-B71E-504328F65BB1}
// *********************************************************************//
  ISlurryDump = interface(IUnknown)
    ['{6880B920-1E02-4BDA-B71E-504328F65BB1}']
    function Get_Identifier: Integer; safecall;
    function Get_DumpName: WideString; safecall;
    procedure Set_DumpName(const Value: WideString); safecall;
    function Get_DumpSurfaceArea: Double; safecall;
    procedure Set_DumpSurfaceArea(Value: Double); safecall;
    function Get_RunoffFactorToPCD: Double; safecall;
    procedure Set_RunoffFactorToPCD(Value: Double); safecall;
    function Get_SeepageSplitFactor: Double; safecall;
    procedure Set_SeepageSplitFactor(Value: Double); safecall;
    function Get_PCDStorageCapacity: Double; safecall;
    procedure Set_PCDStorageCapacity(Value: Double); safecall;
    function Get_PCDSurfaceArea: Double; safecall;
    procedure Set_PCDSurfaceArea(Value: Double); safecall;
    function Get_PCDAnalysisStartVolume: Double; safecall;
    procedure Set_PCDAnalysisStartVolume(Value: Double); safecall;
    function Get_RechargeFactor(AIndex: Integer): Double; safecall;
    procedure Set_RechargeFactor(AIndex: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property Identifier: Integer read Get_Identifier;
    property DumpName: WideString read Get_DumpName write Set_DumpName;
    property DumpSurfaceArea: Double read Get_DumpSurfaceArea write Set_DumpSurfaceArea;
    property RunoffFactorToPCD: Double read Get_RunoffFactorToPCD write Set_RunoffFactorToPCD;
    property SeepageSplitFactor: Double read Get_SeepageSplitFactor write Set_SeepageSplitFactor;
    property PCDStorageCapacity: Double read Get_PCDStorageCapacity write Set_PCDStorageCapacity;
    property PCDSurfaceArea: Double read Get_PCDSurfaceArea write Set_PCDSurfaceArea;
    property PCDAnalysisStartVolume: Double read Get_PCDAnalysisStartVolume write Set_PCDAnalysisStartVolume;
    property RechargeFactor[AIndex: Integer]: Double read Get_RechargeFactor write Set_RechargeFactor;
  end;

// *********************************************************************//
// DispIntf:  ISlurryDumpDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6880B920-1E02-4BDA-B71E-504328F65BB1}
// *********************************************************************//
  ISlurryDumpDisp = dispinterface
    ['{6880B920-1E02-4BDA-B71E-504328F65BB1}']
    property Identifier: Integer readonly dispid 101;
    property DumpName: WideString dispid 102;
    property DumpSurfaceArea: Double dispid 103;
    property RunoffFactorToPCD: Double dispid 104;
    property SeepageSplitFactor: Double dispid 105;
    property PCDStorageCapacity: Double dispid 106;
    property PCDSurfaceArea: Double dispid 107;
    property PCDAnalysisStartVolume: Double dispid 108;
    property RechargeFactor[AIndex: Integer]: Double dispid 109;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 110;
  end;

// *********************************************************************//
// Interface: IMine
// Flags:     (320) Dual OleAutomation
// GUID:      {0701352E-A9A4-4C99-A665-097099769D6F}
// *********************************************************************//
  IMine = interface(IUnknown)
    ['{0701352E-A9A4-4C99-A665-097099769D6F}']
    function Get_Identifier: Integer; safecall;
    function Get_NodeNumber: Integer; safecall;
    function Get_MineName: WideString; safecall;
    procedure Set_MineName(const Value: WideString); safecall;
    function Get_RiverChannelNumber: Integer; safecall;
    function Get_PCDChannelNumber: Integer; safecall;
    function Get_HydrologyNodeNumber: Integer; safecall;
    procedure Set_HydrologyNodeNumber(Value: Integer); safecall;
    function Get_BeneficiationPlantArea: Double; safecall;
    procedure Set_BeneficiationPlantArea(Value: Double); safecall;
    function Get_BeneficiationRunoffFactor: Double; safecall;
    procedure Set_BeneficiationRunoffFactor(Value: Double); safecall;
    function Get_PanEvaporation(AIndex: Integer): Double; safecall;
    procedure Set_PanEvaporation(AIndex: Integer; Value: Double); safecall;
    function Get_LakeEvaporation(AIndex: Integer): Double; safecall;
    procedure Set_LakeEvaporation(AIndex: Integer; Value: Double); safecall;
    function Get_OpenCastCount: Integer; safecall;
    function Get_UndergroundCount: Integer; safecall;
    function Get_SlurryDumpCount: Integer; safecall;
    function Get_OpenCastByIndex(AIndex: Integer): IOpenCast; safecall;
    function Get_OpenCastByIdentifier(AIdentifier: Integer): IOpenCast; safecall;
    function Get_UnderGroundByIndex(AIndex: Integer): IUnderground; safecall;
    function Get_UnderGroundByIdentifier(AIdentifier: Integer): IUnderground; safecall;
    function Get_SlurryDumpByIndex(AIndex: Integer): ISlurryDump; safecall;
    function Get_SlurryDumpByIdentifier(AIdentifier: Integer): ISlurryDump; safecall;
    function Get_RiverChannel: IGeneralFlowChannel; safecall;
    function Get_PCDChannel: IGeneralFlowChannel; safecall;
    function Get_PolutionControlDam: IReservoirData; safecall;
    function Get_HydrologyNode: IReservoirData; safecall;
    function CreateOpenCast: IOpenCast; safecall;
    function CreateUnderGround: IUnderground; safecall;
    function CreateSlurryDump: ISlurryDump; safecall;
    function RemoveOpenCast(AIdentifier: Integer): WordBool; safecall;
    function RemoveUnderGround(AIdentifier: Integer): WordBool; safecall;
    function RemoveSlurryDump(AIdentifier: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_RiverNode: IReservoirData; safecall;
    function Get_MineNode: IReservoirData; safecall;
    function Get_CatchmentRefNrByReservoirID(AReservoirID: Integer): Integer; safecall;
    function Get_PolutionControlDamExists: WordBool; safecall;
    procedure Set_PolutionControlDamExists(Value: WordBool); safecall;
    property Identifier: Integer read Get_Identifier;
    property NodeNumber: Integer read Get_NodeNumber;
    property MineName: WideString read Get_MineName write Set_MineName;
    property RiverChannelNumber: Integer read Get_RiverChannelNumber;
    property PCDChannelNumber: Integer read Get_PCDChannelNumber;
    property HydrologyNodeNumber: Integer read Get_HydrologyNodeNumber write Set_HydrologyNodeNumber;
    property BeneficiationPlantArea: Double read Get_BeneficiationPlantArea write Set_BeneficiationPlantArea;
    property BeneficiationRunoffFactor: Double read Get_BeneficiationRunoffFactor write Set_BeneficiationRunoffFactor;
    property PanEvaporation[AIndex: Integer]: Double read Get_PanEvaporation write Set_PanEvaporation;
    property LakeEvaporation[AIndex: Integer]: Double read Get_LakeEvaporation write Set_LakeEvaporation;
    property OpenCastCount: Integer read Get_OpenCastCount;
    property UndergroundCount: Integer read Get_UndergroundCount;
    property SlurryDumpCount: Integer read Get_SlurryDumpCount;
    property OpenCastByIndex[AIndex: Integer]: IOpenCast read Get_OpenCastByIndex;
    property OpenCastByIdentifier[AIdentifier: Integer]: IOpenCast read Get_OpenCastByIdentifier;
    property UnderGroundByIndex[AIndex: Integer]: IUnderground read Get_UnderGroundByIndex;
    property UnderGroundByIdentifier[AIdentifier: Integer]: IUnderground read Get_UnderGroundByIdentifier;
    property SlurryDumpByIndex[AIndex: Integer]: ISlurryDump read Get_SlurryDumpByIndex;
    property SlurryDumpByIdentifier[AIdentifier: Integer]: ISlurryDump read Get_SlurryDumpByIdentifier;
    property RiverChannel: IGeneralFlowChannel read Get_RiverChannel;
    property PCDChannel: IGeneralFlowChannel read Get_PCDChannel;
    property PolutionControlDam: IReservoirData read Get_PolutionControlDam;
    property HydrologyNode: IReservoirData read Get_HydrologyNode;
    property RiverNode: IReservoirData read Get_RiverNode;
    property MineNode: IReservoirData read Get_MineNode;
    property CatchmentRefNrByReservoirID[AReservoirID: Integer]: Integer read Get_CatchmentRefNrByReservoirID;
    property PolutionControlDamExists: WordBool read Get_PolutionControlDamExists write Set_PolutionControlDamExists;
  end;

// *********************************************************************//
// DispIntf:  IMineDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {0701352E-A9A4-4C99-A665-097099769D6F}
// *********************************************************************//
  IMineDisp = dispinterface
    ['{0701352E-A9A4-4C99-A665-097099769D6F}']
    property Identifier: Integer readonly dispid 101;
    property NodeNumber: Integer readonly dispid 102;
    property MineName: WideString dispid 103;
    property RiverChannelNumber: Integer readonly dispid 104;
    property PCDChannelNumber: Integer readonly dispid 105;
    property HydrologyNodeNumber: Integer dispid 106;
    property BeneficiationPlantArea: Double dispid 107;
    property BeneficiationRunoffFactor: Double dispid 108;
    property PanEvaporation[AIndex: Integer]: Double dispid 109;
    property LakeEvaporation[AIndex: Integer]: Double dispid 110;
    property OpenCastCount: Integer readonly dispid 111;
    property UndergroundCount: Integer readonly dispid 112;
    property SlurryDumpCount: Integer readonly dispid 113;
    property OpenCastByIndex[AIndex: Integer]: IOpenCast readonly dispid 114;
    property OpenCastByIdentifier[AIdentifier: Integer]: IOpenCast readonly dispid 115;
    property UnderGroundByIndex[AIndex: Integer]: IUnderground readonly dispid 116;
    property UnderGroundByIdentifier[AIdentifier: Integer]: IUnderground readonly dispid 117;
    property SlurryDumpByIndex[AIndex: Integer]: ISlurryDump readonly dispid 118;
    property SlurryDumpByIdentifier[AIdentifier: Integer]: ISlurryDump readonly dispid 119;
    property RiverChannel: IGeneralFlowChannel readonly dispid 120;
    property PCDChannel: IGeneralFlowChannel readonly dispid 121;
    property PolutionControlDam: IReservoirData readonly dispid 122;
    property HydrologyNode: IReservoirData readonly dispid 123;
    function CreateOpenCast: IOpenCast; dispid 124;
    function CreateUnderGround: IUnderground; dispid 125;
    function CreateSlurryDump: ISlurryDump; dispid 126;
    function RemoveOpenCast(AIdentifier: Integer): WordBool; dispid 127;
    function RemoveUnderGround(AIdentifier: Integer): WordBool; dispid 128;
    function RemoveSlurryDump(AIdentifier: Integer): WordBool; dispid 129;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 130;
    property RiverNode: IReservoirData readonly dispid 131;
    property MineNode: IReservoirData readonly dispid 132;
    property CatchmentRefNrByReservoirID[AReservoirID: Integer]: Integer readonly dispid 133;
    property PolutionControlDamExists: WordBool dispid 134;
  end;

// *********************************************************************//
// Interface: IMineList
// Flags:     (320) Dual OleAutomation
// GUID:      {444D81E8-F01B-4545-8D27-86D19E266DF6}
// *********************************************************************//
  IMineList = interface(IUnknown)
    ['{444D81E8-F01B-4545-8D27-86D19E266DF6}']
    function CreateMine: IMine; safecall;
    function RemoveMine(AMineNumber: Integer): WordBool; safecall;
    function Get_MineCount: Integer; safecall;
    function Get_MineByIndex(AIndex: Integer): IMine; safecall;
    function Get_MineByIdentifier(AIdentifier: Integer): IMine; safecall;
    function Get_MineByNodeNumber(AMineNodeNumber: Integer): IMine; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_MineByPCDNumber(APCDNumber: Integer): IMine; safecall;
    function Get_ChannelToUnderGroundDamByDamNumber(AReservoirNumber: Integer): IGeneralFlowChannel; safecall;
    function CopyCreate(AMineNumber: Integer): IMine; safecall;
    property MineCount: Integer read Get_MineCount;
    property MineByIndex[AIndex: Integer]: IMine read Get_MineByIndex;
    property MineByIdentifier[AIdentifier: Integer]: IMine read Get_MineByIdentifier;
    property MineByNodeNumber[AMineNodeNumber: Integer]: IMine read Get_MineByNodeNumber;
    property MineByPCDNumber[APCDNumber: Integer]: IMine read Get_MineByPCDNumber;
    property ChannelToUnderGroundDamByDamNumber[AReservoirNumber: Integer]: IGeneralFlowChannel read Get_ChannelToUnderGroundDamByDamNumber;
  end;

// *********************************************************************//
// DispIntf:  IMineListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {444D81E8-F01B-4545-8D27-86D19E266DF6}
// *********************************************************************//
  IMineListDisp = dispinterface
    ['{444D81E8-F01B-4545-8D27-86D19E266DF6}']
    function CreateMine: IMine; dispid 101;
    function RemoveMine(AMineNumber: Integer): WordBool; dispid 102;
    property MineCount: Integer readonly dispid 103;
    property MineByIndex[AIndex: Integer]: IMine readonly dispid 104;
    property MineByIdentifier[AIdentifier: Integer]: IMine readonly dispid 105;
    property MineByNodeNumber[AMineNodeNumber: Integer]: IMine readonly dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
    property MineByPCDNumber[APCDNumber: Integer]: IMine readonly dispid 108;
    property ChannelToUnderGroundDamByDamNumber[AReservoirNumber: Integer]: IGeneralFlowChannel readonly dispid 109;
    function CopyCreate(AMineNumber: Integer): IMine; dispid 110;
  end;

// *********************************************************************//
// Interface: IWRYMRunOptions
// Flags:     (320) Dual OleAutomation
// GUID:      {33D69FE6-3CC2-4C88-B246-B95EA4A1A3C9}
// *********************************************************************//
  IWRYMRunOptions = interface(IUnknown)
    ['{33D69FE6-3CC2-4C88-B246-B95EA4A1A3C9}']
    function Get_RunSilent: WordBool; safecall;
    procedure Set_RunSilent(Value: WordBool); safecall;
    function Get_AutoRun: WordBool; safecall;
    procedure Set_AutoRun(Value: WordBool); safecall;
    function Get_CloseOnComplete: WordBool; safecall;
    procedure Set_CloseOnComplete(Value: WordBool); safecall;
    function Get_RunDebugVersion: WordBool; safecall;
    procedure Set_RunDebugVersion(Value: WordBool); safecall;
    function Get_WRYM_DAT: WideString; safecall;
    procedure Set_WRYM_DAT(const Value: WideString); safecall;
    procedure SaveToINI; safecall;
    function Get_CreateSumOutFile: WordBool; safecall;
    procedure Set_CreateSumOutFile(Value: WordBool); safecall;
    function Get_SaveOutputAsBinaryFile: WordBool; safecall;
    procedure Set_SaveOutputAsBinaryFile(Value: WordBool); safecall;
    function Get_SaveOutputToDB: WordBool; safecall;
    procedure Set_SaveOutputToDB(Value: WordBool); safecall;
    function Get_FirmYield: Double; safecall;
    procedure Set_FirmYield(Value: Double); safecall;
    function Get_SumOutBlobAddress: LongWord; safecall;
    procedure Set_SumOutBlobAddress(Value: LongWord); safecall;
    function Get_BlobSize: Integer; safecall;
    procedure Set_BlobSize(Value: Integer); safecall;
    property RunSilent: WordBool read Get_RunSilent write Set_RunSilent;
    property AutoRun: WordBool read Get_AutoRun write Set_AutoRun;
    property CloseOnComplete: WordBool read Get_CloseOnComplete write Set_CloseOnComplete;
    property RunDebugVersion: WordBool read Get_RunDebugVersion write Set_RunDebugVersion;
    property WRYM_DAT: WideString read Get_WRYM_DAT write Set_WRYM_DAT;
    property CreateSumOutFile: WordBool read Get_CreateSumOutFile write Set_CreateSumOutFile;
    property SaveOutputAsBinaryFile: WordBool read Get_SaveOutputAsBinaryFile write Set_SaveOutputAsBinaryFile;
    property SaveOutputToDB: WordBool read Get_SaveOutputToDB write Set_SaveOutputToDB;
    property FirmYield: Double read Get_FirmYield write Set_FirmYield;
    property SumOutBlobAddress: LongWord read Get_SumOutBlobAddress write Set_SumOutBlobAddress;
    property BlobSize: Integer read Get_BlobSize write Set_BlobSize;
  end;

// *********************************************************************//
// DispIntf:  IWRYMRunOptionsDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {33D69FE6-3CC2-4C88-B246-B95EA4A1A3C9}
// *********************************************************************//
  IWRYMRunOptionsDisp = dispinterface
    ['{33D69FE6-3CC2-4C88-B246-B95EA4A1A3C9}']
    property RunSilent: WordBool dispid 201;
    property AutoRun: WordBool dispid 202;
    property CloseOnComplete: WordBool dispid 203;
    property RunDebugVersion: WordBool dispid 204;
    property WRYM_DAT: WideString dispid 205;
    procedure SaveToINI; dispid 101;
    property CreateSumOutFile: WordBool dispid 102;
    property SaveOutputAsBinaryFile: WordBool dispid 103;
    property SaveOutputToDB: WordBool dispid 104;
    property FirmYield: Double dispid 105;
    property SumOutBlobAddress: LongWord dispid 106;
    property BlobSize: Integer dispid 107;
  end;

// *********************************************************************//
// Interface: IYieldModelIterationTracker
// Flags:     (320) Dual OleAutomation
// GUID:      {FE44BCBF-7DAC-41BC-A096-9633F83B9CB7}
// *********************************************************************//
  IYieldModelIterationTracker = interface(IUnknown)
    ['{FE44BCBF-7DAC-41BC-A096-9633F83B9CB7}']
    function Get_IntervalCount: Integer; safecall;
    procedure Set_IntervalCount(Value: Integer); safecall;
    function Get_MonthCount: Integer; safecall;
    procedure Set_MonthCount(Value: Integer); safecall;
    function Get_YearCount: Integer; safecall;
    procedure Set_YearCount(Value: Integer); safecall;
    function Get_TargetDraftCount: Integer; safecall;
    procedure Set_TargetDraftCount(Value: Integer); safecall;
    function Get_SequenceCount: Integer; safecall;
    procedure Set_SequenceCount(Value: Integer); safecall;
    function Get_CurrentMonth: Integer; safecall;
    procedure Set_CurrentMonth(Value: Integer); safecall;
    function Get_CurrentInterval: Integer; safecall;
    procedure Set_CurrentInterval(Value: Integer); safecall;
    function Get_CurrentYearGregorian: Integer; safecall;
    procedure Set_CurrentYearGregorian(Value: Integer); safecall;
    function Get_CurrentYearIndex: Integer; safecall;
    procedure Set_CurrentYearIndex(Value: Integer); safecall;
    function Get_CurrentTargetDraft: Integer; safecall;
    procedure Set_CurrentTargetDraft(Value: Integer); safecall;
    function Get_CurrentSequence: Integer; safecall;
    procedure Set_CurrentSequence(Value: Integer); safecall;
    function Get_CurrentGood: Integer; safecall;
    procedure Set_CurrentGood(Value: Integer); safecall;
    function Get_Abort: WordBool; safecall;
    procedure Set_Abort(Value: WordBool); safecall;
    function Get_SimulationInProgress: WordBool; safecall;
    procedure Set_SimulationInProgress(Value: WordBool); safecall;
    function Get_IterationEventHandler: IIterationEventHandler; safecall;
    procedure Set_IterationEventHandler(const Value: IIterationEventHandler); safecall;
    procedure SaveToINI; safecall;
    function Get_PreviousMonth: Integer; safecall;
    procedure Set_PreviousMonth(Value: Integer); safecall;
    function Get_SubsequentMonth: Integer; safecall;
    procedure Set_SubsequentMonth(Value: Integer); safecall;
    property IntervalCount: Integer read Get_IntervalCount write Set_IntervalCount;
    property MonthCount: Integer read Get_MonthCount write Set_MonthCount;
    property YearCount: Integer read Get_YearCount write Set_YearCount;
    property TargetDraftCount: Integer read Get_TargetDraftCount write Set_TargetDraftCount;
    property SequenceCount: Integer read Get_SequenceCount write Set_SequenceCount;
    property CurrentMonth: Integer read Get_CurrentMonth write Set_CurrentMonth;
    property CurrentInterval: Integer read Get_CurrentInterval write Set_CurrentInterval;
    property CurrentYearGregorian: Integer read Get_CurrentYearGregorian write Set_CurrentYearGregorian;
    property CurrentYearIndex: Integer read Get_CurrentYearIndex write Set_CurrentYearIndex;
    property CurrentTargetDraft: Integer read Get_CurrentTargetDraft write Set_CurrentTargetDraft;
    property CurrentSequence: Integer read Get_CurrentSequence write Set_CurrentSequence;
    property CurrentGood: Integer read Get_CurrentGood write Set_CurrentGood;
    property Abort: WordBool read Get_Abort write Set_Abort;
    property SimulationInProgress: WordBool read Get_SimulationInProgress write Set_SimulationInProgress;
    property IterationEventHandler: IIterationEventHandler read Get_IterationEventHandler write Set_IterationEventHandler;
    property PreviousMonth: Integer read Get_PreviousMonth write Set_PreviousMonth;
    property SubsequentMonth: Integer read Get_SubsequentMonth write Set_SubsequentMonth;
  end;

// *********************************************************************//
// DispIntf:  IYieldModelIterationTrackerDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {FE44BCBF-7DAC-41BC-A096-9633F83B9CB7}
// *********************************************************************//
  IYieldModelIterationTrackerDisp = dispinterface
    ['{FE44BCBF-7DAC-41BC-A096-9633F83B9CB7}']
    property IntervalCount: Integer dispid 101;
    property MonthCount: Integer dispid 102;
    property YearCount: Integer dispid 103;
    property TargetDraftCount: Integer dispid 104;
    property SequenceCount: Integer dispid 105;
    property CurrentMonth: Integer dispid 106;
    property CurrentInterval: Integer dispid 107;
    property CurrentYearGregorian: Integer dispid 108;
    property CurrentYearIndex: Integer dispid 109;
    property CurrentTargetDraft: Integer dispid 110;
    property CurrentSequence: Integer dispid 111;
    property CurrentGood: Integer dispid 112;
    property Abort: WordBool dispid 113;
    property SimulationInProgress: WordBool dispid 116;
    property IterationEventHandler: IIterationEventHandler dispid 117;
    procedure SaveToINI; dispid 118;
    property PreviousMonth: Integer dispid 114;
    property SubsequentMonth: Integer dispid 115;
  end;

// *********************************************************************//
// Interface: IIterationEventHandler
// Flags:     (320) Dual OleAutomation
// GUID:      {6964220E-D400-4DD9-9318-6F45D7011980}
// *********************************************************************//
  IIterationEventHandler = interface(IUnknown)
    ['{6964220E-D400-4DD9-9318-6F45D7011980}']
    function OnIterationEvent(const AIterationName: WideString): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  IIterationEventHandlerDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6964220E-D400-4DD9-9318-6F45D7011980}
// *********************************************************************//
  IIterationEventHandlerDisp = dispinterface
    ['{6964220E-D400-4DD9-9318-6F45D7011980}']
    function OnIterationEvent(const AIterationName: WideString): WordBool; dispid 101;
  end;

// *********************************************************************//
// Interface: ICurtailedChannel
// Flags:     (320) Dual OleAutomation
// GUID:      {27CA6B7A-8A20-4BA2-83C6-BB94692D3556}
// *********************************************************************//
  ICurtailedChannel = interface(IUnknown)
    ['{27CA6B7A-8A20-4BA2-83C6-BB94692D3556}']
    function Get_Identifier: Integer; safecall;
    function Get_AllocationFactors(AIndex: Integer): Double; safecall;
    procedure Set_AllocationFactors(AIndex: Integer; Value: Double); safecall;
    function Get_ChannelNumber: Integer; safecall;
    function Get_AllocationFactorsCount: Integer; safecall;
    procedure Set_AllocationFactorsCount(ACount: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property Identifier: Integer read Get_Identifier;
    property AllocationFactors[AIndex: Integer]: Double read Get_AllocationFactors write Set_AllocationFactors;
    property ChannelNumber: Integer read Get_ChannelNumber;
    property AllocationFactorsCount: Integer read Get_AllocationFactorsCount write Set_AllocationFactorsCount;
  end;

// *********************************************************************//
// DispIntf:  ICurtailedChannelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {27CA6B7A-8A20-4BA2-83C6-BB94692D3556}
// *********************************************************************//
  ICurtailedChannelDisp = dispinterface
    ['{27CA6B7A-8A20-4BA2-83C6-BB94692D3556}']
    property Identifier: Integer readonly dispid 101;
    property AllocationFactors[AIndex: Integer]: Double dispid 103;
    property ChannelNumber: Integer readonly dispid 102;
    property AllocationFactorsCount: Integer dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
  end;

// *********************************************************************//
// Interface: IDroughtRestriction
// Flags:     (320) Dual OleAutomation
// GUID:      {D19E4AF0-BED8-4B77-B674-91A65ACBF719}
// *********************************************************************//
  IDroughtRestriction = interface(IUnknown)
    ['{D19E4AF0-BED8-4B77-B674-91A65ACBF719}']
    function Get_Identifier: Integer; safecall;
    function Get_ReservoirCount: Integer; safecall;
    function Get_ChannelCount: Integer; safecall;
    function Get_ChannelNumbers: WideString; safecall;
    procedure Set_ChannelNumbers(const Value: WideString); safecall;
    function Get_ReservoirNumbers: WideString; safecall;
    procedure Set_ReservoirNumbers(const Value: WideString); safecall;
    function Get_ReferenceStorageVolumes(AIndex: Integer): Double; safecall;
    procedure Set_ReferenceStorageVolumes(AIndex: Integer; Value: Double); safecall;
    function Get_AllocationFactors(AIndex: Integer): Double; safecall;
    procedure Set_AllocationFactors(AIndex: Integer; Value: Double); safecall;
    function Get_DroughtRestrictionName: WideString; safecall;
    procedure Set_DroughtRestrictionName(const Value: WideString); safecall;
    function Get_ReservoirNumberByIndex(AIndex: Integer): Integer; safecall;
    function Get_ChannelNumberByIndex(AIndex: Integer): Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property Identifier: Integer read Get_Identifier;
    property ReservoirCount: Integer read Get_ReservoirCount;
    property ChannelCount: Integer read Get_ChannelCount;
    property ChannelNumbers: WideString read Get_ChannelNumbers write Set_ChannelNumbers;
    property ReservoirNumbers: WideString read Get_ReservoirNumbers write Set_ReservoirNumbers;
    property ReferenceStorageVolumes[AIndex: Integer]: Double read Get_ReferenceStorageVolumes write Set_ReferenceStorageVolumes;
    property AllocationFactors[AIndex: Integer]: Double read Get_AllocationFactors write Set_AllocationFactors;
    property DroughtRestrictionName: WideString read Get_DroughtRestrictionName write Set_DroughtRestrictionName;
    property ReservoirNumberByIndex[AIndex: Integer]: Integer read Get_ReservoirNumberByIndex;
    property ChannelNumberByIndex[AIndex: Integer]: Integer read Get_ChannelNumberByIndex;
  end;

// *********************************************************************//
// DispIntf:  IDroughtRestrictionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {D19E4AF0-BED8-4B77-B674-91A65ACBF719}
// *********************************************************************//
  IDroughtRestrictionDisp = dispinterface
    ['{D19E4AF0-BED8-4B77-B674-91A65ACBF719}']
    property Identifier: Integer readonly dispid 101;
    property ReservoirCount: Integer readonly dispid 102;
    property ChannelCount: Integer readonly dispid 103;
    property ChannelNumbers: WideString dispid 104;
    property ReservoirNumbers: WideString dispid 105;
    property ReferenceStorageVolumes[AIndex: Integer]: Double dispid 106;
    property AllocationFactors[AIndex: Integer]: Double dispid 107;
    property DroughtRestrictionName: WideString dispid 108;
    property ReservoirNumberByIndex[AIndex: Integer]: Integer readonly dispid 109;
    property ChannelNumberByIndex[AIndex: Integer]: Integer readonly dispid 110;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 111;
  end;

// *********************************************************************//
// Interface: ICurtailmentAndDrought
// Flags:     (320) Dual OleAutomation
// GUID:      {7802E932-40AC-448E-A2BF-0B446E6AD0D1}
// *********************************************************************//
  ICurtailmentAndDrought = interface(IUnknown)
    ['{7802E932-40AC-448E-A2BF-0B446E6AD0D1}']
    function Get_CurtailmentPeriodCount: Integer; safecall;
    procedure Set_CurtailmentPeriodCount(Value: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_StartMonthsByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_StartMonthsByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_CurtailedChannelCount: Integer; safecall;
    function Get_CurtailedChannelByIndex(AIndex: Integer): ICurtailedChannel; safecall;
    function Get_CurtailedChannelByID(AIdentifier: Integer): ICurtailedChannel; safecall;
    function Get_CurtailedChannelByChannelNumber(AChannelNumber: Integer): ICurtailedChannel; safecall;
    function CreateChannelCurtailment(AChannelNumber: Integer): ICurtailedChannel; safecall;
    function RemoveChannelCurtailment(AChannelNumber: Integer): WordBool; safecall;
    function Get_DroughtRestrictionCount: Integer; safecall;
    function Get_DroughtRestrictionByIndex(AIndex: Integer): IDroughtRestriction; safecall;
    function Get_DroughtRestrictionByID(AIdentifier: Integer): IDroughtRestriction; safecall;
    function CreateDroughtRestriction: IDroughtRestriction; safecall;
    function RemoveDroughtRestriction(AIdentifier: Integer): WordBool; safecall;
    function Get_ImplementCurtailmentFile: WordBool; safecall;
    procedure Set_ImplementCurtailmentFile(Value: WordBool); safecall;
    function Get_DroughtRestrictionByReservoirNumber(AReservoirNumber: Integer): WideString; safecall;
    function Get_DroughtRestrictionByChannelNumber(AChannelNumber: Integer): WideString; safecall;
    property CurtailmentPeriodCount: Integer read Get_CurtailmentPeriodCount write Set_CurtailmentPeriodCount;
    property StartMonthsByIndex[AIndex: Integer]: Integer read Get_StartMonthsByIndex write Set_StartMonthsByIndex;
    property CurtailedChannelCount: Integer read Get_CurtailedChannelCount;
    property CurtailedChannelByIndex[AIndex: Integer]: ICurtailedChannel read Get_CurtailedChannelByIndex;
    property CurtailedChannelByID[AIdentifier: Integer]: ICurtailedChannel read Get_CurtailedChannelByID;
    property CurtailedChannelByChannelNumber[AChannelNumber: Integer]: ICurtailedChannel read Get_CurtailedChannelByChannelNumber;
    property DroughtRestrictionCount: Integer read Get_DroughtRestrictionCount;
    property DroughtRestrictionByIndex[AIndex: Integer]: IDroughtRestriction read Get_DroughtRestrictionByIndex;
    property DroughtRestrictionByID[AIdentifier: Integer]: IDroughtRestriction read Get_DroughtRestrictionByID;
    property ImplementCurtailmentFile: WordBool read Get_ImplementCurtailmentFile write Set_ImplementCurtailmentFile;
    property DroughtRestrictionByReservoirNumber[AReservoirNumber: Integer]: WideString read Get_DroughtRestrictionByReservoirNumber;
    property DroughtRestrictionByChannelNumber[AChannelNumber: Integer]: WideString read Get_DroughtRestrictionByChannelNumber;
  end;

// *********************************************************************//
// DispIntf:  ICurtailmentAndDroughtDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {7802E932-40AC-448E-A2BF-0B446E6AD0D1}
// *********************************************************************//
  ICurtailmentAndDroughtDisp = dispinterface
    ['{7802E932-40AC-448E-A2BF-0B446E6AD0D1}']
    property CurtailmentPeriodCount: Integer dispid 202;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 203;
    property StartMonthsByIndex[AIndex: Integer]: Integer dispid 204;
    property CurtailedChannelCount: Integer readonly dispid 205;
    property CurtailedChannelByIndex[AIndex: Integer]: ICurtailedChannel readonly dispid 206;
    property CurtailedChannelByID[AIdentifier: Integer]: ICurtailedChannel readonly dispid 207;
    property CurtailedChannelByChannelNumber[AChannelNumber: Integer]: ICurtailedChannel readonly dispid 208;
    function CreateChannelCurtailment(AChannelNumber: Integer): ICurtailedChannel; dispid 209;
    function RemoveChannelCurtailment(AChannelNumber: Integer): WordBool; dispid 210;
    property DroughtRestrictionCount: Integer readonly dispid 211;
    property DroughtRestrictionByIndex[AIndex: Integer]: IDroughtRestriction readonly dispid 212;
    property DroughtRestrictionByID[AIdentifier: Integer]: IDroughtRestriction readonly dispid 213;
    function CreateDroughtRestriction: IDroughtRestriction; dispid 214;
    function RemoveDroughtRestriction(AIdentifier: Integer): WordBool; dispid 215;
    property ImplementCurtailmentFile: WordBool dispid 101;
    property DroughtRestrictionByReservoirNumber[AReservoirNumber: Integer]: WideString readonly dispid 102;
    property DroughtRestrictionByChannelNumber[AChannelNumber: Integer]: WideString readonly dispid 103;
  end;

// *********************************************************************//
// Interface: ISummaryOutputData
// Flags:     (320) Dual OleAutomation
// GUID:      {563FEF84-230F-4F31-B59F-58A9F4F18788}
// *********************************************************************//
  ISummaryOutputData = interface(IUnknown)
    ['{563FEF84-230F-4F31-B59F-58A9F4F18788}']
    function Get_SumOutBlob: ISumOutBlob; safecall;
    function GetElementsByElementDataType(AElementDataType: TOutputDataType;
                                          var ADataContainer: WideString): WordBool; safecall;
    function GetBlockDataByElementID(var ADataContainer: WideString; ADataType: TOutputDataType;
                                     ANetworkElementID: Integer; var AErrors: WideString): WordBool; safecall;
    function GetBlockAverageDataByElementID(var ADataContainer: WideString;
                                            ADataType: TOutputDataType; ANetworkElementID: Integer;
                                            var AErrors: WideString): WordBool; safecall;
    function GetPeriodChangeDataByElementID(var ADataContainer: WideString;
                                            ADataType: TOutputDataType; ANetworkElementID: Integer;
                                            var AErrors: WideString): WordBool; safecall;
    property SumOutBlob: ISumOutBlob read Get_SumOutBlob;
  end;

// *********************************************************************//
// DispIntf:  ISummaryOutputDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {563FEF84-230F-4F31-B59F-58A9F4F18788}
// *********************************************************************//
  ISummaryOutputDataDisp = dispinterface
    ['{563FEF84-230F-4F31-B59F-58A9F4F18788}']
    property SumOutBlob: ISumOutBlob readonly dispid 101;
    function GetElementsByElementDataType(AElementDataType: TOutputDataType;
                                          var ADataContainer: WideString): WordBool; dispid 102;
    function GetBlockDataByElementID(var ADataContainer: WideString; ADataType: TOutputDataType;
                                     ANetworkElementID: Integer; var AErrors: WideString): WordBool; dispid 103;
    function GetBlockAverageDataByElementID(var ADataContainer: WideString;
                                            ADataType: TOutputDataType; ANetworkElementID: Integer;
                                            var AErrors: WideString): WordBool; dispid 104;
    function GetPeriodChangeDataByElementID(var ADataContainer: WideString;
                                            ADataType: TOutputDataType; ANetworkElementID: Integer;
                                            var AErrors: WideString): WordBool; dispid 105;
  end;

// *********************************************************************//
// Interface: ISumOutBlob
// Flags:     (320) Dual OleAutomation
// GUID:      {05247B86-D2B2-4694-A346-4678D4AF6E6D}
// *********************************************************************//
  ISumOutBlob = interface(IUnknown)
    ['{05247B86-D2B2-4694-A346-4678D4AF6E6D}']
    function WriteBlobToFile: WordBool; safecall;
    function ReadBlobFromFile: WordBool; safecall;
    function LoadBlobFromDB: WordBool; safecall;
    function SaveBlobToDB: WordBool; safecall;
    function CopyFromMemory(APointer: LongWord; ASize: Integer): WordBool; safecall;
    function WriteSumOutFile: WordBool; safecall;
    function Get_BlobLoaded: WordBool; safecall;
    procedure Set_BlobLoaded(Value: WordBool); safecall;
    property BlobLoaded: WordBool read Get_BlobLoaded write Set_BlobLoaded;
  end;

// *********************************************************************//
// DispIntf:  ISumOutBlobDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {05247B86-D2B2-4694-A346-4678D4AF6E6D}
// *********************************************************************//
  ISumOutBlobDisp = dispinterface
    ['{05247B86-D2B2-4694-A346-4678D4AF6E6D}']
    function WriteBlobToFile: WordBool; dispid 101;
    function ReadBlobFromFile: WordBool; dispid 102;
    function LoadBlobFromDB: WordBool; dispid 103;
    function SaveBlobToDB: WordBool; dispid 104;
    function CopyFromMemory(APointer: LongWord; ASize: Integer): WordBool; dispid 105;
    function WriteSumOutFile: WordBool; dispid 106;
    property BlobLoaded: WordBool dispid 107;
  end;

// *********************************************************************//
// Interface: IGroundWater
// Flags:     (320) Dual OleAutomation
// GUID:      {8AAB9F91-24FB-40F4-82F2-3324C658E25F}
// *********************************************************************//
  IGroundWater = interface(IUnknown)
    ['{8AAB9F91-24FB-40F4-82F2-3324C658E25F}']
    function Get_AquiferStorativity: Double; safecall;
    procedure Set_AquiferStorativity(Value: Double); safecall;
    function Get_AquiferStaticWaterLevel: Double; safecall;
    procedure Set_AquiferStaticWaterLevel(Value: Double); safecall;
    function Get_UnsaturatedStorageCapacity: Double; safecall;
    procedure Set_UnsaturatedStorageCapacity(Value: Double); safecall;
    function Get_InitialUnsaturatedStorage: Double; safecall;
    procedure Set_InitialUnsaturatedStorage(Value: Double); safecall;
    function Get_MaximumDischargeRate: Double; safecall;
    procedure Set_MaximumDischargeRate(Value: Double); safecall;
    function Get_MovingAverageRecharge: Double; safecall;
    procedure Set_MovingAverageRecharge(Value: Double); safecall;
    function Get_PitmanSoilMoistureCapacity: Double; safecall;
    procedure Set_PitmanSoilMoistureCapacity(Value: Double); safecall;
    function Get_PitmanSoilMoistureStorageCapacity: Double; safecall;
    procedure Set_PitmanSoilMoistureStorageCapacity(Value: Double); safecall;
    function Get_PitmansoilMoistureFlowState: Double; safecall;
    procedure Set_PitmansoilMoistureFlowState(Value: Double); safecall;
    function Get_PitmanSoilMoistureFlowEquation: Double; safecall;
    procedure Set_PitmanSoilMoistureFlowEquation(Value: Double); safecall;
    function Get_PitmanMaximumGroundwaterFlow: Double; safecall;
    procedure Set_PitmanMaximumGroundwaterFlow(Value: Double); safecall;
    function Get_PitmanSoilMoistureRechargeEquation: Double; safecall;
    procedure Set_PitmanSoilMoistureRechargeEquation(Value: Double); safecall;
    function Get_PitmanGroundwaterFlow: Double; safecall;
    procedure Set_PitmanGroundwaterFlow(Value: Double); safecall;
    function Get_MaximumRateOfGroundwaterBaseFlow: Double; safecall;
    procedure Set_MaximumRateOfGroundwaterBaseFlow(Value: Double); safecall;
    function Get_PowerHeadDifferenceBaseFlowEquation: Double; safecall;
    procedure Set_PowerHeadDifferenceBaseFlowEquation(Value: Double); safecall;
    function Get_MaximumHydrologicalGradient: Double; safecall;
    procedure Set_MaximumHydrologicalGradient(Value: Double); safecall;
    function Get_AquiferTransmissivity: Double; safecall;
    procedure Set_AquiferTransmissivity(Value: Double); safecall;
    function Get_BoreHoleDistanceToRiver: Double; safecall;
    procedure Set_BoreHoleDistanceToRiver(Value: Double); safecall;
    function Get_MaximumGroundwaterAbstraction: Double; safecall;
    procedure Set_MaximumGroundwaterAbstraction(Value: Double); safecall;
    function Get_ParameterK2: Double; safecall;
    procedure Set_ParameterK2(Value: Double); safecall;
    function Get_ParameterK3: Double; safecall;
    procedure Set_ParameterK3(Value: Double); safecall;
    function Get_MonthlyWaterEvaporation(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyWaterEvaporation(AIndex: Integer; Value: Double); safecall;
    function Get_MonthlyWaterUsageFactors(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyWaterUsageFactors(AIndex: Integer; Value: Double); safecall;
    function Get_GroundWaterEvaporationArea: Double; safecall;
    procedure Set_GroundWaterEvaporationArea(Value: Double); safecall;
    function Get_Identifier: Integer; safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_AbstractionNode: IReservoirData; safecall;
    function Get_CollectionNode: IReservoirData; safecall;
    function Get_BaseFlowNode: IReservoirData; safecall;
    function Get_AquiferInflowChannel: IGeneralFlowChannel; safecall;
    function Get_AquiferInflowChannelNr: Integer; safecall;
    function Get_AquiferExcessInterflowChannel: IGeneralFlowChannel; safecall;
    function Get_AquiferExcessInterflowChannelNr: Integer; safecall;
    function Get_GroundWaterBaseflowChannel: IGeneralFlowChannel; safecall;
    function Get_GroundWaterBaseflowChannelNr: Integer; safecall;
    function Get_AbstractionFromAquiferChannel: IGeneralFlowChannel; safecall;
    function Get_AbstractionFromAquiferChannelNr: Integer; safecall;
    function Get_AbstractionFromBaseFlowChannel: IGeneralFlowChannel; safecall;
    function Get_AbstractionFromBaseflowChannelNr: Integer; safecall;
    function Get_OutflowToDownstreamAquiferChannel: IGeneralFlowChannel; safecall;
    function Get_OutflowToDownstreamAquiferChannelNr: Integer; safecall;
    function Get_SurfaceRunoffAndSoilInterflowChannel: IGeneralFlowChannel; safecall;
    function Get_SurfaceRunoffAndSoilInterflowChannelNr: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_AbstractionNodeNr: Integer; safecall;
    function Get_CollectionNodeNr: Integer; safecall;
    function Get_BaseFlowNodeNr: Integer; safecall;
    function Get_AquiferNode: IReservoirData; safecall;
    function Get_AquiferNodeNr: Integer; safecall;
    function Get_RefNodeNumber: Integer; safecall;
    procedure Set_RefNodeNumber(Value: Integer); safecall;
    function Get_GroundWaterBaseFlowRemainderChannel: IGeneralFlowChannel; safecall;
    function Get_GroundWaterBaseFlowRemainderChannelNr: Integer; safecall;
    function Get_OutflowToNetworkChannelNr: Integer; safecall;
    function Get_OutflowToNetworkChannel: IGeneralFlowChannel; safecall;
    function Get_GroundWaterAbstractionChannelNr: Integer; safecall;
    function Get_GroundWaterAbstractionChannel: IGeneralFlowChannel; safecall;
    function Get_InflowFromUpstreamAquiferChannel: IGeneralFlowChannel; safecall;
    function Get_InflowFromUpstreamAquiferChannelNr: Integer; safecall;
    property AquiferStorativity: Double read Get_AquiferStorativity write Set_AquiferStorativity;
    property AquiferStaticWaterLevel: Double read Get_AquiferStaticWaterLevel write Set_AquiferStaticWaterLevel;
    property UnsaturatedStorageCapacity: Double read Get_UnsaturatedStorageCapacity write Set_UnsaturatedStorageCapacity;
    property InitialUnsaturatedStorage: Double read Get_InitialUnsaturatedStorage write Set_InitialUnsaturatedStorage;
    property MaximumDischargeRate: Double read Get_MaximumDischargeRate write Set_MaximumDischargeRate;
    property MovingAverageRecharge: Double read Get_MovingAverageRecharge write Set_MovingAverageRecharge;
    property PitmanSoilMoistureCapacity: Double read Get_PitmanSoilMoistureCapacity write Set_PitmanSoilMoistureCapacity;
    property PitmanSoilMoistureStorageCapacity: Double read Get_PitmanSoilMoistureStorageCapacity write Set_PitmanSoilMoistureStorageCapacity;
    property PitmansoilMoistureFlowState: Double read Get_PitmansoilMoistureFlowState write Set_PitmansoilMoistureFlowState;
    property PitmanSoilMoistureFlowEquation: Double read Get_PitmanSoilMoistureFlowEquation write Set_PitmanSoilMoistureFlowEquation;
    property PitmanMaximumGroundwaterFlow: Double read Get_PitmanMaximumGroundwaterFlow write Set_PitmanMaximumGroundwaterFlow;
    property PitmanSoilMoistureRechargeEquation: Double read Get_PitmanSoilMoistureRechargeEquation write Set_PitmanSoilMoistureRechargeEquation;
    property PitmanGroundwaterFlow: Double read Get_PitmanGroundwaterFlow write Set_PitmanGroundwaterFlow;
    property MaximumRateOfGroundwaterBaseFlow: Double read Get_MaximumRateOfGroundwaterBaseFlow write Set_MaximumRateOfGroundwaterBaseFlow;
    property PowerHeadDifferenceBaseFlowEquation: Double read Get_PowerHeadDifferenceBaseFlowEquation write Set_PowerHeadDifferenceBaseFlowEquation;
    property MaximumHydrologicalGradient: Double read Get_MaximumHydrologicalGradient write Set_MaximumHydrologicalGradient;
    property AquiferTransmissivity: Double read Get_AquiferTransmissivity write Set_AquiferTransmissivity;
    property BoreHoleDistanceToRiver: Double read Get_BoreHoleDistanceToRiver write Set_BoreHoleDistanceToRiver;
    property MaximumGroundwaterAbstraction: Double read Get_MaximumGroundwaterAbstraction write Set_MaximumGroundwaterAbstraction;
    property ParameterK2: Double read Get_ParameterK2 write Set_ParameterK2;
    property ParameterK3: Double read Get_ParameterK3 write Set_ParameterK3;
    property MonthlyWaterEvaporation[AIndex: Integer]: Double read Get_MonthlyWaterEvaporation write Set_MonthlyWaterEvaporation;
    property MonthlyWaterUsageFactors[AIndex: Integer]: Double read Get_MonthlyWaterUsageFactors write Set_MonthlyWaterUsageFactors;
    property GroundWaterEvaporationArea: Double read Get_GroundWaterEvaporationArea write Set_GroundWaterEvaporationArea;
    property Identifier: Integer read Get_Identifier;
    property Description: WideString read Get_Description write Set_Description;
    property Name: WideString read Get_Name write Set_Name;
    property AbstractionNode: IReservoirData read Get_AbstractionNode;
    property CollectionNode: IReservoirData read Get_CollectionNode;
    property BaseFlowNode: IReservoirData read Get_BaseFlowNode;
    property AquiferInflowChannel: IGeneralFlowChannel read Get_AquiferInflowChannel;
    property AquiferInflowChannelNr: Integer read Get_AquiferInflowChannelNr;
    property AquiferExcessInterflowChannel: IGeneralFlowChannel read Get_AquiferExcessInterflowChannel;
    property AquiferExcessInterflowChannelNr: Integer read Get_AquiferExcessInterflowChannelNr;
    property GroundWaterBaseflowChannel: IGeneralFlowChannel read Get_GroundWaterBaseflowChannel;
    property GroundWaterBaseflowChannelNr: Integer read Get_GroundWaterBaseflowChannelNr;
    property AbstractionFromAquiferChannel: IGeneralFlowChannel read Get_AbstractionFromAquiferChannel;
    property AbstractionFromAquiferChannelNr: Integer read Get_AbstractionFromAquiferChannelNr;
    property AbstractionFromBaseFlowChannel: IGeneralFlowChannel read Get_AbstractionFromBaseFlowChannel;
    property AbstractionFromBaseflowChannelNr: Integer read Get_AbstractionFromBaseflowChannelNr;
    property OutflowToDownstreamAquiferChannel: IGeneralFlowChannel read Get_OutflowToDownstreamAquiferChannel;
    property OutflowToDownstreamAquiferChannelNr: Integer read Get_OutflowToDownstreamAquiferChannelNr;
    property SurfaceRunoffAndSoilInterflowChannel: IGeneralFlowChannel read Get_SurfaceRunoffAndSoilInterflowChannel;
    property SurfaceRunoffAndSoilInterflowChannelNr: Integer read Get_SurfaceRunoffAndSoilInterflowChannelNr;
    property AbstractionNodeNr: Integer read Get_AbstractionNodeNr;
    property CollectionNodeNr: Integer read Get_CollectionNodeNr;
    property BaseFlowNodeNr: Integer read Get_BaseFlowNodeNr;
    property AquiferNode: IReservoirData read Get_AquiferNode;
    property AquiferNodeNr: Integer read Get_AquiferNodeNr;
    property RefNodeNumber: Integer read Get_RefNodeNumber write Set_RefNodeNumber;
    property GroundWaterBaseFlowRemainderChannel: IGeneralFlowChannel read Get_GroundWaterBaseFlowRemainderChannel;
    property GroundWaterBaseFlowRemainderChannelNr: Integer read Get_GroundWaterBaseFlowRemainderChannelNr;
    property OutflowToNetworkChannelNr: Integer read Get_OutflowToNetworkChannelNr;
    property OutflowToNetworkChannel: IGeneralFlowChannel read Get_OutflowToNetworkChannel;
    property GroundWaterAbstractionChannelNr: Integer read Get_GroundWaterAbstractionChannelNr;
    property GroundWaterAbstractionChannel: IGeneralFlowChannel read Get_GroundWaterAbstractionChannel;
    property InflowFromUpstreamAquiferChannel: IGeneralFlowChannel read Get_InflowFromUpstreamAquiferChannel;
    property InflowFromUpstreamAquiferChannelNr: Integer read Get_InflowFromUpstreamAquiferChannelNr;
  end;

// *********************************************************************//
// DispIntf:  IGroundWaterDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {8AAB9F91-24FB-40F4-82F2-3324C658E25F}
// *********************************************************************//
  IGroundWaterDisp = dispinterface
    ['{8AAB9F91-24FB-40F4-82F2-3324C658E25F}']
    property AquiferStorativity: Double dispid 201;
    property AquiferStaticWaterLevel: Double dispid 101;
    property UnsaturatedStorageCapacity: Double dispid 102;
    property InitialUnsaturatedStorage: Double dispid 103;
    property MaximumDischargeRate: Double dispid 104;
    property MovingAverageRecharge: Double dispid 105;
    property PitmanSoilMoistureCapacity: Double dispid 106;
    property PitmanSoilMoistureStorageCapacity: Double dispid 107;
    property PitmansoilMoistureFlowState: Double dispid 108;
    property PitmanSoilMoistureFlowEquation: Double dispid 109;
    property PitmanMaximumGroundwaterFlow: Double dispid 110;
    property PitmanSoilMoistureRechargeEquation: Double dispid 111;
    property PitmanGroundwaterFlow: Double dispid 112;
    property MaximumRateOfGroundwaterBaseFlow: Double dispid 113;
    property PowerHeadDifferenceBaseFlowEquation: Double dispid 114;
    property MaximumHydrologicalGradient: Double dispid 115;
    property AquiferTransmissivity: Double dispid 116;
    property BoreHoleDistanceToRiver: Double dispid 117;
    property MaximumGroundwaterAbstraction: Double dispid 118;
    property ParameterK2: Double dispid 119;
    property ParameterK3: Double dispid 120;
    property MonthlyWaterEvaporation[AIndex: Integer]: Double dispid 121;
    property MonthlyWaterUsageFactors[AIndex: Integer]: Double dispid 122;
    property GroundWaterEvaporationArea: Double dispid 123;
    property Identifier: Integer readonly dispid 124;
    property Description: WideString dispid 126;
    property Name: WideString dispid 127;
    property AbstractionNode: IReservoirData readonly dispid 128;
    property CollectionNode: IReservoirData readonly dispid 129;
    property BaseFlowNode: IReservoirData readonly dispid 130;
    property AquiferInflowChannel: IGeneralFlowChannel readonly dispid 131;
    property AquiferInflowChannelNr: Integer readonly dispid 132;
    property AquiferExcessInterflowChannel: IGeneralFlowChannel readonly dispid 133;
    property AquiferExcessInterflowChannelNr: Integer readonly dispid 134;
    property GroundWaterBaseflowChannel: IGeneralFlowChannel readonly dispid 135;
    property GroundWaterBaseflowChannelNr: Integer readonly dispid 136;
    property AbstractionFromAquiferChannel: IGeneralFlowChannel readonly dispid 137;
    property AbstractionFromAquiferChannelNr: Integer readonly dispid 138;
    property AbstractionFromBaseFlowChannel: IGeneralFlowChannel readonly dispid 139;
    property AbstractionFromBaseflowChannelNr: Integer readonly dispid 140;
    property OutflowToDownstreamAquiferChannel: IGeneralFlowChannel readonly dispid 141;
    property OutflowToDownstreamAquiferChannelNr: Integer readonly dispid 142;
    property SurfaceRunoffAndSoilInterflowChannel: IGeneralFlowChannel readonly dispid 143;
    property SurfaceRunoffAndSoilInterflowChannelNr: Integer readonly dispid 144;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 145;
    property AbstractionNodeNr: Integer readonly dispid 146;
    property CollectionNodeNr: Integer readonly dispid 147;
    property BaseFlowNodeNr: Integer readonly dispid 148;
    property AquiferNode: IReservoirData readonly dispid 149;
    property AquiferNodeNr: Integer readonly dispid 150;
    property RefNodeNumber: Integer dispid 125;
    property GroundWaterBaseFlowRemainderChannel: IGeneralFlowChannel readonly dispid 151;
    property GroundWaterBaseFlowRemainderChannelNr: Integer readonly dispid 152;
    property OutflowToNetworkChannelNr: Integer readonly dispid 153;
    property OutflowToNetworkChannel: IGeneralFlowChannel readonly dispid 154;
    property GroundWaterAbstractionChannelNr: Integer readonly dispid 155;
    property GroundWaterAbstractionChannel: IGeneralFlowChannel readonly dispid 156;
    property InflowFromUpstreamAquiferChannel: IGeneralFlowChannel readonly dispid 157;
    property InflowFromUpstreamAquiferChannelNr: Integer readonly dispid 158;
  end;

// *********************************************************************//
// Interface: IGroundWaterList
// Flags:     (320) Dual OleAutomation
// GUID:      {CEC4F1FD-7A19-4D8F-9857-93A63BEFE09A}
// *********************************************************************//
  IGroundWaterList = interface(IUnknown)
    ['{CEC4F1FD-7A19-4D8F-9857-93A63BEFE09A}']
    function Get_GroundWaterCount: Integer; safecall;
    function Get_GroundWaterByIndex(AIndex: Integer): IGroundWater; safecall;
    function Get_GroundWaterByID(AGroundWaterID: Integer): IGroundWater; safecall;
    function Get_GroundWaterByNodeNumber(ANodeNumber: Integer): IGroundWater; safecall;
    function CreateGroundWater: IGroundWater; safecall;
    function RemoveGroundWater(AGroundWaterID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_GroundWaterByBaseFlowNumber(ABaseFlowNumber: Integer): IGroundWater; safecall;
    function Get_GroundWaterByAbstractionNodeNumber(AAbstractionNodeNumber: Integer): IGroundWater; safecall;
    function Get_GroundWaterByCollectionNodeNumber(ACollectionNodeNumber: Integer): IGroundWater; safecall;
    function CopyGroundwater(AGroundWaterID: Integer): IGroundWater; safecall;
    property GroundWaterCount: Integer read Get_GroundWaterCount;
    property GroundWaterByIndex[AIndex: Integer]: IGroundWater read Get_GroundWaterByIndex;
    property GroundWaterByID[AGroundWaterID: Integer]: IGroundWater read Get_GroundWaterByID;
    property GroundWaterByNodeNumber[ANodeNumber: Integer]: IGroundWater read Get_GroundWaterByNodeNumber;
    property GroundWaterByBaseFlowNumber[ABaseFlowNumber: Integer]: IGroundWater read Get_GroundWaterByBaseFlowNumber;
    property GroundWaterByAbstractionNodeNumber[AAbstractionNodeNumber: Integer]: IGroundWater read Get_GroundWaterByAbstractionNodeNumber;
    property GroundWaterByCollectionNodeNumber[ACollectionNodeNumber: Integer]: IGroundWater read Get_GroundWaterByCollectionNodeNumber;
  end;

// *********************************************************************//
// DispIntf:  IGroundWaterListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {CEC4F1FD-7A19-4D8F-9857-93A63BEFE09A}
// *********************************************************************//
  IGroundWaterListDisp = dispinterface
    ['{CEC4F1FD-7A19-4D8F-9857-93A63BEFE09A}']
    property GroundWaterCount: Integer readonly dispid 101;
    property GroundWaterByIndex[AIndex: Integer]: IGroundWater readonly dispid 102;
    property GroundWaterByID[AGroundWaterID: Integer]: IGroundWater readonly dispid 103;
    property GroundWaterByNodeNumber[ANodeNumber: Integer]: IGroundWater readonly dispid 104;
    function CreateGroundWater: IGroundWater; dispid 105;
    function RemoveGroundWater(AGroundWaterID: Integer): WordBool; dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
    property GroundWaterByBaseFlowNumber[ABaseFlowNumber: Integer]: IGroundWater readonly dispid 108;
    property GroundWaterByAbstractionNodeNumber[AAbstractionNodeNumber: Integer]: IGroundWater readonly dispid 109;
    property GroundWaterByCollectionNodeNumber[ACollectionNodeNumber: Integer]: IGroundWater readonly dispid 110;
    function CopyGroundwater(AGroundWaterID: Integer): IGroundWater; dispid 111;
  end;

// *********************************************************************//
// Interface: IImplementedNetworkFeatures
// Flags:     (320) Dual OleAutomation
// GUID:      {53AE96E0-3DF8-454F-840D-FC7AD7E13025}
// *********************************************************************//
  IImplementedNetworkFeatures = interface(IUnknown)
    ['{53AE96E0-3DF8-454F-840D-FC7AD7E13025}']
    function Get_PowerPlantFeatureImplemented: WordBool; safecall;
    procedure Set_PowerPlantFeatureImplemented(Value: WordBool); safecall;
    function Get_IrrigationAreaFeatureImplemented: WordBool; safecall;
    procedure Set_IrrigationAreaFeatureImplemented(Value: WordBool); safecall;
    function Get_IrrigationBlockFeatureImplemented: WordBool; safecall;
    procedure Set_IrrigationBlockFeatureImplemented(Value: WordBool); safecall;
    function Get_WetlandFeatureImplemented: WordBool; safecall;
    procedure Set_WetlandFeatureImplemented(Value: WordBool); safecall;
    function Get_YMDemandCentreFeatureImplemented: WordBool; safecall;
    procedure Set_YMDemandCentreFeatureImplemented(Value: WordBool); safecall;
    function Get_StreamFlowReductionFeatureImplemented: WordBool; safecall;
    procedure Set_StreamFlowReductionFeatureImplemented(Value: WordBool); safecall;
    function Get_IFRSiteFeatureImplemented: WordBool; safecall;
    procedure Set_IFRSiteFeatureImplemented(Value: WordBool); safecall;
    function Get_MineFeatureImplemented: WordBool; safecall;
    procedure Set_MineFeatureImplemented(Value: WordBool); safecall;
    function Get_CurtailmentAndDroughtFeatureImplemented: WordBool; safecall;
    procedure Set_CurtailmentAndDroughtFeatureImplemented(Value: WordBool); safecall;
    function Get_GroundWaterFeatureImplemented: WordBool; safecall;
    procedure Set_GroundWaterFeatureImplemented(Value: WordBool); safecall;
    property PowerPlantFeatureImplemented: WordBool read Get_PowerPlantFeatureImplemented write Set_PowerPlantFeatureImplemented;
    property IrrigationAreaFeatureImplemented: WordBool read Get_IrrigationAreaFeatureImplemented write Set_IrrigationAreaFeatureImplemented;
    property IrrigationBlockFeatureImplemented: WordBool read Get_IrrigationBlockFeatureImplemented write Set_IrrigationBlockFeatureImplemented;
    property WetlandFeatureImplemented: WordBool read Get_WetlandFeatureImplemented write Set_WetlandFeatureImplemented;
    property YMDemandCentreFeatureImplemented: WordBool read Get_YMDemandCentreFeatureImplemented write Set_YMDemandCentreFeatureImplemented;
    property StreamFlowReductionFeatureImplemented: WordBool read Get_StreamFlowReductionFeatureImplemented write Set_StreamFlowReductionFeatureImplemented;
    property IFRSiteFeatureImplemented: WordBool read Get_IFRSiteFeatureImplemented write Set_IFRSiteFeatureImplemented;
    property MineFeatureImplemented: WordBool read Get_MineFeatureImplemented write Set_MineFeatureImplemented;
    property CurtailmentAndDroughtFeatureImplemented: WordBool read Get_CurtailmentAndDroughtFeatureImplemented write Set_CurtailmentAndDroughtFeatureImplemented;
    property GroundWaterFeatureImplemented: WordBool read Get_GroundWaterFeatureImplemented write Set_GroundWaterFeatureImplemented;
  end;

// *********************************************************************//
// DispIntf:  IImplementedNetworkFeaturesDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {53AE96E0-3DF8-454F-840D-FC7AD7E13025}
// *********************************************************************//
  IImplementedNetworkFeaturesDisp = dispinterface
    ['{53AE96E0-3DF8-454F-840D-FC7AD7E13025}']
    property PowerPlantFeatureImplemented: WordBool dispid 101;
    property IrrigationAreaFeatureImplemented: WordBool dispid 102;
    property IrrigationBlockFeatureImplemented: WordBool dispid 103;
    property WetlandFeatureImplemented: WordBool dispid 104;
    property YMDemandCentreFeatureImplemented: WordBool dispid 105;
    property StreamFlowReductionFeatureImplemented: WordBool dispid 106;
    property IFRSiteFeatureImplemented: WordBool dispid 107;
    property MineFeatureImplemented: WordBool dispid 108;
    property CurtailmentAndDroughtFeatureImplemented: WordBool dispid 109;
    property GroundWaterFeatureImplemented: WordBool dispid 110;
  end;

// *********************************************************************//
// Interface: IReservoirAreaGroup
// Flags:     (320) Dual OleAutomation
// GUID:      {1D915594-27DF-48D1-A410-8D28D1B2ADA6}
// *********************************************************************//
  IReservoirAreaGroup = interface(IUnknown)
    ['{1D915594-27DF-48D1-A410-8D28D1B2ADA6}']
    function Get_GroupID: Integer; safecall;
    function Get_GroupName: WideString; safecall;
    procedure Set_GroupName(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property GroupID: Integer read Get_GroupID;
    property GroupName: WideString read Get_GroupName write Set_GroupName;
  end;

// *********************************************************************//
// DispIntf:  IReservoirAreaGroupDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {1D915594-27DF-48D1-A410-8D28D1B2ADA6}
// *********************************************************************//
  IReservoirAreaGroupDisp = dispinterface
    ['{1D915594-27DF-48D1-A410-8D28D1B2ADA6}']
    property GroupID: Integer readonly dispid 201;
    property GroupName: WideString dispid 202;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 203;
  end;

// *********************************************************************//
// Interface: IReservoirAreaGroupList
// Flags:     (320) Dual OleAutomation
// GUID:      {BA43B71A-8542-45CE-B5D8-8D9D268DED94}
// *********************************************************************//
  IReservoirAreaGroupList = interface(IUnknown)
    ['{BA43B71A-8542-45CE-B5D8-8D9D268DED94}']
    function Get_GroupAreaCount: Integer; safecall;
    function ReservoirAreaGroupByIndex(AIndex: Integer): IReservoirAreaGroup; safecall;
    function ReservoirAreaGroupByID(AGroupID: Integer): IReservoirAreaGroup; safecall;
    function ReservoirAreaGroupByName(const AGroupName: WideString): IReservoirAreaGroup; safecall;
    function CreateReservoirAreaGroup: IReservoirAreaGroup; safecall;
    function RemoveReservoirAreaGroup(AGroupID: Integer): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property GroupAreaCount: Integer read Get_GroupAreaCount;
  end;

// *********************************************************************//
// DispIntf:  IReservoirAreaGroupListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {BA43B71A-8542-45CE-B5D8-8D9D268DED94}
// *********************************************************************//
  IReservoirAreaGroupListDisp = dispinterface
    ['{BA43B71A-8542-45CE-B5D8-8D9D268DED94}']
    property GroupAreaCount: Integer readonly dispid 101;
    function ReservoirAreaGroupByIndex(AIndex: Integer): IReservoirAreaGroup; dispid 102;
    function ReservoirAreaGroupByID(AGroupID: Integer): IReservoirAreaGroup; dispid 103;
    function ReservoirAreaGroupByName(const AGroupName: WideString): IReservoirAreaGroup; dispid 104;
    function CreateReservoirAreaGroup: IReservoirAreaGroup; dispid 105;
    function RemoveReservoirAreaGroup(AGroupID: Integer): WordBool; dispid 106;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 107;
  end;

// *********************************************************************//
// Interface: IMineSubCatchmentList
// Flags:     (320) Dual OleAutomation
// GUID:      {BF54785C-8D2E-4FCC-9BAC-ABFCC36FD27C}
// *********************************************************************//
  IMineSubCatchmentList = interface(IUnknown)
    ['{BF54785C-8D2E-4FCC-9BAC-ABFCC36FD27C}']
    function CreateMineSubCatchment(ACatchmentRefNr: Integer): IMineSubCatchment; safecall;
    function Get_MineSubCatchmentCount: Integer; safecall;
    function Get_MineSubCatchmentByRefNodeNr(ARefNodeNr: Integer): IMineSubCatchment; safecall;
    function Get_MineSubCatchmentByIdentifier(AIdentifier: Integer): IMineSubCatchment; safecall;
    function Get_MineSubCatchmentByIndex(AIndex: Integer): IMineSubCatchment; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property MineSubCatchmentCount: Integer read Get_MineSubCatchmentCount;
    property MineSubCatchmentByRefNodeNr[ARefNodeNr: Integer]: IMineSubCatchment read Get_MineSubCatchmentByRefNodeNr;
    property MineSubCatchmentByIdentifier[AIdentifier: Integer]: IMineSubCatchment read Get_MineSubCatchmentByIdentifier;
    property MineSubCatchmentByIndex[AIndex: Integer]: IMineSubCatchment read Get_MineSubCatchmentByIndex;
  end;

// *********************************************************************//
// DispIntf:  IMineSubCatchmentListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {BF54785C-8D2E-4FCC-9BAC-ABFCC36FD27C}
// *********************************************************************//
  IMineSubCatchmentListDisp = dispinterface
    ['{BF54785C-8D2E-4FCC-9BAC-ABFCC36FD27C}']
    function CreateMineSubCatchment(ACatchmentRefNr: Integer): IMineSubCatchment; dispid 101;
    property MineSubCatchmentCount: Integer readonly dispid 102;
    property MineSubCatchmentByRefNodeNr[ARefNodeNr: Integer]: IMineSubCatchment readonly dispid 103;
    property MineSubCatchmentByIdentifier[AIdentifier: Integer]: IMineSubCatchment readonly dispid 104;
    property MineSubCatchmentByIndex[AIndex: Integer]: IMineSubCatchment readonly dispid 105;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 106;
  end;

// *********************************************************************//
// Interface: IMineSubCatchment
// Flags:     (320) Dual OleAutomation
// GUID:      {4DC181F1-7FBE-4BEB-A3B2-C03468D45E9C}
// *********************************************************************//
  IMineSubCatchment = interface(IUnknown)
    ['{4DC181F1-7FBE-4BEB-A3B2-C03468D45E9C}']
    function Get_Identifier: Integer; safecall;
    function Get_CatchmentReferenceNr: Integer; safecall;
    procedure Set_CatchmentReferenceNr(Value: Integer); safecall;
    function Get_MinimunGroundwaterFlowVolume(AIndex: Integer): Double; safecall;
    procedure Set_MinimunGroundwaterFlowVolume(AIndex: Integer; Value: Double); safecall;
    function Get_ProportionAntecedentFlows: Double; safecall;
    procedure Set_ProportionAntecedentFlows(Value: Double); safecall;
    function Get_GroundwaterFlowVolume: Double; safecall;
    procedure Set_GroundwaterFlowVolume(Value: Double); safecall;
    function Get_AntecedentRunoffDecayFactor: Double; safecall;
    procedure Set_AntecedentRunoffDecayFactor(Value: Double); safecall;
    function Get_CatchmentRefName: WideString; safecall;
    procedure Set_CatchmentRefName(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_CatchmentRefUsed: WordBool; safecall;
    procedure Set_CatchmentRefUsed(Value: WordBool); safecall;
    property Identifier: Integer read Get_Identifier;
    property CatchmentReferenceNr: Integer read Get_CatchmentReferenceNr write Set_CatchmentReferenceNr;
    property MinimunGroundwaterFlowVolume[AIndex: Integer]: Double read Get_MinimunGroundwaterFlowVolume write Set_MinimunGroundwaterFlowVolume;
    property ProportionAntecedentFlows: Double read Get_ProportionAntecedentFlows write Set_ProportionAntecedentFlows;
    property GroundwaterFlowVolume: Double read Get_GroundwaterFlowVolume write Set_GroundwaterFlowVolume;
    property AntecedentRunoffDecayFactor: Double read Get_AntecedentRunoffDecayFactor write Set_AntecedentRunoffDecayFactor;
    property CatchmentRefName: WideString read Get_CatchmentRefName write Set_CatchmentRefName;
    property CatchmentRefUsed: WordBool read Get_CatchmentRefUsed write Set_CatchmentRefUsed;
  end;

// *********************************************************************//
// DispIntf:  IMineSubCatchmentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4DC181F1-7FBE-4BEB-A3B2-C03468D45E9C}
// *********************************************************************//
  IMineSubCatchmentDisp = dispinterface
    ['{4DC181F1-7FBE-4BEB-A3B2-C03468D45E9C}']
    property Identifier: Integer readonly dispid 101;
    property CatchmentReferenceNr: Integer dispid 102;
    property MinimunGroundwaterFlowVolume[AIndex: Integer]: Double dispid 103;
    property ProportionAntecedentFlows: Double dispid 104;
    property GroundwaterFlowVolume: Double dispid 105;
    property AntecedentRunoffDecayFactor: Double dispid 106;
    property CatchmentRefName: WideString dispid 107;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 108;
    property CatchmentRefUsed: WordBool dispid 109;
  end;

// *********************************************************************//
// Interface: IChannelTariff
// Flags:     (320) Dual OleAutomation
// GUID:      {2EB5ED1C-D4B5-4CA6-91BD-AC1EB88DF393}
// *********************************************************************//
  IChannelTariff = interface(IUnknown)
    ['{2EB5ED1C-D4B5-4CA6-91BD-AC1EB88DF393}']
    function Populate(AIdentifier: Integer; AChannelNumber: Integer; ATariff: Double;
                      const AEscalationFactors: WideString): WordBool; stdcall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; stdcall;
    function Get_Identifier: Integer; safecall;
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(AValue: Integer); safecall;
    function Get_EscalationFactors: WideString; safecall;
    procedure Set_EscalationFactors(const AValue: WideString); safecall;
    function Get_Tariff: Double; safecall;
    procedure Set_Tariff(AValue: Double); safecall;
    property Identifier: Integer read Get_Identifier;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property EscalationFactors: WideString read Get_EscalationFactors write Set_EscalationFactors;
    property Tariff: Double read Get_Tariff write Set_Tariff;
  end;

// *********************************************************************//
// DispIntf:  IChannelTariffDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {2EB5ED1C-D4B5-4CA6-91BD-AC1EB88DF393}
// *********************************************************************//
  IChannelTariffDisp = dispinterface
    ['{2EB5ED1C-D4B5-4CA6-91BD-AC1EB88DF393}']
    function Populate(AIdentifier: Integer; AChannelNumber: Integer; ATariff: Double;
                      const AEscalationFactors: WideString): WordBool; dispid 201;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 202;
    property Identifier: Integer readonly dispid 203;
    property ChannelNumber: Integer dispid 204;
    property EscalationFactors: WideString dispid 206;
    property Tariff: Double dispid 207;
  end;

// *********************************************************************//
// Interface: ITariffCalculationData
// Flags:     (320) Dual OleAutomation
// GUID:      {14DF8EB9-A444-4CB0-87CE-9A27444D0F93}
// *********************************************************************//
  ITariffCalculationData = interface(IUnknown)
    ['{14DF8EB9-A444-4CB0-87CE-9A27444D0F93}']
    function NewChannelTariff(AChannelNumber: Integer): WordBool; safecall;
    function RemoveChannelTariffByChannelNumber(AChannelNumber: Integer): WordBool; safecall;
    function Get_ChannelTariffCount: Integer; safecall;
    function Get_ChannelTariffByIndex(AIndex: Integer): IChannelTariff; safecall;
    function Get_ChannelTariffByChannelNumber(AChannelNumber: Integer): IChannelTariff; safecall;
    function Get_DataYears: Integer; safecall;
    procedure Set_DataYears(Value: Integer); safecall;
    function Populate(ADataYears: Integer): WordBool; safecall;
    property ChannelTariffCount: Integer read Get_ChannelTariffCount;
    property ChannelTariffByIndex[AIndex: Integer]: IChannelTariff read Get_ChannelTariffByIndex;
    property ChannelTariffByChannelNumber[AChannelNumber: Integer]: IChannelTariff read Get_ChannelTariffByChannelNumber;
    property DataYears: Integer read Get_DataYears write Set_DataYears;
  end;

// *********************************************************************//
// DispIntf:  ITariffCalculationDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {14DF8EB9-A444-4CB0-87CE-9A27444D0F93}
// *********************************************************************//
  ITariffCalculationDataDisp = dispinterface
    ['{14DF8EB9-A444-4CB0-87CE-9A27444D0F93}']
    function NewChannelTariff(AChannelNumber: Integer): WordBool; dispid 201;
    function RemoveChannelTariffByChannelNumber(AChannelNumber: Integer): WordBool; dispid 202;
    property ChannelTariffCount: Integer readonly dispid 203;
    property ChannelTariffByIndex[AIndex: Integer]: IChannelTariff readonly dispid 204;
    property ChannelTariffByChannelNumber[AChannelNumber: Integer]: IChannelTariff readonly dispid 205;
    property DataYears: Integer dispid 206;
    function Populate(ADataYears: Integer): WordBool; dispid 101;
  end;

// *********************************************************************//
// Interface: IPlanningModel
// Flags:     (320) Dual OleAutomation
// GUID:      {F3EAE7EA-2F4C-490C-A455-60FD75B344B9}
// *********************************************************************//
  IPlanningModel = interface(IYieldModel)
    ['{F3EAE7EA-2F4C-490C-A455-60FD75B344B9}']
    function Get_PlanningModelData: IPlanningModelData; safecall;
    property PlanningModelData: IPlanningModelData read Get_PlanningModelData;
  end;

// *********************************************************************//
// DispIntf:  IPlanningModelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {F3EAE7EA-2F4C-490C-A455-60FD75B344B9}
// *********************************************************************//
  IPlanningModelDisp = dispinterface
    ['{F3EAE7EA-2F4C-490C-A455-60FD75B344B9}']
    property PlanningModelData: IPlanningModelData readonly dispid 234;
    function DoValidateAllFiles: WordBool; dispid 101;
    function DoExportAllFiles: WordBool; dispid 102;
    function DoImportAllFiles: WordBool; dispid 103;
    function DoClearModelData: WordBool; dispid 104;
    function DoWizardNewReservoir: WordBool; dispid 105;
    function DoWizardNewNodeWithInflow: WordBool; dispid 106;
    function DoInvokeWizard: WordBool; dispid 107;
    function DoWizardRunYieldHistoric: WordBool; dispid 108;
    function DoWizardRunYieldStochastic: WordBool; dispid 109;
    function DoWizardNewChannel(const AUpDownNodeNumbers: WideString): WordBool; dispid 110;
    property YieldModelData: IYieldModelData readonly dispid 111;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 112;
    function DoExportAllFilesAndRunModel(ASilent: WordBool): WordBool; dispid 113;
    function GetYieldChannelYield: Double; dispid 114;
    function DoRunModel: WordBool; dispid 115;
    function DoCreateReservoir: IReservoirData; dispid 116;
    function DoDeleteReservoir(AReservoirNumber: Integer): WordBool; dispid 117;
    function DoCreateNodeWithInflow: IReservoirData; dispid 118;
    function DoDeleteNodeWithInflow(ANodeNumber: Integer): WordBool; dispid 119;
    function DoCreateNodeWithoutInflow: IReservoirData; dispid 120;
    function DoDeleteNodeWithoutInflow(ANodeNumber: Integer): WordBool; dispid 121;
    function DoCreateChannel(AUpStreamNodeNumber: Integer; ADownStreamNodeNumber: Integer): IGeneralFlowChannel; dispid 122;
    function DoDeleteChannel(AChannelNumber: Integer): WordBool; dispid 123;
    function DoConvertChannel(AChannelNumber: Integer): WordBool; dispid 124;
    function DoCreateMinimumFlowFeature(AChannelNumber: Integer): IMinimumFlowConstraint; dispid 125;
    function DoCreateMinMaxFlowFeature(AChannelNumber: Integer): IMinMaxFlowConstraint; dispid 126;
    function DoCreatePumpingFeature(AChannelNumber: Integer): IPumpingFeature; dispid 127;
    function DoCreateLossFeature(AChannelNumber: Integer): ILossFeature; dispid 128;
    function DoCreateSpecifiedDemandFeature(AChannelNumber: Integer): ISpecifiedDemandFeature; dispid 129;
    function DoCreateDiversionFeature(AChannelNumber: Integer): IDiversionFeature; dispid 130;
    function DoCreateSpecifiedInflowFeature(AChannelNumber: Integer): ISpecifiedInflowFeature; dispid 131;
    function DoCreateIFRFeature(AChannelNumber: Integer; AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature; dispid 132;
    function DoCreatePhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint; dispid 133;
    function DoCreateIrrigationArea: IIrrigationArea; dispid 134;
    function DoCreatePowerPlant: IPowerPlant; dispid 135;
    function DoDeletePumpingFeature(AChannelNumber: Integer): WordBool; dispid 136;
    function DoDeleteIFRFeature(AChannelNumber: Integer): WordBool; dispid 137;
    function DoDeletePhysicalFlowConstraint(AChannelNumber: Integer): WordBool; dispid 138;
    function DoDeleteIrrigationArea(AFeatureID: Integer): WordBool; dispid 139;
    function DoDeletePowerPlant(AFeatureID: Integer): WordBool; dispid 140;
    function DoCreateMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature; dispid 141;
    function DoDeleteMasterControlFeature(AChannelNumber: Integer): WordBool; dispid 142;
    function DoCreateWaterDemandFeature(AChannelNumber: Integer): IWaterDemandFeature; dispid 143;
    function DoDeleteWaterDemandFeature(AChannelNumber: Integer): WordBool; dispid 144;
    function ViewInputReservoirDialog(AResevoirNumber: Integer): WordBool; dispid 145;
    function ViewInputNodeWithInflowDialog(ANodeNumber: Integer): WordBool; dispid 146;
    function ViewInputChannelDialog(AChannelNumber: Integer): WordBool; dispid 147;
    function HandleVNVEvent(const AVisioApp: IUnknown; const AVisioDoc: IUnknown;
                            AVisioEventCode: Integer; const ASourceObj: IUnknown;
                            AEventID: Integer; AEventSeqNum: Integer; const ASubjectObj: IUnknown;
                            AMoreInfo: OleVariant): WordBool; dispid 148;
    function ProcessVNVSpecial(const AParameter: WideString): WordBool; dispid 150;
    function DoCreateIrrigationBlock: IIrrigationBlock; dispid 151;
    function DoDeleteIrrigationBlock(AFeatureID: Integer): WordBool; dispid 152;
    function DoCreateWetland: IWetland; dispid 153;
    function DoDeleteWetland(AFeatureID: Integer): WordBool; dispid 154;
    function ViewInputIrrigationBlockDialog(AIrrigationBlockNr: Integer): WordBool; dispid 155;
    function ViewInputWetlandDialog(AWetlandNr: Integer): WordBool; dispid 156;
    function DoCreateYMDemandCentre: IYMDemandCentre; dispid 157;
    function DoDeleteYMDemandCentre(ANodeNumber: Integer): WordBool; dispid 158;
    function DoCreateSFRSubCatchment: IStreamFlowReduction; dispid 160;
    function DoDeleteSFRSubCatchment(AIdentifier: Integer): WordBool; dispid 161;
    function ViewInputDemandCentreDialog(AYMDemandCentreNr: Integer): WordBool; dispid 162;
    function ReadFirmYieldFromDebugFile: Double; dispid 165;
    function DoCreateMine: IMine; dispid 166;
    function DoDeleteMine(AMineNumber: Integer): WordBool; dispid 167;
    function DoCreateOpenCast(AMineNumber: Integer): IOpenCast; dispid 168;
    function DoDeleteOpenCast(AMineNumber: Integer; AOpenCastIdentifier: Integer): WordBool; dispid 169;
    function DoCreateUnderGround(AMineNumber: Integer): IUnderground; dispid 170;
    function DoDeleteUnderGround(AMineNumber: Integer; AUnderGroundIdentifier: Integer): WordBool; dispid 171;
    function DoCreateSlurryDump(AMineNumber: Integer): ISlurryDump; dispid 172;
    function DoDeleteSlurryDump(AMineNumber: Integer; ASlurryDumpIdentifier: Integer): WordBool; dispid 173;
    function ViewInputConfigurationDialog(const AViewName: WideString): WordBool; dispid 174;
    function ImportOutputFiles: WordBool; dispid 175;
    function ViewInputNodeWithoutInflowDialog(ANodeNumber: Integer): WordBool; dispid 176;
    function ViewInputPowerPlantDialog(APowerPlantNumber: Integer): WordBool; dispid 177;
    function ViewInputIrrigationAreaDialog(AIrrigationAreaNumber: Integer): WordBool; dispid 178;
    function ViewInputStreamFlowReductionDialog(AStreamFlowReductionNumber: Integer): WordBool; dispid 179;
    function ViewInputMineDialog(AMineNodeNr: Integer): WordBool; dispid 180;
    function ViewInputMinePCDDamDialog(APCDDamNr: Integer): WordBool; dispid 181;
    function ViewInputMineUndergroundDamDialog(AUndergroundDamNr: Integer): WordBool; dispid 182;
    function ViewInputMasterControlChannelDialog(AChannelNumber: Integer): WordBool; dispid 183;
    function ViewOutputMasterControlChannelDialog(AChannelNumber: Integer): WordBool; dispid 184;
    function ViewOutputReservoirDialog(AResevoirNumber: Integer): WordBool; dispid 185;
    function ViewOutputNodeWithInflowDialog(ANodeNumber: Integer): WordBool; dispid 186;
    function ViewOutputNodeWithoutInflowDialog(ANodeNumber: Integer): WordBool; dispid 187;
    function ViewOutputChannelDialog(AChannelNumber: Integer): WordBool; dispid 188;
    function ViewOutputPowerPlantDialog(APowerPlantNumber: Integer): WordBool; dispid 189;
    function ViewOutputIrrigationAreaDialog(AIrrigationAreaNumber: Integer): WordBool; dispid 190;
    function ViewOutputIrrigationBlockDialog(AIrrigationBlockNr: Integer): WordBool; dispid 191;
    function ViewOutputWetlandDialog(AWetlandNr: Integer): WordBool; dispid 192;
    function ViewOutputStreamFlowReductionDialog(AStreamFlowReductionNumber: Integer): WordBool; dispid 193;
    function ViewOutputDemandCentreDialog(AYMDemandCentreNr: Integer): WordBool; dispid 194;
    function ViewOutputMineDialog(AMineNodeNr: Integer): WordBool; dispid 195;
    function ViewOutputMinePCDDamDialog(APCDDamNr: Integer): WordBool; dispid 196;
    function ViewOutputMineUndergroundDamDialog(AUndergroundDamNr: Integer): WordBool; dispid 197;
    property WRYMRunOptions: IWRYMRunOptions readonly dispid 149;
    property YieldModelIterationTracker: IYieldModelIterationTracker readonly dispid 159;
    function DoCreateDroughtRestriction: IDroughtRestriction; dispid 163;
    function DoDeleteDroughtRestriction(AIdentifier: Integer): WordBool; dispid 164;
    function DoCopyIFRFeature(AChannelNumber: Integer): IIFRFeature; dispid 198;
    function DoCopyChannel(AChannelNumber: Integer): IGeneralFlowChannel; dispid 199;
    function DoCopyMinimumFlowFeature(AChannelNumber: Integer): IMinimumFlowConstraint; dispid 200;
    function DoCopyMinMaxFlowFeature(AChannelNumber: Integer): IMinMaxFlowConstraint; dispid 201;
    function DoCopyLossFeature(AChannelNumber: Integer): ILossFeature; dispid 202;
    function DoCopySpecifiedDemandFeature(AChannelNumber: Integer): ISpecifiedDemandFeature; dispid 203;
    function DoCopyDiversionFeature(AChannelNumber: Integer): IDiversionFeature; dispid 204;
    function DoCopySpecifiedInflowFeature(AChannelNumber: Integer): ISpecifiedInflowFeature; dispid 205;
    function CopyPhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint; dispid 206;
    function DoCopyMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature; dispid 207;
    function DoCopyPumpingFeature(AChannelNumber: Integer): IPumpingFeature; dispid 208;
    function DoCopyWaterDemandFeature(AChannelNumber: Integer): IWaterDemandFeature; dispid 209;
    function DoCopyPowerPlant(AChannelNumber: Integer): IPowerPlant; dispid 210;
    function DoCopyIrrigationArea(AFeatureID: Integer): IIrrigationArea; dispid 211;
    function DoCopyIrrigationBlock(AFeatureID: Integer): IIrrigationBlock; dispid 212;
    function DoCopyWetland(AWetlandID: Integer): IWetland; dispid 213;
    function DoCopySFRSubCatchment(AStreamFlowReductionID: Integer): IStreamFlowReduction; dispid 214;
    function DoCopyYMDemandCentre(ANodeNumber: Integer): IYMDemandCentre; dispid 215;
    function DoCopyMine(AMineNumber: Integer): IMine; dispid 216;
    function DoCopyReservoir(AReservoirNumber: Integer): IReservoirData; dispid 217;
    function DoRunStorageVsYield(AReservoirNumber: Integer;
                                 const AStartingStorageCommaText: WideString;
                                 var AMinTargetDraftCommaText: WideString;
                                 var AMaxTargetDraftCommaText: WideString;
                                 var AYieldCommaText: WideString): WordBool; dispid 218;
    function DoCreateGroundWater: IGroundWater; dispid 219;
    function DoDeleteGroundWater(AGroundWaterID: Integer): WordBool; dispid 220;
    function DoCopyReservoirFromScenario: WordBool; dispid 221;
    function DoCopyChannelFromScenario: WordBool; dispid 222;
    function DoCopyIrrigationAreaFromScenario: WordBool; dispid 223;
    function DoCopyPowerPlantFromScenario: WordBool; dispid 224;
    function DoCopyIrrigationBlockFromScenario: WordBool; dispid 225;
    function DoCopyWetlandFromScenario: WordBool; dispid 226;
    function DoCopyYMDemandCentreFromScenario: WordBool; dispid 227;
    function DoCopySFRFromScenario: WordBool; dispid 228;
    function DoCopyMineFromScenario: WordBool; dispid 229;
    function DoCopyGroundWaterFromScenario: WordBool; dispid 230;
    function ViewInputGroundwaterDialog(AAquiferNodeNr: Integer): WordBool; dispid 231;
    function ViewOutputGroundwaterDialog(AAquiferNodeNr: Integer): WordBool; dispid 232;
    function StudyPropertiesCommaText: WideString; dispid 233;
  end;

// *********************************************************************//
// Interface: IMinMaxUpperBoundChannel
// Flags:     (320) Dual OleAutomation
// GUID:      {D66F4E22-7296-48C7-9303-6FC166C8B9F2}
// *********************************************************************//
  IMinMaxUpperBoundChannel = interface(IUnknown)
    ['{D66F4E22-7296-48C7-9303-6FC166C8B9F2}']
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(Value: Integer); safecall;
    function Get_NoOfBoundedChannels: Integer; safecall;
    function Get_BoundedChannels: WideString; safecall;
    procedure Set_BoundedChannels(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property NoOfBoundedChannels: Integer read Get_NoOfBoundedChannels;
    property BoundedChannels: WideString read Get_BoundedChannels write Set_BoundedChannels;
  end;

// *********************************************************************//
// DispIntf:  IMinMaxUpperBoundChannelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {D66F4E22-7296-48C7-9303-6FC166C8B9F2}
// *********************************************************************//
  IMinMaxUpperBoundChannelDisp = dispinterface
    ['{D66F4E22-7296-48C7-9303-6FC166C8B9F2}']
    property ChannelNumber: Integer dispid 101;
    property NoOfBoundedChannels: Integer readonly dispid 102;
    property BoundedChannels: WideString dispid 103;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 104;
  end;

// *********************************************************************//
// Interface: IWQConstriantsChannel
// Flags:     (320) Dual OleAutomation
// GUID:      {C7931A9B-780E-46CC-B0F1-B16847AC76DA}
// *********************************************************************//
  IWQConstriantsChannel = interface(IUnknown)
    ['{C7931A9B-780E-46CC-B0F1-B16847AC76DA}']
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(Value: Integer); safecall;
    function Get_Target: Double; safecall;
    procedure Set_Target(Value: Double); safecall;
    function Get_NoOfRefChannelsBlending: Integer; safecall;
    procedure Set_NoOfRefChannelsBlending(Value: Integer); safecall;
    function Get_ReservoirRef: Integer; safecall;
    procedure Set_ReservoirRef(Value: Integer); safecall;
    function Get_WQConType: Integer; safecall;
    procedure Set_WQConType(Value: Integer); safecall;
    function Get_BlendingChannels: WideString; safecall;
    procedure Set_BlendingChannels(const Value: WideString); safecall;
    function Get_BlendingChannelFactors: WideString; safecall;
    procedure Set_BlendingChannelFactors(const Value: WideString); safecall;
    function Get_LimitingSlope: Integer; safecall;
    procedure Set_LimitingSlope(Value: Integer); safecall;
    function Get_EstimatedRelease: WideString; safecall;
    procedure Set_EstimatedRelease(const Value: WideString); safecall;
    function Get_Concentration: WideString; safecall;
    procedure Set_Concentration(const Value: WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property Target: Double read Get_Target write Set_Target;
    property NoOfRefChannelsBlending: Integer read Get_NoOfRefChannelsBlending write Set_NoOfRefChannelsBlending;
    property ReservoirRef: Integer read Get_ReservoirRef write Set_ReservoirRef;
    property WQConType: Integer read Get_WQConType write Set_WQConType;
    property BlendingChannels: WideString read Get_BlendingChannels write Set_BlendingChannels;
    property BlendingChannelFactors: WideString read Get_BlendingChannelFactors write Set_BlendingChannelFactors;
    property LimitingSlope: Integer read Get_LimitingSlope write Set_LimitingSlope;
    property EstimatedRelease: WideString read Get_EstimatedRelease write Set_EstimatedRelease;
    property Concentration: WideString read Get_Concentration write Set_Concentration;
  end;

// *********************************************************************//
// DispIntf:  IWQConstriantsChannelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {C7931A9B-780E-46CC-B0F1-B16847AC76DA}
// *********************************************************************//
  IWQConstriantsChannelDisp = dispinterface
    ['{C7931A9B-780E-46CC-B0F1-B16847AC76DA}']
    property ChannelNumber: Integer dispid 101;
    property Target: Double dispid 102;
    property NoOfRefChannelsBlending: Integer dispid 103;
    property ReservoirRef: Integer dispid 104;
    property WQConType: Integer dispid 105;
    property BlendingChannels: WideString dispid 106;
    property BlendingChannelFactors: WideString dispid 107;
    property LimitingSlope: Integer dispid 108;
    property EstimatedRelease: WideString dispid 109;
    property Concentration: WideString dispid 110;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 111;
  end;

// *********************************************************************//
// Interface: IWQConstraintData
// Flags:     (320) Dual OleAutomation
// GUID:      {F9F7CE9B-61C1-45BD-84EA-B11041A13F4A}
// *********************************************************************//
  IWQConstraintData = interface(IUnknown)
    ['{F9F7CE9B-61C1-45BD-84EA-B11041A13F4A}']
    function NewWQConstraintsChannels(AChannel: Integer): IWQConstriantsChannel; safecall;
    function RemoveWQConstriantsChannel(AChannelNo: Integer): WordBool; safecall;
    function Get_WQConstraintsChannelCount: Integer; safecall;
    function Get_WQConstraintsChannelByIndex(AIndex: Integer): IWQConstriantsChannel; safecall;
    function Get_WQConstraintsChannelByChannelNo(AChannelNo: Integer): IWQConstriantsChannel; safecall;
    function NewMinMaxUpperBoundChannel(AChannelNo: Integer): IMinMaxUpperBoundChannel; safecall;
    function RemoveMinMaxUpperBoundChannel(AChannelNo: Integer): WordBool; safecall;
    function Get_MinMaxUpperBoundChannelCount: Integer; safecall;
    function Get_MinMaxUpperBoundChannelByIndex(AIndex: Integer): IMinMaxUpperBoundChannel; safecall;
    function Get_MinMaxUpperBoundChannelNo(AChannelNo: Integer): IMinMaxUpperBoundChannel; safecall;
    property WQConstraintsChannelCount: Integer read Get_WQConstraintsChannelCount;
    property WQConstraintsChannelByIndex[AIndex: Integer]: IWQConstriantsChannel read Get_WQConstraintsChannelByIndex;
    property WQConstraintsChannelByChannelNo[AChannelNo: Integer]: IWQConstriantsChannel read Get_WQConstraintsChannelByChannelNo;
    property MinMaxUpperBoundChannelCount: Integer read Get_MinMaxUpperBoundChannelCount;
    property MinMaxUpperBoundChannelByIndex[AIndex: Integer]: IMinMaxUpperBoundChannel read Get_MinMaxUpperBoundChannelByIndex;
    property MinMaxUpperBoundChannelNo[AChannelNo: Integer]: IMinMaxUpperBoundChannel read Get_MinMaxUpperBoundChannelNo;
  end;

// *********************************************************************//
// DispIntf:  IWQConstraintDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {F9F7CE9B-61C1-45BD-84EA-B11041A13F4A}
// *********************************************************************//
  IWQConstraintDataDisp = dispinterface
    ['{F9F7CE9B-61C1-45BD-84EA-B11041A13F4A}']
    function NewWQConstraintsChannels(AChannel: Integer): IWQConstriantsChannel; dispid 101;
    function RemoveWQConstriantsChannel(AChannelNo: Integer): WordBool; dispid 102;
    property WQConstraintsChannelCount: Integer readonly dispid 103;
    property WQConstraintsChannelByIndex[AIndex: Integer]: IWQConstriantsChannel readonly dispid 104;
    property WQConstraintsChannelByChannelNo[AChannelNo: Integer]: IWQConstriantsChannel readonly dispid 105;
    function NewMinMaxUpperBoundChannel(AChannelNo: Integer): IMinMaxUpperBoundChannel; dispid 106;
    function RemoveMinMaxUpperBoundChannel(AChannelNo: Integer): WordBool; dispid 107;
    property MinMaxUpperBoundChannelCount: Integer readonly dispid 108;
    property MinMaxUpperBoundChannelByIndex[AIndex: Integer]: IMinMaxUpperBoundChannel readonly dispid 109;
    property MinMaxUpperBoundChannelNo[AChannelNo: Integer]: IMinMaxUpperBoundChannel readonly dispid 110;
  end;

// *********************************************************************//
// Interface: IMultiResMultiChannelCurtail
// Flags:     (320) Dual OleAutomation
// GUID:      {9D444A57-3146-4842-AD63-F5AF097826D5}
// *********************************************************************//
  IMultiResMultiChannelCurtail = interface(IUnknown)
    ['{9D444A57-3146-4842-AD63-F5AF097826D5}']
    function Get_StartMonth: Integer; safecall;
    procedure Set_StartMonth(Value: Integer); safecall;
    function Get_ReservoirNo: Integer; safecall;
    procedure Set_ReservoirNo(Value: Integer); safecall;
    function Get_ChannelNo: Integer; safecall;
    procedure Set_ChannelNo(Value: Integer); safecall;
    function Get_DecisionMonth: Integer; safecall;
    procedure Set_DecisionMonth(Value: Integer); safecall;
    function Get_ElevationByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ElevationByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_FactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_FactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Validate(var AError: WideString; const AContext: WideString): WordBool; safecall;
    function Get_Identifier: Integer; safecall;
    procedure Set_Identifier(Value: Integer); safecall;
    function ValidateGrid(var AError: WideString; const AContext: WideString; ACol: Integer;
                          ARow: Integer): WordBool; safecall;
    property StartMonth: Integer read Get_StartMonth write Set_StartMonth;
    property ReservoirNo: Integer read Get_ReservoirNo write Set_ReservoirNo;
    property ChannelNo: Integer read Get_ChannelNo write Set_ChannelNo;
    property DecisionMonth: Integer read Get_DecisionMonth write Set_DecisionMonth;
    property ElevationByIndex[AIndex: Integer]: Double read Get_ElevationByIndex write Set_ElevationByIndex;
    property FactorByIndex[AIndex: Integer]: Double read Get_FactorByIndex write Set_FactorByIndex;
    property Identifier: Integer read Get_Identifier write Set_Identifier;
  end;

// *********************************************************************//
// DispIntf:  IMultiResMultiChannelCurtailDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {9D444A57-3146-4842-AD63-F5AF097826D5}
// *********************************************************************//
  IMultiResMultiChannelCurtailDisp = dispinterface
    ['{9D444A57-3146-4842-AD63-F5AF097826D5}']
    property StartMonth: Integer dispid 101;
    property ReservoirNo: Integer dispid 102;
    property ChannelNo: Integer dispid 103;
    property DecisionMonth: Integer dispid 104;
    property ElevationByIndex[AIndex: Integer]: Double dispid 105;
    property FactorByIndex[AIndex: Integer]: Double dispid 106;
    function Validate(var AError: WideString; const AContext: WideString): WordBool; dispid 107;
    property Identifier: Integer dispid 108;
    function ValidateGrid(var AError: WideString; const AContext: WideString; ACol: Integer;
                          ARow: Integer): WordBool; dispid 109;
  end;

// *********************************************************************//
// Interface: IMultiResMultiChannelCurtailList
// Flags:     (320) Dual OleAutomation
// GUID:      {70CFCA36-BB7D-4834-BBCE-1ECBE036F9D9}
// *********************************************************************//
  IMultiResMultiChannelCurtailList = interface(IUnknown)
    ['{70CFCA36-BB7D-4834-BBCE-1ECBE036F9D9}']
    function NewRestriction(AChannelNo: Integer): IMultiResMultiChannelCurtail; safecall;
    function RemoveRestriction(aChannelNo: Integer): WordBool; safecall;
    function Get_RestrictionByReservoirNo(AIndex: Integer): IMultiResMultiChannelCurtail; safecall;
    function Get_RestrictionByChannelNo(AIndex: Integer): IMultiResMultiChannelCurtail; safecall;
    function Get_RestrictionByIndentifier(AIndex: Integer): IMultiResMultiChannelCurtail; safecall;
    property RestrictionByReservoirNo[AIndex: Integer]: IMultiResMultiChannelCurtail read Get_RestrictionByReservoirNo;
    property RestrictionByChannelNo[AIndex: Integer]: IMultiResMultiChannelCurtail read Get_RestrictionByChannelNo;
    property RestrictionByIndentifier[AIndex: Integer]: IMultiResMultiChannelCurtail read Get_RestrictionByIndentifier;
  end;

// *********************************************************************//
// DispIntf:  IMultiResMultiChannelCurtailListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {70CFCA36-BB7D-4834-BBCE-1ECBE036F9D9}
// *********************************************************************//
  IMultiResMultiChannelCurtailListDisp = dispinterface
    ['{70CFCA36-BB7D-4834-BBCE-1ECBE036F9D9}']
    function NewRestriction(AChannelNo: Integer): IMultiResMultiChannelCurtail; dispid 101;
    function RemoveRestriction(aChannelNo: Integer): WordBool; dispid 102;
    property RestrictionByReservoirNo[AIndex: Integer]: IMultiResMultiChannelCurtail readonly dispid 105;
    property RestrictionByChannelNo[AIndex: Integer]: IMultiResMultiChannelCurtail readonly dispid 106;
    property RestrictionByIndentifier[AIndex: Integer]: IMultiResMultiChannelCurtail readonly dispid 103;
  end;

// *********************************************************************//
// Interface: IPlanningMine
// Flags:     (320) Dual OleAutomation
// GUID:      {2D979DB3-2616-4BD0-B5D3-EE767B200305}
// *********************************************************************//
  IPlanningMine = interface(IMine)
    ['{2D979DB3-2616-4BD0-B5D3-EE767B200305}']
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_AssocSaltWashoff: Integer; safecall;
    procedure Set_AssocSaltWashoff(Value: Integer); safecall;
    function Get_MeanAnnualPrecipitation: Double; safecall;
    procedure Set_MeanAnnualPrecipitation(Value: Double); safecall;
    function Get_SaltBuildUpRate: Double; safecall;
    procedure Set_SaltBuildUpRate(Value: Double); safecall;
    function Get_SaltWashOffEfficiencyFactor: Double; safecall;
    procedure Set_SaltWashOffEfficiencyFactor(Value: Double); safecall;
    function Get_IniSaltStore: Double; safecall;
    procedure Set_IniSaltStore(Value: Double); safecall;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property AssocSaltWashoff: Integer read Get_AssocSaltWashoff write Set_AssocSaltWashoff;
    property MeanAnnualPrecipitation: Double read Get_MeanAnnualPrecipitation write Set_MeanAnnualPrecipitation;
    property SaltBuildUpRate: Double read Get_SaltBuildUpRate write Set_SaltBuildUpRate;
    property SaltWashOffEfficiencyFactor: Double read Get_SaltWashOffEfficiencyFactor write Set_SaltWashOffEfficiencyFactor;
    property IniSaltStore: Double read Get_IniSaltStore write Set_IniSaltStore;
  end;

// *********************************************************************//
// DispIntf:  IPlanningMineDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {2D979DB3-2616-4BD0-B5D3-EE767B200305}
// *********************************************************************//
  IPlanningMineDisp = dispinterface
    ['{2D979DB3-2616-4BD0-B5D3-EE767B200305}']
    property RainfallFileName: WideString dispid 202;
    property AssocSaltWashoff: Integer dispid 201;
    property MeanAnnualPrecipitation: Double dispid 203;
    property SaltBuildUpRate: Double dispid 204;
    property SaltWashOffEfficiencyFactor: Double dispid 205;
    property IniSaltStore: Double dispid 206;
    property Identifier: Integer readonly dispid 101;
    property NodeNumber: Integer readonly dispid 102;
    property MineName: WideString dispid 103;
    property RiverChannelNumber: Integer readonly dispid 104;
    property PCDChannelNumber: Integer readonly dispid 105;
    property HydrologyNodeNumber: Integer dispid 106;
    property BeneficiationPlantArea: Double dispid 107;
    property BeneficiationRunoffFactor: Double dispid 108;
    property PanEvaporation[AIndex: Integer]: Double dispid 109;
    property LakeEvaporation[AIndex: Integer]: Double dispid 110;
    property OpenCastCount: Integer readonly dispid 111;
    property UndergroundCount: Integer readonly dispid 112;
    property SlurryDumpCount: Integer readonly dispid 113;
    property OpenCastByIndex[AIndex: Integer]: IOpenCast readonly dispid 114;
    property OpenCastByIdentifier[AIdentifier: Integer]: IOpenCast readonly dispid 115;
    property UnderGroundByIndex[AIndex: Integer]: IUnderground readonly dispid 116;
    property UnderGroundByIdentifier[AIdentifier: Integer]: IUnderground readonly dispid 117;
    property SlurryDumpByIndex[AIndex: Integer]: ISlurryDump readonly dispid 118;
    property SlurryDumpByIdentifier[AIdentifier: Integer]: ISlurryDump readonly dispid 119;
    property RiverChannel: IGeneralFlowChannel readonly dispid 120;
    property PCDChannel: IGeneralFlowChannel readonly dispid 121;
    property PolutionControlDam: IReservoirData readonly dispid 122;
    property HydrologyNode: IReservoirData readonly dispid 123;
    function CreateOpenCast: IOpenCast; dispid 124;
    function CreateUnderGround: IUnderground; dispid 125;
    function CreateSlurryDump: ISlurryDump; dispid 126;
    function RemoveOpenCast(AIdentifier: Integer): WordBool; dispid 127;
    function RemoveUnderGround(AIdentifier: Integer): WordBool; dispid 128;
    function RemoveSlurryDump(AIdentifier: Integer): WordBool; dispid 129;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 130;
    property RiverNode: IReservoirData readonly dispid 131;
    property MineNode: IReservoirData readonly dispid 132;
    property CatchmentRefNrByReservoirID[AReservoirID: Integer]: Integer readonly dispid 133;
    property PolutionControlDamExists: WordBool dispid 134;
  end;

// *********************************************************************//
// Interface: IPlanningMineGrowthFactor
// Flags:     (320) Dual OleAutomation
// GUID:      {6FF1E9C2-3BB7-42AF-B40B-51B44F56C6E7}
// *********************************************************************//
  IPlanningMineGrowthFactor = interface(IUnknown)
    ['{6FF1E9C2-3BB7-42AF-B40B-51B44F56C6E7}']
    function Get_NoOfPoints: Integer; safecall;
    procedure Set_NoOfPoints(Value: Integer); safecall;
    function Get_NoOfYearsByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_NoOfYearsByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_GrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_GrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_InterpolationMethod: Integer; safecall;
    procedure Set_InterpolationMethod(Value: Integer); safecall;
    function Validate(var AError: WideString; const AContext: WideString): WordBool; safecall;
    property NoOfPoints: Integer read Get_NoOfPoints write Set_NoOfPoints;
    property NoOfYearsByIndex[AIndex: Integer]: Integer read Get_NoOfYearsByIndex write Set_NoOfYearsByIndex;
    property GrowthFactorByIndex[AIndex: Integer]: Double read Get_GrowthFactorByIndex write Set_GrowthFactorByIndex;
    property InterpolationMethod: Integer read Get_InterpolationMethod write Set_InterpolationMethod;
  end;

// *********************************************************************//
// DispIntf:  IPlanningMineGrowthFactorDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6FF1E9C2-3BB7-42AF-B40B-51B44F56C6E7}
// *********************************************************************//
  IPlanningMineGrowthFactorDisp = dispinterface
    ['{6FF1E9C2-3BB7-42AF-B40B-51B44F56C6E7}']
    property NoOfPoints: Integer dispid 101;
    property NoOfYearsByIndex[AIndex: Integer]: Integer dispid 102;
    property GrowthFactorByIndex[AIndex: Integer]: Double dispid 103;
    property InterpolationMethod: Integer dispid 104;
    function Validate(var AError: WideString; const AContext: WideString): WordBool; dispid 105;
  end;

// *********************************************************************//
// Interface: IPlanningOpenCast
// Flags:     (320) Dual OleAutomation
// GUID:      {342ED78F-8F33-4323-9DFD-FBCD4312B2B8}
// *********************************************************************//
  IPlanningOpenCast = interface(IOpenCast)
    ['{342ED78F-8F33-4323-9DFD-FBCD4312B2B8}']
    function Get_PCDIniConcentration: Double; safecall;
    procedure Set_PCDIniConcentration(Value: Double); safecall;
    function Get_WorkingCommYear: Integer; safecall;
    procedure Set_WorkingCommYear(Value: Integer); safecall;
    function Get_WorkingDecommYear: Integer; safecall;
    procedure Set_WorkingDecommYear(Value: Integer); safecall;
    function Get_WorkingCommMonth: Integer; safecall;
    procedure Set_WorkingCommMonth(Value: Integer); safecall;
    function Get_WorkingDecommMonth: Integer; safecall;
    procedure Set_WorkingDecommMonth(Value: Integer); safecall;
    function Get_RunOffSaltWashOffEfficiencyFactor: Double; safecall;
    procedure Set_RunOffSaltWashOffEfficiencyFactor(Value: Double); safecall;
    function Get_IniSaltStore: Double; safecall;
    procedure Set_IniSaltStore(Value: Double); safecall;
    function Get_ReChargeRate: Double; safecall;
    procedure Set_ReChargeRate(Value: Double); safecall;
    function Get_AbstractToEvap: Double; safecall;
    procedure Set_AbstractToEvap(Value: Double); safecall;
    function Get_AbstractToRiver: Double; safecall;
    procedure Set_AbstractToRiver(Value: Double); safecall;
    function Get_AbstractToCPD: Double; safecall;
    procedure Set_AbstractToCPD(Value: Double); safecall;
    function Get_AbstractMonthTimeSeriesFile: WideString; safecall;
    procedure Set_AbstractMonthTimeSeriesFile(const Value: WideString); safecall;
    function Get_Abstraction: WordBool; safecall;
    procedure Set_Abstraction(Value: WordBool); safecall;
    property PCDIniConcentration: Double read Get_PCDIniConcentration write Set_PCDIniConcentration;
    property WorkingCommYear: Integer read Get_WorkingCommYear write Set_WorkingCommYear;
    property WorkingDecommYear: Integer read Get_WorkingDecommYear write Set_WorkingDecommYear;
    property WorkingCommMonth: Integer read Get_WorkingCommMonth write Set_WorkingCommMonth;
    property WorkingDecommMonth: Integer read Get_WorkingDecommMonth write Set_WorkingDecommMonth;
    property RunOffSaltWashOffEfficiencyFactor: Double read Get_RunOffSaltWashOffEfficiencyFactor write Set_RunOffSaltWashOffEfficiencyFactor;
    property IniSaltStore: Double read Get_IniSaltStore write Set_IniSaltStore;
    property ReChargeRate: Double read Get_ReChargeRate write Set_ReChargeRate;
    property AbstractToEvap: Double read Get_AbstractToEvap write Set_AbstractToEvap;
    property AbstractToRiver: Double read Get_AbstractToRiver write Set_AbstractToRiver;
    property AbstractToCPD: Double read Get_AbstractToCPD write Set_AbstractToCPD;
    property AbstractMonthTimeSeriesFile: WideString read Get_AbstractMonthTimeSeriesFile write Set_AbstractMonthTimeSeriesFile;
    property Abstraction: WordBool read Get_Abstraction write Set_Abstraction;
  end;

// *********************************************************************//
// DispIntf:  IPlanningOpenCastDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {342ED78F-8F33-4323-9DFD-FBCD4312B2B8}
// *********************************************************************//
  IPlanningOpenCastDisp = dispinterface
    ['{342ED78F-8F33-4323-9DFD-FBCD4312B2B8}']
    property PCDIniConcentration: Double dispid 221;
    property WorkingCommYear: Integer dispid 222;
    property WorkingDecommYear: Integer dispid 223;
    property WorkingCommMonth: Integer dispid 224;
    property WorkingDecommMonth: Integer dispid 225;
    property RunOffSaltWashOffEfficiencyFactor: Double dispid 226;
    property IniSaltStore: Double dispid 227;
    property ReChargeRate: Double dispid 228;
    property AbstractToEvap: Double dispid 229;
    property AbstractToRiver: Double dispid 230;
    property AbstractToCPD: Double dispid 231;
    property AbstractMonthTimeSeriesFile: WideString dispid 232;
    property Abstraction: WordBool dispid 233;
    property Identifier: Integer readonly dispid 201;
    property PitName: WideString dispid 202;
    property CoalReserveArea: Double dispid 203;
    property WorkingsArea: Double dispid 204;
    property DisturbedWorkingsArea: Double dispid 205;
    property DisturbedArea: Double dispid 206;
    property WaterSurfaceEvapArea: Double dispid 207;
    property DisturbedAreaRunoff: Double dispid 208;
    property DecantVolume: Double dispid 209;
    property SeepageVolume: Double dispid 210;
    property AnalysisStartVolume: Double dispid 211;
    property MaximumSeepageRate: Double dispid 212;
    property SeepageExponent: Double dispid 213;
    property PCDSurfaceArea: Double dispid 214;
    property PCDStorageCapacity: Double dispid 215;
    property PCDAnalysisStartVolume: Double dispid 216;
    property DisturbedWorkingsAreaRunoff: Double dispid 217;
    property DisturbedRechargeFactor[AIndex: Integer]: Double dispid 218;
    property WorkingAreaRechargeFactor[AIndex: Integer]: Double dispid 219;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 220;
  end;

// *********************************************************************//
// Interface: ILoadGeneration
// Flags:     (320) Dual OleAutomation
// GUID:      {3B8AAE03-D9FD-42D6-9449-9C149FF7E411}
// *********************************************************************//
  ILoadGeneration = interface(IUnknown)
    ['{3B8AAE03-D9FD-42D6-9449-9C149FF7E411}']
    function Get_FlowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_FlowByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_MeanOfSaltByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MeanOfSaltByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_StandardDeviation: Double; safecall;
    procedure Set_StandardDeviation(Value: Double); safecall;
    function Get_type_: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property FlowByIndex[AIndex: Integer]: Double read Get_FlowByIndex write Set_FlowByIndex;
    property MeanOfSaltByIndex[AIndex: Integer]: Double read Get_MeanOfSaltByIndex write Set_MeanOfSaltByIndex;
    property StandardDeviation: Double read Get_StandardDeviation write Set_StandardDeviation;
    property type_: Integer read Get_type_;
  end;

// *********************************************************************//
// DispIntf:  ILoadGenerationDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {3B8AAE03-D9FD-42D6-9449-9C149FF7E411}
// *********************************************************************//
  ILoadGenerationDisp = dispinterface
    ['{3B8AAE03-D9FD-42D6-9449-9C149FF7E411}']
    property FlowByIndex[AIndex: Integer]: Double dispid 101;
    property MeanOfSaltByIndex[AIndex: Integer]: Double dispid 102;
    property StandardDeviation: Double dispid 103;
    property type_: Integer readonly dispid 104;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 105;
  end;

// *********************************************************************//
// Interface: IPlanningSlurryDump
// Flags:     (320) Dual OleAutomation
// GUID:      {21806A78-10B8-459B-91E0-3F301315D060}
// *********************************************************************//
  IPlanningSlurryDump = interface(ISlurryDump)
    ['{21806A78-10B8-459B-91E0-3F301315D060}']
    function Get_SaltConcentration: Double; safecall;
    procedure Set_SaltConcentration(Value: Double); safecall;
    property SaltConcentration: Double read Get_SaltConcentration write Set_SaltConcentration;
  end;

// *********************************************************************//
// DispIntf:  IPlanningSlurryDumpDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {21806A78-10B8-459B-91E0-3F301315D060}
// *********************************************************************//
  IPlanningSlurryDumpDisp = dispinterface
    ['{21806A78-10B8-459B-91E0-3F301315D060}']
    property SaltConcentration: Double dispid 201;
    property Identifier: Integer readonly dispid 101;
    property DumpName: WideString dispid 102;
    property DumpSurfaceArea: Double dispid 103;
    property RunoffFactorToPCD: Double dispid 104;
    property SeepageSplitFactor: Double dispid 105;
    property PCDStorageCapacity: Double dispid 106;
    property PCDSurfaceArea: Double dispid 107;
    property PCDAnalysisStartVolume: Double dispid 108;
    property RechargeFactor[AIndex: Integer]: Double dispid 109;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; dispid 110;
  end;

// *********************************************************************//
// The Class CoVoaimsComObject provides a Create and CreateRemote method to
// create instances of the default interface IVoaimsComObject exposed by
// the CoClass VoaimsComObject. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoVoaimsComObject = class
    class function Create: IVoaimsComObject;
    class function CreateRemote(const MachineName: string): IVoaimsComObject;
  end;

implementation

uses System.Win.ComObj;

class function CoVoaimsComObject.Create: IVoaimsComObject;
begin
  Result := CreateComObject(CLASS_VoaimsComObject) as IVoaimsComObject;
end;

class function CoVoaimsComObject.CreateRemote(const MachineName: string): IVoaimsComObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_VoaimsComObject) as IVoaimsComObject;
end;

end.

