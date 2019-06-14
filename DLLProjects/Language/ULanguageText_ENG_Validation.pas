{******************************************************************************}
{*  UNIT      : Contains language text for context validation error messages. *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/01/19                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit ULanguageText_ENG_Validation;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  AAdd('ContextValidation','InsufficientParameterObjects','Insufficient objects for business rule validation.');
  AAdd('ContextValidation','InvalidNullValue','Invalid NULL value.');

  // Run Configuration
  AAdd('ContextValidation','DebugPeriodStartAndEnd','Debug end date must be later than debug start date.');
  AAdd('ContextValidation','DebugPeriodOutsideAnalysis','Debug start/end date must fall inside period of analysis.');

  // Reservoirs
  AAdd('ContextValidation','DuplicateReservoirName','Reservoir name already exists.');
  AAdd('ContextValidation','SumAfforestationScale','The sum of the afforestation scaling factors should USUALLY be 100%');
  AAdd('ContextValidation','SumDrainageScale','Sum of drainage scaling factors should USUALLY be 100%');
  AAdd('ContextValidation','SumIrrigationScale','Sum of irrigation scaling factors should USUALLY be 100%');
  AAdd('ContextValidation','ReservoirCountMaximum','The total number of reservoirs should not exceed 70.');
  AAdd('ContextValidation','ReservoirPenaltyCountMaximum','The total number of penalty values in a penalty structure should not exceed 5');
  AAdd('ContextValidation','CatchmentRef','The assigned catchment reference number does not exist in the Param file.');
  AAdd('ContextValidation','AreaWhenFull','Reservoir %s: Area when full value must be within the Reservoir Surface Area ');
  AAdd('ContextValidation','StartingStorage','The Starting Storage level must be within the reservoir elevation range specified');
  AAdd('ContextValidation','FullStorageLevel','The Full Supply Level must be within the reservoir elevation range and must be greater than the Dead Storage Level');
  AAdd('ContextValidation','DeadStorageLevel','The Dead Storage level must be within the reservoir elevation range and must be greater than the value for Bottom of Reservoir');
  AAdd('ContextValidation','BottomOfResevoir','The level for Bottom of Resevoir must be within the reservoir elevation range');
  AAdd('ContextValidation','ElevationRangesNotInDescendingOrder','Reservoir %s: Surface elevation range must be in descending order');
  AAdd('ContextValidation','MonthlyZoneElevationOrder','Reservoir %s: Zone elevation ranges must be in descending order');
  AAdd('ContextValidation','MonthlyZoneElevation','Monthly Reservoir Zone Elevation must be between Full Supply Level and Dead Storage Level');
  AAdd('ContextValidation','VolumeRangesNotInDescendingOrder','Reservoir %s: The Volume range must be in descending order');
  AAdd('ContextValidation','SurfaceAreaRangesNotInDescendingOrder','Reservoir %s: Surface Area range must be in descending order');
  AAdd('ContextValidation','InvalidReservoirPenalty','Reservoir %s: Invalid Reservoir Penalty Structure Number.');
  AAdd('ContextValidation','EvaporationUnassigned','Reservoir %s: Monthly evaporation data may not be unassigned.');

  AAdd('ContextValidation','MineDrainageScale','The sum of the drainage scaling factors should  be 0');
  AAdd('ContextValidation','MineAfforestationScale','Sum of afforestation scaling factors should  be 0');
  AAdd('ContextValidation','MineIrrigationScale','Sum of irrigation scaling factors should  be 0');


  // General Flow Channel
  AAdd('ContextValidation','DuplicateChannelName','WARNING: Channel %s: Name already exists.');
  AAdd('ContextValidation','InvalidChannelPenaltyNumber','Channel %s: Invalid penalty structure number.');
  AAdd('ContextValidation','InvalidChannelPenaltyArcCount','Channel %s: Penalty structure has invalid number of arcs.');
  AAdd('ContextValidation','InvalidUpstreamNodeNumber','Channel %s: Invalid upstream node number.');
  AAdd('ContextValidation','InvalidDownstreamNodeNumber','Channel %s: Invalid downstream node number.');
  AAdd('ContextValidation','ChannelStartAndStopAtSameNode','Channel %s: is not allowed to start and end on the same node.');
  AAdd('ContextValidation','InvalidPenaltyArcCount','Penalty %s: The number of arcs in this penalty structure is invalid for the following channels %s');
  AAdd('ContextValidation','InvalidPenaltyValue','Penalty %s: Arc values are invalid for (IFR feature) channels');
  AAdd('ContextValidation','TooManyZeroUpstreamNodes','Maximum number of channels with Zero upstream node exceeded.');
  AAdd('ContextValidation','TooManyZeroDownstreamNodes','Maximum number of channels with Zero downstream node exceeded.');
  AAdd('ContextValidation','MasterControlUpstreamNodeZero','%s: Master control cannot have a zero upstream node.');
  AAdd('ContextValidation','DanglingUpstreamNode','Channel %s: Dangling upstream node (%d) - no channel flowing into upstream node.');
  AAdd('ContextValidation','DanglingDownstreamNode','Channel %s: Dangling downstream node (%d) -  no channel flowing out of downstream node.');
  AAdd('ContextValidation','MasterControlNoTariffCalculation','%s: Master control does not have required economic analysis parameters in the TAR-file.');

  // Diversion Feature
  AAdd('ContextValidation','DuplicateDiversionFeatureName','WARNING: Diversion on channel %s: Name already exists.');
  AAdd('ContextValidation','FlowRangesNotInAscendingOrder','Diversion on channel %s: Flow ranges should be in ascending order.');
  AAdd('ContextValidation','TooFewFlowRangesAndDivertedFlows','Diversion on channel %s: At least 2 Flow Range and Diverted Flow values must be entered.');
  AAdd('ContextValidation','DiversionFeatureNotAssignedToChannel','WARNING: Diversion %s: Not assigned to channel.');
  AAdd('ContextValidation','ReservoirLevelsNotInPhysicalRange','Diversion on channel %s: Water storage levels should be in physical range of controlling reservoir.');
  AAdd('ContextValidation','DivertedFlowGreaterThanFlowRanges','Diversion on channel %s: Flow ranges should be greater or equal to the actual diverted flow.');
  AAdd('ContextValidation','ReferenceFlowsNotInAscendingOrder','Diversion on channel %s: Reference flows should be in ascending order.');
  AAdd('ContextValidation','ResLevelsNotInAscendingOrder','Diversion on channel %s: Water storage levels should be in ascending order.');


  // Minimum Flow Constraints Feature
   AAdd('ContextValidation','DuplicateMinFlowFeatureName','WARNING: Minimum Flow Constraints on channel %s: Name already exists.');
   AAdd('ContextValidation','ChannelPenaltyArcCount','Number of Flow Constraints not equal to penalty Arc count');

  // Maximum Flow Constraints Feature
   AAdd('ContextValidation','DuplicateMinMaxFeatureName','WARNING: Min-Max Constraints on  channel %s: Name already exists.');
   AAdd('ContextValidation','FlowConstraintValuesNotDescending','Flow constraint values must be in descending order.');

  // Loss Channel Feature
   AAdd('ContextValidation','DuplicateLossChannelName','WARNING: Loss Feature on channel %s: Name already exists.');
   AAdd('ContextValidation','InvalidReferenceNodeNumber','Reference Node Number  [%s] is invalid.');

   // Specified Demand Feature
   AAdd('ContextValidation','DuplicateSpecifiedDemandName','WARNING: Specified Demand Feature on channel %s: Name already exists.');
   AAdd('ContextValidation','InvalidStochasticIndicator','Stochastic Indicator [%s] not in list of Acceptable Values: [%s]');
   AAdd('ContextValidation','InvalidCatchmentRefNumber','Catchment ref number  [%s] is invalid.');
   AAdd('ContextValidation','FullNamelength','Full Name path should not exceed [%s] characters.');
   AAdd('ContextValidation','InvalidFileName','Invalid FileName Path.');
   AAdd('ContextValidation','FileEmpty','File Name cannot be empty');

   // Specified Inflow Feature
   AAdd('ContextValidation','DuplicateSpecifiedInflowFeatureName','WARNING: Specified Inflow Feature on channel %s: Name already exists.');

  // Pumping Channel Feature
   AAdd('ContextValidation','DuplicatePumpingFeatureName','WARNING: Pumping Feature on channel %s: Name already exists.');

  // Physical Flow Constraint
  AAdd('ContextValidation','InvalidUpstreamReservoir','Physical Flow Constraint on channel %s: Invalid upstream reservoir.');
  AAdd('ContextValidation','InvalidDownstreamReservoir','Physical Flow Constraint on channel %s: Invalid downstream reservoir.');
  AAdd('ContextValidation','StructureTypesNotSupported','Physical Flow Constraint on channel %s: Structure type 1, 7 and 12 are not supported.');
  AAdd('ContextValidation','NotEnoughArrayValues','Physical Flow Constraint on channel %s: At least 1 pair of values must be entered in arrays.');
  AAdd('ContextValidation','UnequalNrOfArrayValues','Physical Flow Constraint on channel %s: Equal number of values must be entered in arrays.');
  AAdd('ContextValidation','PhysicalFlowConstraintNotAssignedToChannel','WARNING: Physical Flow Constraint %s: Not assigned to channel.');
  AAdd('ContextValidation','HOSLNotGreaterThanSILL','The Maximum Gate Height must be greater than the Sill Elevation.');
  AAdd('ContextValidation','SILLOutOfRange','The SILLL must be in the upstream reservoir elevation range FSL to DSL.');    

  // IFR Features
  AAdd('ContextValidation','DuplicateIFRFeatureName','WARNING: IFR Feature on channel %s: Name already exists.');
  AAdd('ContextValidation','IFRFeaturePenaltyRelaxArc','IFR feature on channel %s: Relax arc penalty must be higher that forward arc penalty.');
  AAdd('ContextValidation','IFRValuesNotInAscendingOrder','IFR feature on channel %s: Inflow values must be in ascending order..');
  AAdd('ContextValidation','IFRReleaseNotInAscendingOrder','IFR feature on channel %s: IFR values must be in ascending order..');
  AAdd('ContextValidation','IFRReferenceNodeNotSpecified','IFR feature on channel %s: No reference node numbers have been specified.');
  AAdd('ContextValidation','InvalidIFRReferenceNodeNumber','IFR feature on channel %s: Invalid reference node number.');
  AAdd('ContextValidation','IFRFeatureNotAssignedToChannel','WARNING: IFR Feature %s: Not assigned to channel.');
  AAdd('ContextValidation','IFRNotInAscendingOrder','IFR associated flow values must be in ascending order..');
  AAdd('ContextValidation','IFRAnnualInFlowNotIndiscendingOrder','IFR feature on channel %s: Annual Inflow values must be in discending order..');
  AAdd('ContextValidation','IFRAnnualReleaseNotIndiscendingOrder','IFR feature on channel %s: Annual Inflow values must be in discending order..');
  AAdd('ContextValidation','IFRAnnualCountErr','The maximum number of annual IFR structures has been exceeded');
  AAdd('ContextValidation','IFRMonthlyCountErr','The maximum number of monthly IFR structures has been exceeded');

  // Irrigation Area
  AAdd('ContextValidation','DuplicateIrrigationAreaName','WARNING: Irrigation area %s: Name already exists.');
  AAdd('ContextValidation','DivertedGreaterThanReturnFlows','Irrigation area %s: The monthly diversion flow must be greater than or equal to the monthly return flow.');

  // Power Plant
  AAdd('ContextValidation','DuplicatePowerPlantName','WARNING: Power plant %s: Name already exists.');
  AAdd('ContextValidation','InvalidDownstreamPowerChannelNumber','Power plant %s: Invalid downstream power channel number.');

  // Master Control Feature
  AAdd('ContextValidation','InsufficientLoadCases','At least one load case must be specified.');
  AAdd('ContextValidation','DuplicateMasterControlFeatureName','WARNING: Master control feature %s: Name already exists.');
  AAdd('ContextValidation','OneMasterControlPerType','A master control feature of this type already exists.');
  AAdd('ContextValidation','InvalidMasterControlType','Invalid master control type for model.');

  // Stream Flow Reduction
  AAdd('ContextValidation','DuplicateSFRName','Stream flow reduction area name already exists..');

  //Planning : Allocation Definition.....
  AAdd('ContextValidation','DuplicateAllocDefName','WARNING: Allocation Definition %s: Name already exists.');
  AAdd('ContextValidation','DuplicateAllocDefFileName','WARNING: Allocation Definition FileName %s: already exists.');
  AAdd('ContextValidation','InvalidStartMonth','WARNING: %s : is not a valid start month.');
  AAdd('ContextValidation','InvalidEndMonth','WARNING: %s : is not a valid end month.');
  AAdd('ContextValidation','DateExceptionError','ERROR: Start Date %s : is bigger than End Date %s.');
  AAdd('ContextValidation','InvalidDate','WARNING: invalid date');
  AAdd('ContextValidation','InvalidDecisionCurveSet','WARNING: Decision curve set is greater than Number of curve sets permited');
  AAdd('ContextValidation','InvalidStartEndDate','End date may not be earlier than start date' );
  AAdd('ContextValidation','UnspecifiedSubSystem','Unspecified Sub-system.');
  AAdd('ContextValidation','SubSystemNotExist','Sub-system %s does not exist.');
  AAdd('ContextValidation','UnspecifiedUserCategory','Unspecified User Category.');
  AAdd('ContextValidation','UserCategoryNotExist','User category (%S) does not exist.');
  AAdd('ContextValidation','UnspecifiedChannel','Unspecified Channel.');
  AAdd('ContextValidation','ChannelNotExist','Channel (%s) does not exist.');
  AAdd('ContextValidation','DuplicateChannel','Duplicate Channel (%s).');
  AAdd('ContextValidation','AllocationDefinition','Allocation definition %s');
  AAdd('ContextValidation','AllocationDefinitionFileName','Allocation definition filename %s');
  AAdd('ContextValidation','DuplicateDemandDefinitionName','Duplicate Demand Definition Name (%s)');
  AAdd('ContextValidation','DemandCentreDoesNotExist','Demand Centre (%s) does not exist');
  AAdd('ContextValidation','PositionNumberError','Position number must be less than total number of sub-sytems (%s).');
  AAdd('ContextValidation','UrbanRunOff','Sum of UrbanRunOff should USUALLY be 100%');
  AAdd('ContextValidation','NumberOfYears','Growth Factor Data will be lost. Are you sure you want to set number of years to %d?');
  AAdd('ContextValidation','TotalCatchmentAreaError','The Catchment area (%s) shall not be less then the sum of the irrigation block areas and the afforestation areas (%s) associated with that catchment through reference nodes.');

end;

end.
