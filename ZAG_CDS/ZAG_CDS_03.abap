@AbapCatalog.sqlViewName: 'ZAGCDS03'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View EKKO with Params'
define view ZAG_CDS_03 
    as select from ekko
    inner join ekpo on ekpo.ebeln = ekko.ebeln
{
    @EndUserText.label: 'Purchasing Doc.'
    @Consumption.filter.mandatory: false
    @Consumption.filter.selectionType: #RANGE
    key ekko.ebeln,
    key ekpo.ebelp,
    @EndUserText.label: 'Material'
    ekpo.matnr,
    ekpo.meins,
    ekpo.menge,
    ekpo.netwr,
    @EndUserText.label: 'Company'
    ekko.bukrs,
    ekko.bstyp,
    ekko.bsart,
    ekko.loekz,
    ekko.aedat,
    ekko.ernam
} where ekko.mandt = $session.client
