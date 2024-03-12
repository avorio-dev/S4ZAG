@AbapCatalog.sqlViewName: 'ZAGCDS01'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View EKKO'
define view ZAG_CDS_01 as select from ekko
{
    @EndUserText.label: 'Purchasing Doc.'
    key ebeln,
    @EndUserText.label: 'Company'
    bukrs,
    bstyp,
    bsart,
    loekz,
    aedat,
    ernam
} where mandt = $session.client
