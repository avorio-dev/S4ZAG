@AbapCatalog.sqlViewName: 'ZAGCDS02'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View EKKO with Params'
define view ZAG_CDS_02 
    with parameters
        p_ebeln : ebeln,
        p_bukrs : bukrs
    as select from ekko
{
    key ebeln,
    bukrs,
    bstyp,
    bsart,
    loekz,
    aedat,
    ernam
} where mandt = $session.client
    and ebeln = $parameters.p_ebeln
    and bukrs = $parameters.p_bukrs
