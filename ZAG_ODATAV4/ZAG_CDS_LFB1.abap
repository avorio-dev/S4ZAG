@AbapCatalog.sqlViewName: 'ZAGCDSLFB1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Vendor Company'
define view ZAG_CDS_LFB1
  as select from lfb1
{
  key lifnr,
  key bukrs,
      sperr,
      loevm,
      akont,
      zterm,
      hbkid
}
