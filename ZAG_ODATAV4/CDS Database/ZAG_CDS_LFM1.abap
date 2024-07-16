@AbapCatalog.sqlViewName: 'ZAGCDSLFM1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Vendor Purch. Org.'
define view ZAG_CDS_LFM1
  as select from lfm1
{
  key lifnr,
  key ekorg,
      sperm,
      loevm,
      waers,
      zterm,
      inco1,
      inco2,
      ekgrp
}
