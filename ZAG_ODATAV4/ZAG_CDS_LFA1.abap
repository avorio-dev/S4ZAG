@AbapCatalog.sqlViewName: 'ZAGCDSLFA1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Vendor'
define view ZAG_CDS_LFA1
  as select from lfa1 as _Vendor
  association [1..*] to ZAG_CDS_LFB1 as _Company  on _Company.lifnr  = $projection.lifnr
  association [1..*] to ZAG_CDS_LFM1 as _PurchOrg on _PurchOrg.lifnr = $projection.lifnr
{
  key lifnr,
      name1,
      name2,
      name3,
      name4,
      land1,
      ort01,
      pstlz,
      regio,
      stras,
      adrnr,
      loevm,
      sperm,
      stcd1,
      stcd2,
      stceg,

      //Associations
      _Company,
      _PurchOrg
}
