@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS List Report'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@OData.publish: true
define view entity ZAG_CDS_LIST_REPORT 
    with parameters p_bukrs : bukrs
    as select from ekko
    inner join ekpo on ekpo.ebeln = ekko.ebeln
{
    @UI.lineItem: [{ position: 10, importance: #HIGH }]
    @UI.selectionField: [{ position: 10 }]
    key ekko.ebeln as PurchaseOrderID,
    
    @UI.lineItem: [{ position: 20, importance: #HIGH }]
    key ekpo.ebelp as PurchaseItem,
    
    @UI.lineItem: [{ position: 30, importance: #MEDIUM }]
    @UI.selectionField: [{ position: 20 }]
    ekko.bsart as DocType,
    
    @UI.lineItem: [{ position: 40, importance: #MEDIUM }]
    ekko.bukrs as Company,
    
    @UI.lineItem: [{ position: 50, label: 'Amount in Curr.' }]
    @Semantics.amount.currencyCode: 'Currency'
    ekpo.netwr as NetAmount,
    ekko.waers as Currency
} where ekko.bukrs = $parameters.p_bukrs
