@AbapCatalog.sqlViewName: 'ZAGCDSLISTREPORT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS List Report'
@OData.publish:true
@UI.headerInfo.typeNamePlural: 'Sales Orders'
define view ZAG_CDS_LIST_REPORT 
    as select from ekko
    inner join ekpo on ekpo.ebeln = ekko.ebeln
{
    @UI.lineItem: [{ position: 10 }]
    @UI.lineItem.importance: #HIGH
    @UI.selectionField: [{ position: 10 }]
    key ekko.ebeln as PurchaseOrderID,
    
    @UI.lineItem: [{ position: 20 }]
    @UI.lineItem.importance: #HIGH
    key ekpo.ebelp as PurchaseItem,
    
    @UI.lineItem: [{ position: 30 }]
    @UI.lineItem.importance: #MEDIUM
    ekko.bsart as DocType,
    
    @UI.lineItem: [{ position: 40 }]
    @UI.lineItem.importance: #MEDIUM
    @UI.selectionField: [{ position: 20 }]
    ekko.bukrs as Company,
    
    @UI.lineItem: [{ position: 50 }, { label: 'Amount in Curr.'}]
    @Semantics.amount.currencyCode: 'Currency'
    ekpo.netwr as NetAmount,
    
    @Semantics.currencyCode: true
    ekko.waers as Currency
}

// Use @UI.lineItem: [{ position: 10 }] // It determines position in ALV, it's mandatory

// @UI.lineItem.importance: #HIGH // It determines in which clients the field should display
    // #HIGH // Default value, display in all clients
    // #MEDIUM // Only display in desktop browser or tablet
    // #LOW // Only display in desktop browser

// @Semantics.amount.currencyCode: 'Currency' // Set the name of currency field for display with amount 
// @Semantics.currencyCode: true // Set the current field as Currency field type

// @UI.hidden // Hide fields

// @UI.selectionField: [{ position: 10 }] // Set Field as selection field
