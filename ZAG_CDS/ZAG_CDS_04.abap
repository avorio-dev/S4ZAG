@AbapCatalog.sqlViewName: 'ZAGCDS04'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View EKKO with TF'
define view ZAG_CDS_04
    with parameters p_bukrs : bukrs
as select from ZAG_TF_01( p_clnt: $session.client, p_bukrs: $parameters.p_bukrs )
{
    *
}
