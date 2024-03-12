@AbapCatalog.sqlViewName: 'ZAGCDS05'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View EKKO with TF Complex'
define view ZAG_CDS_05 
    with parameters
        p_bukrs : bukrs
    as select from ZAG_TF_02( p_clnt: $session.client, p_bukrs: $parameters.p_bukrs )
{
  key ebeln,
  bukrs,
  case bstyp
    when 'A' then 'Rich. Off.'
    when 'F' then 'Ordine d acquisto'
    when 'K' then 'Contratto'
    when 'L' then 'Piano di consegna'
    when 'R' then 'Richiesta di offerta'
    when 'O' then 'Offerta'
    else 'Unknown'
  end as title
}
