# OData V4 Theory

## 1. What's the difference between v4 and v2?

The list of new & improved features in v4 is [extensive](http://docs.oasis-open.org/odata/new-in-odata/v4.0/new-in-odata-v4.0.html) and I will highlight just a few here:  

-   Better performance through reduced payload size (both for metadata and response data)  
-   Improved data types, e.g. separate date and time types rather than combined datetime only
-   You can filter and sort on expanded properties. For example, we can query order headers expanded to order items, with a filter for only order item quantity > 100. In v2 we could only filter by the main entity (in this case the order header)
-   The query syntax is much more logical with multiple expands. This is because the parameters are nested
  
Lets consider the last point in more detail. With v2 we would query like this  
> ..../Continents?$expand=Countries/Cities

With v4 we can query  
> ..../Continents?$expand=Countries($expand=Cities)

It's clear that with this syntax we can expand to any number of levels. We can also apply parameters like $select or $filter at any level we choose, e.g.  
> ..../Continents?$expand=Countries($expand=Cities($expand=Suburbs;$select=Name,Population),CapitalCity;$select=Name,Population)&$select=Name

What are we doing here? We select the _Continents_ and expand to _Countries_. From _Countries_ we expand to both _Cities_ and _CapitalCity_. From _Cities_ we expand again to _Suburbs_. For field selection we specify _Name_ only for _Continent_ and _Name_ & _Population_ for _Countries_ and for _Cities_.

## OData V4 Framework 
- /IWBEP/IF_V4_DP_BASIC 
	- Methods provide basic functionality (Create, Update, Delete, Navigation, …)
	- When being implemented à Working OData service supporting most requests

- /IWBEP/IF_V4_DP_INTERMEDIATE
	- Medium complex functionality
	- eTag handling, PATCH, $expand
	- Contains generic calls to other (especially the basic) interfaces
  
- /IWBEP/IF_V4_DP_ADVANCED
	- Always called first by the framework
	- Contains generic calls to the other (especially the basic) interfaces
	- Will for example be overwritten by the new RESTful ABAP Programming model (planned)
  
- /IWBEP/IF_V4_DP_BATCH
	- $batch. Generic $batch and changeset
  
- /IWBEP/IF_V4_DP_PROCESS_STEPS
	- Transaction and lifecycle handling
 


### io_request and io_response
All interface methods have an import parameter called **io_request**.
It can be used to retrieve all information you need to handle the request in your service implemenation.
A UPDATE_ENTITY method for example will have the following methods
1. GET_BUSI_DATA to retrieve entity data from the request, for example the payload of the incoming request.
2. GET_ENTITY_SET to retrieve the entity set of the processed entity. So we can switch to entity set specific methods

The corresponding parameter **ip_response** is used to return business data to the SAP Gateway framework 
and to tell the framework which processing steps the service implementation has handled iself (see todo and done flags below).

### ToDo and Done-Flags

The SAP Gateway V4 framework has introduced so called ToDo-Flags which provide a hint for the application developer what his implemenations has to do. 
Depending ont the query options that have been used in the request you will get simple list with boolean values for the following flags:
deltatoken, select, filter, skip, orderby, skiptoken, search, top, ...

Done-Flags confirm that the response fits to the request. They allow the application developer to inform the framework to handle feature generically e.g., $top, $skip, and $select. 
Using such flags also allows an implementation tobe compatible in the future. Instead of a wrong result an exception will be raised if a done flag is not set.







   
