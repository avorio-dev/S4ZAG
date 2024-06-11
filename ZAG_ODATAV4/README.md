# OData V4 Code-Based Implementation

## What's the difference between v4 and v2?

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

## What do you need to implement?

-  **CDS Databse**
	- [ZAG_CDS_LFA1](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_ODATAV4/ZAG_CDS_LFA1.abap)
	- [ZAG_CDS_LFB1](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_ODATAV4/ZAG_CDS_LFB1.abap)
	- [ZAG_CDS_LFM1](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_ODATAV4/ZAG_CDS_LFM1.abap)
- **Interface**
	- [ZAG_IF_ODATAV4_VENDOR](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_ODATAV4/zag_if_odatav4_vendor.abap)
- **Data Model**
	- [ZAG_CL_ODATAV4_VENDOR_MODEL](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_ODATAV4/zag_cl_odatav4_vendor_model.abap)

- **Data Provider**
	- [ZAG_CL_ODATAV4_VENDOR_DATA](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_ODATAV4/zag_cl_odatav4_vendor_data.abap)


## How to configure the service?

1. Run TCode **/iwbep/v4_admin**
2. Register a service group **ZAG_SG_VENDOR** 
3. Register a service **ZAG_SRV_VENDOR** providing the just created model and provider classes
4. Assign the service to the service group **ZAG_SG_VENDOR**
5. Publish the service group using transaction **/iwfnd/v4_admin** – (Gateway System)


## How to run the service?

>Each link listed below will need to be concatenated to your **<system_host>** if you are testing with Browser/Postman.
>If you're using GW_CLIENT instead, it will be enough copy/paste it
	
- how to get the **metadata**:
	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/$metadata?sap-statistics=true

- how to get the **Vendors List**/**Companies List**/**Purch.Org List**
	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor
 
 	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Company

   	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Purchorg

- how to get a **Single Vendor**
  	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor('1')

- how to get a **Single Vendor** with all company/purchorg related infor
  	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor('1')?$expand=_Company,_Purchorg









   
