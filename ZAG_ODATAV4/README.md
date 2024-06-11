# OData V4 Code-Based Implementation

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
- how to get the **metadata**:
###
	><your_system_host>/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/$metadata?sap-statistics=true
- how to get the **Vendors List**/**Companies List**/**Purch.Org List**
###
	><your_system_host>/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor
###
 	><your_system_host>/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Company
###
  	><your_system_host>/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Purchorg
