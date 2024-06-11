# OData V4 Code-Based Implementation

## 1. What do you need to implement?

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

---

## 2. How to configure the service?

1. Run TCode **/iwbep/v4_admin**
2. Register a service group **ZAG_SG_VENDOR** 
3. Register a service **ZAG_SRV_VENDOR** providing the just created model and provider classes
4. Assign the service to the service group **ZAG_SG_VENDOR**
5. Publish the service group using transaction **/iwfnd/v4_admin** – (Gateway System)

---

## 3. How to run the service?

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

