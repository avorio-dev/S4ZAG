# [OData V4 Code-Based Implementation](https://community.sap.com/t5/technology-blogs-by-members/simple-odata-v4-service-using-code-based-implementation/ba-p/13519406)

## 1. [System prerequisites](#system_prerequisites)
## 2. [What will be implemented](#implemented_objects)
## 3. [How to configure the service?](#service_config)
## 4. [How to run the service](#service_run)

---

## 1. System Prerequisites <a name="system_prerequisites"/>

## 2. What do you need to implement?<a name="implemented_objects"/>

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

> If you are using deep structures and you are having problems like
> "sap scan profile virus /IWBEP/V4/ODATA_UPLOAD inactive"
> You can:
> - Config the profile /IWBEP/V4/ODATA_UPLOAD in tcode **VSCANPROFILE** 
> - Disable in Tcode **/IWFND/VIRUS_SCAN** virus scan profile for V4

---

## 3. How to configure the service?<a name="service_config"/>

1. Run TCode **/iwbep/v4_admin**
2. Register a service group **ZAG_SG_VENDOR** 
3. Register a service **ZAG_SRV_VENDOR** providing the just created model and provider classes
4. Assign the service to the service group **ZAG_SG_VENDOR**
5. Publish the service group using transaction **/iwfnd/v4_admin** – (Gateway System)

---

## 4. How to run the service?<a name="service_run"/>

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

- how to get a **Single Vendor** with all company/purchorg related info
  	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor('1')?$expand=_Company,_Purchorg

- how to get the **first 10** records
  	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor?$top=5

- how to get the **first 1/2 records** of respect. company/purchorg of a specific vendor
  	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor('100047')?$expand=_Company($top=1),_Purchorg($top=2)
   
- how to get the **data filtered** both main entity and expanded entities
  	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor?$expand=_Company($filter=Bukrs eq 'FCH1'),_Purchorg($filter=Ekorg eq 'BA00')&$filter=Ort01 eq 'Bari'

- how to get **specific fields**
	>		/sap/opu/odata4/sap/zag_sg_vendor/default/sap/zag_srv_vendor/0001/Vendor('1')?$select=Name1,Adrnr&$expand=_Company($select=Loevm,Akont)

