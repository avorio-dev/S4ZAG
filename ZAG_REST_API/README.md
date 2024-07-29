# REST API

---

## Consumer
Implement this solution if you need to call an external service from SAP

  - Implementation: [here](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_REST_API/Consumer/ZAG_CL_REST_CONSUMER.abap)
  - Examples: [here](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_REST_API/Consumer/ZAG_CL_REST_CONSUMER.md)

--- 

## Provider
Implement this solution if you need to provide a service which will be called from an external System.

### 1. Provider class
  The first step will be to implement the Provider, in other words, the class which will execute your core logic ( Extraction, Bapi, ecc. ).

  Here the [Implementation](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_REST_API/Provider/ZAG_CL_REST_PROVIDER.abap).

  >Basically, this class will perform the CRUD Operation

  >GET/POST already implemented in the implementation example. You will need to adapt this method.


### 2. Http Handler class
  Now, you will need to implement the Handler, in other words, the class which will "Listen" the call from browser.

  Here the [Implementation](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_REST_API/Provider/ZAG_CL_REST_HANDLER.abap).

  >You will need to adatp the method 
  > - get_root_handler: to set up the call to your provider class
  > - handle_csrf_token: set up authentication if needed
  
### 3. Set SICF Service
  The final step is to register your service using tcode **SICF**, setting your Handler Class.