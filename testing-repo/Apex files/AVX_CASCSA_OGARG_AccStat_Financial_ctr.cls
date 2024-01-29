/***************************************************************************************************************************
   Developed by: Globant Avanxo                                                                                        
   Author: Ismael Sánchez Maximino                                                                      
   Email: ismael.sanchez@globant.com                                                                                  
   Date of creation: 04-04-2022                                                                                                                                                                                                    
 ***************************************************************************************************************************
   Description: Apex class with all the logic related to the Account Statement Financial Service
                                                                                                                           
 ***************************************************************************************************************************
                                                 Version Control                                                    
 ***************************************************************************************************************************
 No.   Date           Author                    Email                           Description                     
 ***************************************************************************************************************************
 1.0   04-04-2022     Ismael Sanchez Maximino   ismael.sanchez@globant.com      Initial version
 ***************************************************************************************************************************/
public without sharing class AVX_CASCSA_OGARG_AccStat_Financial_ctr {

    /**
     * This method is responsible to:
     * a) Get the valid tokens to consume the service (Azure and Layer7)
     * b) Call the service
     * c) Return the service response
     */
    @AuraEnabled
    public static DocumentDetailWrapper getDocumentDetails(String jsonParams) {
        DocumentDetailWrapper response;
        try {
            AVX_CASCSA_OGARG_AccStat_Filter_ctr.FinancialRequestWrapper requestParams = (AVX_CASCSA_OGARG_AccStat_Filter_ctr.FinancialRequestWrapper)JSON.deserialize(jsonParams, AVX_CASCSA_OGARG_AccStat_Filter_ctr.FinancialRequestWrapper.class);
            AVX_CASCSA_OGARG_AccStat_Filter_ctr.AuthenticationContainer auth = AVX_CASCSA_OGARG_AccStat_Filter_ctr.getAuthenticacionValues(AVX_CASCSA_OGARG_AccStat_Filter_ctr.FINANCIAL_INTEG_CONFIG); 
            HttpRequest req = new HttpRequest();
            Http http = new Http();
            req.setTimeout(120000);
            req.setMethod(auth?.integrationConfig?.Method__c);
            if(auth.integrationConfig.UseERPAuthenticationToken__c){
                req.setEndpoint(auth?.integrationConfig.EndPoint__c + auth?.integrationConfig.Path__c);
            } else {
                req.setEndpoint('callout:' + auth.integrationConfig.JDETokenNamedCredentials__c + auth?.integrationConfig.Path__c);
                if(Test.isRunningTest()) {
                    req.setEndpoint(auth?.integrationConfig.EndPoint__c + auth?.integrationConfig.Path__c);
                }
            }
            req.setBody(JSON.serialize(getRequestParamsWithDynamicAuthDDFinancial(jsonParams, auth) ,true)); 
            req.setHeader('Content-Type', 'application/json');
            req.setHeader('Authorization', 'Bearer ' + auth?.azure?.access_token);
            HTTPResponse resp = http.send(req);
            response = (DocumentDetailWrapper)JSON.deserialize(resp.getBody(), DocumentDetailWrapper.class);
        } catch (Exception e) {
            throw new AuraHandledException(e.getMessage());
        }
        return response;
    }

    /**
     * [Request] Based on the metadata configurations for the service the request params is built
     * Now this is a text to test long documentation:
     * a second line to test long documentation
     * a third line to test long documentation
     * a forth line to test long documentation
     * a fifth line to test long documentation
     * a sixth line to test long documentation
     * a seventh line to test long documentation
     * a eighth line to test long documentation
     * a nineth line to test long documentation
     * a tenth and final line to test long documentation
     */
    public static AVX_CASCSA_OGARG_AccStat_Filter_ctr.FinancialRequestWrapper getRequestParamsWithDynamicAuthDDFinancial(String jsonParam, AVX_CASCSA_OGARG_AccStat_Filter_ctr.AuthenticationContainer auth) {
        AVX_CASCSA_OGARG_AccStat_Filter_ctr.FinancialRequestWrapper requestParams = (AVX_CASCSA_OGARG_AccStat_Filter_ctr.FinancialRequestWrapper)JSON.deserialize(jsonParam, AVX_CASCSA_OGARG_AccStat_Filter_ctr.FinancialRequestWrapper.class);
        if(auth.integrationConfig.UseERPAuthenticationToken__c) {//Use Token
            requestParams.token = auth?.layer7?.userInfo?.token;
        } else {//Use Credentials
            requestParams.environment = auth.layer7.environment;
            requestParams.role = auth.layer7.role;
            requestParams.username = '{!$Credential.Username}'; 
            requestParams.password = '{!$Credential.Password}';
        }
        if(requestParams.Harvest == '--None--') {//ISM-> 8/30/2022
            requestParams.Harvest = '*';
        }
        System.debug('ISM-->ACCOUNT_STATEMENT_FINANCIAL_DETAIL_DOCUMENT [REQUEST]:');
        System.debug(JSON.serializePretty(requestParams, true));
        return requestParams;
    } 

    /**
     * [FIRTS RESPONSE ATTRIBUTE] This class is used to store the Document Details response returned by the 
     * Account Statement Financial service
     */
    public class DocumentDetailWrapper {
        @AuraEnabled public DocumentDetailContainer REQ_E1_CCFINANCIERA_V002_102_20 {set;get;}
        public DocumentDetailWrapper(){
            this.REQ_E1_CCFINANCIERA_V002_102_20 = new DocumentDetailContainer();
        }
    }

    /**
     * Wrapper container class used to store the Account Statement Financial response
     */
    public class DocumentDetailContainer {
        @AuraEnabled public String formId {set;get;}
        @AuraEnabled public String gridId {set;get;}
        @AuraEnabled public String title {set;get;}
        @AuraEnabled public List<DocumentDetailRowset> rowset {set;get;}
        @AuraEnabled public Integer records {set;get;}
        @AuraEnabled public Boolean moreRecords {set;get;}
        public DocumentDetailContainer(){
            this.formId = null;
            this.gridId = null;
            this.title = null;
            this.rowset = new List<DocumentDetailRowset>();
            this.records = null;
            this.moreRecords = null;
        }
    }

    /**
     * Class used to store all the row set values returned by the Account Statement Financial Service
     */
    public class DocumentDetailRowset {
        @AuraEnabled public String tp_doc {set;get;}
        @AuraEnabled public Decimal document_number {set;get;}
        @AuraEnabled public Decimal order_quantity {set;get;}
        @AuraEnabled public String um {set;get;}
        @AuraEnabled public String article {set;get;}
        @AuraEnabled public String description {set;get;}
        @AuraEnabled public Decimal unit_price_ME {set;get;}
        @AuraEnabled public Decimal total_amount_ME {set;get;}
        @AuraEnabled public Decimal unit_price_ML {set;get;}
        @AuraEnabled public Decimal total_amount_ML {set;get;}
        @AuraEnabled public Decimal doc_related_base {set;get;}
        @AuraEnabled public String doc_ty_related_base {set;get;}
        @AuraEnabled public String issuing_code {set;get;}
        @AuraEnabled public String co_doc {set;get;}
        @AuraEnabled public String tax_description {set;get;}
        @AuraEnabled public String item_description {set;get;}
        @AuraEnabled public String related_doc_type_description {set;get;}
        @AuraEnabled public String description_type_document {set;get;}
        @AuraEnabled public String description_type_doc {set;get;}
        @AuraEnabled public String incited_payment_description {set;get;}
        @AuraEnabled public Decimal gross_amount {set;get;}
        @AuraEnabled public Decimal amount_paid {set;get;}
        @AuraEnabled public String incited_payment {set;get;}
        @AuraEnabled public Decimal slip_number {set;get;}
        @AuraEnabled public String related_doc_number {set;get;}
        @AuraEnabled public Decimal bnfro_number {set;get;}
        @AuraEnabled public String observation {set;get;}
        @AuraEnabled public String payee_name {set;get;}
        @AuraEnabled public Decimal quantity {set;get;}
        @AuraEnabled public String document_type {set;get;}
        @AuraEnabled public String related_doc_type {set;get;}
        @AuraEnabled public String tp_slip {set;get;}
        @AuraEnabled public Decimal value_tax {set;get;}
        @AuraEnabled public Decimal total_merchandise_value {set;get;}
        @AuraEnabled public String payment_item {set;get;}
        public DocumentDetailRowset(){
            this.tp_doc = null;
            this.document_number = null;
            this.order_quantity = null;
            this.um = null;
            this.article = null;
            this.description = null;
            this.unit_price_ME = null;
            this.total_amount_ME = null;
            this.unit_price_ML = null;
            this.total_amount_ML = null;
            this.doc_related_base = null;
            this.doc_ty_related_base = null;
            this.issuing_code = null;
            this.co_doc = null;
            this.tax_description = null;
            this.item_description = null;
            this.related_doc_type_description = null;
            this.description_type_document = null;
            this.description_type_doc = null;
            this.incited_payment_description = null;
            this.gross_amount = null;
            this.amount_paid = null;
            this.incited_payment = null;
            this.slip_number = null;
            this.related_doc_number = null;
            this.bnfro_number = null;
            this.observation = null;
            this.payee_name = null;
            this.quantity = null;
            this.document_type = null;
            this.related_doc_type = null;
            this.tp_slip = null;
            this.value_tax = null;
            this.total_merchandise_value = null;
            this.payment_item = null;
        }
    }
}