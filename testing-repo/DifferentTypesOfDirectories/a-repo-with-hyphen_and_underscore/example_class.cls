/**
 * @description Class doc 😂 汉字`
 *
 * @group a tag
 */
global class ExampleClass extends Exception {

    transient integer currentValue;
    

    public String hello(String param1) {
        // inline comment with multibyte chars 😂 汉字.
        insert l;
        update myAcct;
        upsert accList;
        try {
            merge masterAcct mergeAcct;
        } catch (DmlException e) {
            // Process exception here
        }
        try {
            undelete savedAccts;
        } catch (DmlException e) {
            // Process exception here
        }
        return 'Hello World!';
    }

    /**
     * Class doc 😂 汉字
     * @description description
     * @example
     * ExampleClass.hello()
     **/
    public void docstring(String param2) {
        return 1;
    }

    trigger myAccountTrigger on Account(before insert, before update) {
        if (Trigger.isBefore) {
            for (Account a : Trigger.old) {
                if (a.name != 'okToDelete') {
                    a.addError('You can\'t delete this record!');
                }
            }
        }
    }

    static testmethod testFoo() {
        return 'Test';
    }

    private class InnerClass{
        override void innerMethod() {
            return;
        }
    }
    global class MyWebService {
        webService static Id makeContact(String lastName, Account a) {
            Contact c = new Contact(lastName = 'Weissman', AccountId = a.Id);
            insert c; 
            return c.id;
        }
    }
    public with sharing class ExampleSharingClass() {
        public String sharingMethod(String param1) {
            return 'Hello World!';
        }
    }

    public interface InterfaceExample {
        public void doWork();
    }

    public enum EnumExample {
        X;
        public int enumMethod() {
            return 1;
        }
    }

    public interface InterfaceExample {

        default int defaultMethod() {
            return 1;
        }
    }
    
}
