# xml-conduit-decode

Support for historical cursors & decoding on top of xml-conduit. Created in the sprit of scalaz-xml so that we can get useful context out of xml decoding failures.

Not releasing this to hackage yet as I'm not sure on the namespace and it needs some tidying, documentation and testing before it is fit for hackage. :)

We're using this in production to interact with a monstrous vendor SOAP API, so it should work for you if you're desperate for something like this (like I was faced with the job of taming such a SOAPY beast).

Expect changes to the API over the course of the next few months as it is used in another iseek project and integrating with an even scarier SOAP API. 

For now, see the tests for an example of what the heck this does. 
