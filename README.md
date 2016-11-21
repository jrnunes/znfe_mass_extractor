ZNFE_MASS_EXTRACTOR
===================

### Mass Extractor for SAP NF-e - XML &amp; PDF format

PoC of Mass Extraction tool for DANFE's PDF & XML using SAP NFe standard objects.

The tool do mass extraction of XML & DANFE's PDF files for both Inbound & Outbound NF-e documents. It is intended to run on SAP NF-e system and was developed on SLL-NFE SP22 box - not sure if it is compactible with newer or older versions.

Some important things are still missing, like authority-checks and performance optimization, as well as some design tweaks. I'll add it in the near future.


### How to use it

You have a list of access key's that you want to generate the DANFE's PDF or the XML. Save it as a text file and put the file path into the first parameter.
In case you want to extract information from a remote SAP NF-e system, inform the RFC destination.
Inform if the documents are Inbound or Outbound and if the output should be in PDF or XML format.
