# Makefile for \preparation 
# requires GNU Make

IN = ../../gen/preparation/input
TEMP = ../../gen/preparation/temp
OUT = ../../gen/preparation/output

REGION ?= US

### BUILD COMMANDS ###

# complete build

all: $(TEMP)/variables.txt $(TEMP)/OriginalData.csv


### DATA PREPARATION ###

$(TEMP)/rawDescriptives_$(REGION).html: rawDescriptives.Rmd
	Rscript -e "rmarkdown::render('rawDescriptives.Rmd', output_format = 'html_document')"
	
# Infer Subscriptions
$(TEMP)/panelSubscription_$(REGION).csv: panelSubscription.R
	RScript panelSubscription.R $(REGION)

$(TEMP)/subscriptionDescriptives_$(REGION).html: subscriptionDescriptives.Rmd
	Rscript -e "rmarkdown::render('subscriptionDescriptives.Rmd', output_format = 'html_document')"

# Operationalize Subscription Plan
$(TEMP)/panelSubscriptionPlan_$(REGION).csv: panelSubscriptionPlan.R
	RScript panelSubscriptionPlan.R $(REGION)

$(TEMP)/subscriptionPlanDescriptives_$(REGION).html: subscriptionPlanDescriptives.Rmd
	Rscript -e "rmarkdown::render('subscriptionPlanDescriptives.Rmd', output_format = 'html_document')"

# Measure Usage
$(TEMP)/panelUsage_$(REGION).csv: panelUsage.R
	RScript panelUsage.R $(REGION)

$(TEMP)/usageDescriptives_$(REGION).html: usageDescriptives.Rmd
	Rscript -e "rmarkdown::render('usageDescriptives.Rmd', output_format = 'html_document')"
	
# Get Additional Variables from Panel
$(TEMP)/panelVariable_$(REGION).csv: panelVariable.R $(TEMP)/sub.txt
	RScript panelVariable.R $(REGION)
	
# Form Content Data
$(TEMP)/OriginalData.csv: contentsCharacteristics.R
	R --vanilla < contentCharacteristics.R
	
# ...


	










