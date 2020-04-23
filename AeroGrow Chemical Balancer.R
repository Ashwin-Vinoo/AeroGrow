#---------------------------Hydroponics Chemical Analysis----------------------------

#Removes all data from the workspace
rm(list=ls(all=TRUE))

#---------------------------Hyper Parameters---------------------------
# Controls whether to refresh the element percentage contribution data in the first sheet of the excel
Chemical_element_composition_calculate = FALSE

# Controls how many iterations of chemical balancing is done before exiting
chemical_balance_iterations = 10000;

# Determines  the maximum element concentration offset that is accepted
element_percentage_Offset = 0.005

# Chelate treatment process - The list of all chelate ions are listed
chelate_Ion_List = c("Iron","Zinc","Manganese","Copper")
#----------------------------------------------------------------------

# Installs all packages if they are not already available
if(!'XLConnect' %in% rownames(installed.packages())){
  install.packages('XLConnect')
}

# Loads all libraries needed for the analysis
library(XLConnect)

# Choose the file to be loaded into memory
fertilizer_file_location=file.choose()

# Read the fertilizer workbook from memory
fertilizer_workbook = loadWorkbook (fertilizer_file_location,create = FALSE)
# Preventing XLConnect from overwritting the current cell style
setStyleAction(fertilizer_workbook,XLC$"STYLE_ACTION.NONE")
# Read the table on important chemicals
chemical_data = readTable(fertilizer_workbook,sheet="Chemicals",table="Chemicals_Table")
# Read the corresponding element table
element_data = readTable(fertilizer_workbook,sheet="Elements",table="Elements_Table")[c("Element","Symbol","Atomic.Mass")]

#----- Section consisting of relevant functions -----

# Function to find the elemental composition within a chemical
elementCompostion = function(chemical_formula, water_of_hydration){
  water_molecular_mass=18.01528
  element_vector=elementVectoriser(chemical_formula)
  for(i in 1:nrow(element_data)){
    element_data$Composition[i]=
      as.numeric(element_data$Atomic.Mass[i])*elementInstanceCounter(element_vector, element_data$Symbol[i])
  }
  total_atomic_mass=sum(element_data$Composition)+water_of_hydration*water_molecular_mass
  # Make replacement for Ammonical Nitrogen and Nitrate Nitrogen
  chemical_formula=gsub("NO3","No",gsub("NH4","Nh",chemical_formula))
  element_vector=elementVectoriser(chemical_formula)
  i=i+1
  element_data[i,c("Element","Composition")]=
    c("Ammonical.Nitrogen",as.numeric(element_data[which(element_data$Element=="Nitrogen"),"Atomic.Mass"])*elementInstanceCounter(element_vector,"Nh"))
  i=i+1
  element_data[i,c("Element","Composition")]=
    c("Nitrate.Nitrogen",as.numeric(element_data[which(element_data$Element=="Nitrogen"),"Atomic.Mass"])*elementInstanceCounter(element_vector,"No"))
  element_data$Composition=as.numeric(element_data$Composition)/total_atomic_mass
  return(list(element_data[,c("Element","Composition")],total_atomic_mass))
}

# Function to split a chemical formula into separate sections
elementVectoriser = function(chemical_formula){
  chemical_formula = gsub("\\s+", "", chemical_formula)
  element_vector=unlist(strsplit(chemical_formula,""))
  # Separate out the different sections of the chemical formula
  i=j=0
  while(i<length(element_vector)){
    i=i+1
    j=j+1
    if(grepl("[[:digit:]]", element_vector[i])){
      if(i>1){
        if(grepl("[[:digit:]]", element_vector[i-1])){
          j=j-1
          element_vector[j]=paste(c(element_vector[i-1],element_vector[i]),collapse="")
        } else {
          element_vector[j]=element_vector[i]
        }
      } else {
        element_vector[j]=element_vector[i]
      }
    } else if(grepl("[[:upper:]]", element_vector[i])){
      if(i+1<=length(element_vector)){
        if(grepl("[[:lower:]]", element_vector[i+1])){
          element_vector[j]=paste(element_vector[c(i,i+1)],collapse="")
          i=i+1
        } else {
          element_vector[j]=element_vector[i]
        }
      } else {
        element_vector[j]=element_vector[i]
      }
    } else{
      element_vector[j]=element_vector[i]
    }
  }
  return(element_vector[1:j])
}

# Function to count the number of atoms of an element within a molecule
elementInstanceCounter = function(element_vector, analysis_element){
  layer_table=data.frame(layer=integer(), instances=integer(), stringsAsFactors=FALSE) 
  i=0
  current_layer=0
  layer_count=1
  layer_table[layer_count,]=c(current_layer,0)
  while(i<length(element_vector)){
    i=i+1
    if(element_vector[i]=="("){
      layer_count=layer_count+1
      current_layer=current_layer+1
      layer_table[layer_count,]=c(current_layer,0)
    }else if(element_vector[i]==")"){
      if(i+1<=length(element_vector)){
        if(grepl("[[:digit:]]", element_vector[i+1])){
          i=i+1
          layer_count_back = layer_count
          while(layer_table[layer_count,"layer"]<=layer_table[layer_count_back,"layer"]){
            layer_table[layer_count_back,]$instances=layer_table[layer_count_back,]$instances*as.integer(element_vector[i])
            layer_count_back = layer_count_back-1 
          }
        } 
      }     
      layer_count=layer_count+1
      current_layer=current_layer-1
      layer_table[layer_count,]=c(current_layer,0)
    }else if(element_vector[i]==analysis_element){
      if(i+1<=length(element_vector)){
        if(grepl("[[:digit:]]", element_vector[i+1])){
          i=i+1
          layer_table[layer_count,]$instances=layer_table[layer_count,]$instances+as.integer(element_vector[i])
        } else {
          layer_table[layer_count,]$instances=layer_table[layer_count,]$instances+1
        }
      } else{
        layer_table[layer_count,]$instances=layer_table[layer_count,]$instances+1
      }
    }
  }
  return(sum(layer_table$instances))
}

#----- Continuing the main code block -----

#Write element percentage contribution data to the excel
if(Chemical_element_composition_calculate) {
  for(i in 1:nrow(chemical_data)){
    chemical_formula=chemical_data[i,"Formula"]
    water_of_hydration=chemical_data[i,"Water.of.Hydration"]
    element_composition=elementCompostion(chemical_formula,water_of_hydration)
    # Writing the effective molecular mass to the chemical data data frame
    chemical_data[i,"Effective.Molecular.Mass"] = element_composition[[2]]
    element_composition = element_composition[[1]]
    # Writing the elemental content data for each chemical in the chemical data frame 
    for(j in 1:nrow(element_composition)){
      chemical_data[i,paste(element_composition[j,"Element"],"Content",sep=".")]=element_composition[j,"Composition"]
    }
  }
  # Writing the table to the excel worksheet and then saving the file
  writeWorksheet(fertilizer_workbook,data=chemical_data[,-c(1:6)],sheet="Chemicals",startRow=3,startCol=8,header=FALSE)
  saveWorkbook(fertilizer_workbook)
}

#----- Section to process all chemicals -----

#The molecular mass of water
water_molecular_mass=18.01528

# The total volume of the reservoir
reservoir_volume = readTable(fertilizer_workbook,sheet="Analysis Conditions",table="Analysis_Reservoir_Table")$Reservoir.Volume.in.Litres

# The final concentration levels needed for the nutrients (1 means normal concentration)
nutrient_concentration_level = readTable(fertilizer_workbook,sheet="Analysis Conditions",table="Analysis_Reservoir_Table")$Nutrient.Concentration

# Get the chemical requirements table
chemical_requirements = readTable(fertilizer_workbook,sheet="Analysis Conditions",table="Analysis_Chemicals_Table")
chemical_requirements_backup = chemical_requirements

# Get the table to store the analysis results
chemical_results = readTable(fertilizer_workbook,sheet="Analysis Results",table="Analysis_Results_Table")

# The elements which are not required for the nutrient solution
unwanted_elements=chemical_requirements[which((chemical_requirements$Required.PPM-chemical_requirements$Initial.PPM)<=0),"Symbol"]

# The chelates with those ions are identified and all other element contents are made zero
chemical_data[which(grepl("Chelate",chemical_data$Chemical.Name)),
              colnames(chemical_data)[which(grepl("Content",colnames(chemical_data)) 
                                            & rowSums((sapply(chelate_Ion_List,grepl,colnames(chemical_data))))==0)]]=0

# The chemicals which contain these elements are also to be removed
unwanted_chemicals=chemical_data[(rowSums(sapply(unwanted_elements,grepl,chemical_data$Formula))>0|chemical_data$Usage=="No"),"Chemical.Name"]

# Ensuring that all chelates that are being used are removed from the unwanted chemicals list
unwanted_chemicals=unwanted_chemicals[which(!(rowSums(sapply(chelate_Ion_List,grepl,unwanted_chemicals))>0 
                                              & grepl("Chelate",unwanted_chemicals) 
                                              & (unwanted_chemicals %in% chemical_data[which(chemical_data$Usage=="Yes"),"Chemical.Name"])))]

# The chemicals results are initialized to zero
chemical_results[,c("Grams","Cost")] = 0

# Removing the elements with no requirements
chemical_requirements=chemical_requirements[which((chemical_requirements$Required.PPM-chemical_requirements$Initial.PPM)>0),]

# Creating the chemical addition weights matrix as a data frame
chemical_addition_weights = data.frame(matrix(ncol=nrow(chemical_requirements), nrow=nrow(chemical_data)))
colnames(chemical_addition_weights) = chemical_requirements$Element
row.names(chemical_addition_weights) = chemical_data$Chemical.Name
chemical_addition_weights[]=0

# Creating the chemical removal weights matrix as a copy of the addition matrix
chemical_removal_weights = chemical_addition_weights

# The best score which is used later to finalize the combination of chemicals used is set to Infinite
best_score=Inf

# The number of grams of water introduced in the form of water of hydration
water_of_hydration_grams=0

# Processing to identify the grams of chemicals needed
for(i in 1:chemical_balance_iterations) {
  for(j in 1:nrow(chemical_requirements)) {
    
    # The element which is to be balanced out in this iteration
    current_Element = chemical_requirements[j,"Element"]
    
    # The number of grams of that element that has to be added
    current_Gram_Requirements = (chemical_requirements[j,"Required.PPM"]*nutrient_concentration_level-chemical_requirements[j,"Initial.PPM"]) * (reservoir_volume+water_of_hydration_grams/1000)/1000
    
    # Modification required as their are spaces in between the content column name for ammonical and nitrate nitrogen
    current_content_name=paste(paste(strsplit(current_Element," ")[[1]],collapse="."),"Content",sep=".")
    
    # The chemical data has been filtered to contain only the relevant chemicals for the current element being balanced
    current_chemical_data = chemical_data[which(chemical_data[,current_content_name]>0 & !(chemical_data[,"Chemical.Name"] %in% unwanted_chemicals)),]
    
    # Calculating the minimum values of chemical costs and current element content
    min_chemical_cost=min(current_chemical_data[,"Cost.Per.Kg"])
    min_element_content=min(current_chemical_data[,current_content_name])
    
    # Calculating the ranges of chemical costs and current element content
    range_chemical_cost=max(current_chemical_data[,"Cost.Per.Kg"])-min_chemical_cost
    range_element_content=max(current_chemical_data[,current_content_name])-min_element_content
    
    if(current_Gram_Requirements>0) {
      
      # The chemical weights for the current element are obtained for all the relevant chemicals
      Current_chemical_weights = chemical_addition_weights[which(row.names(chemical_addition_weights) %in% current_chemical_data$Chemical.Name),which(colnames(chemical_addition_weights)==current_Element)]
      # The chemical for balancing the current element is decided based on the cost per kilo, element concentration and the previous addition weight
      current_chemical_data = current_chemical_data[order( 
        (current_chemical_data$Cost.Per.Kg-min_chemical_cost)/range_chemical_cost
        -(current_chemical_data[,current_content_name]-min_element_content)/(range_element_content)
        +Current_chemical_weights),][1,]
      # Updating the chemical weight for the choosen chemical and current element
      chemical_addition_weights[current_chemical_data$Chemical.Name,current_Element]=chemical_addition_weights[current_chemical_data$Chemical.Name,current_Element]+0.05
     
      # saving the row number on the chemical results table for ease of use later on
      current_chemical_results_row=which(chemical_results$Chemical.Name==current_chemical_data[1,"Chemical.Name"])
      
      # this was the number of grams of the chemical previously
      previous_grams = chemical_results[current_chemical_results_row,"Grams"]
      # this is the amount of grams to add in order to balance the current element (notice that we are decreasing the effect of amount added as the iteration count progresses)
      added_grams = current_Gram_Requirements/current_chemical_data[1,current_content_name] * ((chemical_balance_iterations-i+1)/chemical_balance_iterations)
      # Now we are updating the previous grams with the added amount
      chemical_results[current_chemical_results_row,"Grams"] = previous_grams + added_grams
      # The corresponding cost for the chemical is also recalculated
      chemical_results[current_chemical_results_row,"Cost"]= chemical_results[current_chemical_results_row,"Grams"]*current_chemical_data[1,"Cost.Per.Kg"]/1000
      # In case the chemical existed as a hydrate, we must update the grams contributed by the water of hydration
      water_of_hydration_grams = water_of_hydration_grams + added_grams * water_molecular_mass * current_chemical_data[1,"Water.of.Hydration"]/current_chemical_data[1,"Effective.Molecular.Mass"]
      
      # Adding chemicals for the current element causes imbalances for other elements which also must be updated
      for(k in 1:nrow(chemical_requirements)) {
        current_content_name=paste(paste(strsplit(chemical_requirements[k,"Element"]," ")[[1]],collapse="."),"Content",sep=".")
        chemical_requirements[k,"Initial.PPM"]=chemical_requirements[k,"Initial.PPM"] + current_chemical_data[1,current_content_name] * added_grams * 1000/(reservoir_volume+water_of_hydration_grams/1000)
      }
       
    }else if(current_Gram_Requirements<0){
      
      # The same set of operations are performed while removing chemicals
      Current_chemical_weights = chemical_removal_weights[which(row.names(chemical_removal_weights) %in% current_chemical_data$Chemical.Name),which(colnames(chemical_removal_weights)==current_Element)]
      # The chemical removed for balancing the current element is decided based on the cost per kilo, element concentration and the previous addition weight
      current_chemical_data = current_chemical_data[order( 
        -(current_chemical_data$Cost.Per.Kg-min_chemical_cost)/range_chemical_cost
        -(current_chemical_data[,current_content_name]-min_element_content)/(range_element_content)
        +Current_chemical_weights),]
      # An extra step is taken to ensure we are not removing any chemicals having zero grams already
      current_chemical_data=current_chemical_data[which(current_chemical_data$Chemical.Name %in% chemical_results[which(chemical_results$Grams>0),"Chemical.Name"]),][1,]
      # Updating the chemical weight for the choosen chemical and current element
      chemical_removal_weights[current_chemical_data$Chemical.Name,current_Element]=chemical_removal_weights[current_chemical_data$Chemical.Name,current_Element]+0.05
    
      # saving the row number on the chemical results table for ease of use later on
      current_chemical_results_row=which(chemical_results$Chemical.Name==current_chemical_data[1,"Chemical.Name"])
      
      if(chemical_results[current_chemical_results_row,"Grams"]>0) {
        # this was the number of grams of the chemical previously
        previous_grams = chemical_results[current_chemical_results_row,"Grams"]
        # this is the amount of grams to remove in order to balance the current element (notice that we are decreasing the effect of amount added as the iteration count progresses)
        removed_grams = max(current_Gram_Requirements/current_chemical_data[1,current_content_name] * ((chemical_balance_iterations-i+1)/chemical_balance_iterations),-previous_grams)
        # Now we are updating the previous grams with the removed amount while ensuring that the new grams is greater than or equal to zero
        chemical_results[current_chemical_results_row,"Grams"] = previous_grams + removed_grams
        # The corresponding cost for the chemical is also recalculated
        chemical_results[current_chemical_results_row,"Cost"]= chemical_results[current_chemical_results_row,"Grams"]*current_chemical_data[1,"Cost.Per.Kg"]/1000
        # In case the chemical existed as a hydrate, we must update the grams contributed by the water of hydration
        water_of_hydration_grams = water_of_hydration_grams + removed_grams * water_molecular_mass * current_chemical_data[1,"Water.of.Hydration"]/current_chemical_data[1,"Effective.Molecular.Mass"]
        
        # Removing chemicals for the current element causes imbalances for other elements
        for(k in 1:nrow(chemical_requirements)) {
          current_content_name=paste(paste(strsplit(chemical_requirements[k,"Element"]," ")[[1]],collapse="."),"Content",sep=".")
          chemical_requirements[k,"Initial.PPM"]=chemical_requirements[k,"Initial.PPM"] + current_chemical_data[1,current_content_name] * removed_grams * 1000/(reservoir_volume+water_of_hydration_grams/1000)
        }
      }
    }
    
    # Calculate the best score by finding the case having the minimum distance between the required and actual PPM's for all elements
    if(sum(abs(chemical_requirements$Required.PPM*nutrient_concentration_level-chemical_requirements$Initial.PPM))<best_score) {
      best_score = sum(abs(chemical_requirements$Required.PPM*nutrient_concentration_level-chemical_requirements$Initial.PPM))
      selected_chemical_results = chemical_results
      selected_current_requirements = chemical_requirements
    }
    
    # Will break execution if all the elements are balanced within the limits of the 'element_percentage_Offset' parameter
    if(all(abs(chemical_requirements$Required.PPM*nutrient_concentration_level-chemical_requirements$Initial.PPM)/(chemical_requirements$Required.PPM*nutrient_concentration_level) < element_percentage_Offset)){
      break
    } 
  }
}

# Bringing the finalized PPM levels to the normal concentration levels
selected_current_requirements$Initial.PPM = selected_current_requirements$Initial.PPM/nutrient_concentration_level
# Left join the selected chemical requirements data frame with the backup created earlier
selected_current_requirements=merge(chemical_requirements_backup,selected_current_requirements,by = "Element", all.x=TRUE)
# Convert any NA's to zero
selected_current_requirements[is.na(selected_current_requirements$Initial.PPM.y),"Initial.PPM.y"] = 0
# Sort the chemical requirements data frame according to the chemical requirements backup
selected_current_requirements=selected_current_requirements[match(chemical_requirements_backup$Element,selected_current_requirements$Element),c("Required.PPM.x","Initial.PPM.y")]

# Write the current elemental PPM into the excel file
writeWorksheet(fertilizer_workbook,data=selected_current_requirements,sheet="Analysis Conditions",startRow=6,startCol=5,header=FALSE)
# Write the processed chemical results into the excel file
writeWorksheet(fertilizer_workbook,data=selected_chemical_results[order(-selected_chemical_results$Grams),],sheet="Analysis Results",startRow=3,startCol=2,header=FALSE)
# Write the total grams and cost details to the bottom of the analysis results table
writeWorksheet(fertilizer_workbook,data=data.frame(sum(selected_chemical_results[,c("Grams")]),sum(selected_chemical_results[,c("Cost")])),sheet="Analysis Results",startRow=3+nrow(chemical_results),startCol=4,header=FALSE)
# Save the workbook
saveWorkbook(fertilizer_workbook)
