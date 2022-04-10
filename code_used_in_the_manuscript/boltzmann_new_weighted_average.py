# Import necessary libraries
import math
import os
import sys

# Pre-define all column id for specific values (0-index)
molecule_id_col_id = 1
dE_col_id = 3
values_col_id = 5 # Start index to extract values
kT = 0.001986*295

def getDirectoryFileDictionary(directory):
    files = {'txt':[], 'out':[]}
    for file in os.listdir(directory):
        if file.endswith(".txt"):
            files['txt'].append(file)
        if file.endswith(".out"):
            files['out'].append(file)
    return files

def getFileToCompoundDictionary(file_list):
    files_to_compound = {txt_file:"".join(filter(str.isalnum, txt_file[:-8])) for 
                        txt_file in file_list}
    return files_to_compound

def getCompoundWeightsDictionary(weights_file):
    translate = {
    'bernil':'dmz',
    'ethidiumbromide':'etbr',
    'gentamicin':'gentamycin',
    'neomycinb':'neomycin',
    'paromomycin':'paromycin',
    'thiazoleorangetosylate':'thiazoleorange'}
    
    f = open(weights_file)
    lines = f.readlines()[1:]
    
    compound_weights = {}
    
    for line in lines:
        line = line.split(',')
        compound_name = "".join(filter(str.isalnum, line[0].lower()))
        for k,v in translate.items():
            compound_name = compound_name.replace(k, v)
        weights = []
        for weight in line[1:]:
            if weight.strip().replace('.','',1).isdigit():
                weights.append(float(weight))
        compound_weights[compound_name] = weights
    return compound_weights

def readInputToMoleculeDictionary(filename):
    f = open(filename)
    lines = f.readlines()[1:] # Remove first line with header names

    # Dictionary for each molecule mapped to lines of data
    molecule_to_data = {}

    # Loop through all lines and consolidate by molecule into dictionary
    for line in lines: 
        line = line.split(',') # Split line into list of values by comma
        molecule_id = int(line[molecule_id_col_id])

        data_entry = [float(line[dE_col_id])] # Save dE to first value in data_entry
        data_entry += [float(value.strip()) if value.strip() != '' else '' for value in line[values_col_id:]]
        # Check if there is empty values, if not save data_entry to dictionary
        if '' not in data_entry:
            molecule_to_data[molecule_id] = molecule_to_data.get(molecule_id, [])
            molecule_to_data[molecule_id].append(data_entry)
    return molecule_to_data

def calculateBoltzmannAverage(molecule_to_data):
    # List for Boltzmann calculations
    boltzmann = []

    # Calculates boltzmann average for all values for each molecule
    for molecule_id, data_entries in molecule_to_data.items():
        molecule_weights = []
        # Calculate boltz value for each data entry of the same molecule
        for data_entry in data_entries:
            dE = data_entry[0]
            molecule_weights.append(math.exp(-dE/kT))

        # Get the weights for boltzmann calculation for the molecule
        molecule_weights = [weight/sum(molecule_weights) for weight in molecule_weights]
        sum_product_list = []

        for i, data_entry in enumerate(data_entries): 
            # Calculate product for each value in data entry
            sum_product_list.append([value*molecule_weights[i] for value in data_entry[1:]])

        # Calculate sumproduct for each value in all data entries
        sum_product = [sum(values) for values in zip(*sum_product_list)]
        boltzmann.append([molecule_id] + sum_product)
    return boltzmann

def getMoleculeWeightsForCompound(boltzmann, compound_weights, compound_name):
    molecule_id_list = [molecule[0] for molecule in boltzmann]
    molecule_weights = [compound_weights[compound_name][molecule_id-1] for molecule_id in molecule_id_list]
    molecule_weights = [weight/sum(molecule_weights) for weight in molecule_weights]
    return molecule_weights
    
def calculateBoltzmannAverageWeightedAverage(boltzmann, molecule_weights):
    boltzmann_weighted = []
    for values in list(zip(*boltzmann))[1:]:   
        boltzmann_weighted.append(sum([v * w for v, w in zip(values, molecule_weights)]))
    return boltzmann_weighted

def printResultToStandardOut(boltzmann, molecule_weights, boltzmann_weighted):
    for i, line in enumerate(boltzmann):
        print(str(line[0]) + " (" + str(molecule_weights[i-1]/sum(molecule_weights)) + "), " + str(line[1]) + ", " + str(line[2]))
    print("weighted: " + str(boltzmann_weighted[0]) + ", " + str(boltzmann_weighted[1]))
    
def outputToFile(directory, filename, data):
    os.makedirs(directory + "/out", exist_ok=True)
    outfile=open(directory + "/out" + "/" +filename,"w")
    
    for line in data:
        outfile.writelines(str(line[0]))
        for obj in line[1:]:
            outfile.writelines(", " + str(obj))
        outfile.writelines("\n")
    outfile.close()
    
def main(argv):
    
    directory = argv[0]
    weights_file = argv[1]
    
    # Get list of txt and out files in the directory
    files = getDirectoryFileDictionary(directory)
    # Get dictionary of txt file names mapped to compound names
    files_to_compound = getFileToCompoundDictionary(files['txt'])
    # Get dictionary of compound names mapped to compound weights list
    compound_weights = getCompoundWeightsDictionary(weights_file)
    
    # Process each txt file
    for filename in files_to_compound.keys():
        
        # If there exist out file for this txt file, skip
        if filename[:-3]+'out' in files['out']:
            continue
            
        compound_name = files_to_compound[filename]
        print(filename) # Print to standard out txt filename
        # Get dictionary of molecule id mapped to data entries
        molecule_to_data = readInputToMoleculeDictionary(directory + '/' + filename)
        # Get List of molecule id mapped to boltzmann average values
        boltzmann = calculateBoltzmannAverage(molecule_to_data)
        # Calculates weighted average of boltzmann average for each compound
        molecule_weights = getMoleculeWeightsForCompound(boltzmann, compound_weights, compound_name)
        boltzmann_weighted = calculateBoltzmannAverageWeightedAverage(boltzmann, molecule_weights)
        
        printResultToStandardOut(boltzmann, molecule_weights, boltzmann_weighted)
        
        # Write results to output file
        outputToFile(directory, filename[:-3]+"out", boltzmann)
        outputToFile(directory, filename[:-4]+"_weighted"+".out", [boltzmann_weighted])
    
if __name__ == "__main__":
    main(sys.argv[1:])