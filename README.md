# Null Synthetic Data Generation 

The **Null Synthetic Data Generation** application is an R Shiny app developed by Parker Combs (NIH/NIEHS) and adapted from code by Deepak Mav, PhD (Sciome, LLC). Given an input file of a specific format (see examples/Example.txt), the app can:
- Generate some number *N* of synthetic null samples based off of the existing null samples **only** in the input file.
- Generate some number *N* of synthetic null samples based off of the existing null samples **only** in the input file and randomly sample and categorize them into arbitrary dosage levels of size *M<sub>i</sub> ... M<sub>j</sub>* where *M<sub>i</sub>* is the first defined dosage level and *M<sub>j</sub>* is the last defined dosage level.
- Generate some number *N* of synthetic null samples **for each** dosage level in the input file and randomly sample and categorize them into arbitrary dosage levels of size *M<sub>i</sub> ... M<sub>j</sub>* where *M<sub>i</sub>* is the first defined dosage level and *M<sub>j</sub>* is the last defined dosage level.

## Usage
1. First, click the "Browse..." button to open the file browser. Select one or more input files to upload to the app.
<br>
![Selecting input files](https://github.com/combspk/null-synthetic-data-generation/tree/main/img/1.png)

2. Next, select the output mode.
<br>
![Specifying output mode](https://github.com/combspk/null-synthetic-data-generation/tree/main/img/2.png)

3. For the **Generate synthetic null control samples** mode, you can set the **number of synthetic samples** option to determine the total number of original + synthesized samples to be output.
<br>
![Generate synthetic null samples options](https://github.com/combspk/null-synthetic-data-generation/tree/main/img/3.png)

4. For the **Generate synthetic null control samples & create expression data files** mode, you can set the following options:
- **number of synthetic samples** - The total number of original + synthesized samples to be output.
- **number of expression data files** - The number of output files to be created. Each output file will have a different sampling of the total samples specified in the **number of synthetic samples**.
- **dose input mode** - How the dosage levels for each sample should be generated.

5. For the **Automatic generation**  dose input mode, you can set the following options:
- **Number of treatment groups (including D0)** - The total number of dosage groups, including the null dosage group (dose = 0).
- **Number of replicates = D0** - The total number of samples per output file where dose = 0.
- **Number of replicates ≠ D0** - The total number of samples per output file where dose ≠ 0 (this is applied to **each** nonzero dose level).
- **Top dose** - The maximum dose level to include in each output file.
- **Dose spacing** - The spacing between each dose level.

6. For the **Manual input** dose input mode, you can click the **Add dose level** button to specify a new dose level as well as the number of repetitions for each level. 
<br>
![Options for automatic dose level spacing](https://github.com/combspk/null-synthetic-data-generation/tree/main/img/4.png)
<br>
![Options for manual dose level spacing](https://github.com/combspk/null-synthetic-data-generation/tree/main/img/5.png)

7. For the **Supplement existing data** mode, you can configure the options similarly to the previous modes to generate new samples for each dose level given one or more input data file(s).
<br>
![Options for supplementing existing data](https://github.com/combspk/null-synthetic-data-generation/tree/main/img/6.png)

8. Press the **GO!** button to start the sample generation process.

9. Once the sample and file generation is complete, a new button will appear that allows you to download a .zip archive of the files generated.
