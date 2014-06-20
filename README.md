courseraR
=========


App is formed by funcions:
* download_file : verify existng file. if not exists download the data set.
* readData: read info from due branch train an test, recursive funcion to read all data in teh set of train and test.
* firstLevelData: funcion callend in readData to merge anche create column in meta data analysis
* replaceActivityName: function called by firstLevelData for add activity columns name
* readInertialData: read data from files  "body_acc*","body_gyro*","total_acc*" and merging all to the variable original_df
* extractMeanStdColumns: function to calculate Standard Means for columns
* summarizedData: function to summarized data from dataset.
* Main execution

