
//'global' variables
	

	//version
	version = "Netalyser_v9";

	//path separator
	pathSep = "\\";
	
	//preset variables
	mode = "8-bit"
	channels_init = newArray("C1-","C2-");
	thr_method_init = "Huang";
	thr_lw_init = 0;
	thr_up_init = 0;
	size_lw_init = 50;
	size_up_init = 500;
	methods = getList("threshold.methods");
	thr_method_list = Array.concat(methods,"manual");
	q_score_init = 0.8;
	watershed_init = "No";
	
	//assigned variables
	channels = channels_init; 
	thr_method = thr_method_init;
	thr_lw = thr_lw_init;
	thr_up = thr_lw_init;
	size_lw = size_lw_init;
	size_up = size_up_init;
	q_score = q_score_init;
	watershed = watershed_init;

//functions


function listFiles(dir, file_list) {
	list = getFileList(dir);
	for (i=0; i<list.length; i++) {
		if (endsWith(list[i], "/"))
			file_list = listFiles(""+dir+list[i], file_list);
        else
           file_list = Array.concat(file_list, dir+list[i]);    
    }
	return file_list;
}

function getIDs(base_dir, file_name) {
	file_dir = File.getParent(file_name)+pathSep;
	IDs = newArray;
	while (base_dir != file_dir){
		IDs = Array.concat(IDs, File.getName(file_dir));
		file_dir = File.getParent(file_dir)+pathSep;
	}
	return IDs;
}


//main script


	//main dialog presets
	Structure = "Generate structured data set of Composite images";
	Channels = "Open prototype image, determine which channel is used for segmentation (DNA signal)";
	Segmentation = "Determine intensity and size parameters for segmentation (detecting nuclei)";
	Review =    "Review the procedure on images you like...";
	Analysis = "Analyse structured data set and write data to txt file";
	Mapping = "Map NET identifications back on image data";
	Fin = "Finished for today...";

	//control variable
	Script_choice = "";

	//close all open images
	close("*");

	
	do{

	//main dialog
	Radio_Choice = newArray(Structure, Channels, Segmentation, Review, Analysis, Mapping, Fin);
	Dialog.create("Main Selection");
		Dialog.addMessage("   Choose what to do:               "+"\n");
		Dialog.setInsets(5, 50, 5);
		Dialog.addRadioButtonGroup("", Radio_Choice, 7, 1, Fin);
		Dialog.addMessage(""+"\n");
	Dialog.show();
	Script_choice = Dialog.getRadioButton();


	//Generate structured data set from single channels ///////////////////////////////////////////////////////////////////////////////////////////////
	if(Script_choice == Structure){

		//sub dialog presets
		start_over = "Generate another structured data set...                                 ";
		fin =    "Finished...";

		//control variable
		structure_choice = start_over;

		
		do{
			//dialog to gexplain procedure
			
			Dialog.create("Generate structured data set of Composite images");
				Dialog.addMessage("Procedure:");
				Dialog.addMessage("   1: Select folder with the images of the SEGMENTATION channel (e.g. DNA)");
				Dialog.addMessage("   2: Select folder with the images of the ANALYSIS channel (e.g. antibody staining)");
				Dialog.addMessage("      - make sure that the images are in corresponding order in both directories");
				Dialog.addMessage("      - the script then assembles one Composite image after the other");
				Dialog.addMessage("      - the segmentation channel will later on have the prefix C1- , the analysis channel C2-");
				Dialog.addMessage("   3: Save the image to the respective folder in a structured directory");
				Dialog.addMessage("      - for example an image could end up in a folder like this:");
				Dialog.addMessage("          structured_data/stimulus/timepoint/replicate_1.tiff");
				Dialog.addMessage("              - the structured_data folder has sub-folders for all stimuli used in the analysis");
				Dialog.addMessage("              - the stimulus sub-folder has sub-folders for all timepoints done for that stimulus");
				Dialog.addMessage("              - the timepoint sub-folder has all replicates for that timepoint ");
				Dialog.addMessage("      - you can have as many 'layers', such as stimulus, timepoint or else, as you wish and in which order you'd like");
				Dialog.addMessage("      - the names of the sub-folders will later be read and used in the analysis. E.g. the data from the image above");
				Dialog.addMessage("         will be assinged to 'stimulus' and 'timepoint'");
				Dialog.addMessage("\n");
				Dialog.addMessage("Remarks:");
				Dialog.addMessage("   - If you already have Composite images simply put them in such a structured directory");
				Dialog.addMessage("   - In all images the same channel has to be the one for segmentation (e.g. DNA) ");
				Dialog.addMessage("   - The analysis will ONLY WORK with ImageJ/Fiji COMPOSITE images. RGBs will not work");
			Dialog.show();

			//select the images for segmentation channel
			waitForUser("Select folder with the images of the SEGMENTATION channel (e.g. DNA)");
			ch1_dir = getDirectory("Choose directory with images for channel 1");
			ch1_list = getFileList(ch1_dir);

			//select the images for analysis channel
			waitForUser("Select folder with the images of the ANALYSIS channel (e.g. antibody staining)");
			ch2_dir = getDirectory("Choose directory with images for channel 2");
			ch2_list = getFileList(ch2_dir);

			//open images and make the Composites

			//if same number of images for both channels
			if (ch1_list.length == ch2_list.length){
				for (i=0; i<ch1_list.length; i++){
					open(ch1_dir + ch1_list[i]);
					rename("ch1");
					run(mode);
					open(ch2_dir + ch2_list[i]);
					run(mode);
					rename("ch2");
					run("Merge Channels...", "c1=ch1 c2=ch2 create");
					selectWindow("Composite");
					rename(ch1_list[i]+"  "+ch2_list[i]);
					selectWindow(ch1_list[i]+"  "+ch2_list[i]);
					saveAs("Tiff");
					close();
				}
			}else{
				showMessage("Error", "Number of images for segmentation does not equal number of images for analysis");
				};			
			
			//sub dialog
			Radio_Choice = newArray(start_over, fin);
			Dialog.create("Generate structured data set of Composite images");
				Dialog.addMessage("   Choose what to do:");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", Radio_Choice, 5, 1,fin);
			Dialog.show();
			structure_choice = Dialog.getRadioButton();		
			
		}while(structure_choice == start_over);		
	};


	//Map analysis back on original image data////////////////////////////////////////////////////////////////////////////////////////////////////
	if(Script_choice == Mapping){
		
		//sub dialog presets
		start_over = "Map another data set...                                 ";
		fin =    "Finished...";

		//control variable
		mapping_choice = start_over;

		do{
			
			//dialog to get the color scheme
			Col_choice = newArray("red", "green", "blue", "cyan", "yellow");
			Dialog.create("Configuration");
				Dialog.addMessage("Choose color scheme for the mapping:               "+"\n\n");
				Dialog.setInsets(5, 50, 5);
				Dialog.addChoice("Color for resting cells:", Col_choice, "cyan");
				Dialog.setInsets(5, 50, 5);
				Dialog.addChoice("Color for NETs:", Col_choice, "red");
				Dialog.setInsets(5, 50, 5);
				Dialog.addChoice("Color for marked resting cells:", Col_choice, "green");
				Dialog.setInsets(5, 50, 5);
				Dialog.addChoice("Color for marked NETs:", Col_choice, "yellow");
				Dialog.setInsets(5, 50, 5);
				Dialog.addChoice("Color for objects larger than single nucleus/NET:", Col_choice, "blue");
			Dialog.show();
			rest_Col = Dialog.getChoice();
			NET_Col = Dialog.getChoice();
			mark_rest_Col = Dialog.getChoice();
			mark_NET_Col = Dialog.getChoice();
			NA_Col = Dialog.getChoice();
			 
			//open and parse the R text file into arrays
			waitForUser("Select txt file wit the mapping information");
			pathfile=File.openDialog("Open ReAnalyse file"); 
			filestring=File.openAsString(pathfile); 
			rows=split(filestring, "\n"); 
	
			//get the directory of the R file and create a directory for the annotated images
			dir_name = File.getParent(pathfile);
			file_name = File.getName(pathfile);
			file_name_array = split(file_name,".");
			target_dir = dir_name+pathSep+file_name_array[0]+"_images"+pathSep;
			File.makeDirectory(target_dir);
	
			//get the first image
			firstLine = split(rows[0],"\t");
			target_file = firstLine[0];
			target_name = File.getName(target_file);
			
			//check if the file exists - directory may have been moved
			if(File.exists(target_file)){
				open(target_file);
				rename("target");
				
				//go through all rows of the file
				for(i=0; i<rows.length; i++){ 
							
					//split row into fields
					columns=split(rows[i],"\t"); 
							
					//check if dealing with new target image
					if(columns[0] != target_file){
						selectWindow("target");
						saveAs("Tiff",target_dir+target_name);
						Overlay.show;
						target_file = columns[0];
						target_name = File.getName(target_file);
						open(target_file);
						rename("target");
					}
							
					//draw the selections
					xCoord =columns[1];
					yCoord =columns[2];
					Color =columns[3];
					x = split(xCoord, ",");
					y = split(yCoord, ",");
					x = Array.trim(x, (x.length-1)); 
					y = Array.trim(y, (y.length-1)); 
					makeSelection("freehand", x, y);
						if (Color == 0){
							Overlay.addSelection(rest_Col, 1);
						}
						if (Color == 1){
							Overlay.addSelection(mark_rest_Col, 1);
						}
						if (Color == 2){
							Overlay.addSelection(NET_Col, 1);
						}
						if (Color == 3){
							Overlay.addSelection(mark_NET_Col, 1);
						}	
						if (Color == 4){
							Overlay.addSelection(NA_Col, 1);
						}	
				} 	
			};else{

				// choose the structered data directory
				waitForUser("Original data not found! Please select the directory with your structured data.");
				str_directory = getDirectory("Choose Directory");
				str_directory_name = File.getName(str_directory);

				//determine how far (from end) the path from the R file has to be used
				R_path = target_file;
				indent = 0;
				do{	indent = indent +1;
					R_path_name = File.getName(R_path);
					R_path = File.getParent(R_path);
				}while(str_directory_name != R_path_name);
				
				//construct the new path from the selected directory and the part that comes from the R file
				R_path = target_file;
				stable_path = File.getName(R_path);
				for (add = 1; add < indent; add++){
					R_path = File.getParent(R_path);
					stable_path = File.getName(R_path)+pathSep+stable_path;
				};
				IJ_path = File.getParent(str_directory)+pathSep+stable_path;

				//open the first file
				open(IJ_path);
				rename("target");

				//go through all rows of the file
				for(i=0; i<rows.length; i++){ 
							
					//split row into fields
					columns=split(rows[i],"\t"); 
							
					//check if dealing with new target image
					if(columns[0] != target_file){
						selectWindow("target");
						saveAs("Tiff",target_dir+target_name);
						Overlay.show;
						target_file = columns[0];
						target_name = File.getName(target_file);
						//reconstruct the new path and open next image
						R_path = target_file;
						stable_path = File.getName(R_path);
						for (add = 1; add < indent; add++){
							R_path = File.getParent(R_path);
							stable_path = File.getName(R_path)+pathSep+stable_path;
						};
						IJ_path = File.getParent(str_directory)+pathSep+stable_path;
						open(IJ_path);
						rename("target");
					}
							
					//draw the selections
					xCoord =columns[1];
					yCoord =columns[2];
					Color =columns[3];
					x = split(xCoord, ",");
					y = split(yCoord, ",");
					x = Array.trim(x, (x.length-1)); 
					y = Array.trim(y, (y.length-1)); 
					makeSelection("freehand", x, y);
						if (Color == 0){
							Overlay.addSelection(rest_Col, 1);
						}
						if (Color == 1){
							Overlay.addSelection(mark_rest_Col, 1);
						}
						if (Color == 2){
							Overlay.addSelection(NET_Col, 1);
						}
						if (Color == 3){
							Overlay.addSelection(mark_NET_Col, 1);
						}	
						if (Color == 4){
							Overlay.addSelection(NA_Col, 1);
						}	
				}
			};
			
			//for the last image
			selectWindow("target");
			saveAs("Tiff",target_dir+target_name);
			Overlay.show;

			//allow reviewing the images
			waitForUser("Click OK when finished reviewing the mapping (they are also saved!)");

			//sub dialog
			Radio_Choice = newArray(start_over, fin);
			Dialog.create("Mapping analysis data back on original images");
				Dialog.addMessage("   Mapping: "+file_name);
				Dialog.addMessage("   finished!");
				Dialog.addMessage("   Choose what to do:");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", Radio_Choice, 5, 1,fin);
			Dialog.show();
			mapping_choice = Dialog.getRadioButton();
			
			//close al images 
			close("*");	
			
		}while(mapping_choice == start_over);
	};
	

	//Run analysis on a structured dataset///////////////////////////////////////////////////////////////////////////////////////////////////////
	if(Script_choice == Analysis){

		//sub dialog presets
		start_over = "Analyse another data set...                                 ";
		fin =    "Finish and discard settings...";

		//control variable
		analysis_choice = start_over;

		do{
			//dialoge to fill in parameters, use presets from previous script parts if available
			MSingle = newArray("Yes", "No");
			Dialog.create("Configuration of structured data set analysis");
				Dialog.setInsets(5, 300, 5);
				Dialog.addString("Analysis name:", "", 40);
				Dialog.addMessage("\n");
				Dialog.addMessage("Assignment of channels:                                                                       ");
				Dialog.setInsets(5, 50, 5);
				Dialog.addString ("Prefix for segmentation channel (e.g. DNA):", channels[0]);
				Dialog.addString ("Prefix for measurement channel (e.g. Ab):   ", channels[1]);
				Dialog.addMessage("\n");
				Dialog.addMessage("Use unsupervised thresholding?");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", MSingle, 1, 2,"Yes");
				Dialog.addMessage("Method for segmentation:               ");
				Dialog.setInsets(5, -250, 5);
				Dialog.addChoice("", thr_method_list, thr_method);
				Dialog.addNumber("Manual threshold lower intensty limit:", thr_lw); 
				Dialog.addNumber("Manual threshold upper intensty limit:", thr_up); 
				Dialog.addMessage("\n");
				Dialog.addMessage("Particle size selection criteria:               ");
				Dialog.addNumber("Lower size limit:", size_lw); 
				Dialog.addNumber("Upper size limit:", size_up); 
				Dialog.addMessage("\n");
				Dialog.addMessage("Segmentation quality score:               "+"\n");
				Dialog.addNumber("area of particles / total area with signal ", q_score);
				Dialog.addMessage("\n");
				Dialog.addMessage("Use Watershed?");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", MSingle, 1, 2, watershed);
				Dialog.addMessage("\n");
				Dialog.addMessage("Use batch processing (faster, no pictures shown)?");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", MSingle, 1, 2,"Yes"); 
			Dialog.show();
			analysis_name = Dialog.getString();
			channels[0] = Dialog.getString();
			channels[1] = Dialog.getString();
			automated = Dialog.getRadioButton();
			thr_method = Dialog.getChoice();
			thr_lw = Dialog.getNumber();
			thr_up = Dialog.getNumber();
			size_lw = Dialog.getNumber();
			size_up = Dialog.getNumber();
			q_score = Dialog.getNumber();
			watershed =  Dialog.getRadioButton();
			batch =  Dialog.getRadioButton();

			//set batchmode to false if supervised thresholding is chosen
			if (automated == "No"){
				batch =  "No";
			};

			//close all open images
			close("*");

			//get the directory - this is the 'Starting point' of the analysis and the list of the files in the analysis
			waitForUser("Select directory with structured data set");
			dir = getDirectory("Choose a Directory ");
			file_list = newArray; 
			file_list = listFiles(dir, file_list);


			//check whether all files are in the same 'depth' - meaning that they will get the same number of IDs 
			checkID_array = newArray;
			for (ii=0; ii<file_list.length; ii++){
				IDs = getIDs(dir, file_list[ii]);
				IDs_length = IDs.length;
				checkID_array = Array.concat(checkID_array, IDs_length);
			}
			Array.getStatistics(checkID_array, min, max, mean, std);

			//batchmode
			if(batch == "Yes"){
				setBatchMode(true);
				}else{
					setBatchMode(false);
					};

			//set measurement options
			run("Set Measurements...", "area mean redirect=None decimal=3");

			//if all files will get same number of IDs go on
			if(min == max){

				//get the number of IDs and put them into a string like ID1 ID2 ID3 for the column headers in Results
				ID_1 = getIDs(dir, file_list[0]);
				ID_name = "";
				for (ii=0; ii<ID_1.length; ii++){
					ID_name = ID_name+"ID"+(ii+1)+"\t";
				}
			
				//for results and report - these files will bewritten in the parent of the directory that was chosen
				result_string = "";
				report_string = "";
				File.makeDirectory(File.getParent(dir)+pathSep+analysis_name)
				CustomResults = File.open(File.getParent(dir)+pathSep+analysis_name+pathSep+analysis_name+".txt");
				print(CustomResults,"Area \t Int \t "+ID_name+"x.cord"+"\t"+"y.cord"+"\t"+"file"+"\t"+"max.size"+"\n");	
	
				//go through all files in the list
				for (file=0; file<file_list.length; file++){
				
					//get the values for the IDs for the results
					IDs = getIDs(dir, file_list[file]);
					IDs_length = IDs.length;
					ID_val = "";
					for (ii=0; ii<IDs.length; ii++){
						ID_val = ID_val+IDs[ii]+"\t";
					};
	
				//restet results and ROIManager
				roiManager("Reset");
				run("Clear Results");
				
				
				//open the file and analyse
				open(file_list[file]);
				image_name = getTitle();
				run("Split Channels");
				
				//get the area of all signal in segmentation channel above threshold
				selectWindow(channels[0]+image_name);
				run("Grays");
				rename("segmenting");
				
				//set threshold according to choices
				if(automated == "No"){
					if(thr_method == "manual"){
					run("Threshold...");
					setThreshold(thr_lw, thr_up);
					waitForUser("Click OK when finished with threshold supervision");	
						}else{
							run("Threshold...");
							setAutoThreshold(thr_method+" dark");
							waitForUser("Click OK when finished with threshold supervision");
							}
				}
				if(automated == "Yes"){
					if(thr_method == "manual"){
					setThreshold(thr_lw, thr_up);	
						}else{
							setAutoThreshold(thr_method+" dark");
							}
				}	
				//convert to mask
				setOption("BlackBackground", false);
				run("Convert to Mask");
				
				//run Watershed if selected
				if (watershed == "Yes"){
					run("Watershed");
				};
				
				//get area of total signal
				run("Analyze Particles...", "size=0-Infinity pixel show=Nothing exclude include add");
				roiManager("Measure");
				area_all = 0;
				for (row=0; row<nResults; row++){
					 area_all = area_all + d2s(getResult("Area",row),0);
				}
				roiManager("Reset");
				run("Clear Results");
	
				//get area of size selected signal
				selectWindow("segmenting");
				run("Analyze Particles...", "size="+size_lw+"-"+size_up+" show=Nothing exclude include add");
				roiManager("Measure");
				area_sized = 0;
				for (row=0; row<nResults; row++){
					 area_sized = area_sized + d2s(getResult("Area",row),0);
				}
				roiManager("Reset");
				run("Clear Results");
	
				//if quality score is matched go on and get the detections from the other channel
				if(area_sized/area_all >= q_score){
					
					//get the particles including those larger than size limit
					selectWindow("segmenting");
					run("Analyze Particles...", "size="+size_lw+"-Infinity pixel show=Nothing exclude include add");
					//measure the intensity of target (antibody) signal in these particles
					selectWindow(channels[1]+image_name);
					rename("measure");
					roiManager("Measure");
	
					//write to results file
					for (row=0; row<nResults; row++){
						Area = d2s(getResult("Area",row),0);
						Int = d2s(getResult("Mean",row),3);
						roiManager("select", row);
						Roi.getCoordinates(x_local, y_local);
						x_out ="";
						y_out ="";	
							for (c=0; c<x_local.length; c++) {
								x_out = x_out + x_local[c] + ",";
								y_out = y_out + y_local[c] + ",";
							}
					 	result_string = Area+"\t"+Int+"\t"+ID_val+x_out+"\t"+y_out+"\t"+file_list[file]+"\t"+size_up+"\n";
					 	print(CustomResults, result_string);	
					}
					report_string = report_string+File.getName(file_list[file])+"\t"+ID_val+(nResults+1)+"\t"+(area_sized/area_all)+"\t"+"passed"+"\n";
					selectWindow("measure");
					close();
				}else{
					report_string = report_string+File.getName(file_list[file])+"\t"+ID_val+"-"+"\t"+(area_sized/area_all)+"\t"+"failed"+"\n";
				}
				selectWindow("segmenting");
				close();
				
			}
			
			File.close(CustomResults);
			
			//write the report
			
			Report = File.open(File.getParent(dir)+pathSep+analysis_name+pathSep+analysis_name+"_report.txt");
			print(Report, version +" Report:\n");
			print(Report,"Name of analysis:  "+analysis_name+"\n");
			print(Report,"Unsupervised segmentation:  "+automated+"\n");
			if(automated == "Yes"){
					if(thr_method == "manual"){
					print(Report,"Manual thresholding:  "+"\n");
					print(Report,"lower:  "+thr_lw+"\n");
					print(Report,"upper:  "+thr_up+"\n");	
						}else{
							print(Report,"Auto thresholding:  "+"\n");
							print(Report,"method:  "+thr_method+"\n");
							}
				}
			print(Report,"Lower size limit:  "+size_lw+"\n");
			print(Report,"Upper size limit:  "+size_up+"\n");
			print(Report,"Quality score cutoff (fraction of total area with signal used for segmentation):  "+q_score+"\n\n");
			print(Report,"File name"+"\t"+ID_name+"cells"+"\t"+"Quality score"+"\n\n");
			print(Report, report_string);
			File.close(Report);			
		}else{
			showMessage("Error", "Directory for structured data, well... not correctly structured ");
			}; //end of analysis as checked by the fact that all files are in the same 'depth' in the structured data directory
			
		//sub dialog
		Radio_Choice = newArray(start_over, fin);
		Dialog.create("Analysis of structured data set");
			Dialog.addMessage("   Analysis: "+analysis_name);
			Dialog.addMessage("   finished!");
			Dialog.addMessage("   Choose what to do:");
			Dialog.setInsets(5, 50, 5);
			Dialog.addRadioButtonGroup("", Radio_Choice, 5, 1,fin);
		Dialog.show();
		analysis_choice = Dialog.getRadioButton();
		
		//reset settings if wanted
		if(analysis_choice == fin){
			channels = channels_init; 
			thr_method = thr_method_init;
			thr_lw = thr_lw_init;
			thr_up = thr_lw_init;
			size_lw = size_lw_init;
			size_up = size_up_init;
			q_score = q_score_init;
			watershed = watershed_init;
		};
		close("*");
		setBatchMode(false);
		
	}while(analysis_choice == start_over);
};


	//Determine how channels are to be used - channels///////////////////////////////////////////////////////////////////////////////////////////////
	if(Script_choice == Channels){

		//sub dialog presets
		start_over = "Start over with modified settings...                             ";
		fin_save = "Proceede and keep settings...";
		fin =    "Finish and discard settings...";
		//control variable
		channels_choice = start_over;
		
		do{
			//to be executed
			
			//make array to assign images to channels
			channels_images = newArray(channels.length);
			//close all open images
			close("*");
			//open multichannel image and split channels
			waitForUser("Open Composite image");
			image_path = File.openDialog("Select a File");
			open(image_path);
			run("Split Channels");
			//check if channels from presets are there in the channels generated at this point
			images = getList("image.titles");
			counter = newArray(channels.length);

			if(images.length >= channels.length){
				for (i=0; i<channels.length; i++){	
					for (ii=0; ii<images.length; ii++){
						if(substring(images[ii], 0, 3) == channels[i]){
							counter[i] = counter[i] + 1;
							channels_images[i] = images[ii];
						};
					};
				};
			}else{
				showMessage("Error", "Not enough channels in Composite image");
				};
			Array.getStatistics(counter, min, max, mean, std);
			
			//if they are there go on here
			if(min==1 && max==1){

				selectWindow(channels_images[0]);
				run("Duplicate...", "title=");
				rename("Image for segmentation (DNA)");

				selectWindow(channels_images[1]);
				run("Duplicate...", "title=");
				rename("Image for analysis (e.g. antibody staining)");

			//else go on here
				}else{
		
					waitForUser("Select image for segmentation (DNA)");
					channels[0] = substring(getTitle(), 0, 3);
					run("Duplicate...", "title=");
					rename("Image for segmentation (DNA)");
					waitForUser("Select image for analysis (e.g. antibody staining)");
					channels[1] = substring(getTitle(), 0, 3);
					run("Duplicate...", "title=");
					rename("Image for analysis (e.g. antibody staining)");
				};

			//check the selection before going on
			waitForUser("check your selection and then go on");

			//sub dialog
			Radio_Choice = newArray(start_over, fin_save, fin);
			Dialog.create("Open prototype image, determine which channel is used for segmentation (DNA signal)");
				Dialog.addMessage("   Settings:                  ");
				Dialog.addString ("Prefix for segmentation channel (e.g. DNA):", channels[0]);
				Dialog.addString ("Prefix for measurement channel (e.g. Ab):   ", channels[1]);
				Dialog.addMessage("\n");
				Dialog.addMessage("   Choose what to do:");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", Radio_Choice, 5, 1,fin_save);
			Dialog.show();
			channels[0] = Dialog.getString();
			channels[1] = Dialog.getString();
			channels_choice = Dialog.getRadioButton();
		//if to start over with new settings
		if(channels_choice == start_over){
			close("*"); 
			};
		
		}while(channels_choice == start_over);
		//close images
		for (i=0; i<images.length; i++){
				selectWindow(images[i]);
				close();
		};
		//clear channels if presets are not to be remembered and close all images
		if(channels_choice == fin){
			channels = channels_init;
			close("*"); 
			};	
	};
	

		
	//Determine DNA signal and size thresholding//////////////////////////////////////////////////////////////////////////////////////////////////////
	if(Script_choice == Segmentation){

		//sub dialog presets
		start_over = "Start over and modify settings...                             ";
		fin_save = "Proceede and keep settings...";
		fin =    "Finish and discard settings...";
		//control variable
		intensity_choice = start_over;

		//Determine DNA signal thresholding 
		do{
			//to be executed
			//ask to select an image
			waitForUser("Open or select SINGLE CHANNEL image for segmentation");
			
			//if no images are open open one
			if(nImages == 0){
				image_path = File.openDialog("Select a File");
				open(image_path);
				//if Composite image, split the channels
				if(is("composite")){
					run("Split Channels");
					waitForUser("Open or select SINGLE CHANNEL image for segmentation");
					};	
			};
			
			//check if selected image is single channel than make 8bit
			while(is("composite")){
				waitForUser("Open or select SINGLE CHANNEL image for segmentation");
				};
			run(mode);
			
			//duplicate for mapping of particles later on
			run("Duplicate...", "title=");
			rename("Particle mapping");	
			//prepare for thresholding
			run("Duplicate...", "title=");
			rename("Adjust threshold");
			run(mode);
			run("Grays");
			run("Duplicate...", "title=");
			rename("control");
			//threshold iomage
			selectWindow("Adjust threshold");
			run("Threshold...");
			waitForUser("Click OK whe finished with thresholding, no need to press 'Set'... ");
			//read the threshold and compare to result of the "purely automatic" version - was manual treshold set? 
			getThreshold(low, upp);
			thr_method = getInfo("threshold.method");
			selectWindow("control");
			setAutoThreshold(thr_method+" dark");
			getThreshold(auto_low, auto_upp);
			close();

			if(low==auto_low && upp==auto_upp){
				thr_string = "automatic - "+thr_method+" dark";
				thr_lw = 0;
				thr_up = 0;
			}else{
				thr_string = "manual - "+low+", "+upp;
				thr_method = "manual";
				thr_lw = low;
				thr_up = upp;
			};

			//make a mask
			selectWindow("Adjust threshold");
			setOption("BlackBackground", false);
			run("Convert to Mask");

			//go through the size selection
			size_choice = start_over;
			do{
				//ask for size limits and whether to use watershed
				MSingle = newArray("Yes", "No");
				Dialog.create("Size selection of particles");
					Dialog.addMessage("   Select lower and upper limits for individual nuclei/NETs:      ");
					Dialog.addMessage("\n");
					Dialog.addNumber("lower size limit:", size_lw); 
					Dialog.addNumber("upper size limit:", size_up);
					Dialog.addMessage("\n");
					Dialog.addMessage("Use Watershed?");
					Dialog.setInsets(5, 50, 5);
					Dialog.addRadioButtonGroup("", MSingle, 1, 2, watershed); 
				Dialog.show();
				size_lw = Dialog.getNumber();
				size_up = Dialog.getNumber();
				watershed =  Dialog.getRadioButton();

				//run the particle analysis
				roiManager("Reset");
				selectWindow("Adjust threshold");

				//run Watershed if selected
				if (watershed == "Yes"){
					run("Duplicate...", "title=");
					rename("Watershed");
					run("Watershed");
				};
			
				//do the particle analysis
				run("Analyze Particles...", "size="+size_lw+"-"+size_up+" pixel show=Nothing exclude include add");
				
				//remove the Watershed image if made
				if (watershed == "Yes"){
					selectWindow("Watershed");
					close();
				};
				
				//show the particle analysis
				selectWindow("Particle mapping");
				roiManager("Show All without labels");
				selectWindow("Particle mapping");

				//ask if try again
				Radio_Choice = newArray(start_over, fin_save);
				Dialog.create("Size selection parameters");
					Dialog.addMessage("   Lower size limit for particles:      "+size_lw);
					Dialog.addMessage("   Upper size limit for particles:      "+size_up);
					Dialog.addMessage("\n");
					Dialog.addMessage("   Choose what to do:");
					Dialog.setInsets(5, 50, 5);
					Dialog.addRadioButtonGroup("", Radio_Choice, 5, 1,start_over);
				Dialog.show();
				size_choice = Dialog.getRadioButton();
			}while(size_choice == start_over);

			//sub dialog
			Radio_Choice = newArray(start_over, fin_save, fin);
			Dialog.create("Intensity and size parameters for image segmentation");
				Dialog.addMessage("   Intensity threshold settings:      "+thr_string);
				Dialog.addMessage("   Lower size limit for particles:      "+size_lw);
				Dialog.addMessage("   Upper size limit for particles:      "+size_up);
				Dialog.addMessage("\n");
				Dialog.addMessage("   Choose what to do:");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", Radio_Choice, 5, 1,fin_save);
				Dialog.show();
			intensity_choice = Dialog.getRadioButton();
			//if to start over close some images
			if(intensity_choice == start_over){
				selectWindow("Adjust threshold");
				close();
				selectWindow("Particle mapping");
				close();
			};
			
		}while(intensity_choice == start_over);
		//clear presets are not to be remembered and close all images
		close("*");
		if(intensity_choice == fin){
			thr_method = thr_method_init;
			thr_lw = thr_lw_init;
			thr_up = thr_up_init;
			size_lw = size_lw_init;
			size_up = size_up_init;
			watershed = watershed_init;
		};		
	};

		
	//Run the entire thing and check whether it is working///////////////////////////////////////////////////////////////////////////////////////////////
	if(Script_choice == Review){

		//sub dialog presets
		start_over = "Start over and modify settings...                             ";
		next_file = "Check another image...";
		fin_save = "Proceede and keep settings...";
		fin =    "Finish and discard settings...";
		//control variable
		review_choice = next_file;

		//Run the entire thing on a test image
		do{
			//dialoge to fill in parameters, use presets from previous script parts if available
			MSingle = newArray("Yes", "No");
			Dialog.create("Configuration individual image test run");
				Dialog.addMessage("Assignment of channels:                                                                       ");
				Dialog.setInsets(5, 50, 5);
				Dialog.addString ("Prefix for segmentation channel (e.g. DNA):", channels[0]);
				Dialog.addString ("Prefix for measurement channel (e.g. Ab):   ", channels[1]);
				Dialog.addMessage("\n");
				Dialog.addMessage("Method for segmentation:               ");
				Dialog.setInsets(5, -250, 5);
				Dialog.addChoice("", thr_method_list, thr_method);
				Dialog.addNumber("Manual threshold lower intensty limit:", thr_lw); 
				Dialog.addNumber("Manual threshold upper intensty limit:", thr_up); 
				Dialog.addMessage("\n");
				Dialog.addMessage("Particle size selection criteria:               ");
				Dialog.addNumber("Lower size limit:", size_lw); 
				Dialog.addNumber("Upper size limit:", size_up);
				Dialog.addMessage("\n");
				Dialog.addMessage("Use Watershed?");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", MSingle, 1, 2, watershed); 
				Dialog.addMessage("\n");
			Dialog.show();
			channels[0] = Dialog.getString();
			channels[1] = Dialog.getString();
			thr_method = Dialog.getChoice();
			thr_lw = Dialog.getNumber() ;
			thr_up = Dialog.getNumber() ;
			size_lw = Dialog.getNumber() ;
			size_up = Dialog.getNumber() ;
			watershed =  Dialog.getRadioButton();

			//close all open images
			close("*");
			
			//select multichannel image if not already selected 
			if(review_choice == next_file){
				waitForUser("Open multichannel image");
				image_path = File.openDialog("Select a File");
			};
			//open multichannel image and split channels
			open(image_path);
			image_name = getTitle();
			run(mode);
			run("Split Channels");
			
			//do the thresholding and size selection with the 'DNA' image
			selectWindow(channels[1]+image_name);
			rename("Image for analysis (e.g. antibody staining)");
			selectWindow(channels[0]+image_name);
			rename("Image for segmentation (DNA)");
			run("Duplicate...", "title=");
			rename("segmenting");

			//set the threshold
			if(thr_method == "manual"){
				setThreshold(thr_lw, thr_up);	
					}else{
						setAutoThreshold(thr_method+" dark");
						}

			//make mask and analyse particles
			roiManager("Reset");
			setOption("BlackBackground", false);
			run("Convert to Mask");

			//run Watershed if selected
			if (watershed == "Yes"){
				run("Watershed");
			};
			
			run("Analyze Particles...", "size="+size_lw+"-"+size_up+" pixel show=Nothing exclude include add");
			selectWindow("segmenting");
			close();
			selectWindow("Image for segmentation (DNA)");
			roiManager("Show All without labels");
			selectWindow("Image for analysis (e.g. antibody staining)");
			roiManager("Show All without labels");

			//allow review
			//waitForUser("Click OK when finished with review");

			//sub dialog
			Radio_Choice = newArray(start_over, fin_save, next_file, fin);
			Dialog.create("Individual image test run");
				Dialog.addMessage("   Choose what to do:");
				Dialog.setInsets(5, 50, 5);
				Dialog.addRadioButtonGroup("", Radio_Choice, 5, 1,fin_save);
				Dialog.show();
			review_choice = Dialog.getRadioButton();
		close("*");
		}while(review_choice == start_over || review_choice == next_file)
		//close all images and discard settings in case
		if(review_choice == fin){
			thr_method = thr_method_init;
			thr_lw = thr_lw_init;
			thr_up = thr_up_init;
			size_lw = size_lw_init;
			size_up = size_up_init;
			watershed = watershed_init;
		};
	};
		
}while(Script_choice != Fin);







	