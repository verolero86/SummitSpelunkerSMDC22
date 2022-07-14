import java.io.*;
import java.util.*;

public class FileParser {
    final static int totalMemory = 601183424;
    
    public static void main(String[] args) {
        //Directory to be inputed
        File directory = new File("C:\\2022SDCDataChallengeFiles\\summit-data-pseudonymized");
        
        List<String> outputList = new ArrayList<String>();
        
        //CSV Output File
        File csvOutput = new File("C:\\2022SDCDataChallengeFiles\\SummitData.csv");
        
        BufferedReader reader = null;
        BufferedWriter writer = null;
        
        File[] files = directory.listFiles();
        parseFile(files, reader, writer, outputList, csvOutput);
          
    }
    
    private static void parseFile(File[] files, BufferedReader reader, 
                            BufferedWriter writer, List<String> outputList, File csvOutput) {
        for (File file : files) {
            if (file.isDirectory()) {
                System.out.println("Directory: " + file.getAbsolutePath());
                parseFile(file.listFiles(), reader, writer, outputList, csvOutput);
            }
            
            if (file.isFile()) {
                try {
                    reader = new BufferedReader(new FileReader(file));
                    
                    String monthAndDay = file.getName();
                    
                    String[] fileTitle = monthAndDay.split("\\.");
                    monthAndDay = fileTitle[fileTitle.length - 2];
                    outputList.add(monthAndDay);
                    
                    String loginNode = fileTitle[0];
                    outputList.add(loginNode);

                    int memoryUsed = 0;
                    
                    //Values for storing number of running & total jobs
                    int runningJobs = 0;
                    int totalJobs = 0;
                    boolean jobSection = false;
                    
                    //Values for storing number of running procs
                    int totalProcs = 0;
                    boolean runningProcSection = false;
                    
                    //Boolean for marking colored ls section
                    boolean lsColoredSection = false;
                    
                    //Boolean for marking unaliased ls section
                    boolean lsUnaliasedSection = false;
                    
                    //Boolean for marking gpfs time to 1 GB section
                    boolean gpfsSection = false;
                    
                    //Boolean for marking disk util section
                    boolean diskUtilSection = false;
                    
                    String line = reader.readLine();
                    String[] splitLine = null;
                    while (line != null) {
                        
                        //Upon seeing the time marker line, add to temporary list
                        if (line.contains("Hour")) {
                            outputList.add(line);
                        
                        //Upon seeing the number of users line, add user number to temporary list
                        } else if (line.contains("users,") && !line.contains("top")) {
                            splitLine = line.split(",");
                            if (splitLine.length == 6) {
                                line = splitLine[2];
                                outputList.add(line); //adding the number of users
                                line = splitLine[3];
                                line = line.replaceAll("load average:", "");
                                outputList.add(line); //adding the CPU load average for 1 minute
                                line = splitLine[4];
                                outputList.add(line); //adding the CPU load average for 5 minutes
                                line = splitLine[5];
                                outputList.add(line); //adding the CPU load average for 15 minutes
                            } else if (splitLine.length == 5) {
                                line = splitLine[1];
                                outputList.add(line); //adding the number of users
                                line = splitLine[2];
                                line = line.replaceAll("load average:", "");
                                outputList.add(line); //adding the CPU load average for 1 minute
                                line = splitLine[3];
                                outputList.add(line); //adding the CPU load average for 5 minutes
                                line = splitLine[4];
                                outputList.add(line); //adding the CPU load average for 15 minutes
                            }
                        
                        } else if (line.contains("1 user,") && !line.contains("top")) {
                            splitLine = line.split(",");
                            if (splitLine.length == 6) {
                                line = splitLine[2];
                                outputList.add(line); //adding the number of users
                                line = splitLine[3];
                                line = line.replaceAll("load average:", "");
                                outputList.add(line); //adding the CPU load average for 1 minute
                                line = splitLine[4];
                                outputList.add(line); //adding the CPU load average for 5 minutes
                                line = splitLine[5];
                                outputList.add(line); //adding the CPU load average for 15 minutes
                            } else if (splitLine.length == 5) {
                                line = splitLine[1];
                                outputList.add(line); //adding the number of users
                                line = splitLine[2];
                                line = line.replaceAll("load average:", "");
                                outputList.add(line); //adding the CPU load average for 1 minute
                                line = splitLine[3];
                                outputList.add(line); //adding the CPU load average for 5 minutes
                                line = splitLine[4];
                                outputList.add(line); //adding the CPU load average for 15 minutes
                            }
                        
                        //If see the top of the proc section, set the section bool to true
                        } else if (line.contains("LOGIN@")) {
                            runningProcSection = true;
                        
                        //If see the end of the proc section, set the section bool to false
                        } else if (line.contains("endw") && runningProcSection) {
                            runningProcSection = false;
                            line = String.valueOf(totalProcs) + " procs";
                            outputList.add(line); //adds the total number of procs
                            totalProcs = 0;
                        
                        //If in but not at end of proc section, mark one for each line
                        } else if (runningProcSection) {
                            totalProcs++;
                            
                        //Upon seeing the amount of available memory, subtract from total to get used memory
                        } else if (line.contains("MemAvailable:")) {
                            splitLine = line.split("\\s+");
                            line = splitLine[1];
                            memoryUsed = totalMemory - Integer.parseInt(line);
                            line = String.valueOf(memoryUsed);
                            outputList.add(line);
                            
                        //Upon seeing the time-to-ls unaliased section, find the real time
                        } else if (line.contains("home response time unaliased") && !line.contains("endhome")) {
                            lsUnaliasedSection = true;
                            
                        } else if (line.contains("real") && lsUnaliasedSection) {
                            splitLine = line.split("\\s+");
                            line = splitLine[1];
                            outputList.add(line);
                            lsUnaliasedSection = false;
                            
                        //Upon seeing the time-to-ls colored section, find the real time
                        } else if (line.contains("home response time colored ls") && !line.contains("endhome")) {
                            lsColoredSection = true;
                            
                        } else if (line.contains("real") && lsColoredSection) {
                            splitLine = line.split("\\s+");
                            line = splitLine[1];
                            outputList.add(line);
                            lsColoredSection = false;
                            
                        } else if (line.contains("JOBID")) {
                            jobSection = true;
                            
                        } else if (line.contains("endbjobs") && jobSection) {
                            jobSection = false;
                            line = String.valueOf(runningJobs) + " jobs";
                            outputList.add(line);
                            line = String.valueOf(totalJobs) + " jobs";
                            outputList.add(line);
                            runningJobs = 0;
                            totalJobs = 0;
                            
                        } else if (jobSection) {
                            if (line.contains("RUN")) {
                                runningJobs++;
                            }
//                            splitLine = line.split("\\s+");
//                            line = splitLine[2];
//                            if (line.equals("RUN")) {
//                                runningJobs++;
//                            }
                            
                            totalJobs++;
                            
                        } else if (line.contains("gpfs scratch response") && !line.contains("endgpfs")) {
                            gpfsSection = true;
                            
                        } else if (line.contains("real") && gpfsSection) {
                            
                            splitLine = line.split("\\s+");
                            line = splitLine[1];
                            outputList.add(line);
                            gpfsSection = false;
                         
                        } else if (line.contains("fail") && gpfsSection) {
                            outputList.add("-1");
                            gpfsSection = false;
                            
                        } else if (line.contains("df --") && !line.contains("enddf --")) {
                            diskUtilSection = true;
                            
                        } else if (line.contains("rootfs") && diskUtilSection) {
                            splitLine = line.split("\\s+");
                            line = splitLine[4];
                            outputList.add(line);
                            diskUtilSection = false;
                        }
                        
                        line = reader.readLine();
                    }
                    
                    reader.close();
                    
                    System.out.println(monthAndDay);
                    writer = new BufferedWriter(new FileWriter(csvOutput));
                    
                    for (String s : outputList) {
                        
                        if (s.contains("_")) {
                            writer.newLine();
                        }
                        
                        writer.write(",");
                        writer.write(s);
                    }
                    
                    writer.flush();
                    writer.close();
                } catch (Exception e) {
                    e.printStackTrace();
                } finally {
                    try {
                        reader.close();
                        writer.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

}
