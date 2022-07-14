#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cstdlib>
#include <vector>

using namespace std;

vector<string> INFO(17);

//output of w command
void wComm(ifstream& file, ofstream& out) {

  bool first = true;	
  string line;
  ofstream out2;
  out2.open("wComm.txt");
  
  while(getline(file, line)) {
    if (line != "endw --") {
      out2 << line << '\n';
      if(first){
        first = false;
	string users, trash;
	char comma;
	float avg1, avg5, avg15;
        size_t pos = ((string)line).find("user");
        string node = ((string)line).substr(pos - 4);
	istringstream ss (node);
	ss >> users >> trash >> trash >> trash >> avg1 >> comma >> avg5 >> comma >> avg15;
	//ss >> trash >> trash >> trash >> trash >> trash >> comma  >> users >> trash >> trash >> trash >> avg1 >> comma >> avg5 >> comma >> avg15;
        INFO.at(3) = users;
	//out << "Logged Users: " << users << '\n';
	INFO.at(5) = to_string(avg1);
	//out << "1 min Avg Load: " << avg1 << '\n';
	INFO.at(6) = to_string(avg5);
	//out << "5 min Avg Load: " << avg5 << '\n';	
	INFO.at(7) = to_string(avg15);
	//out << "15 min Avg Load: " << avg15 << '\n';
      }
    }
    else 
      return;
  }

}

//contents of /proc/meminfo
void meminfo(ifstream& file, ofstream& out) {
  
  int count = 1;
  int total, avail;  
  string line;
  ofstream out2;
  out2.open("meminfo.txt");
  
  while(getline(file, line)) {
    if (line != "endmeminfo --"){
      out2 << line << '\n';
      if(count == 1 || count == 3) {
	string trash;
	istringstream ss(line);
	if (count == 1){
          ss >> trash >> total;
	  INFO.at(8) = to_string(total);
          //out << "Total mem: " << total << '\n';
	}
	if (count == 3){
          ss >> trash >> avail;
          INFO.at(9) = to_string(avail);
	  //out << "Avail mem: " << avail << '\n';
	}
      }
      count++;
    }
    else {
      INFO.at(10) = to_string(total - avail);	    
      //out << "Used mem: " << total - avail << '\n';
      return;
    }
  }


}

//contents of /proc/vmstat
void vmstat(ifstream& file){
  
  string line;
  ofstream out;
  out.open("vmstat.txt");

  while(getline(file, line)) {
    if (line != "endvmstat --")
      out << line << '\n';
    else
      return;
  }

}

//output of ps aux command
void psaux(ifstream& file, ofstream& out) {

  int numProc = -1;
  string line;
  ofstream out2;
  out2.open("psaux.txt");

  while(getline(file, line)) {
    if (line != "endps aux --"){
      out2 << line << '\n';
      numProc++;
    }
    else {
      INFO.at(4) = to_string(numProc);
      //out <<"Processes: " <<  numProc << '\n';
      return;
    }
  }

}

//output of top command
void top(ifstream& file) {

  string line;
  ofstream out;
  out.open("top.txt");

  while(getline(file, line)) {
    if (line != "endtop -n 1 -bc | awk !~/u1776/ --")
      out << line << '\n';
    else
      return;
  }

}

//info on all jobs currently active in scheduler
void jobs(ifstream& file, ofstream& out) {

  int numJob = 0;
  int numRun = 0;
  string line;
  ofstream out2;
  out2.open("jobs.txt");

  while(getline(file, line)) {
    if (line != "endbjobs -a -u all --") {
      out2 << line << '\n';
      numJob++;
      if(line.find("RUN") != string::npos)
        numRun++;
    }
      //out << line << '\n';
    else{
      INFO.at(14) = to_string(numJob);
      INFO.at(13) = to_string(numRun);
      //out << "Total Jobs: " << numJob << '\n';
      //out << "Running Jobs: " << numRun << '\n';
      return;
    }
  }

}

//time to run unaliased ls command
void unaliased(ifstream& file, ofstream& out) {

  string line;
  ofstream out2;
  out2.open("unaliased.txt");

  while(getline(file, line)) {
    if (line != "endhome response time unaliased ls"){
      out2 << line << '\n';
      if(line.find("real") != string::npos) {
	istringstream ss(line);
        string trash;
        ss >> trash >> line;
	istringstream ss2(line);
	float mins, secs;
	char trash2;
	ss2 >> mins >> trash2 >> secs;
        INFO.at(11) = to_string(mins*60 + secs);
	//out << "Unaliased ls Time: " <<  mins*60.0 + secs << '\n';
      }
    }
    else
      return;
  }

}

//time to run colors ls command
void color(ifstream& file, ofstream& out) {

  string line;
  ofstream out2;
  out2.open("color.txt");

  while(getline(file, line)) {
    if (line != "endhome response time colored ls") {
      out2 << line << '\n';
      if(line.find("real") != string::npos) {
	istringstream ss (line);
        string trash;
        ss >> trash >> line;
	istringstream ss2(line);
	float mins, secs;
	char trash2;
	ss2 >> mins >> trash2 >> secs;
        INFO.at(12) = to_string(mins*60 + secs);
	//out << "Color ls Time: " <<  mins*60.0 + secs << '\n';
      }
    }
    else
      return;
  }

}

//time to create 1g file in gpfs scratch
void gpfstime(ifstream& file, ofstream& out) {

  string line;
  ofstream out2;
  out2.open("gpfstime.txt");

  while(getline(file, line)) {
    if (line != "endgpfs scratch response time to create a 1 G file --"){
      out2 << line << '\n';
      if(line.find("real") != string::npos){
	istringstream ss (line);
        string trash;
        ss >> trash >> line;
	istringstream ss2(line);
	float mins, secs;
	char trash2;
	ss2 >> mins >> trash2 >> secs;
	INFO.at(15) = to_string(mins*60 + secs);
        //out << "GPFS 1G Time: " <<  mins*60.0 + secs << '\n';
      }
    }
    else
      return;
  }

}

//time for df -h command
void df(ifstream& file) {

  string line;
  ofstream out;
  out.open("df.txt");

  while(getline(file, line)) {
    if (line != "enddf --"){
      out << line << '\n';
      if(line.find("rootfs") != string::npos) {
        size_t pos = ((string)line).find("%");
        string temp = ((string)line).substr(pos - 3);
	istringstream ss(temp);
	int usage;
	ss >> usage;
	INFO.at(16) = to_string(usage);
      }
    }
    else
      return;
  }

}

int main(int argc, char** argv) {
  //string hourFile = "hour.txt";
  string temp;
  ifstream file;
  ofstream out;
  file.open(argv[1]);
  out.open("sorted.txt", fstream::app);
  
  //find login node
  size_t pos = ((string)argv[1]).find("login");
  string node = ((string)argv[1]).substr(pos, 6);
  INFO.at(0) = node;
  //out << node << ',';
  
  //find date
  pos = ((string)argv[1]).find("_202");
  string date = ((string)argv[1]).substr(pos-5, 10);
  INFO.at(1) = date;
  //out  << date << ',';

  while(getline(file, temp)) {
    if (temp.find("Hour") != string::npos)
      INFO.at(2) = temp;
      //out << temp << ",";
    else if (temp == "w --")
      wComm(file, out);
    else if (temp == "meminfo --")
      meminfo(file, out);
    else if (temp == "vmstat --")
      vmstat(file);
    else if (temp == "ps aux --")
      psaux(file, out);
    else if (temp == "top -n 1 -bc | awk !~/u1776/ --")
      top(file);
    else if (temp == "bjobs -a -u all --")
      jobs(file, out);
    else if (temp == "home response time unaliased ls --")
      unaliased(file, out);
    else if (temp == "home response time colored ls --")
      color(file, out);
    else if (temp == "gpfs scratch response time to create a 1 G file --")
      gpfstime(file, out);
    else if (temp == "df --")
      df(file);
    else if (temp == "endsnap"){
      for(int i = 0; i < INFO.size()-1; i++)
        out << INFO.at(i) << ',';

        out << INFO.back() << '\n';
    }
  }

  out.close();
  file.close();
}
