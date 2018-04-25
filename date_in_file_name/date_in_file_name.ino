#include <SD.h>
#include <DS1307RTC.h>
#include <string.h>
#include <SPI.h>
#include <Wire.h>
#include <TimeLib.h>


const int chipSelect = 10;  // chip select pin for teensy with SD module 
char filename[12] = {0};    // character array variable to store our converted date string for SD.open()
File dataFile;              // file object
tmElements_t tm;

void setup()
{
//initialize serial ports
  Serial.begin(9600);  
  Serial.setTimeout(1500);    //set the Timeout to 1500ms, longer than the data transmission periodic time of the sensor

  Serial1.begin(9600);         
  Serial1.setTimeout(1500);    //set the Timeout to 1500ms, longer than the data transmission periodic time of the sensor

  setSyncProvider(RTC.get);

  if(timeStatus() != timeSet)
    Serial.println("Unable to sync with RTC");
  else
    Serial.println("RTC has set the system time");

  //lines through Serial.println("card initialized") from Teensy Datalogger example
  //to initialize the SD card, which is on the Teensy Audio board
  //chipSelect should be set to 10

  Serial.print("Initializing SD card...");
  SPI.setMOSI(11);
  SPI.setMISO(12);
  SPI.setSCK(13); 
  
  // see if the card is present and can be initialized:
  if (!SD.begin(chipSelect)) {
    Serial.println("Card failed, or not present");
    //while (1); // wait until card is recognized...
    return;
  }
  Serial.println("card initialized.");
}

void loop() {
  getFileName();
  createFileName();
  //Serial.println("writing file name");
  delay(1000);
}

// creates a new file name with today's date
void getFileName() {

  // store the date in this empty string
  String date = "";

  // check the day each loop
  tmElements_t tm;

 
  if (RTC.read(tm)) {

    // SD library only supports 8 character file name and 4 character extension
    date = String(tmYearToCalendar(tm.Year)) +
               String(tm.Month) +
               String(tm.Day) + ".txt";

    date.toCharArray(filename, 16);

    Serial.println("filename: " + String(filename));

    //dataFile = SD.open(str, FILE_WRITE);
    }
}


// creates a new file name each day if it doesn't already exist
void createFileName() {
  //Check if file exists?
  if (SD.exists(filename)) {
    Serial.println("exists"); 
    Serial.println("appending to existing file");
  }
  else {
    Serial.println("doesn't exist");
    Serial.println("Creating new file.");
    Serial.println(filename);
    dataFile = SD.open(filename, FILE_WRITE);
    Serial.println("dataFile: " + String(dataFile));
    Serial.println("sd.exists: " + String(SD.exists(filename)));
    Serial.println("");
    dataFile.close();
  }
}

// adds leading zeros to single digit month and day characters
String as2digits(int number) {
  String answer = "";
  if (number >= 0 && number < 10) {
    answer += ('0');
  }
  answer += String(number);
  return(answer);
}


