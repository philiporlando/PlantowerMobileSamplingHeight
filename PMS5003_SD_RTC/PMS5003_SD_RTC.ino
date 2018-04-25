//******************************
//*Abstract: Firmware for mobile pm monitor 
//*Team: Sustainable Atmospheres Research Lab
//*Author：Zuyang @ HUST
//*Modified by Cain for Arduino Hardware Serial port compatibility
//*Modified by Bennett Battaile, Meenakshi Rao, & Philip Orlando
//*Date：April.25.2018
//******************************

// sourcing dependencies
#include <Arduino.h>
#include <SD.h>
#include <SPI.h>
#include <Wire.h>
#include <TimeLib.h>
//#include <DS3232RTC.h>     // we're using the DS3231 module, but the DS1307 library is what works...
#include <DS1307RTC.h>       // we're using Paul's DS1307RTC library instead
#define LENG 31             // 0x42 + 31 bytes equal to 32 bytes

// declaring variables for interfacing plantower serial data
unsigned char buf[LENG];                // plantower buffer

// declaring variables for SD module's SPI protocol
const int chipSelect = 10;              // SD card chip select for SPI protocol 

// declaring variables for 1-second loop
//const unsigned long PERIOD = 1*1*1000UL;  // time-resolution (log once every 1 second)
#define PERIOD 1*1*1000L
unsigned long target_time = 0L;         // counter variable for 1-second loop

// variables for date in file name 
char filename[12] = {0};    // character array variable to store our converted date string for SD.open()
File dataFile;              // file object
tmElements_t tm;


// mass density variables (micrograms per cubic meter)
int PM1_0S = 0;             //define PM1.0 standard value of the PMS5003
int PM10_0S = 0;            //define PM10 standard value of the PMS5003
int PM2_5S = 0;             //define PM2.5 standard value of the PMS5003
int PM1_0A = 0;             //define PM1.0 atmospheric value of the PMS5003
int PM2_5A = 0;             //define PM2.5 atmospheric value of the PMS5003
int PM10_0A = 0;            //define PM10 atmospheric value of the PMS5003

// number density variables (particles per 100 mL)
int PNC0_3=0;        //define PNC0.3 value of the PMS5003
int PNC0_5=0;        //define PNC0.5 value of the PMS5003
int PNC1_0=0;        //define PNC1.0 value of the PMS5003
int PNC2_5=0;        //define PNC2.5 value of the PMS5003
int PNC5_0=0;        //define PNC5.0 value of the PMS5003
int PNC10_0=0;       //define PNC10.0 value of the PMS5003


void setup()
{
//initialize serial ports
  Serial.begin(9600);  
  Serial.setTimeout(1500);    //set the Timeout to 1500ms, longer than the data transmission periodic time of the sensor

  Serial1.begin(9600);         
  Serial1.setTimeout(1500);    //set the Timeout to 1500ms, longer than the data transmission periodic time of the sensor

  setSyncProvider(RTC.get);

//RTC.set(now()); // set the RTC from system time // BB

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
    // don't do anything more:
    return;
  }
  Serial.println("card initialized.");
}




void loop()
{

  /*
  if (millis () - target_time >= PERIOD)
  {
    target_time += PERIOD; //change the scheduled time exactly, no slippage will occur
    /// do stuff
  */

    String dataString = "";
    tmElements_t tm;
      
    if(Serial1.find("B")){    //start to read when detect 0x42 (firststart byte characterA)
      Serial1.readBytes(buf,LENG);
  
      // begin data acquisition loop if the second start character is detected immediately after the first ("M")
      if(buf[0] == 0x4d){
        if(checkValue(buf,LENG)){
          PM1_0S=transmitPM1_0S(buf);     //count PM1.0 standard value of the pms5003 
          PM2_5S=transmitPM2_5S(buf);     //count PM2.5 standard value of the pms5003 
          PM10_0S=transmitPM10_0S(buf);   //count PM10 standard value of the pms5003
          PM1_0A=transmitPM1_0A(buf);     //count PM1.0 atmospheric value of the pms5003 
          PM2_5A=transmitPM2_5A(buf);     //count PM2.5 atmospheric value of the pms5003 
          PM10_0A=transmitPM10_0A(buf);   //count PM10 atmospheric value of the pms5003  
          PNC0_3=transmitPNC0_3(buf);     //count pnc 0_3 value of the pms5003
          PNC0_5=transmitPNC0_5(buf);     //count pnc 0_5 value of the pms5003
          PNC1_0=transmitPNC1_0(buf);     //count pnc 1_0 value of the pms5003 
          PNC2_5=transmitPNC2_5(buf);     //count pnc 2_5 value of the pms5003
          PNC5_0=transmitPNC5_0(buf);     //count pnc 5_0 value of the pms5003
          PNC10_0=transmitPNC10_0(buf);   //count pnc 10_0 value of the pms5003
        }
      }
      
    /*
    static unsigned long OledTimer=millis();  
      if (millis() - OledTimer >=1000) 
      {
        OledTimer=millis(); 
    */
  
  // need to add condition for run the loop each second, sync with the datetime seconds!
  
        //append our transmit buf output to the dataString variable in the order that they're acquired
        dataString += String(PM1_0S);
        dataString += ",";
        dataString += String(PM2_5S);
        dataString += ",";
        dataString += String(PM10_0S);
        dataString += ",";
        dataString += String(PM1_0A);
        dataString += ",";
        dataString += String(PM2_5A);
        dataString += ",";
        dataString += String(PM10_0A);
        dataString += ",";
        dataString += String(PNC0_3);
        dataString += ",";
        dataString += String(PNC0_5);
        dataString += ",";
        dataString += String(PNC1_0);
        dataString += ",";
        dataString += String(PNC2_5);
        dataString += ",";
        dataString += String(PNC5_0);
        dataString += ",";
        dataString += String(PNC10_0);
        //dataString += ",";
        
  
       String dateTime = "";
       
       if (RTC.read(tm)) {
          dateTime += as2digits(tm.Hour);
          dateTime += ':';
          dateTime += as2digits(tm.Minute);
          dateTime += ':';
          dateTime += as2digits(tm.Second);
  
          dateTime = String(tmYearToCalendar(tm.Year)) + "-" + 
                     String(tm.Month) + "-" +
                     String(tm.Day) + " " +
                     as2digits(tm.Hour) + ":" +
                     as2digits(tm.Minute) + ":" +
                     as2digits(tm.Second);
  
       } 
       
       else {
             Serial.println("DS3231 read error!  Please check the circuitry.");
             Serial.println();
       }
  
    //printing datetime to serial monitor
    dateTime = "";
    dateTime += year();
    dateTime += "-" + as2digits(month()) + "-" + as2digits(day()) + " " 
             + as2digits(hour()) + ":" + as2digits(minute()) + ":" + as2digits(second());
    // dateTime += "X";
    //Serial.print("[PM1.0  :  PM2.5  :  PM10] ug/m3  ");  
    //Serial.print(" [ ");
    Serial.print(dateTime);
    Serial.print(',');
    Serial.print(dataString);  
    //Serial.println("  ]");
    Serial.println("");

    // call our file management functions
    getFileName();
    createFileName();

    // check if dataFile exists:
    //Serial.println("dataFile before: " + String(dataFile));
    //Serial.println("sd.exists: " + String(SD.exists(filename)));

    // open dataFile
    dataFile = SD.open(filename, FILE_WRITE);
    //Serial.println("dataFile after: " + String(dataFile));
    
    // if the file is available, write to it:
    if (dataFile) {
      dataFile.println(dateTime + ", " + dataString);
      dataFile.close();
    }  
    // if the file isn't open, pop up an error:
    else {
      Serial.println("error opening" + String(filename));
      } 
    }
  //}   // end of OLEDtimer loop
    
  //}   // end of target time loop
  // do other stuff...?
}


// check sum function
char checkValue(unsigned char *thebuf, char leng)
{  
  char receiveflag=0;
  int receiveSum=0;

  for(int i=0; i<(leng-2); i++){
  receiveSum=receiveSum+thebuf[i];
  }
  receiveSum=receiveSum + 0x42;
 
  if(receiveSum == ((thebuf[leng-2]<<8)+thebuf[leng-1]))  //check the serial data 
  {
    receiveSum = 0;
    receiveflag = 1;
  }
  return receiveflag;
}


//transmit the PM 1.0 Standard value to PC
int transmitPM1_0S(unsigned char *thebuf)
{
  int PM1_0Standard;
  PM1_0Standard=((thebuf[3]<<8) + thebuf[4]); //count PM1.0 value of the air detector module
  return PM1_0Standard;
}

//transmit PM 2.5 Standard value to PC
int transmitPM2_5S(unsigned char *thebuf)
{
  int PM2_5Standard;
  PM2_5Standard=((thebuf[5]<<8) + thebuf[6]);//count PM2.5 value of the air detector module
  return PM2_5Standard;
  }

//transmit PM 10.0 Standard value to PC
int transmitPM10_0S(unsigned char *thebuf)
{
  int PM10_0Standard;
  PM10_0Standard=((thebuf[7]<<8) + thebuf[8]); //count PM10 value of the air detector module  
  return PM10_0Standard;
}


//transmit the PM 1.0 Atmospheric value to PC
int transmitPM1_0A(unsigned char *thebuf)
{
  int PM1_0Atmosphere;
  PM1_0Atmosphere=((thebuf[9]<<8) + thebuf[10]); //count PM1.0 value of the air detector module
  return PM1_0Atmosphere;
}

//transmit PM 2.5 Atmospheric value to PC
int transmitPM2_5A(unsigned char *thebuf)
{
  int PM2_5Atmosphere;
  PM2_5Atmosphere=((thebuf[11]<<8) + thebuf[12]);//count PM2.5 value of the air detector module
  return PM2_5Atmosphere;
  }

//transmit PM 10.0 Atmospheric value to PC
int transmitPM10_0A(unsigned char *thebuf)
{
  int PM10_0Atmosphere;
  PM10_0Atmosphere=((thebuf[13]<<8) + thebuf[14]); //count PM10 value of the air detector module  
  return PM10_0Atmosphere;
}


//transmit PNC 0.3 Value to PC
int transmitPNC0_3(unsigned char *thebuf)
{
  int PNC0_3Val;
  PNC0_3Val=((thebuf[15]<<8) + thebuf[16]); //count PM10 value of the air detector module  
  return PNC0_3Val;
}


//transmit PNC 0.5 Value to PC
int transmitPNC0_5(unsigned char *thebuf)
{
  int PNC0_5Val;
  PNC0_5Val=((thebuf[17]<<8) + thebuf[18]); //count PM10 value of the air detector module  
  return PNC0_5Val;
}

//transmit PNC 1.0 Value to PC
int transmitPNC1_0(unsigned char *thebuf)
{
  int PNC1_0Val;
  PNC1_0Val=((thebuf[19]<<8) + thebuf[20]); //count PM10 value of the air detector module  
  return PNC1_0Val;
}

//transmit PNC 2.5 Value to PC
int transmitPNC2_5(unsigned char *thebuf)
{
  int PNC2_5Val;
  PNC2_5Val=((thebuf[21]<<8) + thebuf[22]); //count PM10 value of the air detector module  
  return PNC2_5Val;
}

//transmit PNC 5.0 Value to PC
int transmitPNC5_0(unsigned char *thebuf)
{
  int PNC5_0Val;
  PNC5_0Val=((thebuf[23]<<8) + thebuf[24]); //count PM10 value of the air detector module  
  return PNC5_0Val;
}

//transmit PNC 10.0 Value to PC
int transmitPNC10_0(unsigned char *thebuf)
{
  int PNC10_0Val;
  PNC10_0Val=((thebuf[25]<<8) + thebuf[26]); //count PM10 value of the air detector module  
  return PNC10_0Val;
}


// utility function for digital clock display: prints preceding colon and leading 0
void printDigits(int digits)
{
    
    if(digits < 10)
        Serial.print('0');
    Serial.print(digits);
}

// add leading zero to single digit month and day with this function
String as2digits(int number) {
  String answer = "";
  if (number >= 0 && number < 10) {
    answer += ('0');
  }
  answer += String(number);
  return(answer);
}


// creates a new file name with current date
void getFileName() {

  // store the date in this empty string
  String date = "";

  // check the current day each loop by re-assigning tm object
  tmElements_t tm;


  // check if the RTC is prodiving a timestamp
  if (RTC.read(tm)) {

    // create a string object of today's date
    date = String(tmYearToCalendar(tm.Year)) +
               String(tm.Month) +
               String(tm.Day) + ".txt";

    // convert to character array for use with SD methods
    date.toCharArray(filename, 16);
    Serial.println("filename: " + String(filename));

    }
}


// creates a new file name each day if it doesn't already exist
void createFileName() {
  //Check if file exists?
  if (SD.exists(filename)) {
    //Serial.println("exists"); 
    Serial.println("appending to existing file");
  }
  else {
    //Serial.println("doesn't exist");
    Serial.println("Creating new file.");
    //Serial.println(filename);
    dataFile = SD.open(filename, FILE_WRITE);


    // create a header row when the file is first created
    // using the PurpleAir SD vector names where possible
    String header = "";
    header += "date,";
    header += "pm1_0_atm,";
    header += "pm2_5_atm,";
    header += "pm10_0_atm,";
    header += "pm1_0_cf_1," ;
    header += "pm2_5_cf_1," ;
    header += "pm10_0_cf_1," ;
    header += "p_0_3_um," ;
    header += "p_0_5_um," ;
    header += "pm_1_0_um,";
    header += "pm_2_5_um,"; 
    header += "pm_5_0_um,";
    header += "pm_10_0_um";
     
    dataFile.println(header);
    //Serial.println("dataFile: " + String(dataFile));
    //Serial.println("sd.exists: " + String(SD.exists(filename)));
    Serial.println("");
    Serial.println(header);
    dataFile.close();
  }
}



