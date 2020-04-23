/* ---------- AeroGrow V2.0 ----------
 * This code helps automate the functions of the high pressure pump, RO purifier
 * the numerous solenid valves and the OLED screen within the AeroGrow system.
 * An infrared based remote control is also available to trigger user conrolled events
 * such as draining the reservoir and triggering all nozzle lines for test purposes
 * 
 * Created by: Arjun Suresh & Ashwin Vinoo
 * 
 */

// The libraries needed are mentioned below
#include <IRremote.h>
#include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>
#include <EEPROM.h>

// Defining the pins used long with their action
#define PRESSURE_PUMP 6
#define RO_PURIFIER 2
#define MAIN_LINE_1 7
#define MAIN_LINE_2 5 
#define MAIN_LINE_3 8
#define BLEED_VALVE 9
// These pins are used to read the reed switch signals
#define REED_SWITCH_LOWER 0
#define REED_SWITCH_UPPER 1
// These pins have not been implemented
#define RELAY_2_UNUSED 10
#define RELAY_3_UNUSED 12
// This is the pin to which the IR module is connected
#define IR_PIN 3
// This is the address in the EEPROM to which the system status byte is stored
# define EEPROM_ADDRESS 0
// This is the spary cycle time after which all lines are activated again in order
#define SPRAY_CYCLE_TIME 120000
// This is time gap between the activation of consecutive lines
#define LIINE_SPRAY_GAP 10000
// This is the amount of time each line is sprayed
#define LINE_SPRAY_TIME 5000 
// This is the amount of time that the system waits before switching on the RO purifiers
#define RO_PURIFIER_DELAY 3600000

// Initializing the OLED Display
Adafruit_SSD1306 display(4);

// Initializing the IR Remote object
IRrecv irReceiver(IR_PIN);

// The results of the IR button decoding process is stored here
decode_results infraredRemoteResults;

// This variable will help control the order in which the lines are activated 
byte lineSequence = 0;

// This variables will help control the timing for each line being activated
unsigned long sprayCycleTimer;

// This variable will help identify when the reservoir was last filled
unsigned long lastReservoirFilledTime = 0;

// This variable will help identify when the reservoir bleed was recorded last as incomplete
unsigned long lastBleedIncompleteTime = 0;

// These are the lines that we will be printing on the OLED screen
String oledLine1;
String oledLine2;
String oledLine3;
String oledLine4;

// This indicates the current state of the menu options
byte stateLine1;
byte stateLine2;
byte stateLine3;
byte stateBleed;

void setup() 
{
  Serial.begin(9600);
  Serial.println("Starting");
  // Specifying that the pins are all output
  pinMode(PRESSURE_PUMP, OUTPUT);
  pinMode(RO_PURIFIER, OUTPUT);
  pinMode(MAIN_LINE_1, OUTPUT);
  pinMode(MAIN_LINE_2, OUTPUT);
  pinMode(MAIN_LINE_3, OUTPUT);
  pinMode(BLEED_VALVE, OUTPUT);
  
  // Switching off all the devices connected to the relay board 
  digitalWrite(PRESSURE_PUMP, HIGH);
  digitalWrite(RO_PURIFIER, HIGH);
  digitalWrite(MAIN_LINE_1, HIGH);
  digitalWrite(MAIN_LINE_2, HIGH);
  digitalWrite(MAIN_LINE_3, HIGH);
  digitalWrite(BLEED_VALVE, HIGH);

  // Setting up the analog input pullup resistors
  digitalWrite(A0, INPUT_PULLUP);
  digitalWrite(A1, INPUT_PULLUP);
  
  // initialize the OLED display with its I2C addr of 0x3C
  display.begin(SSD1306_SWITCHCAPVCC, 0x3C); 
  display.clearDisplay();
  display.setTextSize(1);
  display.setTextColor(WHITE);
  
  // Enabling the IR reciever
  irReceiver.enableIRIn(); 

  // Restores the previous system state
  restoreSystemState();

  // updates the OLED screen
  updateDisplayOLED();

  // Initializing the spray cycle timer
  sprayCycleTimer = millis();
}

// This is the loop that runs forever...
void loop() {

  //---------------- LINE SPRAY CONTROL ----------------
  
  // This block ensures that the misting lines are sprayed at regular intervals
  if(lineSequence < 6) {
    lineAction(MAIN_LINE_1, 0);
  } else if(lineSequence < 12) {
    lineAction(MAIN_LINE_2, LIINE_SPRAY_GAP);
  } else if(lineSequence < 18) {
    lineAction(MAIN_LINE_3, LIINE_SPRAY_GAP*2);
  } else if(abs(millis() - sprayCycleTimer) > SPRAY_CYCLE_TIME) {
    sprayCycleTimer = millis();
    lineSequence = 0;
  }

  //---------------- RESERVOIR LEVEL MAINTENANCE ----------------
  // Updates the time the reservoir was last detected as filled
  if(analogRead(REED_SWITCH_UPPER)>50) {
    lastReservoirFilledTime = millis();
    digitalWrite(RO_PURIFIER, HIGH);
  }
  // Activates the RO purifier if the reservoir has not been detected as filled in the last 1 hour
  else if(abs(millis()-lastReservoirFilledTime)> RO_PURIFIER_DELAY && stateBleed==0) {
    digitalWrite(RO_PURIFIER, LOW);
  }

  //---------------- RESERVOIR BLEED CONTROL ----------------
  
  // Updating the time that the system was last detected to have water levels above the lower reed switch
  if(analogRead(REED_SWITCH_UPPER)>50) {
    lastBleedIncompleteTime = millis();
  }

  // Shutting off the bleed valve when the water level is detected as having reached the lower reed switch
  if(stateBleed==1 && abs(millis() - lastBleedIncompleteTime) > 5000) {
    stateBleed = 0;
    digitalWrite(BLEED_VALVE, HIGH);
    oledLine4 = "Bleed Status: Off";
  }

  //---------------- IR BUTTON READING ---------------- 

  // Checks whether any button on the infrared remote has been pressed and takes corresponding action
  if(irReceiver.decode(&infraredRemoteResults)){
    String buttonPressed = GetButtonName(infraredRemoteResults.value);
    if(buttonPressed == "1") {
        updateLineState(MAIN_LINE_1, stateLine1);
        oledLine1 = updateTextOLED(stateLine1, "Line 1 Status: ");   
    } else if (buttonPressed == "2") { 
        updateLineState(MAIN_LINE_2, stateLine2);
        oledLine2 = updateTextOLED(stateLine2, "Line 2 Status: "); 
    } else if (buttonPressed == "3") {
        updateLineState(MAIN_LINE_3, stateLine3);
        oledLine3 = updateTextOLED(stateLine3, "Line 3 Status: "); 
    } else if (buttonPressed == "*") {
        digitalWrite(BLEED_VALVE, stateBleed);
        digitalWrite(RO_PURIFIER, HIGH);
        stateBleed = (stateBleed+1)%2;
        oledLine4 = updateTextOLED(stateBleed, "Bleed Status: ");
    } 
    // Saves the new system state in EEPROM
    updateSystemState();
    // updates the OLED screen
    updateDisplayOLED();
    // Tells the infrared receiver to get ready for the next button press
    irReceiver.resume();
  }
  
}
/*
 * --------------- FUNCTIONS USED ---------------
 */
// This function helps control the switching on/off of the misting nozzle lines
void lineAction(byte selectedLine, unsigned int timerOffset) {
  
  // This variable holds the offseted line sequence
  byte offsetedLineSequence = lineSequence%6;
  // This variable contains the absolute time difference between now and when the current sparaying cycle was initiated
  unsigned long progressedCycleTime = abs(millis() - sprayCycleTimer);

  // Step 1: Deactivate the pump in case a line is toggled on full time
  if(offsetedLineSequence == 0 && progressedCycleTime > (0+timerOffset)) {
    digitalWrite(PRESSURE_PUMP, HIGH);
    lineSequence++;
  } // Step 2: Activate the solenoid valve for the selected line before switching the pressure pump on
  if(offsetedLineSequence == 1 && progressedCycleTime > (1000+timerOffset)) {
    if((selectedLine == MAIN_LINE_1 && stateLine1!=0) || (selectedLine == MAIN_LINE_2 && stateLine2!=0) || (selectedLine == MAIN_LINE_3 && stateLine3!=0) ) {
      digitalWrite(selectedLine, LOW);
    }
    lineSequence++;
  } // Step 3: Switch the pressure pump on
  else if(offsetedLineSequence == 2 && progressedCycleTime > (2000+timerOffset)) {
    if((selectedLine == MAIN_LINE_1 && stateLine1!=0) || (selectedLine == MAIN_LINE_2 && stateLine2!=0) || (selectedLine == MAIN_LINE_3 && stateLine3!=0) ) {
      digitalWrite(PRESSURE_PUMP, LOW);
    }
    lineSequence++;
  } // Step 4: Switch the pressure pump off
  else if(offsetedLineSequence == 3 && progressedCycleTime > (2000+LINE_SPRAY_TIME+timerOffset)) {
    if(!(selectedLine==MAIN_LINE_1 && stateLine1==1 || selectedLine==MAIN_LINE_2 && stateLine2==1 || selectedLine==MAIN_LINE_3 && stateLine3==1)) {
      digitalWrite(PRESSURE_PUMP, HIGH);
    }
    lineSequence++;
  } // Step 5: Deactivate the solenoid valve
  else if(offsetedLineSequence == 4 && progressedCycleTime > (3000+LINE_SPRAY_TIME+timerOffset)) {
    // if the current line has been toggled on do not shut it off
    if(!(selectedLine==MAIN_LINE_1 && stateLine1==1 || selectedLine==MAIN_LINE_2 && stateLine2==1 || selectedLine==MAIN_LINE_3 && stateLine3==1)) {
    digitalWrite(selectedLine, HIGH);
    }
    lineSequence++;
  }  // Step 6: Turn the pump back on in case it was deactivated and one line has been toggled on
  else if(offsetedLineSequence == 5 && progressedCycleTime > (4000+LINE_SPRAY_TIME+timerOffset)) {
    // Ensure that the pump is reactivated in case on of the lines has been toggled on
    if(stateLine1==1 || stateLine2==1 || stateLine3==1) {
    digitalWrite(PRESSURE_PUMP, LOW);
    }
    lineSequence++;
  }   
  
}

// This function helps return the updated line state and takes action on the the solenoid valvess and pump
void updateLineState(byte selectedLine, byte &currentState) {
  currentState = (currentState+1)%3;
  byte offsetedLineSequence = lineSequence%6;
  if(currentState == 1) {
    if(offsetedLineSequence == 0) {
      digitalWrite(PRESSURE_PUMP, LOW);
    }
    digitalWrite(selectedLine, LOW);
  } else {
    if(stateLine1!=1 && stateLine2!=1 && stateLine3!=1 && offsetedLineSequence == 0) {
      digitalWrite(PRESSURE_PUMP, HIGH);
    }
    digitalWrite(selectedLine, HIGH);
  }
  return currentState;
}

// Updates the text to display on the OLED display
String updateTextOLED(byte currentState, String oledLineText) {
  if(currentState == 0) {
    return oledLineText + "Off";
  } else if(currentState == 1) {
    return oledLineText + "On";
  } else if(currentState == 2) {
    return oledLineText + "Auto";
  } 
}

// This function writes the current system status to EEPROM
void updateSystemState() {
// Writes the lates system states to EEPROM memory
EEPROM.update(0, stateLine1);  
EEPROM.update(1, stateLine2); 
EEPROM.update(2, stateLine3);
EEPROM.update(4, stateBleed);  
}

// This function reads from EEPROM and updates the current system status
void restoreSystemState() {
// Reading the states from EEPROM memory
stateLine1 = EEPROM.read(0);
stateLine2 = EEPROM.read(1);
stateLine3 = EEPROM.read(2);
stateBleed = EEPROM.read(3);
// Checking to see whether they conform to the specified range
stateLine1 = stateLine1>2?2:stateLine1;
stateLine2 = stateLine2>2?2:stateLine2;
stateLine3 = stateLine3>2?2:stateLine3;
stateBleed = stateBleed>1?0:stateBleed;
// Updates the text to display on the OLED screen
oledLine1 = updateTextOLED(stateLine1, "Line 1 Status: ");
oledLine2 = updateTextOLED(stateLine2, "Line 2 Status: ");
oledLine3 = updateTextOLED(stateLine3, "Line 3 Status: ");
oledLine4 = updateTextOLED(stateBleed, "Bleed Status: ");
// Prints the different line states from EEPROM to the serial monitor
Serial.println(stateLine1);
Serial.println(stateLine2);
Serial.println(stateLine3);
Serial.println(stateBleed);
}

// This function decides which button on the IR remote was pressed
String GetButtonName(long raw_value){
  switch(raw_value){
      case 0xD7E84B1B: { return String("OK");} break;
      case 0x00511DBB: { return String("UP");} break;
      case 0xA3C8EDDB: { return String("DOWN");} break;
      case 0x52A3D41F: { return String("LEFT");} break;
      case 0x20FE4DBB: { return String("RIGHT"); } break;
      case 0xC101E57B: { return String("1"); } break;
      case 0x97483BFB: { return String("2"); } break;
      case 0xF0C41643: { return String("3"); } break;
      case 0x9716BE3F: { return String("4"); } break;
      case 0x3D9AE3F7: { return String("5"); } break;
      case 0x6182021B: { return String("6"); } break;
      case 0x8C22657B: { return String("7"); } break;
      case 0x488F3CBB: { return String("8"); } break;
      case 0x0449E79F: { return String("9"); } break;
      case 0x1BC0157B: { return String("0"); } break;
      case 0x32C6FDF7: { return String("*"); } break;
      case 0x3EC3FC1B: { return String("#"); } break;
      default: { return String("UNKNOWN");} break;
  }
}

// This function updates the statues screen on the OLED display
void updateDisplayOLED(){
  display.clearDisplay();
  display.setTextSize(1);  
  display.setCursor(0,0);
  display.println(oledLine1);
  display.println(oledLine2);
  display.println(oledLine3);
  display.println(oledLine4);
  display.display();
}

//-------------- END OF CODE ---------------
