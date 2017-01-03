----------------------------------CC430 RF Examples ---------------------------

The CC430 RF_Examples include the following projects:

1. Fixed_GT_FIFO              -- Packet length is fixed and greater than FIFO size (64 bytes)
2. Fixed_LT_FIFO              -- Packet length is fixed and less than FIFO size (64 bytes)
3. Variable_GT_FIFO           -- Packet length is variable and greater than FIFO size (64 bytes)
4. Variable_LT_FIFO           -- Packet length is variable and less than FIFO size (64 bytes)
5. Asynchronous_Communication -- RF TX/RX in asynchronous mode, internal connections to timerA
6. Synchronous_Communication  -- RF TX/RX in synchronous mode, internal & external connections to timerA
 
These projects were built on the EM430F6137RF900 platform, in both Code Composer Studio (CCS) & IAR.
Each project has two RF frequency build settings,  for 868MHz (ETSI) and 915MHz (FCC). 

The packet handler RF code examples (Fixed/Variable + GT/LT FIFO) require only 
one c file, utilizing the push button to transmit and otherwise receive.

The (a)synchronous communication examples require two c files, one to TX and 
one to RX. Hence these two projects  provide two build configurations (TX/RX)
for each frequency build setting.

The CCS workspace for RF_Examples is located at the RF_Examples_CCS folder, 
with individual RF Examples projects in their respective sub-folders. 

The IAR workspace file  for RF_Examples, RF_ExamplesIAR.www, is located in the 
RF_Examples_IAR folder, with individual RF Examples projects in their respective 
sub-folders.


