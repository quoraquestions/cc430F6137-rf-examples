/* --COPYRIGHT--,TI
 *MSP Source and Object Code Software License Agreement
 *
 *
 *IMPORTANT - PLEASE CAREFULLY READ THE FOLLOWING LICENSE AGREEMENT, WHICH IS LEGALLY BINDING.  AFTER YOU READ IT, YOU WILL BE ASKED WHETHER YOU ACCEPT AND AGREE TO ITS TERMS.  DO NOT CLICK  "I ACCEPT" UNLESS: (1) YOU WILL USE THE LICENSED MATERIALS FOR YOUR OWN BENEFIT AND PERSONALLY ACCEPT, AGREE TO AND INTEND TO BE BOUND BY THESE TERMS; OR (2) YOU ARE AUTHORIZED TO, AND INTEND TO BE BOUND BY, THESE TERMS ON BEHALF OF YOUR COMPANY.
 *
 *
 *Important - Read carefully: This Source and Object Code Software License Agreement ("Agreement") is a legal agreement between you and Texas Instruments Incorporated ("TI").  In this Agreement "you" means you personally if you will exercise the rights granted for your own benefit, but it means your company (or you on behalf of your company) if you will exercise the rights granted for your company's benefit.  The "Licensed Materials" subject to this Agreement include the software programs and any associated electronic documentation (in each case, in whole or in part) that accompany this Agreement, are set forth in the applicable software manifest and you access "on-line", as well as any updates or upgrades to such software programs or documentation, if any, provided to you at TI's sole discretion.  The Licensed Materials are specifically designed and licensed for use solely and exclusively with MSP microcontroller devices manufactured by or for TI ("TI Devices").  By installing, copying or otherwise using the Licensed Materials you agree to abide by the provisions set forth herein.  This Agreement is displayed for you to read prior to using the Licensed Materials.  If you choose not to accept or agree with these provisions, do not download or install the Licensed Materials.  
 *
 *Note Regarding Possible Access to Other Licensed Materials:  The Licensed Materials may be bundled with software and associated electronic documentation, if any, licensed under terms other than the terms of this Agreement (in whole or in part, "Other Licensed Materials"), including, for example Open Source Software and/or TI-owned or third party Proprietary Software licensed under such other terms.  "Open Source Software" means any software licensed under terms requiring that (A) other software ("Proprietary Software") incorporated, combined or distributed with such software or developed using such software: (i) be disclosed or distributed in source code form; or (ii) otherwise be licensed on terms inconsistent with the terms of this Agreement, including but not limited to permitting use of the Proprietary Software on or with devices other than TI Devices, or (B) require the owner of Proprietary Software to license any of its patents to users of the Open Source Software and/or Proprietary Software incorporated, combined or distributed with such Open Source Software or developed using such Open Source Software.  
 *
 *If by accepting this Agreement, you gain access to Other Licensed Materials, they will be listed in the applicable software manifest.  Your use of the Other Licensed Materials is subject to the applicable other licensing terms acknowledgements and disclaimers as specified in the applicable software manifest and/or identified or included with the Other Licensed Materials in the software bundle.  For clarification, this Agreement does not limit your rights under, or grant you rights that supersede, the terms of any applicable Other Licensed Materials license agreement.  If any of the Other Licensed Materials is Open Source Software that has been provided to you in object code only under terms that obligate TI to provide to you or show you where you can access the source code versions of such Open Source Software, TI will provide to you, or show you where you can access, such source code if you contact TI at Texas Instruments Incorporated, 12500 TI Boulevard, Mail Station 8638, Dallas, Texas 75243, Attention: Contracts Manager, Embedded Processing.  In the event you choose not to accept or agree with the terms in any applicable Other Licensed Materials license agreement, you must terminate this Agreement.
 *
 *1.	License Grant and Use Restrictions.
 *
 *a.	Licensed Materials License Grant.  Subject to the terms of this Agreement, TI hereby grants to you a limited, non-transferable, non-exclusive, non-assignable, non-sublicensable, fully paid-up and royalty-free license to:
 *
 *			i.	Limited Source Code License:  make copies, prepare derivative works, display internally and use internally the Licensed Materials provided to you in source code for the sole purpose of developing object and executable versions of such Licensed Materials, or any derivative thereof, that execute solely and exclusively on TI Devices, for end use in Licensee Products, and maintaining and supporting such Licensed Materials, or any derivative thereof, and Licensee Products.  For purposes of this Agreement, "Licensee Product" means a product that consists of both hardware, including one or more TI Devices, and software components, including only executable versions of the Licensed Materials that execute solely and exclusively on such TI Devices.
 *
 *			ii.	Object Code Evaluation, Testing and Use License:  make copies, display internally, distribute internally and use internally the Licensed Materials in object code for the sole purposes of evaluating and testing the Licensed Materials and designing and developing Licensee Products, and maintaining and supporting the Licensee Products;  
 *
 *			iii.	Demonstration License:  demonstrate to third parties the Licensed Materials executing solely and exclusively on TI Devices as they are used in Licensee Products, provided that such Licensed Materials are demonstrated in object or executable versions only and 
 *
 *		iv.	Production and Distribution License:  make, use, import, export and otherwise distribute the Licensed Materials as part of a Licensee Product, provided that such Licensee Products include only embedded executable copies of such Licensed Materials that execute solely and exclusively on TI Devices.
 *
 *	b.	Contractors.  The licenses granted to you hereunder shall include your on-site and off-site contractors (either an individual or entity), while such contractors are performing work for or providing services to you, provided that such contractors have executed work-for-hire agreements with you containing applicable terms and conditions consistent with the terms and conditions set forth in this Agreement and provided further that you shall be liable to TI for any breach by your contractors of this Agreement to the same extent as you would be if you had breached the Agreement yourself. 
 *
 *	c.	No Other License.  Nothing in this Agreement shall be construed as a license to any intellectual property rights of TI other than those rights embodied in the Licensed Materials provided to you by TI.  EXCEPT AS PROVIDED HEREIN, NO OTHER LICENSE, EXPRESS OR IMPLIED, BY ESTOPPEL OR OTHERWISE, TO ANY OTHER TI INTELLECTUAL PROPERTY RIGHTS IS GRANTED HEREIN.
 *
 *	d.	Covenant not to Sue.  During the term of this Agreement, you agree not to assert a claim against TI or its licensees that the Licensed Materials infringe your intellectual property rights.
 *
 *	e.	Restrictions.  You shall maintain the source code versions of the Licensed Materials under password control protection and shall not disclose such source code versions of the Licensed Materials, to any person other than your employees and contractors whose job performance requires access.  You shall not use the Licensed Materials with a processing device other than a TI Device, and you agree that any such unauthorized use of the Licensed Materials is a material breach of this Agreement.  You shall not use the Licensed Materials for the purpose of analyzing or proving infringement of any of your patents by either TI or TI's customers.  Except as expressly provided in this Agreement, you shall not copy, publish, disclose, display, provide, transfer or make available the Licensed Materials to any third party and you shall not sublicense, transfer, or assign the Licensed Materials or your rights under this Agreement to any third party.  You shall not mortgage, pledge or encumber the Licensed Materials in any way.  You may use the Licensed Materials with Open Source Software or with software developed using Open Source Software tools provided you do not incorporate, combine or distribute the Licensed Materials in a manner that subjects the Licensed Materials to any license obligations or any other intellectual property related terms of any license governing such Open Source Software. 
 *
 *	f.	Termination.  This Agreement is effective on the date the Licensed Materials are delivered to you together with this Agreement and will remain in full force and effect until terminated.  You may terminate this Agreement at any time by written notice to TI.  Without prejudice to any other rights, if you fail to comply with the terms of this Agreement or you are acquired, TI may terminate your right to use the Licensed Materials upon written notice to you.  Upon termination of this Agreement, you will destroy any and all copies of the Licensed Materials in your possession, custody or control and provide to TI a written statement signed by your authorized representative certifying such destruction. Except for Sections 1(a), 1(b) and 1(d), all provisions of this Agreement shall survive termination of this Agreement. 
 *
 *2.	Licensed Materials Ownership.  The Licensed Materials are licensed, not sold to you, and can only be used in accordance with the terms of this Agreement.  Subject to the licenses granted to you pursuant to this Agreement, TI and its licensors own and shall continue to own all right, title and interest in and to the Licensed Materials, including all copies thereof.  You agree that all fixes, modifications and improvements to the Licensed Materials conceived of or made by TI that are based, either in whole or in part, on your feedback, suggestions or recommendations are the exclusive property of TI and all right, title and interest in and to such fixes, modifications or improvements to the Licensed Materials will vest solely in TI.  Moreover, you acknowledge and agree that when your independently developed software or hardware components are combined, in whole or in part, with the Licensed Materials, your right to use the combined work that includes the Licensed Materials remains subject to the terms and conditions of this Agreement.
 *
 *3.	Intellectual Property Rights.  
 *
 *	a.	The Licensed Materials contain copyrighted material, trade secrets and other proprietary information of TI and its licensors and are protected by copyright laws, international copyright treaties, and trade secret laws, as well as other intellectual property laws.  To protect TI's and its licensors' rights in the Licensed Materials, you agree, except as specifically permitted by statute by a provision that cannot be waived by contract, not to "unlock", decompile, reverse engineer, disassemble or otherwise translate to a human-perceivable form any portions of the Licensed Materials provided to you in object code format only, nor permit any person or entity to do so.  You shall not remove, alter, cover, or obscure any confidentiality, trade secret, trade mark, patent, copyright or other proprietary notice or other identifying marks or designs from any component of the Licensed Materials and you shall reproduce and include in all copies of the Licensed Materials the copyright notice(s) and proprietary legend(s) of TI and its licensors as they appear in the Licensed Materials.  TI reserves all rights not specifically granted under this Agreement.
 *
 *	b.	Certain Licensed Materials may be based on industry recognized standards or software programs published by industry recognized standards bodies and certain third parties may claim to own patents, copyrights, and other intellectual property rights that cover implementation of those standards.  You acknowledge and agree that this Agreement does not convey a license to any such third party patents, copyrights, and other intellectual property rights and that you are solely responsible for any patent, copyright, or other intellectual property right claim that relates to your use or distribution of the Licensed Materials or your use or distribution of your products that include or incorporate the Licensed Materials.  Moreover, you acknowledge that you are responsible for any fees or royalties that may be payable to any third party based on such third party's interests in the Licensed Materials or any intellectual property rights that cover implementation of any industry recognized standard, any software program published by any industry recognized standards bodies or any other proprietary technology.
 *
 *4.	Confidential Information.  You acknowledge and agree that the Licensed Materials contain trade secrets and other confidential information of TI and its licensors.  You agree to use the Licensed Materials solely within the scope of the licenses set forth herein, to maintain the Licensed Materials in strict confidence, to use at least the same procedures and degree of care that you use to prevent disclosure of your own confidential information of like importance but in no instance less than reasonable care, and to prevent disclosure of the Licensed Materials to any third party, except as may be necessary and required in connection with your rights and obligations hereunder; provided, however, that you may not provide the Licensed Materials to any business organization or group within your company or to customers or contractors that design or manufacture semiconductors unless TI gives written consent.  You agree to obtain executed confidentiality agreements with your employees and contractors having access to the Licensed Materials and to diligently take steps to enforce such agreements in this respect.  TI may disclose your contact information to TI's licensors.
 *
 *5.	Warranties and Limitations.  THE LICENSED MATERIALS ARE PROVIDED "AS IS".  FURTHERMORE, YOU ACKNOWLEDGE AND AGREE THAT THE LICENSED MATERIALS HAVE NOT BEEN TESTED OR CERTIFIED BY ANY GOVERNMENT AGENCY OR INDUSTRY REGULATORY ORGANIZATION OR ANY OTHER THIRD PARTY ORGANIZATION.  YOU AGREE THAT PRIOR TO USING, INCORPORATING OR DISTRIBUTING THE LICENSED MATERIALS IN OR WITH ANY COMMERCIAL PRODUCT THAT YOU WILL THOROUGHLY TEST THE PRODUCT AND THE FUNCTIONALITY OF THE LICENSED MATERIALS IN OR WITH THAT PRODUCT AND BE SOLELY RESPONSIBLE FOR ANY PROBLEMS OR FAILURES.
 *
 *TI AND ITS LICENSORS MAKE NO WARRANTY OR REPRESENTATION, EITHER EXPRESS, IMPLIED OR STATUTORY, REGARDING THE LICENSED MATERIALS, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT OF ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADE SECRETS OR OTHER INTELLECTUAL PROPERTY RIGHTS.  YOU AGREE TO USE YOUR INDEPENDENT JUDGMENT IN DEVELOPING YOUR PRODUCTS.  NOTHING CONTAINED IN THIS AGREEMENT WILL BE CONSTRUED AS A WARRANTY OR REPRESENTATION BY TI TO MAINTAIN PRODUCTION OF ANY TI SEMICONDUCTOR DEVICE OR OTHER HARDWARE OR SOFTWARE WITH WHICH THE LICENSED MATERIALS MAY BE USED.  
 *
 *IN NO EVENT SHALL TI OR ITS LICENSORS, BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL, PUNITIVE OR CONSEQUENTIAL DAMAGES, HOWEVER CAUSED, ON ANY THEORY OF LIABILITY, IN CONNECTION WITH OR ARISING OUT OF THIS AGREEMENT OR THE USE OF THE LICENSED MATERIALS REGARDLESS OF WHETHER TI HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.  EXCLUDED DAMAGES INCLUDE, BUT ARE NOT LIMITED TO, COST OF REMOVAL OR REINSTALLATION, OUTSIDE COMPUTER TIME, LABOR COSTS, LOSS OF DATA, LOSS OF GOODWILL, LOSS OF PROFITS, LOSS OF SAVINGS, OR LOSS OF USE OR INTERRUPTION OF BUSINESS.  IN NO EVENT WILL TI'S OR ITS LICENSORS' AGGREGATE LIABILITY UNDER THIS AGREEMENT OR ARISING OUT OF YOUR USE OF THE LICENSED MATERIALS EXCEED FIVE HUNDRED U.S. DOLLARS (US$500).
 *
 *	Because some jurisdictions do not allow the exclusion or limitation of incidental or consequential damages or limitation on how long an implied warranty lasts, the above limitations or exclusions may not apply to you.
 *
 *6.	Indemnification Disclaimer.  YOU ACKNOWLEDGE AND AGREE THAT TI SHALL NOT BE LIABLE FOR AND SHALL NOT DEFEND OR INDEMNIFY YOU AGAINST ANY THIRD PARTY INFRINGEMENT CLAIM THAT RELATES TO OR IS BASED ON YOUR MANUFACTURE, USE, OR DISTRIBUTION OF THE LICENSED MATERIALS OR YOUR MANUFACTURE, USE, OFFER FOR SALE, SALE, IMPORTATION OR DISTRIBUTION OF YOUR PRODUCTS THAT INCLUDE OR INCORPORATE THE LICENSED MATERIALS.
 *
 *7.	No Technical Support.  TI and its licensors are under no obligation to install, maintain or support the Licensed Materials.  
 *
 *8.	Notices.  All notices to TI hereunder shall be delivered to Texas Instruments Incorporated, 12500 TI Boulevard, Mail Station 8638, Dallas, Texas 75243, Attention: Contracts Manager - Embedded Processing, with a copy to Texas Instruments Incorporated, 13588 N. Central Expressway, Mail Station 3999, Dallas, Texas 75243, Attention: Law Department - Embedded Processing.  All notices shall be deemed served when received by TI. 
 *
 *9.	Export Control.  The Licensed Materials are subject to export control under the U.S. Commerce Department's Export Administration Regulations ("EAR").  Unless prior authorization is obtained from the U.S. Commerce Department, neither you nor your subsidiaries shall export, re-export, or release, directly or indirectly (including, without limitation, by permitting the Licensed Materials to be downloaded), any technology, software, or software source code, received from TI, or export, directly or indirectly, any direct product of such technology, software, or software source code, to any person, destination or country to which the export, re-export, or release of the technology, software, or software source code, or direct product is prohibited by the EAR.  You represent and warrant that you (i) are not located in, or under the control of, a national or resident of Cuba, Iran, North Korea, Sudan and Syria or any other country subject to a U.S. goods embargo; (ii) are not on the U.S. Treasury Department's List of Specially Designated Nationals or the U.S. Commerce Department's Denied Persons List or Entity List; and (iii) will not use the Licensed Materials or transfer the Licensed Materials for use in any military, nuclear, chemical or biological weapons, or missile technology end-uses.  Any software export classification made by TI shall not be construed as a representation or warranty regarding the proper export classification for such software or whether an export license or other documentation is required for the exportation of such software.
 *
 *10.	Governing Law and Severability; Waiver.  This Agreement will be governed by and interpreted in accordance with the laws of the State of Texas, without reference to conflict of laws principles.  If for any reason a court of competent jurisdiction finds any provision of the Agreement to be unenforceable, that provision will be enforced to the maximum extent possible to effectuate the intent of the parties, and the remainder of the Agreement shall continue in full force and effect.  This Agreement shall not be governed by the United Nations Convention on Contracts for the International Sale of Goods, or by the Uniform Computer Information Transactions Act (UCITA).  The parties agree that non-exclusive jurisdiction for any dispute arising out of or relating to this Agreement lies within the courts located in the State of Texas.  Notwithstanding the foregoing, any judgment may be enforced in any United States or foreign court, and either party may seek injunctive relief in any United States or foreign court.  Failure by TI to enforce any provision of this Agreement shall not be deemed a waiver of future enforcement of that or any other provision in this Agreement or any other agreement that may be in place between the parties.
 *
 *11.	PRC Provisions.  If you are located in the People's Republic of China ("PRC") or if the Licensed Materials will be sent to the PRC, the following provisions shall apply:  
 *
 *	a.	Registration Requirements.  You shall be solely responsible for performing all acts and obtaining all approvals that may be required in connection with this Agreement by the government of the PRC, including but not limited to registering pursuant to, and otherwise complying with, the PRC Measures on the Administration of Software Products, Management Regulations on Technology Import-Export, and Technology Import and Export Contract Registration Management Rules.  Upon receipt of such approvals from the government authorities, you shall forward evidence of all such approvals to TI for its records.  In the event that you fail to obtain any such approval or registration, you shall be solely responsible for any and all losses, damages or costs resulting therefrom, and shall indemnify TI for all such losses, damages or costs.
 *
 *b.	Governing Language.  This Agreement is written and executed in the English language and shall be authoritative and controlling, whether or not translated into a language other than English to comply with law or for reference purposes.  If a translation of this Agreement is required for any purpose, including but not limited to registration of the Agreement pursuant to any governmental laws, regulations or rules, you shall be solely responsible for creating such translation.  
 *
 *12.	Contingencies.	TI shall not be in breach of this Agreement and shall not be liable for any non-performance or delay in performance if such non-performance or delay is due to a force majeure event or other circumstances beyond TI's reasonable control.
 *
 *13.		Entire Agreement.  This is the entire agreement between you and TI and this Agreement supersedes any prior agreement between the parties related to the subject matter of this Agreement.  Notwithstanding the foregoing, any signed and effective software license agreement relating to the subject matter hereof and stating expressly that such agreement shall control regardless of any subsequent click-wrap, shrink-wrap or web-wrap, shall supersede the terms of this Agreement.  No amendment or modification of this Agreement will be effective unless in writing and signed by a duly authorized representative of TI.  You hereby warrant and represent that you have obtained all authorizations and other applicable consents required empowering you to enter into this Agreement.
 *
 *
 *
 * --/COPYRIGHT--*/
/******************************************************************************
* CC430 RF Code Example - TX and RX (variable packet length > FIFO size)
*
* Simple RF Link to Toggle Receiver's LED by pressing Transmitter's Button    
* Warning: This RF code example is setup to operate at either 868 or 915 MHz, 
* which might be out of allowable range of operation in certain countries.
* The frequency of operation is selectable as an active build configuration
* in the project menu. 
* 
* Please refer to the appropriate legal sources before performing tests with 
* this code example. 
* 
* This code example can be loaded to 2 CC430 devices. Each device will transmit 
* a 100 byte packet upon a button pressed. Each device will also toggle its LED 
* upon receiving the packet(s). 
* 
* The RF packet engine settings specify variable-length-mode with CRC check 
* enabled. The RX packet also appends 2 status bytes regarding CRC check, RSSI 
* and LQI info. For specific register settings please refer to the comments for 
* each register in RfRegSettings.c, the CC430x613x User's Guide, and/or 
* SmartRF Studio.
* 
* G. Larmore
* Texas Instruments Inc.
* June 2012
* Built with IAR v5.40.1 and CCS v5.2
******************************************************************************/

#include "RF_Toggle_LED_Demo.h"

extern RF_SETTINGS rfSettings;

unsigned char packetReceived;
unsigned char packetTransmit; 

unsigned char txBytesLeft = PACKET_LEN+1;         // +1 for length byte 
unsigned char txPosition = 0; 
unsigned char rxBytesLeft = PACKET_LEN+2;         // +3 for len & status bytes
unsigned char rxPosition = 0;
unsigned char lengthByteRead = 0; 

unsigned char RxBufferLength = 0;
unsigned char TxBufferLength = 0; 
unsigned char * _p_Buffer = 0; 
unsigned char buttonPressed = 0;
unsigned int i = 0;                 

unsigned char transmitting = 0; 
unsigned char receiving = 0; 

unsigned char RxBuffer[PACKET_LEN+2] = {0};

unsigned char TxBuffer[PACKET_LEN+1]= {
PACKET_LEN, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 
            0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 
            0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 
            0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 
            0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 
            0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 
            0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 
            0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 
            0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 
            0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 
};

void main( void )
{  
  // Stop watchdog timer to prevent time out reset 
  WDTCTL = WDTPW + WDTHOLD; 
    
  // Increase PMMCOREV level to 2 for proper radio operation
  SetVCore(2);                               
  
  ResetRadioCore();
  InitButtonLeds();
  InitTimer(); 
    
  // Clean out the RX Buffer 
  rxPosition = PACKET_LEN+2;
  while(rxPosition--)
  {
    RxBuffer[rxPosition] = 0; 
  }

  InitRadio();  
  ReceiveOn(); 
    
  while (1)
  { 
    P1IE |= BIT7;                           // Enable button interrupt
    
    __bis_SR_register( LPM3_bits + GIE );   
    __no_operation(); 
    
    if (buttonPressed)                      // Process a button press->transmit 
    {            
      ReceiveOff();                         // Button means TX, stop RX
      receiving = 0;                         
      
      TransmitPacket();                 

      buttonPressed = 0;                    // Re-enable button press                           
    }    
    if(receiving)
    {
      ReceivePacket(); 
      __no_operation(); 
    }
    if(!transmitting)
    {
      ReceiveOn(); 
    }
  }
}

void ReceivePacket(void)
{
  rxBytesLeft = PACKET_LEN + 2;// Set maximum packet leng + 2 for appended bytes
  rxPosition = 0; 
  packetReceived = 0; 
  
  __delay_cycles(2800);                     // Wait for bytes to fill in RX FIFO

  TA0CCR1   = RX_TIMER_PERIOD;              // x cycles * 1/32768 = y us
  TA0CCTL1 |= CCIE;            
  TA0CTL   |= MC_2 + TACLR;                 // Start the timer- continuous mode

  __bis_SR_register(LPM3_bits + GIE); 
  __no_operation(); 
  
  TA0CCR1 = RX_TIMER_PERIOD;             
  TA0CCTL1 &= ~(CCIE);
  TA0CTL &= ~(MC_3);                  // Turn off timer 
  
  __no_operation(); 
}

void TransmitPacket(void)
{  
  P3OUT |= BIT6;                        // Pulse LED during Transmit 

  txBytesLeft = PACKET_LEN + 1;   
  txPosition = 0; 
  packetTransmit = 0; 
  transmitting = 1;       
  
  
  Strobe( RF_STX );                         // Strobe STX   
  
  TA0CCR1   = TX_TIMER_PERIOD;              // x cycles * 1/32768 = y us
  TA0CCTL1 |= CCIE; 
  TA0CTL |= MC_2 + TACLR;                   // Start the timer- continuous mode   

  __bis_SR_register(LPM3_bits + GIE); 
  __no_operation(); 
  
  TA0CCR1 = TX_TIMER_PERIOD;             // x cycles * 1/32768 = y us
  TA0CCTL1 &= ~(CCIE); 
  TA0CTL &= ~(MC_3);                  // Turn off timer         

  P3OUT &= ~BIT6;                     // Turn off LED after Transmit               
}

void ReceiveOn(void)
{  
  RF1AIES &= ~BIT9; 
  RF1AIFG = 0;                              // Clear pending RFIFG interrupts
  RF1AIE  |= BIT9;                          // Enable the sync word received interrupt
  
  // Radio is in IDLE following a TX, so strobe SRX to enter Receive Mode
  Strobe( RF_SRX );        
  
  __no_operation(); 
}

void ReceiveOff(void)
{
  RF1AIE &= ~BIT9;                          // Disable RX interrupts
  RF1AIFG &= ~BIT9;                         // Clear pending IFG
  RF1AIES &= ~BIT9;                         // Switch back to to sync word

  // It is possible that ReceiveOff is called while radio is receiving a packet.
  // Therefore, it is necessary to flush the RX FIFO after issuing IDLE strobe 
  // such that the RXFIFO is empty prior to receiving a packet.
  Strobe(RF_SIDLE); 
  Strobe(RF_SFRX);                       
}

//------------------------------------------------------------------------------
//  void pktRxHandler(void)
//
//  DESCRIPTION:
//      This function is called every time a timer interrupt occurs. The 
//      function starts by retreiving the status byte. Every time the status 
//      byte indicates that there are available bytes in the RX FIFO, bytes are 
//      read from the RX FIFO and written to RxBuffer. This is done until the 
//      whole packet is received. If the status byte indicates that there has 
//      been an RX FIFO overflow the RX FIFO is flushed. Please see the 
//      EM430F6137RF900 RF Examples User Manual for a flow chart describing this 
//      function.
//------------------------------------------------------------------------------
void pktRxHandler(void) {
  unsigned char RxStatus;
  unsigned char bytesInFifo;
  
  // Which state?
  RxStatus = Strobe(RF_SNOP);
  
  switch(RxStatus & CC430_STATE_MASK)
  {
    case CC430_STATE_RX:
      // If there's anything in the RX FIFO....
      if (bytesInFifo = MIN(rxBytesLeft, RxStatus & CC430_FIFO_BYTES_AVAILABLE_MASK))
      {         
        if((rxBytesLeft == PACKET_LEN + 2) && !lengthByteRead)
        {
          rxBytesLeft = ReadSingleReg(RXFIFO) + 2;  // For appended bytes
          lengthByteRead = 1; 
        }
        // Update how many bytes are left to be received
        rxBytesLeft -= bytesInFifo;
  
        // Read from RX FIFO and store the data in rxBuffer
        while (bytesInFifo--) {
          RxBuffer[rxPosition] = ReadSingleReg(RXFIFO);
          rxPosition++; 
        }  
        if (!rxBytesLeft){           
            packetReceived = 1; 
            receiving = 0;
            lengthByteRead = 0; 
            ReceiveOff();
            
            P1OUT ^= BIT0;                    // Toggle LED1             
        }      
      } 
      break;
    default:
      if(!packetReceived)
      {
        packetReceived = 1; 
      }
      
      rxBytesLeft = 0;
      receiving = 0; 
      ReceiveOff();

      break;  
  }
} // pktRxHandler

//------------------------------------------------------------------------------
//  void pktTxHandler(void)
//
//  DESCRIPTION:
//      This function is called every time a timer interrupt occurs. The function starts
//      by getting the status byte. Every time the status byte indicates that there 
//      is free space in the TX FIFO, bytes are taken from txBuffer and written to 
//      the TX FIFO until the whole packet is written or the TXFIFO has underflowed. 
//      See the EM430F6137RF900 RF Examples User Manual for a flow chart describing 
//      this function.
//------------------------------------------------------------------------------
void pktTxHandler(void) {
    unsigned char freeSpaceInFifo;
    unsigned char TxStatus;  
    
    // Which state?
    TxStatus = Strobe(RF_SNOP);

    switch (TxStatus & CC430_STATE_MASK) {
        case CC430_STATE_TX:
            // If there's anything to transfer..
            if (freeSpaceInFifo = MIN(txBytesLeft, TxStatus & CC430_FIFO_BYTES_AVAILABLE_MASK)) 
            {
              txBytesLeft -= freeSpaceInFifo;

              while(freeSpaceInFifo--)
              {
                WriteSingleReg(TXFIFO, TxBuffer[txPosition]);
                txPosition++; 
              }
              
              if(!txBytesLeft)
              {                
                RF1AIES |= BIT9;      // End-of-packet TX interrupt
                RF1AIFG &= ~BIT9;     // clear RFIFG9
                while(!(RF1AIFG & BIT9)); // poll RFIFG9 for TX end-of-packet  
                RF1AIES &= ~BIT9;      // End-of-packet TX interrupt
                RF1AIFG &= ~BIT9;     // clear RFIFG9
                transmitting = 0; 
                packetTransmit = 1; 
              }
            }
            break;

        case CC430_STATE_TX_UNDERFLOW:
            Strobe(RF_SFTX);  // Flush the TX FIFO
            
            __no_operation(); 
            // No break here!
        default:
            if(!packetTransmit) 
              packetTransmit = 1; 
            
            if (transmitting) {
                if ((TxStatus & CC430_STATE_MASK) == CC430_STATE_IDLE) {
    	          transmitting = 0; 
                }
            }
        break;
    }
} // pktTxHandler

void InitTimer(void)
{
  P5SEL |= 0x03;                            // Set xtal pins
  LFXT_Start(XT1DRIVE_0);
  
  TA0CCR1  = RX_TIMER_PERIOD;               // x cycles * 1/32768 = y us
  TA0CCTL1 = CCIE;                          // Enable interrupts
  TA0CTL   = TASSEL__ACLK + TACLR;          // ACLK source
}

void InitButtonLeds(void)
{
  // Set up the button as interruptible 
  P1DIR &= ~BIT7;
  P1REN |= BIT7;
  P1IES &= BIT7;
  P1IFG = 0;
  P1OUT |= BIT7;
  P1IE  |= BIT7; 

  // Initialize Port J
  PJOUT = 0x00;
  PJDIR = 0xFF; 

  // Set up LEDs 
  P1OUT &= ~BIT0;
  P1DIR |= BIT0;
  P3OUT &= ~BIT6;
  P3DIR |= BIT6;
}

void InitRadio(void)
{
  // Set the High-Power Mode Request Enable bit so LPM3 can be entered
  // with active radio enabled 
  PMMCTL0_H = 0xA5;
  PMMCTL0_L |= PMMHPMRE_L; 
  PMMCTL0_H = 0x00; 
  
  WriteRfSettings(&rfSettings);
  
  WriteSinglePATable(PATABLE_VAL);
}

/**************************************
* Interrupt Service Routines
**************************************/

#pragma vector=TIMER0_A1_VECTOR
__interrupt void TIMER0_A1_ISR(void)
{
  switch(__even_in_range(TA0IV,14))
  {
    case 0:  break;                  
    case 2:  
      if(receiving)
      {
        TA0CCR1 += RX_TIMER_PERIOD;                  // 16 cycles * 1/32768 = ~500 us

        pktRxHandler();  
        
        if(packetReceived)
          __bic_SR_register_on_exit(LPM3_bits); 
      }
      else if(transmitting)
      {
        TA0CCR1 += TX_TIMER_PERIOD;                  // 16 cycles * 1/32768 = ~500 us

        pktTxHandler(); 
        
        if(packetTransmit)
          __bic_SR_register_on_exit(LPM3_bits); 
      }       
      break;
    case 4:  break;                         // CCR2 not used
    case 6:  break;                         // Reserved not used
    case 8:  break;                         // Reserved not used
    case 10: break;                         // Reserved not used
    case 12: break;                         // Reserved not used
    case 14: break;                         // Overflow not used
  }
}

#pragma vector=CC1101_VECTOR
__interrupt void CC1101_ISR(void)
{
  switch(__even_in_range(RF1AIV,32))        // Prioritizing Radio Core Interrupt 
  {
    case  0: break;                         // No RF core interrupt pending                                            
    case  2: break;                         // RFIFG0 
    case  4: break;                         // RFIFG1
    case  6: break;                         // RFIFG2
    case  8: break;                         // RFIFG3
    case 10: break;                         // RFIFG4
    case 12: break;                         // RFIFG5
    case 14: break;                         // RFIFG6          
    case 16: break;                         // RFIFG7
    case 18: break;                         // RFIFG8
    case 20:                                // RFIFG9
      if(!(RF1AIES & BIT9))                 // RX sync word received 
      {
        receiving = 1;         
        __bic_SR_register_on_exit(LPM3_bits); // Exit active    
      }	   
      else while(1); 			    // trap 
      break;
    case 22: break;                         // RFIFG10
    case 24: break;                         // RFIFG11
    case 26: break;                         // RFIFG12
    case 28: break;                         // RFIFG13
    case 30: break;                         // RFIFG14
    case 32: break;                         // RFIFG15
  }      
}

#pragma vector=PORT1_VECTOR
__interrupt void PORT1_ISR(void)
{
  switch(__even_in_range(P1IV, 16))
  {
    case  0: break;
    case  2: break;                         // P1.0 IFG
    case  4: break;                         // P1.1 IFG
    case  6: break;                         // P1.2 IFG
    case  8: break;                         // P1.3 IFG
    case 10: break;                         // P1.4 IFG
    case 12: break;                         // P1.5 IFG
    case 14: break;                         // P1.6 IFG
    case 16:                                // P1.7 IFG
      __delay_cycles(1000);                 // debounce delay 
      buttonPressed = 1;
      P1IE = 0;                             // Debounce by disabling buttons
      P1IFG = 0; 
      __bic_SR_register_on_exit(LPM3_bits); // Exit active    
      break;
  }
}
