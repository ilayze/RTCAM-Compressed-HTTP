package com.jce;

import java.util.*;

import org.jnetpcap.Pcap;
import org.jnetpcap.packet.Payload;
import org.jnetpcap.packet.PcapPacket;
import org.jnetpcap.packet.PcapPacketHandler;
import org.jnetpcap.protocol.tcpip.Tcp;

/**
 * This example is similar to the classic libpcap example shown in nearly every
 * tutorial on libpcap. The main difference is that a file is opened instead of
 * a live network interface. Using a packet handler it goes into a loop to read
 * a few packets, say 10. Prints some simple info about the packets, and then
 * closes the pcap handle and exits.
 *
 * Here is the output generated by this example :
 *
 * Opening file for reading: tests/test-l2tp.pcap
 * Received at Tue Jan 27 16:17:17 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:17 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:18 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:18 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:19 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:19 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:20 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:20 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:21 EST 2004 caplen=114  len=114  jNetPcap rocks!
 * Received at Tue Jan 27 16:17:21 EST 2004 caplen=114  len=114  jNetPcap rocks!
 *
 * @author Mark Bednarczyk
 * @author Sly Technologies, Inc.
 */
public class OfflineCapturer {

    List<String> packetsPayload = new ArrayList<String>();


    /**
     * Main startup method
     *
     * @param args
     *          ignored
     */
    public static void main(String[] args) {
        OfflineCapturer oc=new OfflineCapturer();
        List<String> payloads = oc.Capture("resources/outside.tcpdump",9);
        System.out.println(payloads);
    }

    public static int counter =0;

    public List<String> Capture(){
        return Capture("resources/outside.tcpdump",1000);
    }

    public List<String> Capture(String fileName,int numberOfPackets) {


        /***************************************************************************
         * First we setup error buffer and name for our file
         **************************************************************************/
        final StringBuilder errbuf = new StringBuilder(); // For any error msgs
        final String file = fileName;

        System.out.printf("Opening file for reading: %s%n", file);

        /***************************************************************************
         * Second we open up the selected file using openOffline call
         **************************************************************************/
        Pcap pcap = null;
        try {
            pcap = Pcap.openOffline(file, errbuf);
        } catch (Exception e) {
            e.printStackTrace();
        }

        if (pcap == null) {
            System.err.printf("Error while opening device for capture: "
                    + errbuf.toString());
            return null;
        }

        /***************************************************************************
         * Third we create a packet handler which will receive packets from the
         * libpcap loop.
         **************************************************************************/
        PcapPacketHandler<String> jpacketHandler = new PcapPacketHandler<String>() {

            public void nextPacket(PcapPacket packet, String user) {
                counter++;
                String payloadAsString="";
                Payload payloadHeader = new Payload();
                if(packet.hasHeader(payloadHeader) && payloadHeader.size()>0) {
                     payloadAsString = payloadHeader.getUTF8String(0, payloadHeader.size()); // offset, length
                }
                System.out.printf("Received at %s caplen=%-4d len=%-4d payload: %s,payload length %s\n",
                        new Date(packet.getCaptureHeader().timestampInMillis()),
                        packet.getCaptureHeader().caplen(), // Length actually captured
                        packet.getCaptureHeader().wirelen(),
                        payloadAsString,// Original length
                        payloadAsString.length()
                );

                if( payloadAsString.length()>10)
                    packetsPayload.add(payloadAsString);


            }


        };

        /***************************************************************************
         * Fourth we enter the loop and tell it to capture 10 packets. The loop
         * method does a mapping of pcap.datalink() DLT value to JProtocol ID, which
         * is needed by JScanner. The scanner scans the packet buffer and decodes
         * the headers. The mapping is done automatically, although a variation on
         * the loop method exists that allows the programmer to sepecify exactly
         * which protocol ID to use as the data link type for this pcap interface.
         **************************************************************************/
        try {
            pcap.loop(numberOfPackets, jpacketHandler, "");
            return packetsPayload;
        } finally {
            /***************************************************************************
             * Last thing to do is close the pcap handle
             **************************************************************************/
            pcap.close();

        }
    }
}

