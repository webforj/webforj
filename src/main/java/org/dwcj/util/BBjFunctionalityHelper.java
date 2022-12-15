package org.dwcj.util;

public class BBjFunctionalityHelper {
    
    public static byte[] byteArrayCreation(Boolean visible, Boolean enabled){

        byte bFlag = (byte)0x00;
        
        if(Boolean.FALSE.equals(visible)){
            bFlag += (byte)0x10;
        }
        if(Boolean.FALSE.equals(enabled)){
            bFlag += (byte)0x01;
        }

        return new byte[]{(byte)0x00, bFlag};

    }
}
