package org.dwcj.bbj;

import java.math.BigDecimal;
import java.util.HashMap;

public class BBjVar {

    private final BigDecimal numVal;
    private final Integer intVal;
    private final String strVal;
    private final Object objVal;
    private final BBjGenericType type;

    public static enum BBjGenericType {
        NUMERIC,
        STRING,
        INTEGER,
        OBJECT
    }

    public BBjVar(BigDecimal numVal){
        this.numVal = numVal;
        this.intVal = null;
        this.strVal = null;
        this.objVal = null;
        this.type = BBjGenericType.NUMERIC;
    }

    public BBjVar(Double numVal){
        this.numVal = BigDecimal.valueOf(numVal);
        this.intVal = null;
        this.strVal = null;
        this.objVal = null;
        this.type = BBjGenericType.NUMERIC;
    }

    public BBjVar(Integer intVal){
        this.numVal = null;
        this.intVal = intVal;
        this.strVal = null;
        this.objVal = null;
        this.type = BBjGenericType.INTEGER;
    }

    public BBjVar(String strVal){
        this.numVal = null;
        this.intVal = null;
        this.strVal = strVal;
        this.objVal = null;
        this.type = BBjGenericType.STRING;
    }

    public BBjVar(Object objVal){
        this.numVal = null;
        this.intVal = null;
        this.strVal = null;
        this.objVal = objVal;
        this.type = BBjGenericType.OBJECT;
    }

    public BBjGenericType getType(){
        return this.type;
    }

    public BigDecimal getNumVal(){
        return numVal;
    }

    public String getStrVal(){
        return strVal;
    }

    public Object getObjVal(){
        return objVal;
    }

    public Integer getIntVal(){
        return intVal;
    }



}
