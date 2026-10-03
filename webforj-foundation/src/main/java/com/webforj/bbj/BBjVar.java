package com.webforj.bbj;

import java.math.BigDecimal;

/**
 * A value passed to or returned from BBj, holding a number, an integer, a string or an object.
 *
 * @author Stephan Wald
 * @since 0.006
 */
public class BBjVar {

  private final BigDecimal numVal;
  private final Integer intVal;
  private final String strVal;
  private final Object objVal;
  private final BBjGenericType type;

  /**
   * The type of the value held by a {@link BBjVar}.
   *
   * @since 0.006
   */
  public enum BBjGenericType {
    NUMERIC, STRING, INTEGER, OBJECT
  }

  /**
   * Creates a numeric value.
   *
   * @param numVal the numeric value
   */
  public BBjVar(BigDecimal numVal) {
    this.numVal = numVal;
    this.intVal = null;
    this.strVal = null;
    this.objVal = null;
    this.type = BBjGenericType.NUMERIC;
  }

  /**
   * Creates a numeric value.
   *
   * @param numVal the numeric value
   */
  public BBjVar(Double numVal) {
    this.numVal = BigDecimal.valueOf(numVal);
    this.intVal = null;
    this.strVal = null;
    this.objVal = null;
    this.type = BBjGenericType.NUMERIC;
  }

  /**
   * Creates an integer value.
   *
   * @param intVal the integer value
   */
  public BBjVar(Integer intVal) {
    this.numVal = null;
    this.intVal = intVal;
    this.strVal = null;
    this.objVal = null;
    this.type = BBjGenericType.INTEGER;
  }

  /**
   * Creates a string value.
   *
   * @param strVal the string value
   */
  public BBjVar(String strVal) {
    this.numVal = null;
    this.intVal = null;
    this.strVal = strVal;
    this.objVal = null;
    this.type = BBjGenericType.STRING;
  }

  /**
   * Creates an object value.
   *
   * @param objVal the object value
   */
  public BBjVar(Object objVal) {
    this.numVal = null;
    this.intVal = null;
    this.strVal = null;
    this.objVal = objVal;
    this.type = BBjGenericType.OBJECT;
  }

  public BBjGenericType getType() {
    return this.type;
  }

  public BigDecimal getNumVal() {
    return numVal;
  }

  public String getStrVal() {
    return strVal;
  }

  public Object getObjVal() {
    return objVal;
  }

  public Integer getIntVal() {
    return intVal;
  }



}
