package org.dwcj.component.field;

/** Enum to descripe the Fields types. */
public enum FieldType {
  COLOR,
  DATE,
  DATETIME,
  EMAIL,
  FILE,
  MONTH,
  NUMBER,
  PASSWORD,
  RANGE,
  SEARCH,
  TEL, 
  TEXT, 
  TIME, 
  URL, 
  WEEK;
  
  @Override
  public String toString() {
    if (this == DATETIME) {
      return "datetime-local";
    }
    return super.toString().toLowerCase();
  }
}
