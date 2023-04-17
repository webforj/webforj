package org.dwcj.component.field;

/** Enum to descripe the Fields types. */
public enum FieldType {
  /** A control for specifying a color; opening a color picker when active. */
  COLOR,

  /** A control for entering a date (year, month, and day, with no time). 
   * Opens a date picker or numeric wheels for year, month, day when active. 
   */
  DATE,
  
  /** A control for entering a date and time, with no time zone. 
   * Opens a date picker or numeric wheels for date- and time-components when active. 
   */
  DATETIME,
  
  /** A field for editing an email address. 
   * Looks like a text input, but has validation parameters. 
   */
  EMAIL,
  
  /** A control that lets the user select a file. 
   * Use the accept attribute to define the types of files that the control can select.
   */
  FILE,
  
  /** A control for entering a month and year, with no time zone. */
  MONTH,

  /** A control for entering a number. 
   * Displays a spinner and adds default validation. 
   */
  NUMBER,

  /** A single-line text field whose value is obscured. */
  PASSWORD,

  /** A control for entering a number whose exact value is not important. 
   * Displays as a range widget defaulting to the middle value. 
   * Used in conjunction min and max to define the range of acceptable values.
   */
  RANGE,

  /** A single-line text field for entering search strings. 
   * Line-breaks are automatically removed from the input value. 
   * May include a delete icon in supporting browsers that can be used to clear the field. 
   * Displays a search icon instead of enter key on some devices with dynamic keypads. 
   */
  SEARCH,

  /** A control for entering a telephone number. */
  TEL,

  /** The default value. 
   * A single-line text field. 
   * Line-breaks are automatically removed from the input value. 
   */
  TEXT,

  /** A control for entering a time value with no time zone. */
  TIME,

  /** A field for entering a URL. 
   * Looks like a text input, but has validation parameters.
   */
  URL,

  /** A control for entering a date consisting 
   * of a week-year number and a week number with no time zone. 
   */
  WEEK;

  @Override
  public String toString() {
    if (this == DATETIME) {
      return "datetime-local";
    }
    return super.toString().toLowerCase();
  }
}
