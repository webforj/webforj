package com.webforj.devtools.craftforj.inspector.model;

/**
 * Represents a select option with value and display label.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SelectOption {

  private String value;
  private String label;

  /**
   * Creates a SelectOption with the given values.
   *
   * @param value the value
   * @param label the label
   */
  public SelectOption(String value, String label) {
    this.value = value;
    this.label = label;
  }

  /**
   * Gets the option value (sent back to server).
   *
   * @return the value
   */
  public String getValue() {
    return value;
  }

  /**
   * Sets the option value.
   *
   * @param value the value
   */
  public void setValue(String value) {
    this.value = value;
  }

  /**
   * Gets the display label (shown in UI).
   *
   * @return the label
   */
  public String getLabel() {
    return label;
  }

  /**
   * Sets the display label.
   *
   * @param label the label
   */
  public void setLabel(String label) {
    this.label = label;
  }

}
