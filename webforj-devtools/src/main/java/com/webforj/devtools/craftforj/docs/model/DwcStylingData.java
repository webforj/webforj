package com.webforj.devtools.craftforj.docs.model;

import java.util.ArrayList;
import java.util.List;

/**
 * DWC styling data fetched from dwc.style.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class DwcStylingData {

  private List<Part> parts;
  private List<CssProperty> cssProperties;
  private List<Slot> slots;
  private List<ReflectedAttribute> reflects;
  private List<String> dependencies;

  /**
   * Creates a DwcStylingData with the given values.
   *
   * @param parts the parts
   * @param cssProperties the CSS properties
   * @param slots the slots
   * @param reflects the reflected attributes
   * @param dependencies the dependencies
   */
  public DwcStylingData(List<Part> parts, List<CssProperty> cssProperties, List<Slot> slots,
      List<ReflectedAttribute> reflects, List<String> dependencies) {
    this.parts = parts != null ? parts : new ArrayList<>();
    this.cssProperties = cssProperties != null ? cssProperties : new ArrayList<>();
    this.slots = slots != null ? slots : new ArrayList<>();
    this.reflects = reflects != null ? reflects : new ArrayList<>();
    this.dependencies = dependencies != null ? dependencies : new ArrayList<>();
  }

  /**
   * Gets the shadow parts.
   *
   * @return the parts
   */
  public List<Part> getParts() {
    return parts;
  }

  /**
   * Sets the shadow parts.
   *
   * @param parts the parts
   */
  public void setParts(List<Part> parts) {
    this.parts = parts;
  }

  /**
   * Gets the CSS custom properties.
   *
   * @return the CSS properties
   */
  public List<CssProperty> getCssProperties() {
    return cssProperties;
  }

  /**
   * Sets the CSS custom properties.
   *
   * @param cssProperties the CSS properties
   */
  public void setCssProperties(List<CssProperty> cssProperties) {
    this.cssProperties = cssProperties;
  }

  /**
   * Gets the slots.
   *
   * @return the slots
   */
  public List<Slot> getSlots() {
    return slots;
  }

  /**
   * Sets the slots.
   *
   * @param slots the slots
   */
  public void setSlots(List<Slot> slots) {
    this.slots = slots;
  }

  /**
   * Gets the reflected attributes.
   *
   * @return the reflected attributes
   */
  public List<ReflectedAttribute> getReflects() {
    return reflects;
  }

  /**
   * Sets the reflected attributes.
   *
   * @param reflects the reflected attributes
   */
  public void setReflects(List<ReflectedAttribute> reflects) {
    this.reflects = reflects;
  }

  /**
   * Gets the component dependencies.
   *
   * @return the dependencies
   */
  public List<String> getDependencies() {
    return dependencies;
  }

  /**
   * Sets the component dependencies.
   *
   * @param dependencies the dependencies
   */
  public void setDependencies(List<String> dependencies) {
    this.dependencies = dependencies;
  }

  /**
   * Checks if this data has any styling information.
   *
   * @return true if any styling data is present
   */
  public boolean hasData() {
    return (parts != null && !parts.isEmpty())
        || (cssProperties != null && !cssProperties.isEmpty())
        || (slots != null && !slots.isEmpty()) || (reflects != null && !reflects.isEmpty())
        || (dependencies != null && !dependencies.isEmpty());
  }

  /**
   * A shadow part.
   */
  public static class Part {

    private String name;
    private String description;

    /**
     * Creates a Part with the given values.
     *
     * @param name the name
     * @param description the description
     */
    public Part(String name, String description) {
      this.name = name;
      this.description = description;
    }

    /**
     * Gets the part name.
     *
     * @return the name
     */
    public String getName() {
      return name;
    }

    /**
     * Sets the part name.
     *
     * @param name the name
     */
    public void setName(String name) {
      this.name = name;
    }

    /**
     * Gets the part description.
     *
     * @return the description
     */
    public String getDescription() {
      return description;
    }

    /**
     * Sets the part description.
     *
     * @param description the description
     */
    public void setDescription(String description) {
      this.description = description;
    }

  }

  /**
   * A CSS custom property.
   */
  public static class CssProperty {

    private String name;
    private String description;

    /**
     * Creates a CssProperty with the given values.
     *
     * @param name the name
     * @param description the description
     */
    public CssProperty(String name, String description) {
      this.name = name;
      this.description = description;
    }

    /**
     * Gets the property name.
     *
     * @return the name
     */
    public String getName() {
      return name;
    }

    /**
     * Sets the property name.
     *
     * @param name the name
     */
    public void setName(String name) {
      this.name = name;
    }

    /**
     * Gets the property description.
     *
     * @return the description
     */
    public String getDescription() {
      return description;
    }

    /**
     * Sets the property description.
     *
     * @param description the description
     */
    public void setDescription(String description) {
      this.description = description;
    }

  }

  /**
   * A slot.
   */
  public static class Slot {

    private String name;
    private String description;

    /**
     * Creates a Slot with the given values.
     *
     * @param name the name
     * @param description the description
     */
    public Slot(String name, String description) {
      this.name = name;
      this.description = description;
    }

    /**
     * Gets the slot name.
     *
     * @return the name
     */
    public String getName() {
      return name;
    }

    /**
     * Sets the slot name.
     *
     * @param name the name
     */
    public void setName(String name) {
      this.name = name;
    }

    /**
     * Gets the slot description.
     *
     * @return the description
     */
    public String getDescription() {
      return description;
    }

    /**
     * Sets the slot description.
     *
     * @param description the description
     */
    public void setDescription(String description) {
      this.description = description;
    }

  }

  /**
   * A reflected attribute.
   */
  public static class ReflectedAttribute {

    private String name;
    private String type;
    private String description;

    /**
     * Creates a ReflectedAttribute with the given values.
     *
     * @param name the name
     * @param type the type
     * @param description the description
     */
    public ReflectedAttribute(String name, String type, String description) {
      this.name = name;
      this.type = type;
      this.description = description;
    }

    /**
     * Gets the attribute name.
     *
     * @return the name
     */
    public String getName() {
      return name;
    }

    /**
     * Sets the attribute name.
     *
     * @param name the name
     */
    public void setName(String name) {
      this.name = name;
    }

    /**
     * Gets the attribute type.
     *
     * @return the type
     */
    public String getType() {
      return type;
    }

    /**
     * Sets the attribute type.
     *
     * @param type the type
     */
    public void setType(String type) {
      this.type = type;
    }

    /**
     * Gets the attribute description.
     *
     * @return the description
     */
    public String getDescription() {
      return description;
    }

    /**
     * Sets the attribute description.
     *
     * @param description the description
     */
    public void setDescription(String description) {
      this.description = description;
    }

  }
}
