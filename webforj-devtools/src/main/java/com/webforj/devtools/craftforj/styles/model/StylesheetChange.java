package com.webforj.devtools.craftforj.styles.model;


/**
 * A single stylesheet change.
 *
 * <p>
 * The {@link Type type} selects the operation. {@link Type#EDIT} replaces {@code oldText} with
 * {@code newText}, {@link Type#PREPEND} inserts {@code text} at the top of the file,
 * {@link Type#APPEND} adds {@code text} at the end and {@link Type#REGION} replaces the fenced
 * region named by {@code region} with {@code text}. Changes apply in list order and the whole list
 * fails atomically.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StylesheetChange {

  /**
   * Defines the stylesheet change operations.
   */
  public enum Type {

    /**
     * Replaces {@code oldText} with {@code newText}. The {@code oldText} must match the current
     * file content exactly once.
     */
    EDIT,

    /**
     * Inserts {@code text} at the top of the file, the place for {@code @import} rules, which must
     * precede all other rules.
     */
    PREPEND,

    /**
     * Adds {@code text} at the end of the file.
     */
    APPEND,

    /**
     * Replaces the region named by {@code region} with {@code text}, adding the region at the end
     * of the file when it is not there yet and removing it when the text is blank. The same region
     * may be written repeatedly without the duplicate checks the other operations apply.
     */
    REGION,

    /**
     * Replaces the whole file with {@code text}. Requires a base version.
     */
    REPLACE
  }

  private Type type;
  private String oldText;
  private String newText;
  private String text;
  private String region;
  private RegionPlacement placement;

  /**
   * Gets the change type.
   *
   * @return the change type
   */
  public Type getType() {
    return type;
  }

  /**
   * Sets the change type.
   *
   * @param type the change type
   */
  public void setType(Type type) {
    this.type = type;
  }

  /**
   * Gets the text to replace.
   *
   * @return the text to replace
   */
  public String getOldText() {
    return oldText;
  }

  /**
   * Sets the text to replace.
   *
   * @param oldText the text to replace
   */
  public void setOldText(String oldText) {
    this.oldText = oldText;
  }

  /**
   * Gets the replacement text.
   *
   * @return the replacement text
   */
  public String getNewText() {
    return newText;
  }

  /**
   * Sets the replacement text.
   *
   * @param newText the replacement text
   */
  public void setNewText(String newText) {
    this.newText = newText;
  }

  /**
   * Gets the text to add.
   *
   * @return the text to add
   */
  public String getText() {
    return text;
  }

  /**
   * Sets the text to add.
   *
   * @param text the text to add
   */
  public void setText(String text) {
    this.text = text;
  }

  /**
   * Gets the name of the region the change owns.
   *
   * @return the region name, or {@code null} when the change is not a region write
   */
  public String getRegion() {
    return region;
  }

  /**
   * Sets the name of the region the change owns.
   *
   * @param region the region name
   */
  public void setRegion(String region) {
    this.region = region;
  }

  /**
   * Where a region the file does not carry yet is put.
   *
   * @return the placement, or {@code null} for the default
   */
  public RegionPlacement getPlacement() {
    return placement;
  }

  /**
   * Sets where a region the file does not carry yet is put.
   *
   * @param placement the placement
   */
  public void setPlacement(RegionPlacement placement) {
    this.placement = placement;
  }
}
