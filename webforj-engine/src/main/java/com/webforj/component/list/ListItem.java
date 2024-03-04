package com.webforj.component.list;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.UUID;

/**
 * Represents a list item, which consists of a key and text. List items are used in list components
 * and provide a way to associate unique keys with display text.
 *
 * @see ListBox
 * @see ComboBox
 * @see ChoiceBox
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ListItem {

  private Object key;
  private String text;
  private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);

  /**
   * Constructs a list item with the given key and text.
   *
   * @param key the key
   * @param text the text
   *
   * @throws IllegalArgumentException if the key is null
   */
  public ListItem(Object key, String text) {
    if (key == null) {
      throw new IllegalArgumentException("Key cannot be null");
    }

    this.key = key;
    this.text = text;
  }

  /**
   * Constructs a list item with the given text.
   *
   * @param text the text
   */
  public ListItem(String text) {
    this(UUID.randomUUID(), text);
  }

  /**
   * Gets the key of the list item.
   *
   * @return the key
   */
  public Object getKey() {
    return key;
  }

  /**
   * Gets the text of the list item.
   *
   * @return the text
   */
  public String getText() {
    return text;
  }

  /**
   * Sets the text of the list item.
   *
   * @param text the text to set
   * @return the list item
   */
  public ListItem setText(String text) {
    String oldValue = this.text;
    this.text = text;
    changeSupport.firePropertyChange("text", oldValue, text);

    return this;
  }

  /**
   * Adds a property change listener.
   *
   * @param listener the listener
   * @return the list item
   */
  ListItem addPropertyChangeListener(PropertyChangeListener listener) {
    this.changeSupport.addPropertyChangeListener(listener);
    return this;
  }
}
