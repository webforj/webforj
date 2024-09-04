package com.webforj.component.icons;

import java.util.Locale;

/**
 * Enumeration of all Dwc core icons.
 *
 * <p>
 * DWC icons are a small subset of the Tabler icons.
 * </p>
 *
 * @see <a href="https://tablericons.com/">Tabler Icons</a>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public enum DwcIcon implements IconFactory {
  CALENDAR, CHECK, CHEVRON_DOWN, CHEVRON_LEFT, CHEVRON_RIGHT, CHEVRON_UP, CHEVRONS_LEFT, CHEVRONS_RIGHT, EYE_OFF, EYE, ICON_X, PLUS, MINUS, SQUARE_PLUS, SQUARE_MINUS, FILE, FOLDER, FOLDER_OPENED, STOP, QUESTION, WARNING, INFO, COLOR_PICKER, COPY, CHECKS, UPLOAD, REFRESH, PLAY, ANIMATED_SPINNER, ARROW_DOWN, ARROW_LEFT, ARROW_RIGHT, ARROW_UP, TYPOGRAPHY, GRIP_HORIZONTAL, GRIP_VERTICAL, LIST, GRID, HARD_DRIVE, COLUMNS, TIME;

  /**
   * {@inheritDoc}
   */
  @Override
  public Icon create() {
    return new Icon(String.valueOf(this), getPool());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return name().toLowerCase(Locale.ENGLISH).replace('_', '-');
  }
}
