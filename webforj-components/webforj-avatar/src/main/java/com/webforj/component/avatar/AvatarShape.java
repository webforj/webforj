package com.webforj.component.avatar;

import com.google.gson.annotations.SerializedName;

/**
 * Supported avatar shapes as defined by the Dwc Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
public enum AvatarShape {
  /** Circle shape (default). */
  @SerializedName("circle")
  CIRCLE,

  /** Square shape. */
  @SerializedName("square")
  SQUARE;
}
