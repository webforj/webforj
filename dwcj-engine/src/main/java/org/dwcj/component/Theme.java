package org.dwcj.component;

import com.google.gson.annotations.SerializedName;

/**
 * Supported themes as defined by the Dwc Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public enum Theme implements ThemeBase {
  /* The danger theme as defined by the Dwc Theme Engine. */
  @SerializedName("danger")
  DANGER,

  /* The default theme as defined by the Dwc Theme Engine. */
  @SerializedName("default")
  DEFAULT,

  /* The gray theme as defined by the Dwc Theme Engine. */
  @SerializedName("gray")
  GRAY,

  /* The info theme as defined by the Dwc Theme Engine. */
  @SerializedName("info")
  INFO,

  /* The primary theme as defined by the Dwc Theme Engine. */
  @SerializedName("primary")
  PRIMARY,

  /* The success theme as defined by the Dwc Theme Engine. */
  @SerializedName("success")
  SUCCESS,

  /* The warning theme as defined by the Dwc Theme Engine. */
  @SerializedName("warning")
  WARNING;
}
